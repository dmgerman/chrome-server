;;; chrome-server-chatgpt.el --- ChatGPT backend for chrome-server

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: browser, http, org, chatgpt
;; Homepage: https://github.com/dmgerman

;;; Commentary:

;; ChatGPT-specific backend for chrome-server.
;; Provides the POST /chatgpt endpoint which saves a full ChatGPT conversation
;; to ~/sync/chatgpt/<basename>/ as an org file, converting HTML turns via
;; pandoc.  Embedded images are extracted to the same directory.
;; Recapturing the same conversation refreshes the existing directory.
;;
;; Payload:
;;   { "version": 1, "payload": { "url": "...", "title": "...",
;;     "turns": [ { "role": "...", "html": "...", "text": "..." } ] } }

;;; Code:

(require 'chrome-server)

;; ── Configuration ────────────────────────────────────────────────────────────

(defvar chrome-server-chatgpt-dir "~/sync/chatgpt"
  "Directory where ChatGPT conversation org files are saved.")

(defvar chrome-server-pandoc-executable "pandoc"
  "Path to the pandoc executable used for HTML → org conversion.")

;; ── HTML → Org conversion ─────────────────────────────────────────────────────

(defun chrome-server-chatgpt--strip-svg (html)
  "Remove all <svg>...</svg> elements from HTML string."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward "<svg[[:space:]\n][^>]*>" nil t)
      (let ((start (match-beginning 0)))
        (when (re-search-forward "</svg>" nil t)
          (delete-region start (point)))))
    (buffer-string)))

(defun chrome-server-chatgpt--min-heading-level (html)
  "Return the smallest HTML heading level (1-6) found in HTML, or nil if none."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (let ((min-level nil))
        (while (re-search-forward "<h\\([1-6]\\)\\b" nil t)
          (let ((lvl (string-to-number (match-string 1))))
            (when (or (null min-level) (< lvl min-level))
              (setq min-level lvl))))
        min-level))))

(defun chrome-server-chatgpt--html-to-org (html media-dir)
  "Convert HTML string to org format via pandoc.
Images are extracted to MEDIA-DIR via pandoc's --extract-media flag.
Headings are shifted so the topmost heading in HTML becomes org level 3,
keeping pandoc output nested under the surrounding level-2 turn heading.
Signals an error if pandoc is not found or exits non-zero."
  (unless (executable-find chrome-server-pandoc-executable)
    (error "chrome-server-chatgpt: pandoc not found (set chrome-server-pandoc-executable)"))
  (with-temp-buffer
    (let* ((min-lvl (chrome-server-chatgpt--min-heading-level html))
           (shift   (if min-lvl (- 3 min-lvl) 0))
           (exit-code (call-process-region (chrome-server-chatgpt--strip-svg html) nil
                                          chrome-server-pandoc-executable
                                          nil t nil
                                          "-f" "html" "-t" "org"
                                          "--wrap=none"
                                          (format "--shift-heading-level-by=%d" shift)
                                          (format "--extract-media=%s" media-dir))))
      (unless (zerop exit-code)
        (error "chrome-server-chatgpt: pandoc failed (exit %d): %s"
               exit-code (buffer-string)))
      ;; Remove headings that contain only org radio targets (e.g. "** <<_r_nh_>>")
      ;; and strip inline radio targets — both come from invisible HTML anchors
      ;; that pandoc converts to org <<target>> syntax.
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ +<<[^>]+>>\\s-*$" nil t)
        (delete-region (line-beginning-position)
                       (min (1+ (line-end-position)) (point-max))))
      (goto-char (point-min))
      (while (re-search-forward "<<[^>]+>>" nil t)
        (replace-match ""))
      (buffer-string))))

;; ── Endpoint ─────────────────────────────────────────────────────────────────

(defservlet chatgpt application/json (path query request)
  "Handle POST /chatgpt — save a ChatGPT conversation to ~/sync/chatgpt/."
  (condition-case err
      (let* ((data    (chrome-server--parse-request request))
             (payload (plist-get data :payload)))
        (unless payload
          (error "chrome-server-chatgpt: missing 'payload' key in request"))
        (let ((file (chrome-server-chatgpt--save payload)))
          (chrome-server--respond 200 "ok" (format "Saved to %s" file))))
    (error
     (chrome-server--respond 500 "error" (error-message-string err)))))

;; ── Conversation saving ───────────────────────────────────────────────────────

(defun chrome-server-chatgpt--id (url)
  "Extract the conversation ID from a ChatGPT URL, or nil if not found.
Handles both direct conversations (chatgpt.com/c/<id>) and project
conversations (chatgpt.com/g/<project>/c/<id>)."
  (when (string-match "chatgpt\\.com/\\(?:g/[^/?#]+/\\)?c/\\([^/?#]+\\)" url)
    (match-string 1 url)))

(defun chrome-server-chatgpt--sanitize-title (title)
  "Sanitize TITLE for use as a filename component (max 40 chars)."
  (let* ((s (downcase title))
         (s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
         (s (replace-regexp-in-string "^-+\\|-+$" "" s)))
    (truncate-string-to-width s 40)))

(defun chrome-server-chatgpt--find-existing-dir (root id)
  "Return the full path of an existing conversation directory for ID under ROOT.
Returns nil when ID is \"unknown\" (to avoid false matches) or when not found."
  (unless (string= id "unknown")
    (car (seq-filter #'file-directory-p
                     (file-expand-wildcards (format "%s/*-%s-*" root id))))))

(defun chrome-server-chatgpt--format-turn (turn conv-dir)
  "Format a conversation TURN as org text.
User turns: ** heading from first line, remaining lines as body.
  If the first line exceeds 100 characters the heading is truncated with
  ellipsis and the full first line is prepended to the body.
Assistant turns: plain body text.
Converts :html via pandoc (extracting images to CONV-DIR); falls back to :text."
  (let* ((role (plist-get turn :role))
         (html (plist-get turn :html))
         (raw  (plist-get turn :text))
         (body (string-trim
                (if (and html (not (string-empty-p html)))
                    (condition-case err
                        (chrome-server-chatgpt--html-to-org html conv-dir)
                      (error
                       (message "chrome-server-chatgpt: HTML conversion failed, using plain text: %s"
                                (error-message-string err))
                       (or raw "")))
                  (or raw "")))))
    (if (string= role "user")
        (let* ((lines      (split-string body "\n" t))
               (first-line (or (car lines) ""))
               (rest       (cdr lines))
               (heading    (if (> (length first-line) 100)
                               (concat (substring first-line 0 100) "…")
                             first-line))
               (body-lines (if (> (length first-line) 100)
                               (cons first-line rest)
                             rest)))
          (if body-lines
              (format "** %s\n\n%s\n\n" heading (string-join body-lines "\n"))
            (format "** %s\n\n" heading)))
      (format "%s\n\n" body))))

(defun chrome-server-chatgpt--save-html (turns file)
  "Save raw HTML of TURNS to FILE as a minimal HTML document."
  (with-temp-file file
    (insert "<!DOCTYPE html>\n<html><body>\n")
    (dolist (turn turns)
      (let ((role (plist-get turn :role))
            (html (plist-get turn :html)))
        (insert (format "<section data-role=\"%s\">\n%s\n</section>\n\n"
                        role (or html "")))))
    (insert "</body></html>\n")))

(defun chrome-server-chatgpt--save (payload)
  "Save ChatGPT conversation PAYLOAD to a per-item directory, refreshing if it already exists.
Each conversation lives in <root>/<basename>/<basename>.{org,html} plus any
extracted images.  Returns the path of the org file written."
  (let* ((url   (plist-get payload :url))
         (title (or (plist-get payload :title) "chatgpt-conversation"))
         (turns (plist-get payload :turns))
         (id    (or (chrome-server-chatgpt--id url)
                    (progn
                      (message "chrome-server-chatgpt: could not extract ID from URL: %s" url)
                      "unknown")))
         (root  (expand-file-name chrome-server-chatgpt-dir))
         (conv-dir (or (chrome-server-chatgpt--find-existing-dir root id)
                       (expand-file-name
                        (format "%s-%s-%s"
                                (format-time-string "%Y%m%d-%H%M%S")
                                id
                                (chrome-server-chatgpt--sanitize-title title))
                        root)))
         (basename  (file-name-nondirectory conv-dir))
         (file      (expand-file-name (concat basename ".org")  conv-dir))
         (html-file (expand-file-name (concat basename ".html") conv-dir)))
    (condition-case err
        (make-directory conv-dir t)
      (error
       (error "chrome-server-chatgpt: could not create directory %s: %s"
              conv-dir (error-message-string err))))
    (unless turns
      (error "chrome-server-chatgpt: payload contains no 'turns'"))
    (condition-case err
        (chrome-server-chatgpt--save-html turns html-file)
      (error
       (message "chrome-server-chatgpt: could not save HTML file %s: %s"
                html-file (error-message-string err))))
    (condition-case err
        (with-temp-file file
          (insert (format "#+title: %s\n" title))
          (insert (format "#+chatgpt_id: %s\n" id))
          (insert (format "#+chatgpt_url: %s\n" url))
          (insert (format "#+created: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (format "[[%s][Open in ChatGPT]]\n\n" url))
          (insert (format "* %s\n\n" title))
          (dolist (turn turns)
            (insert (chrome-server-chatgpt--format-turn turn conv-dir))))
      (error
       (error "chrome-server-chatgpt: could not write org file %s: %s"
              file (error-message-string err))))
    file))

(provide 'chrome-server-chatgpt)

;;; chrome-server-chatgpt.el ends here
