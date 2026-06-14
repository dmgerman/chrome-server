;;; chrome-server-youtube.el --- YouTube backend for chrome-server  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Version: 0.5
;; Keywords: comm, tools, browser, org, multimedia
;; URL: https://github.com/dmgerman/chrome-server
;; Package-Requires: ((emacs "27.1") (chrome-server "0.5"))

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; YouTube backend for chrome-server.  Registers two request handlers:
;;
;;   YOUTUBE             — capture a video into a configurable org file
;;   YOUTUBE_TRANSCRIPT  — download subtitles and save as an org file
;;                         with clickable timestamp links
;;
;; Required configuration:
;;   (setq chrome-server-youtube-api-key       "YOUR-API-KEY")
;;   (setq chrome-server-youtube-videos-file   "~/org/videos.org")
;;   (setq chrome-server-youtube-transcript-dir "~/sync/youtube")
;;
;; Payload (YOUTUBE):
;;   { "url": "...", "title": "...", "text": "...", "raise": false }
;;
;; Payload (YOUTUBE_TRANSCRIPT):
;;   { "url": "...", "title": "...", "raise": false }

;;; Code:

(require 'chrome-server)
(require 'url-util)
(require 'json)

;; Forward declarations for dynamic vars and functions from org-capture/org.
(defvar org-capture-templates)
(declare-function org-display-inline-images "org" (&optional include-linked refresh beg end))
(declare-function org-link-preview-region   "ol"  (&optional arg interactive? beg end))
(declare-function org-capture               "org-capture" (&optional goto keys))

;; ── Configuration ─────────────────────────────────────────────────────────────

(defvar chrome-server-youtube-api-key nil
  "YouTube Data API v3 key used to fetch video metadata.
Obtain one at https://console.cloud.google.com/ and set this variable.")

(defvar chrome-server-youtube-videos-file "~/org/videos.org"
  "Org file where YouTube video entries are captured.")

(defvar chrome-server-youtube-transcript-dir "~/sync/youtube"
  "Directory where YouTube transcript org files are saved.")

(defvar chrome-server-youtube-transcript-yt-dlp-executable "yt-dlp"
  "Path to the yt-dlp executable.")

;; ── URL helpers ───────────────────────────────────────────────────────────────

(defun chrome-server-youtube--video-id (url)
  "Return the YouTube video ID from URL, or nil."
  (cond
   ((string-match "[?&]v=\\([^&]+\\)" url) (match-string 1 url))
   ((string-match "youtu\\.be/\\([^?&]+\\)" url) (match-string 1 url))
   (t nil)))

(defun chrome-server-youtube--sanitize-title (title)
  "Sanitize TITLE for use as a filename component (max 40 chars)."
  (let* ((s (downcase title))
         (s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
         (s (replace-regexp-in-string "^-+\\|-+$" "" s)))
    (truncate-string-to-width s 40)))

;; ── Metadata fetching ─────────────────────────────────────────────────────────

(defun chrome-server-youtube--fetch-oembed (url)
  "Fetch YouTube oEmbed metadata for URL.  Returns a hash-table or nil."
  (let ((api-url (format "https://www.youtube.com/oembed?url=%s&format=json"
                         (url-hexify-string url))))
    (with-current-buffer (url-retrieve-synchronously api-url t t 5)
      (goto-char (point-min))
      (re-search-forward "\n\n" nil t)
      (json-parse-buffer))))

(defun chrome-server-youtube--fetch-api-info (url)
  "Fetch YouTube Data API v3 metadata for URL.
Returns the first item hash-table, or signals an error."
  (let ((video-id (chrome-server-youtube--video-id url)))
    (unless video-id
      (error "Could not extract video ID from: %s" url))
    (unless chrome-server-youtube-api-key
      (error "Chrome-server-youtube-api-key is not set"))
    (let* ((api-url (format "https://www.googleapis.com/youtube/v3/videos?part=snippet,contentDetails,statistics&id=%s&key=%s"
                            video-id chrome-server-youtube-api-key))
           (coding-system-for-read 'binary)
           (buf (url-retrieve-synchronously api-url t t)))
      (unless buf (error "Failed to retrieve YouTube info"))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\n\n" nil 'move)
            (let* ((raw  (buffer-substring-no-properties (point) (point-max)))
                   (text (decode-coding-string raw 'utf-8)))
              (with-temp-buffer
                (insert text)
                (goto-char (point-min))
                (let ((json-object-type 'hash-table)
                      (json-array-type  'list)
                      (json-key-type    'string))
                  (elt (gethash "items" (json-read)) 0)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;; ── Duration / chapter helpers ────────────────────────────────────────────────

(defun chrome-server-youtube--duration-to-minutes (duration)
  "Convert ISO 8601 DURATION (e.g. PT1H20M19S) to a minutes string.
Returns empty string if DURATION is nil or empty."
  (if (or (not duration) (string-empty-p duration))
      ""
    (let* ((hours   (if (string-match "\\([0-9]+\\)H" duration)
                        (string-to-number (match-string 1 duration)) 0))
           (minutes (if (string-match "\\([0-9]+\\)M" duration)
                        (string-to-number (match-string 1 duration)) 0))
           (seconds (if (string-match "\\([0-9]+\\)S" duration)
                        (string-to-number (match-string 1 duration)) 0)))
      (format "%.1f" (+ (* hours 60.0) minutes (/ seconds 60.0))))))

(defun chrome-server-youtube--timestamp-to-seconds (ts)
  "Convert a timestamp string TS (e.g. \"1:23:45\" or \"3:55\") to total seconds."
  (let* ((parts (mapcar #'string-to-number (split-string ts ":")))
         (len   (length parts)))
    (cond
     ((= len 1) (nth 0 parts))
     ((= len 2) (+ (* (nth 0 parts) 60) (nth 1 parts)))
     ((= len 3) (+ (* (nth 0 parts) 3600) (* (nth 1 parts) 60) (nth 2 parts)))
     (t 0))))

(defun chrome-server-youtube--convert-chapters (description url)
  "Convert chapter timestamps in DESCRIPTION into org links pointing to URL."
  (let* ((video-id (chrome-server-youtube--video-id url))
         (base-url (format "https://www.youtube.com/watch?v=%s" video-id)))
    (string-join
     (mapcar (lambda (line)
               (if (string-match "^\\([0-9:]+\\)[ \t]+\\(.*\\)$" line)
                   (let* ((ts      (match-string 1 line))
                          (label   (match-string 2 line))
                          (seconds (chrome-server-youtube--timestamp-to-seconds ts)))
                     (format "[[%s&t=%ss][%s %s]]" base-url seconds ts label))
                 line))
             (split-string description "\n"))
     "\n")))

;; ── Org entry builder ─────────────────────────────────────────────────────────

(defun chrome-server-youtube--build-entry (url title selection oembed api-info video-id)
  "Build and return an org capture entry string for a YouTube video.
URL, TITLE, SELECTION, and VIDEO-ID identify and seed the entry.
OEMBED and API-INFO may be nil if the respective fetch failed."
  (let* ((title       (if (and (string= title url) oembed (gethash "title" oembed))
                          (gethash "title" oembed) title))
         (title-clean (replace-regexp-in-string "|" ":" title t t))
         (ytline      (if video-id (format "[[yt:%s]]\n" video-id) ""))
         (snippet     (and api-info (gethash "snippet"        api-info)))
         (details     (and api-info (gethash "contentDetails" api-info)))
         (author      (and oembed   (gethash "author_name"    oembed)))
         (author-url  (and oembed   (gethash "author_url"     oembed)))
         (duration    (chrome-server-youtube--duration-to-minutes
                       (and details (gethash "duration" details))))
         (date        (and snippet (gethash "publishedAt"          snippet)))
         (description (chrome-server-youtube--convert-chapters
                       (or (and snippet (gethash "description" snippet)) "")
                       url))
         (category    (and snippet (gethash "categoryId"           snippet)))
         (language    (and snippet (gethash "defaultAudioLanguage" snippet)))
         (sel-text    (if (and selection (not (string-empty-p selection)))
                          (concat "\n" selection "\n") "")))
    (concat "* TODO " title-clean "\n"
            ":PROPERTIES:\n"
            ":URL: "      url "\n"
            ":LANG: "     (or language "") "\n"
            ":CATEGORY: " (or category "") "\n"
            ":LENGTH: "   duration "\n"
            ":AUTHOR:  "  (or author "") "\n"
            ":CHANNEL:  " (or author-url "") "\n"
            ":PDATE: "    (or date "") "\n"
            ":END:\n\n"
            ytline
            "\nThe *Description*:\n\n"
            description
            sel-text
            "\n")))

;; ── YOUTUBE handler ───────────────────────────────────────────────────────────

(defun chrome-server-youtube--handle-youtube (payload)
  "Handle YOUTUBE request with PAYLOAD.
Schedules the capture and returns immediately (respond-fast-then-defer)."
  (chrome-server--require-payload payload)
  (chrome-server-defer #'chrome-server-youtube--capture payload)
  (chrome-server--ok "Video capture started"))

(defun chrome-server-youtube--capture (payload)
  "Capture a YouTube video from PAYLOAD into `chrome-server-youtube-videos-file'."
  (condition-case err
      (let ((url   (plist-get payload :url))
            (title (or (plist-get payload :title) ""))
            (text  (or (plist-get payload :text) "")))
        (unless url
          (error "Missing url in payload"))
        (chrome-server--maybe-raise payload)
        (chrome-server-youtube--video-for-later url title text))
    (error
     (chrome-server--warn "youtube capture failed: %s"
                          (error-message-string err)))))

(defun chrome-server-youtube--refresh-inline-images ()
  "Refresh inline images when visiting `chrome-server-youtube-videos-file'."
  (when (string-match (regexp-quote (expand-file-name chrome-server-youtube-videos-file))
                      (or (buffer-file-name) ""))
    ;; org-display-inline-images was deprecated in Org 9.8.  Prefer
    ;; org-link-preview-region when available; fall back to the legacy name.
    (if (fboundp 'org-link-preview-region)
        (org-link-preview-region nil nil (point-min) (point-max))
      (with-suppressed-warnings ((obsolete org-display-inline-images))
        (org-display-inline-images)))))

(defun chrome-server-youtube--video-for-later (url title selection)
  "Capture YouTube video URL into `chrome-server-youtube-videos-file'.
URL is the watch URL.  TITLE is the user-supplied title (falls back to
the oEmbed title if URL was sent in its place).  SELECTION is text
quoted from the page, if any.
Fetches metadata from oEmbed and the YouTube Data API, then runs
`org-capture'."
  (let* ((video-id   (chrome-server-youtube--video-id url))
         (api-info   (condition-case err
                         (chrome-server-youtube--fetch-api-info url)
                       (error
                        (chrome-server--warn "API fetch failed (continuing without): %s"
                                             (error-message-string err))
                        nil)))
         (oembed     (condition-case err
                         (chrome-server-youtube--fetch-oembed url)
                       (error
                        (chrome-server--warn "oEmbed fetch failed (continuing without): %s"
                                             (error-message-string err))
                        nil)))
         (entry-text (chrome-server-youtube--build-entry
                      url title selection oembed api-info video-id))
         (org-capture-templates
          `(("v" "Video for later" entry
             (file ,chrome-server-youtube-videos-file)
             ,entry-text
             :empty-lines-after 1
             :empty-lines-before 1
             :immediate-finish t
             :after-finalize chrome-server-youtube--refresh-inline-images))))
    (org-capture nil "v")))

;; ── YOUTUBE_TRANSCRIPT handler ────────────────────────────────────────────────

(defun chrome-server-youtube--handle-transcript (payload)
  "Handle YOUTUBE_TRANSCRIPT request with PAYLOAD.
Schedules the transcript fetch and returns immediately
\(respond-fast-then-defer)."
  (chrome-server--require-payload payload)
  (unless (plist-get payload :url)
    (error "Missing url in payload"))
  (chrome-server-defer #'chrome-server-youtube--transcript-download-and-save payload)
  (chrome-server--ok "Transcript download started"))

;; ── yt-dlp helpers ───────────────────────────────────────────────────────────

(defun chrome-server-youtube--transcript-get-info (url)
  "Run yt-dlp -J URL and return parsed JSON hash-table."
  (unless (executable-find chrome-server-youtube-transcript-yt-dlp-executable)
    (error "Yt-dlp not found (set chrome-server-youtube-transcript-yt-dlp-executable)"))
  (let ((default-directory (expand-file-name "~/")))
    (with-temp-buffer
      (let ((exit-code (call-process chrome-server-youtube-transcript-yt-dlp-executable
                                     nil t nil "-J" "--no-playlist" url)))
        (unless (zerop exit-code)
          (error "Yt-dlp -J failed (exit %d)" exit-code))
        (goto-char (point-min))
        (let ((json-object-type 'hash-table)
              (json-array-type  'list)
              (json-key-type    'string))
          (json-read))))))

(defun chrome-server-youtube--transcript-effective-lang (info lang)
  "Return the subtitle language code actually available in INFO for LANG.
Tries exact match first, then base language (e.g. \"en\" for \"en-US\").
Returns nil if nothing is found."
  (let ((manual (gethash "subtitles"          info))
        (auto   (gethash "automatic_captions" info))
        (base   (car (split-string lang "-"))))
    (cond
     ((or (and manual (gethash lang manual))
          (and auto   (gethash lang auto)))  lang)
     ((or (and manual (gethash base manual))
          (and auto   (gethash base auto)))  base)
     (t nil))))

(defun chrome-server-youtube--transcript-download-vtt (url lang conv-dir video-id)
  "Download VTT subtitles for URL in LANG into CONV-DIR.
VIDEO-ID is used as the output filename stem.
Returns the path of the downloaded VTT file, or nil if not found."
  (let ((default-directory conv-dir))
    (with-temp-buffer
      (call-process chrome-server-youtube-transcript-yt-dlp-executable
                    nil t nil
                    "--write-sub" "--write-auto-sub"
                    "--sub-lang"    lang
                    "--sub-format"  "vtt"
                    "--skip-download"
                    "--no-playlist"
                    "-o" (expand-file-name video-id conv-dir)
                    url)))
  (car (file-expand-wildcards (format "%s/*.vtt" conv-dir))))

;; ── VTT → Org conversion ─────────────────────────────────────────────────────

(defun chrome-server-youtube--transcript-vtt-time-to-seconds (ts)
  "Convert VTT timestamp TS (form HH:MM:SS.mmm) to total seconds (integer)."
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" ts)
      (+ (* (string-to-number (match-string 1 ts)) 3600)
         (* (string-to-number (match-string 2 ts)) 60)
         (string-to-number (match-string 3 ts)))
    0))

(defun chrome-server-youtube--transcript-format-timestamp (ts)
  "Format VTT timestamp TS (form HH:MM:SS.mmm) as H:MM:SS for display."
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" ts)
      (let ((h (string-to-number (match-string 1 ts)))
            (m (match-string 2 ts))
            (s (match-string 3 ts)))
        (if (zerop h)
            (format "%s:%s" m s)
          (format "%d:%s:%s" h m s)))
    ts))

(defun chrome-server-youtube--transcript-strip-vtt-tags (text)
  "Strip VTT/HTML markup tags (including word-level timing) from TEXT."
  (string-trim (replace-regexp-in-string "<[^>]+>" "" text)))

(defun chrome-server-youtube--transcript-cue-text (lines)
  "Extract display text from VTT cue LINES (in document order).
YouTube auto-captions use a rolling 2-line window: line 1 repeats the
previous caption; line 2 carries new content with <c> word-level timing.
When a timing-annotated line is present, use only that line so we get
the new content without the repeated prefix.  For plain subtitles with no
timing tags, join all non-blank lines."
  (let ((timing-line (seq-find (lambda (l) (string-match-p "<c>" l)) lines)))
    (chrome-server-youtube--transcript-strip-vtt-tags
     (if timing-line
         timing-line
       (string-join (seq-filter (lambda (l) (not (string-empty-p (string-trim l)))) lines)
                    " ")))))

(defun chrome-server-youtube--transcript-vtt-to-org (vtt-content url)
  "Convert VTT-CONTENT to org lines with clickable timestamp links for URL."
  (let* ((video-id (chrome-server-youtube--video-id url))
         (base-url (format "https://www.youtube.com/watch?v=%s" video-id))
         (lines    (split-string vtt-content "\n"))
         (result   '())
         (prev-text   nil)
         (current-ts  nil)
         (current-txt '())
         (past-header nil))
    (dolist (line lines)
      (cond
       ((string-match "^WEBVTT" line)
        nil)
       ((and (not past-header) (string-match "^[A-Za-z-]+:" line))
        nil)
       ((string-empty-p line)
        (setq past-header t)
        (when (and current-ts current-txt)
          (let ((text (chrome-server-youtube--transcript-cue-text (reverse current-txt))))
            (unless (or (string-empty-p text)
                        (string= text prev-text))
              (push (cons current-ts text) result)
              (setq prev-text text)))
          (setq current-ts nil current-txt '())))
       ((string-match "^\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9]+\\) --> " line)
        (setq past-header t)
        (setq current-ts (match-string 1 line)
              current-txt '()))
       ((string-match "^[0-9]+$" line)
        nil)
       (current-ts
        (push line current-txt))))
    (when (and current-ts current-txt)
      (let ((text (chrome-server-youtube--transcript-cue-text (reverse current-txt))))
        (unless (or (string-empty-p text)
                    (string= text prev-text))
          (push (cons current-ts text) result))))
    (mapconcat
     (lambda (cue)
       (let* ((ts      (car cue))
              (text    (cdr cue))
              (seconds (chrome-server-youtube--transcript-vtt-time-to-seconds ts))
              (display (chrome-server-youtube--transcript-format-timestamp ts)))
         (format "[[%s&t=%ss][%s]] %s" base-url seconds display text)))
     (reverse result)
     "\n")))

;; ── Transcript file I/O ───────────────────────────────────────────────────────

(defun chrome-server-youtube--transcript-find-existing-dir (root video-id)
  "Return existing transcript directory for VIDEO-ID under ROOT.
Returns nil when no matching directory is found."
  (unless (string= video-id "unknown")
    (car (seq-filter #'file-directory-p
                     (file-expand-wildcards (format "%s/*-%s-*" root video-id))))))

(defun chrome-server-youtube--transcript-make-dir (payload info)
  "Create and return the per-transcript directory path for PAYLOAD and INFO."
  (let* ((url      (plist-get payload :url))
         (title    (or (plist-get payload :title)
                       (and info (gethash "title" info))
                       "youtube-transcript"))
         (video-id (or (chrome-server-youtube--video-id url) "unknown"))
         (root     (expand-file-name chrome-server-youtube-transcript-dir))
         (conv-dir (or (chrome-server-youtube--transcript-find-existing-dir root video-id)
                       (expand-file-name
                        (format "%s-%s-%s"
                                (format-time-string "%Y%m%d-%H%M%S")
                                video-id
                                (chrome-server-youtube--sanitize-title title))
                        root))))
    (make-directory conv-dir t)
    conv-dir))

(defun chrome-server-youtube--transcript-write-stub (payload info lang)
  "Write a stub org file noting no transcript was available.
PAYLOAD supplies the URL and title; INFO is the parsed yt-dlp metadata
hash-table (or nil if the call failed); LANG is the requested language
code that turned out to have no captions."
  (condition-case err
      (let* ((url        (plist-get payload :url))
             (title      (or (plist-get payload :title)
                             (and info (gethash "title" info))
                             "YouTube transcript"))
             (manual     (and info (gethash "subtitles"          info)))
             (auto       (and info (gethash "automatic_captions" info)))
             (avail-man  (and manual (hash-table-keys manual)))
             (avail-auto (and auto   (hash-table-keys auto)))
             (conv-dir   (chrome-server-youtube--transcript-make-dir payload info))
             (basename   (file-name-nondirectory conv-dir))
             (file       (expand-file-name (concat basename ".org") conv-dir)))
        (message "No subs for lang=%s; manual=%S auto=%S"
                 lang avail-man avail-auto)
        (with-temp-file file
          (insert (format "#+title: %s\n" title))
          (insert (format "#+youtube_url: %s\n" url))
          (insert (format "#+created: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (format "[[%s][Open in YouTube]]\n\n" url))
          (insert (format "No transcript available (language: %s).\n" lang))
          (when avail-man
            (insert (format "Manual subtitles available: %s\n" (string-join avail-man ", "))))
          (when avail-auto
            (insert (format "Auto captions available: %s\n" (string-join avail-auto ", "))))))
    (error
     (chrome-server--warn "could not write stub: %s"
                          (error-message-string err)))))

(defun chrome-server-youtube--transcript-download-and-save (payload)
  "Fetch info, download transcript for PAYLOAD's URL and save to org.
If no transcript is available, writes a stub org file and logs to *Messages*."
  (condition-case err
      (let* ((url      (plist-get payload :url))
             (title    (or (plist-get payload :title) "YouTube transcript"))
             (info     (chrome-server-youtube--transcript-get-info url))
             (lang     (or (gethash "language" info) "en"))
             (eff-lang (chrome-server-youtube--transcript-effective-lang info lang)))
        (if (not eff-lang)
            (progn
              (chrome-server-youtube--transcript-write-stub payload info lang)
              (message "No transcript for %s (lang: %s)" url lang))
          (let* ((video-id (or (chrome-server-youtube--video-id url) "unknown"))
                 (title    (or (and info (gethash "title" info)) title))
                 (conv-dir (chrome-server-youtube--transcript-make-dir payload info))
                 (basename (file-name-nondirectory conv-dir))
                 (org-file (expand-file-name (concat basename ".org") conv-dir))
                 (vtt-path (chrome-server-youtube--transcript-download-vtt
                            url eff-lang conv-dir video-id)))
            (unless vtt-path
              (error "chrome-server-youtube: VTT file not found after download"))
            (let* ((vtt-dest    (expand-file-name (concat basename ".vtt") conv-dir))
                   (vtt-content (with-temp-buffer
                                  (insert-file-contents vtt-path)
                                  (buffer-string)))
                   (api-info   (condition-case e
                                   (chrome-server-youtube--fetch-api-info url)
                                 (error
                                  (chrome-server--warn "API fetch failed (continuing without): %s"
                                                       (error-message-string e))
                                  nil)))
                   (oembed     (condition-case e
                                   (chrome-server-youtube--fetch-oembed url)
                                 (error
                                  (chrome-server--warn "oEmbed fetch failed (continuing without): %s"
                                                       (error-message-string e))
                                  nil)))
                   (entry      (replace-regexp-in-string
                                "^\\* TODO " "* "
                                (chrome-server-youtube--build-entry
                                 url title "" oembed api-info video-id))))
              (unless (string= vtt-path vtt-dest)
                (rename-file vtt-path vtt-dest t))
              (with-temp-file org-file
                (insert (format "#+title: %s\n" title))
                (insert (format "#+youtube_url: %s\n" url))
                (insert (format "#+youtube_lang: %s\n" eff-lang))
                (insert (format "#+created: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
                (insert entry)
                (insert "\n** Transcript\n\n")
                (insert (chrome-server-youtube--transcript-vtt-to-org vtt-content url))
                (insert "\n")))
            (chrome-server--maybe-raise payload)
            (message "Transcript saved to %s" org-file))))
    (error
     (chrome-server--warn "transcript failed: %s" (error-message-string err)))))

;; ── Register handlers ────────────────────────────────────────────────────────

(chrome-server-register-handler "YOUTUBE"             #'chrome-server-youtube--handle-youtube)
(chrome-server-register-handler "YOUTUBE_TRANSCRIPT"  #'chrome-server-youtube--handle-transcript)

(provide 'chrome-server-youtube)

;;; chrome-server-youtube.el ends here
