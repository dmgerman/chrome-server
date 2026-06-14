;;; chrome-server.el --- WebSocket bridge to a Chrome MV3 extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Version: 0.5
;; Keywords: comm, tools, browser, org
;; URL: https://github.com/dmgerman/chrome-server
;; Package-Requires: ((emacs "27.1") (websocket "1.13"))

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

;; Provides a local WebSocket server that exchanges JSON frames with a
;; Chrome (MV3) extension.  Frames carry one of two shapes:
;;
;;   Request : { "id": "<uuid>", "name": "NAME",   "payload": {...} }
;;   Response: { "requestId": "<uuid>",            "payload": {...} }
;;
;; Request names are SCREAMING_SNAKE_CASE.  Incoming requests are
;; dispatched to handlers registered with
;; `chrome-server-register-handler'.  Outgoing requests are made with
;; `chrome-server-request-async' (callback-based) or
;; `chrome-server-request' (sync wrapper using `accept-process-output').
;;
;; Built-in handlers:
;;
;;   ORG_CAPTURE       -- org-capture (template key configurable)
;;   ORG_ROAM_CAPTURE  -- standard org-roam-capture
;;   EWW               -- open URL in eww
;;
;; Per-feature backends register additional handlers:
;;
;;   chrome-server-chatgpt.el  -- CHATGPT
;;   chrome-server-www.el      -- SAVE_PAGE
;;   chrome-server-youtube.el  -- YOUTUBE, YOUTUBE_TRANSCRIPT
;;
;; Usage:
;;   (require 'chrome-server)
;;   (chrome-server-start)   ; start the server
;;   (chrome-server-stop)    ; stop the server

;;; Code:

(require 'websocket)
(require 'json)
(require 'org-id)
(require 'cl-lib)

;; Forward declarations.  These dynamic variables belong to org-capture and
;; org-roam, which are not necessarily loaded when this file is byte-compiled.
;; Without the defvar declarations a `let' on `org-capture-initial' would be
;; treated as a lexical binding and `org-capture' would never see the value.
(defvar org-capture-initial)
(declare-function org-capture          "org-capture" (&optional goto keys))
(declare-function org-roam-capture-    "org-roam"    (&rest args))
(declare-function org-roam-node-create "org-roam"    (&rest args))

;; ── Configuration ────────────────────────────────────────────────────────────

(defvar chrome-server-port 9130
  "Port the Chrome server WebSocket server listens on.")

(defvar chrome-server-host 'local
  "Host the WebSocket server binds to.  `local' = 127.0.0.1.")

(defvar chrome-server-org-capture-key nil
  "Org-capture template key used by the ORG_CAPTURE handler.
nil means the user selects the template interactively.")

(defvar chrome-server-request-timeout 10
  "Seconds to wait for a response to an Emacs-initiated request before timing out.")

(defvar chrome-server-debug nil
  "When non-nil, log every WebSocket frame to *chrome-server* buffer.")

(defvar chrome-server-pandoc-executable "pandoc"
  "Path to the pandoc executable used for HTML → org conversion.
Shared by chrome-server-www and chrome-server-chatgpt backends.")

;; ── State ────────────────────────────────────────────────────────────────────

(defvar chrome-server--server-process nil
  "The `websocket-server' process, or nil if not running.")

(defvar chrome-server--clients nil
  "List of currently connected websocket client objects.")

(defvar chrome-server--handlers nil
  "Alist mapping request name (string) to handler function.
Handler is called with one argument, the request payload (a plist),
and must return a value JSON-encodable as the response payload.")

(defvar chrome-server--pending-callbacks nil
  "Alist mapping outstanding request id (string) to (CALLBACK . TIMER).
CALLBACK is invoked with the decoded response payload.  TIMER is the
`run-at-time' timer that aborts the request on timeout.")

(defvar chrome-server--rx-buffers nil
  "Per-client accumulators for in-progress fragmented messages.
Alist mapping each client websocket to the bytes received so far for
the in-progress fragmented message on that connection.  Cleared once
the final fragment (FIN bit set) arrives or the client disconnects.")

;; ── Debug logging ────────────────────────────────────────────────────────────

(defun chrome-server--log (fmt &rest args)
  "Append FMT formatted with ARGS to *chrome-server* when debug is enabled."
  (when chrome-server-debug
    (with-current-buffer (get-buffer-create "*chrome-server*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S.%3N] ")
              (apply #'format fmt args)
              "\n"))))

(defun chrome-server--warn (fmt &rest args)
  "Surface a chrome-server warning to the user and the debug log.
FMT and ARGS are passed through `format'.  The formatted message is
emitted to *Messages* and appended to the *chrome-server* debug
buffer."
  (let ((msg (apply #'format fmt args)))
    (message "chrome-server: %s" msg)
    (chrome-server--log "[WARN] %s" msg)))

;; ── Server lifecycle ─────────────────────────────────────────────────────────

;;;###autoload
(defun chrome-server-start ()
  "Start the Chrome server WebSocket server on `chrome-server-port'."
  (interactive)
  (when (and chrome-server--server-process
             (not (eq (process-status chrome-server--server-process) 'closed)))
    (chrome-server-stop))
  (setq chrome-server--clients nil
        chrome-server--pending-callbacks nil
        chrome-server--server-process
        (websocket-server
         chrome-server-port
         :host chrome-server-host
         :on-open    #'chrome-server--on-open
         :on-close   #'chrome-server--on-close
         :on-message #'chrome-server--on-message
         :on-error   #'chrome-server--on-error))
  (chrome-server--log "[SERVER] started on port %d" chrome-server-port)
  (message "Chrome server (WS) started on port %d" chrome-server-port))

;;;###autoload
(defun chrome-server-stop ()
  "Stop the Chrome server WebSocket server."
  (interactive)
  (when chrome-server--server-process
    (websocket-server-close chrome-server--server-process)
    (setq chrome-server--server-process nil))
  (chrome-server--cancel-all-pending "server stopped")
  (setq chrome-server--clients nil)
  (chrome-server--log "[SERVER] stopped")
  (message "Chrome server stopped"))

;; ── Connection callbacks ─────────────────────────────────────────────────────

(defun chrome-server--on-open (ws)
  "Register newly connected client WS."
  (setq chrome-server--clients (cons ws chrome-server--clients))
  (chrome-server--log "[CONNECT] clients=%d" (length chrome-server--clients)))

(defun chrome-server--on-close (ws)
  "Remove disconnected client WS and drop its rx buffer."
  (setq chrome-server--clients
        (cl-remove-if (lambda (c) (eq c ws)) chrome-server--clients)
        chrome-server--rx-buffers
        (cl-remove-if (lambda (c) (eq (car c) ws)) chrome-server--rx-buffers))
  (chrome-server--log "[DISCONNECT] clients=%d" (length chrome-server--clients)))

(defun chrome-server--on-error (_ws sym err)
  "Surface WebSocket error ERR in callback SYM."
  (chrome-server--warn "error in %s: %S" sym err))

;; ── Dispatch ─────────────────────────────────────────────────────────────────

(defun chrome-server--on-message (ws frame)
  "Accumulate FRAME bytes for WS; dispatch once a complete message arrives.
A WebSocket message may be split across many frames (large payloads such
as page HTML routinely run into the hundreds of KB).  We keep a per-client
buffer of frame text and only JSON-parse once the FIN bit is set on the
final frame.  Frames with a `:name' field are requests; frames with a
`:requestId' field are responses to Emacs-initiated requests."
  (let* ((text       (or (websocket-frame-text frame) ""))
         (complete-p (websocket-frame-completep frame))
         (prior-cell (assq ws chrome-server--rx-buffers))
         (combined   (concat (cdr prior-cell) text)))
    (cond
     ;; Still receiving — stash and wait.
     ((not complete-p)
      (if prior-cell
          (setcdr prior-cell combined)
        (setq chrome-server--rx-buffers
              (cons (cons ws combined) chrome-server--rx-buffers)))
      (chrome-server--log "[RECV-CONT] +%d byte(s); total=%d"
                          (length text) (length combined)))
     ;; Final fragment — drop the accumulator and dispatch.
     (t
      (when prior-cell
        (setq chrome-server--rx-buffers
              (cl-remove-if (lambda (c) (eq (car c) ws))
                            chrome-server--rx-buffers)))
      (chrome-server--log "[RECV] %d byte(s)" (length combined))
      (let ((msg (condition-case err
                     (json-parse-string combined
                                        :object-type 'plist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)
                   (error
                    (chrome-server--warn "could not parse frame as JSON: %s"
                                         (error-message-string err))
                    nil))))
        (cond
         ((null msg) nil)
         ((plist-get msg :name)
          (chrome-server--handle-request ws msg))
         ((plist-get msg :requestId)
          (chrome-server--handle-response msg))
         (t
          (chrome-server--warn "unknown frame shape (no :name or :requestId): %S"
                               msg))))))))

(defun chrome-server--handle-request (ws msg)
  "Look up handler for MSG and send the response back over WS."
  (let* ((name    (plist-get msg :name))
         (id      (or (plist-get msg :id) "<unknown>"))
         (payload (plist-get msg :payload))
         (handler (cdr (assoc name chrome-server--handlers))))
    (let ((response-payload
           (if handler
               (condition-case err
                   (funcall handler payload)
                 (error
                  (chrome-server--warn "handler %s signalled: %s"
                                       name (error-message-string err))
                  `((status . "error")
                    (message . ,(error-message-string err)))))
             (progn
               (chrome-server--warn "no handler registered for request: %s"
                                    name)
               `((status . "error")
                 (message . ,(format "Unknown request: %s" name)))))))
      ;; Surface the handler's status line to the user.  Errors are
      ;; already reported via `chrome-server--warn' in the error path
      ;; above, so we only message on success here to avoid duplicates.
      (let ((status (alist-get 'status response-payload))
            (text   (alist-get 'message response-payload)))
        (when (and text (equal status "ok"))
          (message "chrome-server [%s]: %s" name text)))
      (chrome-server--send-to ws
                              `((requestId . ,id)
                                (payload   . ,response-payload))))))

(defun chrome-server--handle-response (msg)
  "Invoke the pending callback for MSG's requestId.
If no pending callback matches (likely already timed out), surfaces a warning."
  (let* ((id   (plist-get msg :requestId))
         (cell (assoc id chrome-server--pending-callbacks)))
    (if (null cell)
        (chrome-server--warn "response for unknown/timed-out request id: %s" id)
      (let ((callback (cadr cell))
            (timer    (cddr cell)))
        (when (timerp timer) (cancel-timer timer))
        (setq chrome-server--pending-callbacks
              (cl-remove-if (lambda (c) (equal (car c) id))
                            chrome-server--pending-callbacks))
        (condition-case err
            (funcall callback (plist-get msg :payload))
          (error
           (chrome-server--warn "response callback for %s signalled: %s"
                                id (error-message-string err))))))))

;; ── Sending ──────────────────────────────────────────────────────────────────

(defun chrome-server--send-to (ws data)
  "JSON-encode DATA and send it on WS."
  (let ((text (json-encode data)))
    (chrome-server--log "[SEND] %s" text)
    (websocket-send-text ws text)))

(defun chrome-server--broadcast (data)
  "JSON-encode DATA and send it to every connected client.
Returns the websocket the frame was sent on, or nil if no clients."
  (let ((ws (car chrome-server--clients)))
    (when ws
      (chrome-server--send-to ws data)
      ws)))

;; ── Handler registry ─────────────────────────────────────────────────────────

(defun chrome-server-register-handler (name handler)
  "Register HANDLER as the handler for request NAME.
NAME is a SCREAMING_SNAKE_CASE string.  HANDLER is called with the
request payload (a plist) and must return a value JSON-encodable as the
response payload.  Re-registering overwrites the previous binding."
  (setq chrome-server--handlers
        (cons (cons name handler)
              (cl-remove-if (lambda (c) (string= (car c) name))
                            chrome-server--handlers))))

(defun chrome-server-unregister-handler (name)
  "Remove the handler for request NAME, if any."
  (setq chrome-server--handlers
        (cl-remove-if (lambda (c) (string= (car c) name))
                      chrome-server--handlers)))

;; ── Async request primitive (Emacs → browser) ────────────────────────────────

(defun chrome-server--cancel-all-pending (reason)
  "Cancel every pending callback with an error payload citing REASON."
  (let ((pending chrome-server--pending-callbacks))
    (setq chrome-server--pending-callbacks nil)
    (dolist (cell pending)
      (let ((id       (car cell))
            (callback (cadr cell))
            (timer    (cddr cell)))
        (when (timerp timer) (cancel-timer timer))
        (condition-case err
            (funcall callback `(:status "error" :message ,reason))
          (error
           (chrome-server--warn "cancellation callback for %s signalled: %s"
                                id (error-message-string err))))))))

(defun chrome-server-request-async (name payload callback)
  "Send a request NAME with PAYLOAD to the browser; invoke CALLBACK on response.
CALLBACK receives the decoded response payload (a plist).  If the
request times out (`chrome-server-request-timeout' seconds) CALLBACK is
called with (:status \"error\" :message \"timeout\").  Returns the
request id, or nil if no client is connected."
  (let ((ws (car chrome-server--clients)))
    (cond
     ((null ws)
      (chrome-server--warn "no client connected; dropping request %s" name)
      (funcall callback '(:status "error" :message "no client connected"))
      nil)
     (t
      (let* ((id    (org-id-uuid))
             (timer (run-at-time chrome-server-request-timeout nil
                                 #'chrome-server--timeout-request id)))
        (setq chrome-server--pending-callbacks
              (cons (cons id (cons callback timer))
                    chrome-server--pending-callbacks))
        (chrome-server--send-to ws
                                `((id      . ,id)
                                  (name    . ,name)
                                  (payload . ,(or payload :null))))
        id)))))

(defun chrome-server--timeout-request (id)
  "Time out the pending request with ID."
  (let ((cell (assoc id chrome-server--pending-callbacks)))
    (when cell
      (setq chrome-server--pending-callbacks
            (cl-remove-if (lambda (c) (equal (car c) id))
                          chrome-server--pending-callbacks))
      (chrome-server--warn "request %s timed out after %ss"
                           id chrome-server-request-timeout)
      (condition-case err
          (funcall (cadr cell) '(:status "error" :message "timeout"))
        (error
         (chrome-server--warn "timeout callback for %s signalled: %s"
                              id (error-message-string err)))))))

(defun chrome-server-request (name &optional payload)
  "Synchronously send NAME/PAYLOAD to the browser and return the response payload.
Blocks via `accept-process-output' until the response arrives or the
timeout elapses.  Signals an error on timeout or when no client is
connected.  Do NOT use this from inside a websocket callback — it can
re-enter the dispatcher."
  (let* ((done nil)
         (result nil)
         (id (chrome-server-request-async
              name payload
              (lambda (payload)
                (setq result payload
                      done   t)))))
    (unless id (error "No client connected"))
    (let ((deadline (+ (float-time)
                       (+ 0.5 chrome-server-request-timeout))))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (unless done (error "Request %s timed out" name))
    result))

;; ── Convenience: respond-fast-then-defer ─────────────────────────────────────

(defun chrome-server-defer (fn &rest args)
  "Schedule FN to run with ARGS on the next idle tick.
Use inside a handler that wants to return immediately while the real
work happens out-of-band."
  (run-at-time 0 nil (lambda () (apply fn args))))

;; ── Payload cache (preserved across the rewrite) ─────────────────────────────
;;
;; Templates pull these via %(chrome-server-get-url) etc.  The variables
;; are populated by `chrome-server--prime-payload-cache' inside each
;; capture handler.

(defvar chrome-server--current-url nil
  "URL from the most recent chrome-server payload.")

(defvar chrome-server--current-title nil
  "Title from the most recent chrome-server payload.")

(defvar chrome-server--current-text nil
  "Selected text from the most recent chrome-server payload.")

(defun chrome-server-get-url ()
  "Return the URL from the current payload and clear it.
Returns an empty string if not set or already consumed."
  (prog1 (or chrome-server--current-url "")
    (setq chrome-server--current-url nil)))

(defun chrome-server-get-title ()
  "Return the title from the current payload and clear it.
Returns an empty string if not set or already consumed."
  (prog1 (or chrome-server--current-title "")
    (setq chrome-server--current-title nil)))

(defun chrome-server-get-selection ()
  "Return the selected text from the current payload and clear it.
Returns an empty string if not set or already consumed."
  (prog1 (or chrome-server--current-text "")
    (setq chrome-server--current-text nil)))

(defun chrome-server--prime-payload-cache (payload)
  "Populate the payload cache vars from PAYLOAD."
  (setq chrome-server--current-url   (plist-get payload :url)
        chrome-server--current-title (or (plist-get payload :title) "")
        chrome-server--current-text  (or (plist-get payload :text)  "")))

;; ── Shared helpers ───────────────────────────────────────────────────────────

(defun chrome-server--maybe-raise (payload)
  "Raise and focus the selected Emacs frame if PAYLOAD's :raise is t."
  (when (eq (plist-get payload :raise) t)
    (select-frame-set-input-focus (selected-frame))))

(defun chrome-server--capture-initial (payload)
  "Build the org-capture-initial string from PAYLOAD's url, title, and text."
  (let* ((url   (plist-get payload :url))
         (title (or (plist-get payload :title) "Web capture"))
         (text  (or (plist-get payload :text) "")))
    (concat (format "[[%s][%s]]" url title)
            (unless (string-empty-p text) (concat "\n\n" text)))))

(defun chrome-server--require-payload (payload)
  "Signal if PAYLOAD is nil."
  (unless payload
    (error "Missing 'payload' in request")))

(defun chrome-server--ok (&optional message)
  "Return a standard OK response payload, optionally with MESSAGE."
  (if message
      `((status . "ok") (message . ,message))
    '((status . "ok"))))

(defun chrome-server--strip-svg (html)
  "Return HTML with every inline <svg>…</svg> block removed.
Used by HTML→org backends to keep decorative icons out of the
pandoc-extracted media (their fixed-pixel-less viewBox-only definitions
render at librsvg's huge default size in org buffers)."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward "<svg[[:space:]\n][^>]*>" nil t)
      (let ((start (match-beginning 0)))
        (when (re-search-forward "</svg>" nil t)
          (delete-region start (point)))))
    (buffer-string)))

;; ── Built-in handlers ────────────────────────────────────────────────────────

(defun chrome-server--handle-org-capture (payload)
  "Handle ORG_CAPTURE request with PAYLOAD.
Schedules the actual capture and returns immediately (respond-fast-then-defer)."
  (chrome-server--require-payload payload)
  (chrome-server-defer #'chrome-server--org-capture payload)
  (chrome-server--ok "Org-capture opened"))

(defun chrome-server--handle-org-roam-capture (payload)
  "Handle ORG_ROAM_CAPTURE request with PAYLOAD.
Schedules the actual capture and returns immediately (respond-fast-then-defer)."
  (chrome-server--require-payload payload)
  (chrome-server-defer #'chrome-server--org-roam-capture payload)
  (chrome-server--ok "Org-roam-capture opened"))

(defun chrome-server--handle-eww (payload)
  "Handle EWW request with PAYLOAD.
Schedules the eww invocation and returns immediately
\(respond-fast-then-defer)."
  (chrome-server--require-payload payload)
  (unless (plist-get payload :url)
    (error "Missing url in payload"))
  (chrome-server-defer #'chrome-server--eww payload)
  (chrome-server--ok "Opening in eww"))

;; ── Action implementations ───────────────────────────────────────────────────

(defun chrome-server--org-capture (payload)
  "Open `org-capture' pre-filled from PAYLOAD.
Uses `chrome-server-org-capture-key' if set, otherwise prompts interactively."
  (condition-case err
      (let ((org-capture-initial (chrome-server--capture-initial payload)))
        (chrome-server--prime-payload-cache payload)
        (chrome-server--maybe-raise payload)
        (org-capture nil chrome-server-org-capture-key))
    (error
     (chrome-server--warn "org-capture failed: %s" (error-message-string err)))))

(defun chrome-server--org-roam-capture (payload)
  "Open org-roam-capture, seeding the payload cache from PAYLOAD."
  (condition-case err
      (let ((org-capture-initial (chrome-server--capture-initial payload)))
        (chrome-server--prime-payload-cache payload)
        (chrome-server--maybe-raise payload)
        (org-roam-capture-
         :node (org-roam-node-create)))
    (error
     (chrome-server--warn "org-roam-capture failed: %s" (error-message-string err)))))

(defun chrome-server--eww (payload)
  "Open the URL from PAYLOAD in eww."
  (condition-case err
      (let ((url (plist-get payload :url)))
        (chrome-server--maybe-raise payload)
        (eww url))
    (error
     (chrome-server--warn "eww failed: %s" (error-message-string err)))))

;; ── Register built-in handlers ───────────────────────────────────────────────

(chrome-server-register-handler "ORG_CAPTURE"      #'chrome-server--handle-org-capture)
(chrome-server-register-handler "ORG_ROAM_CAPTURE" #'chrome-server--handle-org-roam-capture)
(chrome-server-register-handler "EWW"              #'chrome-server--handle-eww)

(provide 'chrome-server)

;;; chrome-server.el ends here
