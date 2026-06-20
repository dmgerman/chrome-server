;;; browsel.el --- WebSocket bridge to a Chrome/Firefox extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: comm, tools, browser, org
;; URL: https://github.com/dmgerman/browsel
;; Package-Requires: ((emacs "27.1") (websocket "1.13") (org "9.4"))

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
;; `browsel-register-handler'.  Outgoing requests are made with
;; `browsel-request-async' (callback-based) or
;; `browsel-request' (sync wrapper using `accept-process-output').
;;
;; Built-in handlers:
;;
;;   ORG_CAPTURE       -- org-capture (template key configurable)
;;   ORG_ROAM_CAPTURE  -- standard org-roam-capture
;;   EWW               -- open URL in eww
;;
;; Per-feature backends register additional handlers:
;;
;;   browsel-chatgpt.el  -- CHATGPT
;;   browsel-www.el      -- SAVE_PAGE
;;   browsel-youtube.el  -- YOUTUBE, YOUTUBE_TRANSCRIPT
;;
;; Usage:
;;   (require 'browsel)
;;   (browsel-start)   ; start the server
;;   (browsel-stop)    ; stop the server

;;; Code:

(require 'websocket)
(require 'json)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)

;; Forward declarations.  These dynamic variables belong to org-capture and
;; org-roam, which are not necessarily loaded when this file is byte-compiled.
;; Without the defvar declarations a `let' on `org-capture-initial' would be
;; treated as a lexical binding and `org-capture' would never see the value.
(defvar org-capture-initial)
(defvar org-store-link-plist)
(defvar org-capture-link-is-already-stored)
(declare-function org-capture          "org-capture" (&optional goto keys))
(declare-function org-roam-capture-    "org-roam"    (&rest args))
(declare-function org-roam-node-create "org-roam"    (&rest args))

(defconst browsel-version "0.90"
  "Current version of the browsel package.")

;;;###autoload
(defun browsel-version (&optional here)
  "Return the browsel version string.
Interactively, display the version in the echo area.  With prefix
argument HERE, insert the version at point instead.  When called
from Lisp the return value is always the version string."
  (interactive "P")
  (let ((string (format "browsel %s" browsel-version)))
    (cond
     (here
      (insert string))
     ((called-interactively-p 'interactive)
      (message "%s" string))))
  browsel-version)

;; ── Configuration ────────────────────────────────────────────────────────────

(defvar browsel-port 9130
  "Port the Chrome server WebSocket server listens on.")

(defvar browsel-host 'local
  "Host the WebSocket server binds to.  `local' = 127.0.0.1.")

(defvar browsel-org-capture-key nil
  "Org-capture template key used by the ORG_CAPTURE handler.
nil means the user selects the template interactively.")

(defvar browsel-request-timeout 10
  "Seconds to wait for a response to an Emacs-initiated request before timing out.")

(defvar browsel-debug nil
  "When non-nil, log every WebSocket frame to *browsel* buffer.")

(defvar browsel-pandoc-executable "pandoc"
  "Path to the pandoc executable used for HTML → org conversion.
Shared by browsel-www and browsel-chatgpt backends.")

(defvar browsel-max-message-bytes (* 64 1024 1024)
  "Maximum bytes a single WebSocket message may accumulate to.
Page-html and ChatGPT-turns payloads are inherently large, so this is
set high (64 MiB) by default — large enough for any plausible page
save, low enough that a stuck or hostile sender cannot grow Emacs's
heap unbounded.  A client whose pending message exceeds this limit is
disconnected and its accumulator dropped; a fresh connection is
required to retry.  Set to nil to disable the cap.")

;; ── State ────────────────────────────────────────────────────────────────────

(defvar browsel--server-process nil
  "The `websocket-server' process, or nil if not running.")

(defvar browsel--clients nil
  "Alist of currently connected clients as (NAME . WS) pairs.
NAME is the identifier the client announced via CLIENT_HELLO, or
\"unknown-N\" until the client identifies itself.  WS is the
underlying websocket object.")

(defvar browsel--connect-counter 0
  "Monotonic counter for naming unidentified clients.
Reset on `browsel-start'; never decrements during a server's
lifetime so two unidentified connections cannot collide on the
same fallback name.")

(defvar browsel--current-ws nil
  "Websocket currently being dispatched, bound during handler execution.
Built-in handlers (notably CLIENT_HELLO) read this to discover which
client sent the request.  User-registered handlers should ignore it.")

(defvar browsel--handlers nil
  "Alist mapping request name (string) to handler function.
Handler is called with one argument, the request payload (a plist),
and must return a value JSON-encodable as the response payload.")

(defvar browsel--pending-callbacks nil
  "Alist mapping outstanding request id (string) to (CALLBACK . TIMER).
CALLBACK is invoked with the decoded response payload.  TIMER is the
`run-at-time' timer that aborts the request on timeout.")

(defvar browsel--rx-buffers nil
  "Per-client accumulators for in-progress fragmented messages.
Alist mapping each client websocket to the bytes received so far for
the in-progress fragmented message on that connection.  Cleared once
the final fragment (FIN bit set) arrives or the client disconnects.")

;; ── Debug logging ────────────────────────────────────────────────────────────

(defun browsel--log (fmt &rest args)
  "Append FMT formatted with ARGS to *browsel* when debug is enabled."
  (when browsel-debug
    (with-current-buffer (get-buffer-create "*browsel*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S.%3N] ")
              (apply #'format fmt args)
              "\n"))))

(defun browsel--warn (fmt &rest args)
  "Surface a browsel warning to the user and the debug log.
FMT and ARGS are passed through `format'.  The formatted message is
emitted to *Messages* and appended to the *browsel* debug
buffer."
  (let ((msg (apply #'format fmt args)))
    (message "browsel: %s" msg)
    (browsel--log "[WARN] %s" msg)))

;; ── Server lifecycle ─────────────────────────────────────────────────────────

;;;###autoload
(defun browsel-start ()
  "Start the Chrome server WebSocket server on `browsel-port'."
  (interactive)
  (when (and browsel--server-process
             (not (eq (process-status browsel--server-process) 'closed)))
    (browsel-stop))
  (setq browsel--clients nil
        browsel--connect-counter 0
        browsel--pending-callbacks nil
        browsel--server-process
        (websocket-server
         browsel-port
         :host browsel-host
         :on-open    #'browsel--on-open
         :on-close   #'browsel--on-close
         :on-message #'browsel--on-message
         :on-error   #'browsel--on-error))
  (browsel--log "[SERVER] started on port %d" browsel-port)
  (message "Chrome server (WS) started on port %d" browsel-port))

;;;###autoload
(defun browsel-stop ()
  "Stop the Chrome server WebSocket server."
  (interactive)
  (when browsel--server-process
    (websocket-server-close browsel--server-process)
    (setq browsel--server-process nil))
  (browsel--cancel-all-pending "server stopped")
  (setq browsel--clients nil
        browsel--connect-counter 0)
  (browsel--log "[SERVER] stopped")
  (message "Chrome server stopped"))

;; ── Connection callbacks ─────────────────────────────────────────────────────

(defun browsel--on-open (ws)
  "Register newly connected client WS under a placeholder name.
The client should send a CLIENT_HELLO request as its first frame to
replace the placeholder with a stable identifier."
  (let ((name (format "unknown-%d" (cl-incf browsel--connect-counter))))
    (setq browsel--clients
          (cons (cons name ws) browsel--clients))
    (browsel--log "[CONNECT] %s (clients=%d)"
                        name (length browsel--clients))))

(defun browsel--on-close (ws)
  "Remove disconnected client WS and drop its rx buffer."
  (let ((cell (rassq ws browsel--clients)))
    (setq browsel--clients
          (cl-remove-if (lambda (c) (eq (cdr c) ws)) browsel--clients)
          browsel--rx-buffers
          (cl-remove-if (lambda (c) (eq (car c) ws)) browsel--rx-buffers))
    (browsel--log "[DISCONNECT] %s (clients=%d)"
                        (if cell (car cell) "?")
                        (length browsel--clients))))

(defun browsel--on-error (_ws sym err)
  "Surface WebSocket error ERR in callback SYM."
  (browsel--warn "error in %s: %S" sym err))

;; ── Dispatch ─────────────────────────────────────────────────────────────────

(defun browsel--drop-client-over-limit (ws combined-len)
  "Drop WS and its rx accumulator; warn that COMBINED-LEN exceeded the cap.
Called from `browsel--on-message' when a pending message would push
the per-client accumulator past `browsel-max-message-bytes'.  The
accumulator is freed before the close so a stalled close does not
keep the buffer pinned."
  (setq browsel--rx-buffers
        (cl-remove-if (lambda (c) (eq (car c) ws)) browsel--rx-buffers))
  (browsel--warn
   "client message exceeded %d bytes (had %d); dropping connection"
   browsel-max-message-bytes combined-len)
  (ignore-errors (websocket-close ws)))

(defun browsel--on-message (ws frame)
  "Accumulate FRAME bytes for WS; dispatch once a complete message arrives.
A WebSocket message may be split across many frames (large payloads such
as page HTML routinely run into the hundreds of KB).  We keep a per-client
buffer of frame text and only JSON-parse once the FIN bit is set on the
final frame.  Frames with a `:name' field are requests; frames with a
`:requestId' field are responses to Emacs-initiated requests.
A pending message that would grow past `browsel-max-message-bytes'
disconnects the client instead of growing the accumulator further."
  (let* ((text       (or (websocket-frame-text frame) ""))
         (complete-p (websocket-frame-completep frame))
         (prior-cell (assq ws browsel--rx-buffers))
         (combined   (concat (cdr prior-cell) text)))
    (cond
     ;; Over the size cap — disconnect and stop accumulating.
     ((and browsel-max-message-bytes
           (> (length combined) browsel-max-message-bytes))
      (browsel--drop-client-over-limit ws (length combined)))
     ;; Still receiving — stash and wait.
     ((not complete-p)
      (if prior-cell
          (setcdr prior-cell combined)
        (setq browsel--rx-buffers
              (cons (cons ws combined) browsel--rx-buffers)))
      (browsel--log "[RECV-CONT] +%d byte(s); total=%d"
                          (length text) (length combined)))
     ;; Final fragment — drop the accumulator and dispatch.
     (t
      (when prior-cell
        (setq browsel--rx-buffers
              (cl-remove-if (lambda (c) (eq (car c) ws))
                            browsel--rx-buffers)))
      (browsel--log "[RECV] %d byte(s)" (length combined))
      (let ((msg (condition-case err
                     (json-parse-string combined
                                        :object-type 'plist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)
                   (error
                    (browsel--warn "could not parse frame as JSON: %s"
                                         (error-message-string err))
                    nil))))
        (cond
         ((null msg) nil)
         ((plist-get msg :name)
          (browsel--handle-request ws msg))
         ((plist-get msg :requestId)
          (browsel--handle-response msg))
         (t
          (browsel--warn "unknown frame shape (no :name or :requestId): %S"
                               msg))))))))

(defun browsel--handle-request (ws msg)
  "Look up handler for MSG and send the response back over WS."
  (let* ((name    (plist-get msg :name))
         (id      (or (plist-get msg :id) "<unknown>"))
         (payload (plist-get msg :payload))
         (handler (cdr (assoc name browsel--handlers))))
    (let ((response-payload
           (if handler
               (condition-case err
                   (let ((browsel--current-ws ws))
                     (funcall handler payload))
                 (error
                  (browsel--warn "handler %s signalled: %s"
                                       name (error-message-string err))
                  `((status . "error")
                    (message . ,(error-message-string err)))))
             (progn
               (browsel--warn "no handler registered for request: %s"
                                    name)
               `((status . "error")
                 (message . ,(format "Unknown request: %s" name)))))))
      ;; Surface the handler's status line to the user.  Errors are
      ;; already reported via `browsel--warn' in the error path
      ;; above, so we only message on success here to avoid duplicates.
      (let ((status (alist-get 'status response-payload))
            (text   (alist-get 'message response-payload)))
        (when (and text (equal status "ok"))
          (message "browsel [%s]: %s" name text)))
      (browsel--send-to ws
                              `((requestId . ,id)
                                (payload   . ,response-payload))))))

(defun browsel--handle-response (msg)
  "Invoke the pending callback for MSG's requestId.
If no pending callback matches (likely already timed out), surfaces a warning."
  (let* ((id   (plist-get msg :requestId))
         (cell (assoc id browsel--pending-callbacks)))
    (if (null cell)
        (browsel--warn "response for unknown/timed-out request id: %s" id)
      (let ((callback (cadr cell))
            (timer    (cddr cell)))
        (when (timerp timer) (cancel-timer timer))
        (setq browsel--pending-callbacks
              (cl-remove-if (lambda (c) (equal (car c) id))
                            browsel--pending-callbacks))
        (condition-case err
            (funcall callback (plist-get msg :payload))
          (error
           (browsel--warn "response callback for %s signalled: %s"
                                id (error-message-string err))))))))

;; ── Sending ──────────────────────────────────────────────────────────────────

(defun browsel--send-to (ws data)
  "JSON-encode DATA and send it on WS."
  (let ((text (json-encode data)))
    (browsel--log "[SEND] %s" text)
    (websocket-send-text ws text)))

(defun browsel--target-for (client name)
  "Resolve a request target without signalling.
Returns one of:
  (ok   . WS)  — send to WS.
  (err  . MSG) — abort: caller-supplied CLIENT not connected, or
                 multiple clients are connected and CLIENT is nil.
  (none . MSG) — no clients connected at all.
NAME appears in MSG and is informational only."
  (cond
   (client
    (let ((cell (assoc client browsel--clients)))
      (if cell
          (cons 'ok (cdr cell))
        (cons 'err
              (format
               "requested client %S is not connected (connected: %s)"
               client
               (if browsel--clients
                   (mapconcat #'car browsel--clients ", ")
                 "none"))))))
   ((null browsel--clients)
    (cons 'none (format "no client connected; dropping request %s" name)))
   ((= 1 (length browsel--clients))
    (cons 'ok (cdar browsel--clients)))
   (t
    (cons 'err
          (format "%d clients connected (%s); specify CLIENT for request %S"
                  (length browsel--clients)
                  (mapconcat #'car browsel--clients ", ")
                  name)))))

(defun browsel-connected-clients ()
  "Return the list of connected client names, in connection order (newest first)."
  (mapcar #'car browsel--clients))

(defun browsel--broadcast (data &optional client)
  "JSON-encode DATA and send it to one connected client.
With CLIENT nil and exactly one client connected, that client is the
target.  With CLIENT a string, the matching named client is targeted.
Returns the websocket the frame was sent on, or nil if the resolution
fails (also surfaces a warning so the failure is not silent)."
  (pcase (browsel--target-for client (alist-get 'name data))
    (`(ok . ,ws)
     (browsel--send-to ws data)
     ws)
    (`(err . ,msg)
     (browsel--warn "%s" msg)
     nil)
    (`(none . ,msg)
     (browsel--warn "%s" msg)
     nil)))

;; ── Handler registry ─────────────────────────────────────────────────────────

(defun browsel-register-handler (name handler)
  "Register HANDLER as the handler for request NAME.
NAME is a SCREAMING_SNAKE_CASE string.  HANDLER is called with the
request payload (a plist) and must return a value JSON-encodable as the
response payload.  Re-registering overwrites the previous binding."
  (setq browsel--handlers
        (cons (cons name handler)
              (cl-remove-if (lambda (c) (string= (car c) name))
                            browsel--handlers))))

(defun browsel-unregister-handler (name)
  "Remove the handler for request NAME, if any."
  (setq browsel--handlers
        (cl-remove-if (lambda (c) (string= (car c) name))
                      browsel--handlers)))

;; ── Built-in CLIENT_HELLO handler ────────────────────────────────────────────

(defun browsel--unique-client-name (requested ws &optional n)
  "Return REQUESTED, possibly with a -N suffix, that no other ws holds.
WS is permitted to already own the requested name (idempotent reuse).
Optional N is an internal recursion counter; callers should omit it."
  (let* ((n         (or n 1))
         (candidate (if (= n 1) requested (format "%s-%d" requested n)))
         (cell      (assoc candidate browsel--clients)))
    (if (or (not cell) (eq (cdr cell) ws))
        candidate
      (browsel--unique-client-name requested ws (1+ n)))))

(defun browsel--handle-client-hello (payload)
  "Built-in CLIENT_HELLO handler.
Renames the entry for the websocket currently being dispatched to
the client name announced in PAYLOAD, with a -N suffix appended if
the name is already taken by a different websocket.

The PAYLOAD must include a `:version' string that exactly matches
`browsel-version'.  The version check is strict: any mismatch
(including a missing or empty version) rejects the hello with an
error payload, leaves the client unregistered (its placeholder
\"unknown-N\" name persists), and the extension's ws-client treats
the connection as incompatible and stops the reconnect loop."
  (let ((ws       browsel--current-ws)
        (requested (plist-get payload :client))
        (version   (plist-get payload :version)))
    (unless ws
      (error "CLIENT_HELLO invoked outside a request dispatch"))
    (unless (and (stringp requested) (not (string-empty-p requested)))
      (error "CLIENT_HELLO requires payload.client (non-empty string)"))
    (unless (and (stringp version) (not (string-empty-p version)))
      (error "CLIENT_HELLO requires payload.version (non-empty string); \
emacs=%s, extension sent: %S" browsel-version version))
    (unless (string= version browsel-version)
      (error "version mismatch: emacs=%s extension=%s; \
rebuild and reload both sides"
             browsel-version version))
    (let* ((final-name (browsel--unique-client-name requested ws))
           (others     (cl-remove-if (lambda (c) (eq (cdr c) ws))
                                     browsel--clients)))
      (setq browsel--clients
            (cons (cons final-name ws) others))
      (browsel--log "[HELLO] %s (clients=%d)"
                          final-name (length browsel--clients))
      `((status . "ok")
        (client . ,final-name)))))

(browsel-register-handler "CLIENT_HELLO"
                                #'browsel--handle-client-hello)

;; ── Async request primitive (Emacs → browser) ────────────────────────────────

(defun browsel--cancel-all-pending (reason)
  "Cancel every pending callback with an error payload citing REASON."
  (let ((pending browsel--pending-callbacks))
    (setq browsel--pending-callbacks nil)
    (dolist (cell pending)
      (let ((id       (car cell))
            (callback (cadr cell))
            (timer    (cddr cell)))
        (when (timerp timer) (cancel-timer timer))
        (condition-case err
            (funcall callback `(:status "error" :message ,reason))
          (error
           (browsel--warn "cancellation callback for %s signalled: %s"
                                id (error-message-string err))))))))

(defun browsel-request-async (name payload callback &optional client)
  "Send a request NAME with PAYLOAD to the browser; invoke CALLBACK on response.
CALLBACK receives the decoded response payload (a plist).  If the
request times out (`browsel-request-timeout' seconds) CALLBACK is
called with (:status \"error\" :message \"timeout\").  Returns the
request id, or nil if no client is connected.

CLIENT, if non-nil, names which connected client to target (e.g.
\"chrome\", \"firefox\").  When omitted, the request is sent to the
sole connected client; when more than one is connected, CALLBACK is
invoked with a status:error payload and nil is returned."
  (pcase (browsel--target-for client name)
    (`(ok . ,ws)
     (let* ((id    (org-id-uuid))
            (timer (run-at-time browsel-request-timeout nil
                                #'browsel--timeout-request id)))
       (setq browsel--pending-callbacks
             (cons (cons id (cons callback timer))
                   browsel--pending-callbacks))
       (browsel--send-to ws
                               `((id      . ,id)
                                 (name    . ,name)
                                 (payload . ,(or payload :null))))
       id))
    (`(err . ,msg)
     (browsel--warn "%s" msg)
     (funcall callback `(:status "error" :message ,msg))
     nil)
    (`(none . ,msg)
     (browsel--warn "%s" msg)
     (funcall callback '(:status "error" :message "no client connected"))
     nil)))

(defun browsel--timeout-request (id)
  "Time out the pending request with ID."
  (let ((cell (assoc id browsel--pending-callbacks)))
    (when cell
      (setq browsel--pending-callbacks
            (cl-remove-if (lambda (c) (equal (car c) id))
                          browsel--pending-callbacks))
      (browsel--warn "request %s timed out after %ss"
                           id browsel-request-timeout)
      (condition-case err
          (funcall (cadr cell) '(:status "error" :message "timeout"))
        (error
         (browsel--warn "timeout callback for %s signalled: %s"
                              id (error-message-string err)))))))

(defun browsel-request (name &optional payload client)
  "Synchronously send NAME/PAYLOAD to the browser and return the response payload.
Blocks via `accept-process-output' until the response arrives or the
timeout elapses.  Signals an error on timeout, when no client is
connected, when more than one client is connected and CLIENT was not
supplied, or when CLIENT names a client that is not connected.
Do NOT use this from inside a websocket callback — it can re-enter
the dispatcher.

CLIENT, if non-nil, names the client to target (e.g. \"chrome\",
\"firefox\").  See `browsel-connected-clients' for the current
roster."
  (catch 'browsel--result
    (let ((id (browsel-request-async
               name payload
               (lambda (response)
                 (throw 'browsel--result response))
               client)))
      (unless id
        ;; Request-async already warned and invoked the callback with a
        ;; status:error payload, so escalate to an error here too.
        (error "browsel-request: no acceptable target for %s" name))
      (let ((deadline (+ (float-time)
                         (+ 0.5 browsel-request-timeout))))
        (cl-labels
            ((pump ()
               (cond
                ((> (float-time) deadline)
                 (error "Request %s timed out" name))
                (t
                 (accept-process-output nil 0.05)
                 (pump)))))
          (pump))))))

;; ── Convenience: respond-fast-then-defer ─────────────────────────────────────

(defun browsel-defer (fn &rest args)
  "Schedule FN to run with ARGS on the next idle tick.
Use inside a handler that wants to return immediately while the real
work happens out-of-band."
  (run-at-time 0 nil (lambda () (apply fn args))))

;; ── Payload cache (preserved across the rewrite) ─────────────────────────────
;;
;; Templates pull these via %(browsel-get-url) etc.  The variables
;; are populated by `browsel--prime-payload-cache' inside each
;; capture handler.

(defvar browsel--current-url nil
  "URL from the most recent browsel payload.")

(defvar browsel--current-title nil
  "Title from the most recent browsel payload.")

(defvar browsel--current-text nil
  "Selected text from the most recent browsel payload.")

(defun browsel-get-url ()
  "Return the URL from the current payload and clear it.
Returns an empty string if not set or already consumed."
  (prog1 (or browsel--current-url "")
    (setq browsel--current-url nil)))

(defun browsel-get-title ()
  "Return the title from the current payload and clear it.
Returns an empty string if not set or already consumed."
  (prog1 (or browsel--current-title "")
    (setq browsel--current-title nil)))

(defun browsel-get-selection ()
  "Return the selected text from the current payload and clear it.
Returns an empty string if not set or already consumed."
  (prog1 (or browsel--current-text "")
    (setq browsel--current-text nil)))

(defun browsel--prime-payload-cache (payload)
  "Populate the payload cache vars from PAYLOAD."
  (setq browsel--current-url   (plist-get payload :url)
        browsel--current-title (or (plist-get payload :title) "")
        browsel--current-text  (or (plist-get payload :text)  "")))

;; ── Shared helpers ───────────────────────────────────────────────────────────

(defun browsel--maybe-raise (payload)
  "Raise and focus the selected Emacs frame if PAYLOAD's :raise is t."
  (when (eq (plist-get payload :raise) t)
    (select-frame-set-input-focus (selected-frame))))

;; ── Org sanitizers ───────────────────────────────────────────────────────────
;;
;; Everything coming off the wire is page-controlled.  Org-mode is a
;; structured language: a stray `\\n* heading' in a description, a
;; `]' in a title, or a captured URL with an `elisp:' scheme can all
;; change the resulting document's meaning — at worst, run elisp when
;; the user later clicks a captured link.  These helpers escape such
;; content at the boundary so handlers can splice page strings into
;; templates and drawers without thinking about it each time.

(defconst browsel--safe-link-schemes
  '("http" "https" "ftp" "ftps" "mailto" "news")
  "Schemes accepted by `browsel--make-link'.
URLs with any other scheme (`elisp:', `shell:', `eshell:', `javascript:',
…) are rendered as plain text instead of a clickable Org link, so a
captured page cannot plant a link that runs code if a user later follows
it.  Add to this list only after weighing what `org-link-parameters'
does with the scheme in your config.")

(defun browsel--safe-link-url-p (url)
  "Return non-nil if URL's scheme is in `browsel--safe-link-schemes'."
  (and (stringp url)
       (let ((case-fold-search t))
         (when (string-match "\\`\\([A-Za-z][A-Za-z0-9+.-]*\\):" url)
           (member (downcase (match-string 1 url))
                   browsel--safe-link-schemes)))))

(defun browsel--escape-org-link-target (s)
  "Make S safe to splice as the target of an Org link.
A literal `]' breaks the link parser; replace with its URL-encoded form."
  (replace-regexp-in-string "\\]" "%5D" (or s "")))

(defun browsel--escape-org-link-desc (s)
  "Make S safe to splice as the description of an Org link.
Collapses newlines (descriptions must be single-line) and replaces the
bracket characters with curly look-alikes so they cannot close the
description bracket."
  (let* ((s (or s ""))
         (s (replace-regexp-in-string "[\n\r]+" " " s))
         (s (replace-regexp-in-string "\\]" "}" s))
         (s (replace-regexp-in-string "\\[" "{" s)))
    s))

(defun browsel--make-link (url description)
  "Return `[[URL][DESCRIPTION]]' when URL has a safe scheme.
Otherwise return a plain-text fallback like `desc (url)' so a captured
page cannot plant a clickable `elisp:'/`shell:'/`javascript:' link.
DESCRIPTION defaults to URL if nil or empty."
  (let* ((url (or url ""))
         (description (if (and (stringp description)
                               (not (string-empty-p description)))
                          description
                        url)))
    (if (browsel--safe-link-url-p url)
        (format "[[%s][%s]]"
                (browsel--escape-org-link-target url)
                (browsel--escape-org-link-desc description))
      (format "%s (%s)"
              (browsel--escape-org-link-desc description)
              (browsel--escape-org-link-desc url)))))

(defun browsel--sanitize-org-meta (s)
  "Sanitize S for a single-line Org metadata context.
Use for property-drawer values, `#+keyword:' lines, headings.
Collapses newlines to spaces and replaces `]' with `}' so a value cannot
terminate a surrounding link or drawer line, or carry a heading break."
  (let* ((s (or s ""))
         (s (replace-regexp-in-string "[\n\r]+" " " s))
         (s (replace-regexp-in-string "\\]" "}" s)))
    s))

(defun browsel--sanitize-org-body (s)
  "Sanitize S for multi-line Org body text.
Indents any line that would otherwise start an Org heading (`*' in
column 0), a drawer marker (`:NAME:'), or a file-level keyword
(`#+...'), so structure cannot be injected by a page-controlled
selection, description, or transcript.  Indented variants of those
constructs are inert to the Org parser."
  (replace-regexp-in-string
   "^\\(\\*+ \\|#\\+\\|:[A-Za-z_-]+:\\)"
   " \\1"
   (or s "")))

(defun browsel--capture-initial (payload)
  "Build the org-capture-initial string from PAYLOAD's url, title, and text.
The link is built via `browsel--make-link' (which blocks unsafe schemes)
and any body text is passed through `browsel--sanitize-org-body' so it
cannot introduce headings or drawer markers."
  (let* ((url   (plist-get payload :url))
         (title (or (plist-get payload :title) "Web capture"))
         (text  (or (plist-get payload :text) "")))
    (concat (browsel--make-link url title)
            (unless (string-empty-p text)
              (concat "\n\n" (browsel--sanitize-org-body text))))))

(defun browsel--store-link-plist (payload)
  "Return an `org-store-link-plist' for PAYLOAD's url and title.
Drives `%a' (annotation) expansion in org-capture templates so that
each capture sees the current browser link rather than whatever link
Emacs happened to store last.  `:annotation' is set explicitly because
org-capture reads it directly when `org-capture-link-is-already-stored'
is non-nil.  For an unsafe URL scheme the `:link' field is left blank
(so `%L'/`%l' do not splice a clickable bad link) and `:annotation' is
the plain-text rendering produced by `browsel--make-link'."
  (let* ((url   (plist-get payload :url))
         (title (or (plist-get payload :title) "Web capture")))
    (list :type "http"
          :link        (if (browsel--safe-link-url-p url) url "")
          :description (browsel--escape-org-link-desc title)
          :annotation  (browsel--make-link url title))))

(defun browsel--require-payload (payload)
  "Signal if PAYLOAD is nil."
  (unless payload
    (error "Missing 'payload' in request")))

(defun browsel--ok (&optional message)
  "Return a standard OK response payload, optionally with MESSAGE."
  (if message
      `((status . "ok") (message . ,message))
    '((status . "ok"))))

(defun browsel--strip-svg (html)
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

(defun browsel--handle-org-capture (payload)
  "Handle ORG_CAPTURE request with PAYLOAD.
Schedules the actual capture and returns immediately (respond-fast-then-defer)."
  (browsel--require-payload payload)
  (browsel-defer #'browsel--org-capture payload)
  (browsel--ok "Org-capture started"))

(defun browsel--handle-org-roam-capture (payload)
  "Handle ORG_ROAM_CAPTURE request with PAYLOAD.
Schedules the actual capture and returns immediately (respond-fast-then-defer)."
  (browsel--require-payload payload)
  (browsel-defer #'browsel--org-roam-capture payload)
  (browsel--ok "Org-roam-capture started"))

(defun browsel--handle-eww (payload)
  "Handle EWW request with PAYLOAD.
Schedules the eww invocation and returns immediately
\(respond-fast-then-defer)."
  (browsel--require-payload payload)
  (unless (plist-get payload :url)
    (error "Missing url in payload"))
  (browsel-defer #'browsel--eww payload)
  (browsel--ok "Eww started"))

;; ── Action implementations ───────────────────────────────────────────────────

(defun browsel--org-capture (payload)
  "Open `org-capture' pre-filled from PAYLOAD.
Uses `browsel-org-capture-key' if set, otherwise prompts interactively."
  (condition-case err
      (let ((org-capture-initial              (browsel--capture-initial payload))
            (org-store-link-plist             (browsel--store-link-plist payload))
            (org-capture-link-is-already-stored t))
        (browsel--prime-payload-cache payload)
        (browsel--maybe-raise payload)
        (org-capture nil browsel-org-capture-key))
    (error
     (browsel--warn "org-capture failed: %s" (error-message-string err)))))

(defun browsel--org-roam-capture (payload)
  "Open org-roam-capture, seeding the payload cache from PAYLOAD."
  (condition-case err
      (let ((org-capture-initial              (browsel--capture-initial payload))
            (org-store-link-plist             (browsel--store-link-plist payload))
            (org-capture-link-is-already-stored t))
        (browsel--prime-payload-cache payload)
        (browsel--maybe-raise payload)
        (org-roam-capture-
         :node (org-roam-node-create)))
    (error
     (browsel--warn "org-roam-capture failed: %s" (error-message-string err)))))

(defun browsel--eww (payload)
  "Open the URL from PAYLOAD in eww."
  (condition-case err
      (let ((url (plist-get payload :url)))
        (browsel--maybe-raise payload)
        (eww url))
    (error
     (browsel--warn "eww failed: %s" (error-message-string err)))))

;; ── Register built-in handlers ───────────────────────────────────────────────

(browsel-register-handler "ORG_CAPTURE"      #'browsel--handle-org-capture)
(browsel-register-handler "ORG_ROAM_CAPTURE" #'browsel--handle-org-roam-capture)
(browsel-register-handler "EWW"              #'browsel--handle-eww)

(provide 'browsel)

;;; browsel.el ends here
