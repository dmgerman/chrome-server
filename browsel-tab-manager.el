;;; browsel-tab-manager.el --- Jump to a browser tab via completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: comm, tools, browser
;; URL: https://github.com/dmgerman/browsel
;; Package-Requires: ((emacs "27.1"))

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

;; Optional browsel module providing `browsel-tab-manager-jump-to-tab',
;; an interactive command that lists every open tab in the connected
;; browser and uses `completing-read' to focus one.
;;
;; When only one browser is connected the command targets it without
;; asking.  When more than one browser is connected the target is read
;; from `browsel-tab-manager-client'; if that variable is nil the user
;; must run `M-x browsel-tab-manager-set-client' first to pick which
;; browser the module should address.

;;; Code:

(require 'browsel)
(require 'url-parse)
(require 'cl-lib)

;; ── Configuration ────────────────────────────────────────────────────────────

(defcustom browsel-tab-manager-client nil
  "Name of the connected browsel client this module addresses.
Either nil (the module uses the sole connected client and errors
when more than one is connected) or one of the strings returned by
`browsel-connected-clients' — typically \"chrome\" or \"firefox\".

Set this interactively via `browsel-tab-manager-set-client'."
  :type '(choice (const  :tag "Use sole connected client" nil)
                 (string :tag "Client name"))
  :group 'browsel)

;; ── Client selection ────────────────────────────────────────────────────────

;;;###autoload
(defun browsel-tab-manager-set-client (&optional client)
  "Set `browsel-tab-manager-client' to CLIENT.
Interactively, prompt with `completing-read' over the currently
connected clients.  With a prefix argument, clear the setting (back
to nil) without prompting."
  (interactive
   (list
    (if current-prefix-arg
        nil
      (let ((connected (browsel-connected-clients)))
        (unless connected
          (user-error "No browsel client connected"))
        (completing-read
         (format "browsel client (%s): "
                 (mapconcat #'identity connected ", "))
         connected nil t nil nil
         (or browsel-tab-manager-client (car connected)))))))
  (setq browsel-tab-manager-client client)
  (message "browsel-tab-manager-client = %S" client))

(defun browsel-tab-manager--resolve-client ()
  "Return the client name to address.
Uses `browsel-tab-manager-client' when it names a currently connected
client; otherwise returns the sole connected client.  Signals a
`user-error' when no client is connected, or when more than one is
connected and `browsel-tab-manager-client' is unset or stale."
  (let ((connected (browsel-connected-clients)))
    (cond
     ((null connected)
      (user-error "browsel-tab-manager: no client connected"))
     ((and browsel-tab-manager-client
           (member browsel-tab-manager-client connected))
      browsel-tab-manager-client)
     ((= 1 (length connected))
      (car connected))
     (browsel-tab-manager-client
      (user-error
       "browsel-tab-manager-client=%S is not connected (connected: %s); \
run M-x browsel-tab-manager-set-client"
       browsel-tab-manager-client
       (mapconcat #'identity connected ", ")))
     (t
      (user-error
       "%d clients connected (%s); \
run M-x browsel-tab-manager-set-client to pick one"
       (length connected) (mapconcat #'identity connected ", "))))))

;; ── Candidate building ──────────────────────────────────────────────────────

(defun browsel-tab-manager--url-host (url)
  "Return the host of URL, or an empty string if it has none."
  (or (and (stringp url)
           (not (string-empty-p url))
           (ignore-errors (url-host (url-generic-parse-url url))))
      ""))

(defun browsel-tab-manager--flags (tab)
  "Return the bracketed flag prefix for TAB.
Three columns, lowercase letter if the flag is set, space otherwise:
  a — active (the focused tab in its window)
  s — sound (audible)
  i — incognito"
  (format "[%c%c%c]"
          (if (plist-get tab :active)    ?a ?\s)
          (if (plist-get tab :audible)   ?s ?\s)
          (if (plist-get tab :incognito) ?i ?\s)))

(defun browsel-tab-manager--display-base (tab)
  "Return the bare \"[asi] TITLE — HOST\" display string for TAB."
  (format "%s %s — %s"
          (browsel-tab-manager--flags tab)
          (or (plist-get tab :title) "(no title)")
          (browsel-tab-manager--url-host (plist-get tab :url))))

(defun browsel-tab-manager--candidates (tabs)
  "Return an alist of (DISPLAY . TAB) pairs for TABS.
DISPLAY is \"[asi] TITLE — HOST\"; bases that would collide are
disambiguated by appending \" (#ID)\" so each key is unique."
  (let ((bases (mapcar #'browsel-tab-manager--display-base tabs)))
    (cl-mapcar
     (lambda (tab base)
       (cons (if (> (cl-count base bases :test #'equal) 1)
                 (format "%s (#%s)" base (plist-get tab :id))
               base)
             tab))
     tabs bases)))

(defun browsel-tab-manager--completion-table (alist)
  "Return a completion table backed by ALIST that preserves entry order.
`completing-read' otherwise sorts candidates alphabetically; the
`display-sort-function' metadata tells modern completion frontends
(vertico, icomplete, the default minibuffer) to keep the MRU order
the caller produced."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function   . identity))
      (complete-with-action action alist string pred))))

;; ── Public command ──────────────────────────────────────────────────────────

;;;###autoload
(defun browsel-tab-manager-jump-to-tab ()
  "Focus a tab in the connected browser, picked via completion.
Lists every open tab from the resolved client (see
`browsel-tab-manager-client' and `browsel-tab-manager-set-client')
and activates the chosen tab plus its parent window via the
extension's FOCUS_TAB handler."
  (interactive)
  (let* ((client (browsel-tab-manager--resolve-client))
         (tabs   (browsel-request "GET_ALL_TABS" nil client)))
    (unless (and (listp tabs) tabs)
      (user-error "browsel-tab-manager: no tabs returned from %s" client))
    (let* ((sorted (seq-sort-by (lambda (tab)
                                  (or (plist-get tab :lastAccessed) 0))
                                #'> tabs))
           (alist  (browsel-tab-manager--candidates sorted))
           (pick   (completing-read
                    (format "Tab (%s): " client)
                    (browsel-tab-manager--completion-table alist)
                    nil t))
           (tab    (cdr (assoc pick alist))))
      (unless tab
        (user-error "browsel-tab-manager: no tab matches %S" pick))
      (browsel-request "FOCUS_TAB"
                       `(:id ,(plist-get tab :id) :focusWindow t)
                       client))))

(provide 'browsel-tab-manager)

;;; browsel-tab-manager.el ends here
