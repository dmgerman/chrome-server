;;; browsel-url-handler.el --- URL routing through browsel  -*- lexical-binding: t; -*-

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

;; Optional browsel module that routes URLs from Emacs to a connected
;; browser via the WebSocket bridge.  Drop-in `browsel-browse-url' is
;; compatible with `browse-url-browser-function', so any Emacs command
;; that opens a URL (org links, eww, mu4e, dired, …) can be made to go
;; through browsel.
;;
;; Routing is configured in `browsel-url-routes' as an ordered list of
;; plists: each entry maps a URL pattern to a client, an
;; incognito flag, and an optional separate match used to detect when
;; the URL is "already open" in some tab.  First match in the list
;; wins; URLs that match nothing fall through to
;; `browsel-default-client', non-incognito.
;;
;; When an existing matching tab is found (under the route's incognito
;; constraint), the function focuses it and raises the browser window.
;; Otherwise a fresh tab is opened — in an incognito window for
;; incognito routes, or via plain OPEN_TAB for the rest.
;;
;; Example configuration:
;;
;;   (setq browsel-url-routes
;;         '((:pattern "\\`https?://\\(www\\.\\)?github\\.com/"
;;            :client  "chrome")
;;           (:pattern "\\`https?://app\\.slack\\.com/"
;;            :client  "chrome"
;;            :tab-match "\\`https?://app\\.slack\\.com/")
;;           (:pattern "\\`https?://news\\."
;;            :client  "firefox"
;;            :incognito t)))
;;
;;   ;; Route every URL Emacs opens through browsel:
;;   (setq browse-url-browser-function #'browsel-browse-url)
;;
;; Incognito caveat: opening an incognito tab requires the user to
;; toggle "Allow in incognito" for the browsel extension in
;; `chrome://extensions' (or the Firefox equivalent).  When the toggle
;; is off the extension cannot enumerate or create incognito windows;
;; `browsel-browse-url' detects the failure, warns, and falls back to
;; a normal (non-incognito) tab so the user still reaches the page.

;;; Code:

(require 'browsel)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'url-parse)

;; ── Configuration ──────────────────────────────────────────────────────────

(defcustom browsel-url-routes nil
  "Ordered list of URL routing rules for `browsel-browse-url'.

Each element is a plist with these keys:

  :pattern REGEX
    Regular expression matched against the full URL.  The match is
    a substring match (anywhere in the URL) — leading/trailing `.*'
    is redundant.  Anchor explicitly with `\\\\=`' and `\\\\='' if a
    strict whole-string match is wanted.

    Convention: include a trailing `/' on host patterns (e.g.
    `\"uvic\\\\.ca/\"' rather than `\"uvic\\\\.ca\"') so a stray
    occurrence of the host string inside a query parameter of some
    other URL doesn't false-match.

    The first matching entry in the list wins.

  :client CLIENT-NAME
    String — the browsel client to address (e.g. \"chrome\" or
    \"firefox\").  Must name a currently-connected client.

  :incognito BOOL
    Optional.  When non-nil, the tab is opened in an incognito /
    private window.  Requires the user to have toggled \"Allow in
    incognito\" on the browsel extension; without that toggle the
    extension cannot create or even enumerate incognito windows,
    `browsel-browse-url' detects the failure, warns once, and
    falls back to a normal tab.

  :tab-match REGEX
    Optional override for the already-open detection.  When set,
    this regex is matched against the tab URLs returned by
    `GET_ALL_TABS'.  Without this key the route's `:pattern' is
    used for tab matching — the same regex that picked the client
    also identifies an open tab as the same destination.  Set
    `:tab-match' explicitly when the tab match needs to be stricter
    or looser than the routing match.

URLs that match no entry fall through to `browsel-default-client',
non-incognito, with identical-URL match."
  :type '(repeat plist)
  :group 'browsel)

;; ── Matching helpers ───────────────────────────────────────────────────────

(defun browsel-url-handler--domain (url)
  "Return the host of URL with a leading `www.' stripped, or nil.
Used by the no-route fallback to find an already-open tab whose URL
contains the same domain — e.g. opening `https://amazon.ca/foo'
will reuse a tab already on `https://www.amazon.ca/dp/B1234'."
  (let ((host (and (stringp url)
                   (ignore-errors
                     (url-host (url-generic-parse-url url))))))
    (and host
         (not (string-empty-p host))
         (replace-regexp-in-string "\\`www\\." "" host))))

(defun browsel-url-handler--match-route (url)
  "Return the first entry in `browsel-url-routes' whose :pattern matches URL.
Returns nil if no entry matches."
  (seq-find (lambda (route)
              (let ((pat (plist-get route :pattern)))
                (and (stringp pat)
                     (string-match-p pat url))))
            browsel-url-routes))

(defun browsel-url-handler--tab-matches-p (tab url tab-match incognito)
  "Return non-nil when TAB qualifies as already-open for URL under this route.
TAB is a plist as returned by `GET_ALL_TABS'.  TAB-MATCH, when a
non-empty string, is a regex used in place of identical-URL
comparison.  INCOGNITO, when non-nil, restricts matching to tabs in
incognito windows."
  (let ((tab-url (plist-get tab :url))
        (tab-incog (eq (plist-get tab :incognito) t)))
    (and (stringp tab-url)
         (or (not incognito) tab-incog)
         (if (and (stringp tab-match) (not (string-empty-p tab-match)))
             (string-match-p tab-match tab-url)
           (string= tab-url url)))))

(defun browsel-url-handler--find-existing-tab (url client incognito tab-match)
  "Return the most-recently-accessed matching tab from CLIENT, or nil.
URL, INCOGNITO, and TAB-MATCH are forwarded to
`browsel-url-handler--tab-matches-p' for the filter."
  (let* ((tabs    (browsel-request "GET_ALL_TABS" nil client))
         (matches (seq-filter
                   (lambda (tab)
                     (browsel-url-handler--tab-matches-p
                      tab url tab-match incognito))
                   (or tabs '()))))
    (car (seq-sort-by
          (lambda (tab) (or (plist-get tab :lastAccessed) 0))
          #'>
          matches))))

;; ── Open primitives ────────────────────────────────────────────────────────

(defun browsel-url-handler--focus (tab client)
  "Activate TAB inside CLIENT, raise its window, and nudge the macOS app.
The OS-level app activation is delegated to `browsel-activate-client'
in `browsel.el' (shared with `browsel-tab-manager')."
  (browsel-request "FOCUS_TAB"
                   (list :id (plist-get tab :id) :focusWindow t)
                   client)
  (browsel-activate-client client))

(defun browsel-url-handler--open-new (url client incognito)
  "Open URL in a new tab on CLIENT.
INCOGNITO non-nil routes via `OPEN_INCOGNITO_TAB'; if that fails
\(typically because the extension lacks the \"Allow in incognito\"
toggle), the function warns and falls back to a normal tab so the
URL still reaches the user.  After the tab is opened,
`browsel-activate-client' brings the browser process to the OS
foreground the same way the focus-existing-tab path does."
  (if (not incognito)
      (browsel-request "OPEN_TAB" (list :url url) client)
    (condition-case err
        (browsel-request "OPEN_INCOGNITO_TAB" (list :url url) client)
      (error
       (message "browsel: incognito open failed (%s); using normal tab"
                (error-message-string err))
       (browsel-request "OPEN_TAB" (list :url url) client))))
  (browsel-activate-client client))

;; ── Public command ─────────────────────────────────────────────────────────

;;;###autoload
(defun browsel-browse-url (url &rest _ignored)
  "Open URL in a browser via browsel, honoring `browsel-url-routes'.

Resolution order:
  1. The first route in `browsel-url-routes' whose :pattern matches
     URL provides the target client, incognito flag, and tab-match
     regex.  Without an explicit :tab-match, the route's :pattern is
     reused for the already-open check — the same regex that picked
     the client also identifies an open tab as the same destination.
  2. When no route matches, `browsel-default-client' is used with
     incognito nil, and the already-open check is relaxed to a
     domain-substring match: any tab whose URL contains the
     requested URL's host (with leading `www.' stripped) is treated
     as the same tab.  So `https://amazon.ca/foo' reuses an open
     tab on `https://www.amazon.ca/dp/B1234'.

After resolution, if a tab qualifies as already open on the target
client (subject to the route's incognito constraint), the function
focuses it and brings the window forward.  Otherwise a fresh tab is
opened — in an incognito window for incognito routes, in the active
window otherwise.

Compatible with `browse-url-browser-function'; extra arguments are
accepted and ignored."
  (interactive (list (read-string "URL: ")))
  (let* ((route     (browsel-url-handler--match-route url))
         (client    (or (plist-get route :client) browsel-default-client))
         (incognito (plist-get route :incognito))
         (tab-match (or (plist-get route :tab-match)
                        ;; When a route matches without an explicit
                        ;; `:tab-match', reuse its `:pattern' for tab
                        ;; matching.  Same principle: if the regex
                        ;; identifies a URL as belonging to a client,
                        ;; any tab whose URL matches the same regex
                        ;; is the same logical destination.
                        (plist-get route :pattern)
                        ;; Fallback when no route matches: any open
                        ;; tab whose URL contains the requested URL's
                        ;; domain wins.  E.g. opening
                        ;; `https://amazon.ca/foo' jumps to a tab
                        ;; already on `https://www.amazon.ca/dp/B1234'.
                        (and (null route)
                             (let ((d (browsel-url-handler--domain url)))
                               (and d (regexp-quote d)))))))
    (unless client
      (user-error
       "browsel-url-handler: no client to route URL to (set browsel-default-client or add a :client to the matching route)"))
    (let ((existing (browsel-url-handler--find-existing-tab
                     url client incognito tab-match)))
      (if existing
          (browsel-url-handler--focus existing client)
        (browsel-url-handler--open-new url client incognito)))))

(provide 'browsel-url-handler)

;;; browsel-url-handler.el ends here
