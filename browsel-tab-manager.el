;;; browsel-tab-manager.el --- Jump to a browser tab via completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: comm, tools, browser
;; URL: https://github.com/dmgerman/browsel

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

;; Optional browsel module providing tab-management commands over
;; the browsel WebSocket bridge.
;;
;; Commands:
;;
;;   `browsel-tab-manager'
;;     List every open tab and focus the pick.  Each row renders
;;     `[flags] DOMAIN  TITLE' with separate faces so the three
;;     columns are visually distinct.  In-prompt action keys:
;;       ?       help (legend + bindings)
;;       RET     focus the tab + window, exit
;;       M-RET   preview: show the tab in its window, stay in the prompt
;;       M-k     close the highlighted tab (see -confirm-close)
;;       C-c c   copy URL to the kill ring, stay in the prompt
;;       C-t     cycle sort: mru -> title -> domain -> window
;;     Both M-k (no-confirm path) and C-t preserve the typed
;;     filter on re-entry.
;;
;;   `browsel-tab-manager-close-duplicates'
;;     Close duplicate tabs in one sweep.  URLs match after the
;;     `#fragment' is stripped; pinned tabs are skipped; the most-
;;     recently-accessed tab in each duplicate group is kept.
;;     Confirms with a count before closing anything.
;;
;; Which connected client (chrome / firefox) the tab manager
;; addresses is the same default the rest of browsel uses — set
;; `browsel-default-client' in `browsel.el' once and every command
;; honours it.  With several clients connected and no default set,
;; the resolver prompts.
;;
;; User-tunable variables: `browsel-tab-manager-sort',
;; `browsel-tab-manager-confirm-close',
;; `browsel-tab-manager-domain-column-width'.  Faces:
;; `browsel-tab-manager-flags-face', `-domain-face', `-title-face'.

;;; Code:

(require 'browsel)
(require 'url-parse)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;; ── Configuration ────────────────────────────────────────────────────────────

(defcustom browsel-tab-manager-domain-column-width 30
  "Width of the domain column in jump-to-tab completion candidates.
Domains longer than this are truncated with `…'; shorter ones get
padded with spaces so titles align across rows."
  :type 'integer
  :group 'browsel)

(defcustom browsel-tab-manager-sort 'mru
  "Default sort order for `browsel-tab-manager' candidates.
Symbol values:
  mru     by `lastAccessed' descending (most-recently-used first)
  title   alphabetically by tab title
  domain  alphabetically by URL host
  window  by `windowId' then `index' (visual tab order per window)
The in-prompt `C-t' key cycles through these without leaving the
minibuffer."
  :type '(choice (const :tag "Most recently used" mru)
                 (const :tag "Title"               title)
                 (const :tag "Domain"              domain)
                 (const :tag "Window order"        window))
  :group 'browsel)

(defconst browsel-tab-manager--sort-cycle '(mru title domain window)
  "Order the `C-t' key steps through in jump-to-tab.")

(defcustom browsel-tab-manager-confirm-close t
  "Whether the in-prompt close key asks before closing a tab.
When non-nil, `M-k' inside `browsel-tab-manager' prompts
with `yes-or-no-p' showing the tab's title before issuing CLOSE_TAB.
When nil, closures fire immediately on the first keystroke.
Has no effect on `browsel-tab-manager-close-duplicates', which has
its own count-based confirmation."
  :type 'boolean
  :group 'browsel)

(defface browsel-tab-manager-flags-face
  '((t :inherit shadow))
  "Face for the `[asi]' flag prefix in jump-to-tab candidates."
  :group 'browsel)

(defface browsel-tab-manager-domain-face
  '((t :inherit font-lock-keyword-face))
  "Face for the domain column in jump-to-tab candidates."
  :group 'browsel)

(defface browsel-tab-manager-title-face
  '((t :inherit default))
  "Face for the title column in jump-to-tab candidates."
  :group 'browsel)

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
  "Return the propertized display string for TAB.
Format is `[asi] DOMAIN  TITLE' where each segment carries its own
face (`browsel-tab-manager-flags-face',
`browsel-tab-manager-domain-face',
`browsel-tab-manager-title-face') so they are visually distinct.
The domain is padded or truncated to
`browsel-tab-manager-domain-column-width' so titles line up across
rows.  Two spaces separate the columns — a single space inside the
domain padding would blend with truncated-but-fits values."
  (let* ((flags  (propertize (browsel-tab-manager--flags tab)
                             'face 'browsel-tab-manager-flags-face))
         (host   (browsel-tab-manager--url-host (plist-get tab :url)))
         (domain (propertize
                  (truncate-string-to-width
                   host browsel-tab-manager-domain-column-width
                   0 ?\s "…")
                  'face 'browsel-tab-manager-domain-face))
         (title  (propertize (or (plist-get tab :title) "(no title)")
                             'face 'browsel-tab-manager-title-face)))
    (concat flags " " domain "  " title)))

(defun browsel-tab-manager--candidates (tabs)
  "Return an alist of (DISPLAY . TAB) pairs for TABS.
DISPLAY is the propertized `[asi] DOMAIN  TITLE' string from
`browsel-tab-manager--display-base'; bases that would collide
\(`equal' compares the underlying text only) get a propertized
\" (#ID)\" suffix in the flags face so each completion key is
unique without distorting the column alignment."
  (let ((bases (mapcar #'browsel-tab-manager--display-base tabs)))
    (cl-mapcar
     (lambda (tab base)
       (cons (if (> (cl-count base bases :test #'equal) 1)
                 (concat base
                         (propertize (format " (#%s)" (plist-get tab :id))
                                     'face 'browsel-tab-manager-flags-face))
               base)
             tab))
     tabs bases)))

(defun browsel-tab-manager--sort-tabs (tabs sort)
  "Return TABS sorted according to SORT.
SORT is one of the symbols in `browsel-tab-manager--sort-cycle' —
`mru', `title', `domain', or `window'.  Unknown values pass TABS
through unchanged."
  (pcase sort
    ('mru
     (seq-sort-by (lambda (tab) (or (plist-get tab :lastAccessed) 0))
                  #'> tabs))
    ('title
     (seq-sort-by (lambda (tab)
                    (downcase (or (plist-get tab :title) "")))
                  #'string< tabs))
    ('domain
     (seq-sort-by (lambda (tab)
                    (downcase (browsel-tab-manager--url-host
                               (plist-get tab :url))))
                  #'string< tabs))
    ('window
     (seq-sort (lambda (a b)
                 (let ((wa (or (plist-get a :windowId) 0))
                       (wb (or (plist-get b :windowId) 0)))
                   (if (= wa wb)
                       (< (or (plist-get a :index) 0)
                          (or (plist-get b :index) 0))
                     (< wa wb))))
               tabs))
    (_ tabs)))

(defun browsel-tab-manager--next-sort (current)
  "Return the sort key that follows CURRENT in `--sort-cycle'."
  (let ((tail (cdr (memq current browsel-tab-manager--sort-cycle))))
    (or (car tail) (car browsel-tab-manager--sort-cycle))))

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

;; ── Duplicate detection ────────────────────────────────────────────────────

(defun browsel-tab-manager--strip-url-hash (url)
  "Return URL with any `#...' fragment removed.
Query parameters are kept, so `?id=1' and `?id=2' remain distinct.
Two tabs at the same page but different anchors thus collapse to one."
  (if (and (stringp url) (string-match "\\`\\([^#]*\\)" url))
      (match-string 1 url)
    (or url "")))

(defun browsel-tab-manager--duplicate-victims (tabs)
  "Return the subset of TABS that a duplicate-tab sweep would close.
Pinned tabs are skipped entirely.  In each remaining group (keyed on
URL minus `#fragment') the tab with the highest `lastAccessed' is the
keeper; the others end up in the returned list."
  (let* ((live   (seq-remove (lambda (tab) (eq (plist-get tab :pinned) t))
                             tabs))
         (groups (seq-group-by
                  (lambda (tab)
                    (browsel-tab-manager--strip-url-hash
                     (plist-get tab :url)))
                  live))
         (dup    (seq-filter (lambda (g) (> (length (cdr g)) 1)) groups)))
    (apply #'append
           (mapcar (lambda (g)
                     (cdr (seq-sort-by
                           (lambda (tab) (or (plist-get tab :lastAccessed) 0))
                           #'>
                           (cdr g))))
                   dup))))

;; ── Public commands ─────────────────────────────────────────────────────────

;;;###autoload
(defun browsel-tab-manager-close-duplicates ()
  "Close duplicate tabs in the connected browser, keeping the most recent.
Two tabs are duplicates when their URLs match after stripping any
`#...' fragment; query parameters (`?a=...') are preserved.  Pinned
tabs are skipped — never compared, never closed.  In each duplicate
group the tab with the highest `lastAccessed' is kept and the rest
are closed.  Prompts for confirmation with a count before closing
anything.

Note: `chrome.tabs.remove' bypasses any in-page `beforeunload' prompt
\(those only fire from user-initiated UI closes\); pages with unsaved
form state close without a dialog.  Firefox behaves the same way."
  (interactive)
  (let* ((client  (browsel--read-client-interactive))
         (tabs    (browsel-request "GET_ALL_TABS" nil client))
         (victims (browsel-tab-manager--duplicate-victims tabs))
         (n       (length victims)))
    (cond
     ((zerop n)
      (message "browsel-tab-manager: no duplicate tabs in %s" client))
     ((not (y-or-n-p (format "Close %d duplicate tab(s) in %s? " n client)))
      (message "browsel-tab-manager: aborted (would have closed %d)" n))
     (t
      (let ((outcomes (mapcar
                       (lambda (tab)
                         (condition-case err
                             (progn
                               (browsel-request "CLOSE_TAB"
                                                (list :id (plist-get tab :id))
                                                client)
                               t)
                           (error
                            (message "Could not close tab %s (%s): %s"
                                     (plist-get tab :id)
                                     (plist-get tab :url)
                                     (error-message-string err))
                            nil)))
                       victims)))
        (message "browsel-tab-manager: closed %d/%d duplicate tab(s) in %s"
                 (seq-count #'identity outcomes) n client))))))


;; ── In-prompt action keys for jump-to-tab ──────────────────────────────────
;;
;; While `browsel-tab-manager' is reading a candidate the
;; following keys operate on the highlighted candidate:
;;
;;   ?       show a one-shot help buffer with the legend + bindings
;;   C-c c   copy the candidate's URL to the kill ring (stay in prompt)
;;   M-k     close the candidate's tab and stay in the prompt
;;   RET     focus the tab and exit (default)
;;
;; Both action keys are side-effect-only and do not exit the
;; minibuffer.  The closed tab stays in the in-memory candidate list
;; for the lifetime of the prompt — picking it after closure will
;; simply fail when FOCUS_TAB cannot find it.

(defvar browsel-tab-manager--current-alist nil
  "Dynamic binding: alist of (DISPLAY . TAB) for the active prompt.
Bound by `browsel-tab-manager' for the duration of the
`completing-read' call so the in-prompt action commands can look up
the tab plist that backs the highlighted display string.")

(defvar browsel-tab-manager--current-client nil
  "Dynamic binding: connected client name for the active prompt.
Bound alongside `browsel-tab-manager--current-alist' so in-prompt
action commands (close, copy) target the same browser the prompt was
opened against, without re-resolving the client mid-completion.")

(defvar browsel-tab-manager--current-sort nil
  "Dynamic binding: sort key the active prompt is showing.
Used by `browsel-tab-manager-jump-cycle-sort' to compute the next
sort key without re-reading `browsel-tab-manager-sort' (which is the
default, not the current state).")

(defun browsel-tab-manager--current-display ()
  "Return the display string of the highlighted completion candidate.
Prefers `vertico--candidate' when Vertico is the active frontend in
this minibuffer (detected via `bound-and-true-p' on its buffer-local
marker, since the defvar is bound globally), then the first entry of
`completion-all-sorted-completions' (Icomplete and default cycle),
and finally falls back to the typed minibuffer contents passed
through `try-completion'."
  (cond
   ((and (fboundp 'vertico--candidate)
         (bound-and-true-p vertico--input))
    (vertico--candidate))
   ((and (boundp 'completion-all-sorted-completions)
         completion-all-sorted-completions)
    (car completion-all-sorted-completions))
   (t (let* ((input (minibuffer-contents-no-properties))
             (m     (and minibuffer-completion-table
                         (try-completion input
                                         minibuffer-completion-table))))
        (cond ((stringp m) m)
              ((eq m t)    input)
              (t           input))))))

(defun browsel-tab-manager--current-tab ()
  "Return the tab plist for the highlighted candidate, or nil."
  (let ((display (browsel-tab-manager--current-display)))
    (and (stringp display)
         (cdr (assoc display browsel-tab-manager--current-alist)))))

(defun browsel-tab-manager-jump-help ()
  "Show in-prompt help for `browsel-tab-manager'."
  (interactive)
  (with-help-window "*browsel-tab-manager help*"
    (princ "browsel-tab-manager — jump-to-tab in-prompt actions\n")
    (princ "\n")
    (princ "  Flag prefix [asi]:\n")
    (princ "    a — active tab in its window\n")
    (princ "    s — sound (audible)\n")
    (princ "    i — incognito\n")
    (princ "  Trailing (#ID) appears only when two tabs would render to\n")
    (princ "  the same display; the numeric tab id disambiguates them.\n")
    (princ "\n")
    (princ "  Action keys (operate on the highlighted candidate):\n")
    (princ "    ?       this help\n")
    (princ "    C-c c   copy URL to the kill ring (stay in prompt)\n")
    (princ "    M-k     close the tab and stay in the prompt\n")
    (princ "    M-RET   show the tab in Chrome without raising the window\n")
    (princ "            (preview — stay in the prompt, Emacs keeps focus)\n")
    (princ "    C-t     cycle sort order (mru -> title -> domain -> window)\n")
    (princ "    RET     focus the tab + window, exit the prompt\n")))

(defun browsel-tab-manager-jump-show-tab ()
  "Make the highlighted tab the active tab in its browser window.
Calls `FOCUS_TAB' without `:focusWindow' so the tab becomes visible
inside Chrome but the OS-level window is not raised — Emacs keeps
focus.  After the FOCUS_TAB call the prompt re-enters with fresh
tabs so the `[a]' flag reflects the new active tab; the highlight
stays on the shown candidate and any typed filter is preserved."
  (interactive)
  (let ((tab (browsel-tab-manager--current-tab)))
    (if (null tab)
        (message "No candidate selected")
      (condition-case err
          (progn
            (browsel-request "FOCUS_TAB"
                             `(:id ,(plist-get tab :id))
                             browsel-tab-manager--current-client)
            (throw 'browsel-tab-manager--cycle
                   (list :sort   browsel-tab-manager--current-sort
                         :input  (minibuffer-contents-no-properties)
                         :anchor (plist-get tab :id))))
        (error
         (message "Could not show %s: %s"
                  (plist-get tab :title)
                  (error-message-string err)))))))

(defun browsel-tab-manager-jump-copy-url ()
  "Copy the highlighted candidate's tab URL to the kill ring."
  (interactive)
  (let* ((tab (browsel-tab-manager--current-tab))
         (url (and tab (plist-get tab :url))))
    (if (and (stringp url) (not (string-empty-p url)))
        (progn (kill-new url)
               (message "Copied: %s" url))
      (message "No candidate selected"))))

(defun browsel-tab-manager-jump-cycle-sort ()
  "Re-open the jump-to-tab prompt under the next sort key.
Signals the outer wrapper via `throw' so the prompt re-enters with
fresh tabs, the next sort from `browsel-tab-manager--sort-cycle',
and the typed-text preserved as the initial input — your filter
survives the cycle."
  (interactive)
  (throw 'browsel-tab-manager--cycle
         (list :sort   (browsel-tab-manager--next-sort
                        browsel-tab-manager--current-sort)
               :input  (minibuffer-contents-no-properties)
               :anchor nil)))

(defun browsel-tab-manager-jump-close-tab ()
  "Close the highlighted candidate's tab.
Honours `browsel-tab-manager-confirm-close': when non-nil prompts
with `yes-or-no-p' and leaves you in the prompt afterwards (so a
deliberate close is followed by a stable candidate view).  When
nil the closure fires immediately and the prompt is re-entered
with a fresh `GET_ALL_TABS' under the current sort so the closed
tab is gone from the list — chains of `M-k' without typed text
land cleanly.

The re-entry signal is a `throw' to `browsel-tab-manager--cycle';
the catch in `browsel-tab-manager--run-prompt' receives the
current sort key and tail-recurses."
  (interactive)
  (let ((tab (browsel-tab-manager--current-tab)))
    (cond
     ((null tab)
      (message "No candidate selected"))
     ((and browsel-tab-manager-confirm-close
           (not (yes-or-no-p (format "Close tab: %s? "
                                     (plist-get tab :title)))))
      (message "Close aborted"))
     (t
      (condition-case err
          (progn
            (browsel-request "CLOSE_TAB"
                             `(:id ,(plist-get tab :id))
                             browsel-tab-manager--current-client)
            (unless browsel-tab-manager-confirm-close
              (throw 'browsel-tab-manager--cycle
                     (list :sort   browsel-tab-manager--current-sort
                           :input  (minibuffer-contents-no-properties)
                           :anchor (browsel-tab-manager--anchor-above-id)))))
        (error
         (message "Could not close %s: %s"
                  (plist-get tab :title)
                  (error-message-string err))))))))

(defconst browsel-tab-manager--jump-bindings
  '(("?"     . browsel-tab-manager-jump-help)
    ("C-c c" . browsel-tab-manager-jump-copy-url)
    ("M-k"   . browsel-tab-manager-jump-close-tab)
    ("M-RET" . browsel-tab-manager-jump-show-tab)
    ("C-t"   . browsel-tab-manager-jump-cycle-sort))
  "Single source of truth for jump-to-tab in-prompt keys.
Installed onto whatever local map the active completion frontend
\(vertico, icomplete, default) provides; see
`browsel-tab-manager--install-keys'.")

(defun browsel-tab-manager--install-keys ()
  "Add the in-prompt action keys to the current minibuffer's local map.
Earlier code composed `browsel-tab-manager-jump-map' on top of the
frontend's map via `make-composed-keymap', but that diverted RET
lookups through the wrong fallback chain (the user's typed input
came back empty).  Copying the active local map and inserting our
bindings into the copy keeps the frontend's bindings intact and
co-located with ours."
  (let ((map (copy-keymap (current-local-map))))
    (dolist (binding browsel-tab-manager--jump-bindings)
      (define-key map (kbd (car binding)) (cdr binding)))
    (use-local-map map)))

(defun browsel-tab-manager--anchor-above-id ()
  "Return the `:id' of the tab one row above the highlighted one.
Reads vertico's index/candidates and resolves the row above to a tab
plist via `browsel-tab-manager--current-alist'.  The id is the stable
identity used by the re-entered prompt to relocate the highlight
even when the candidate's display string has changed (e.g. the
`[a]' flag flipped).  Returns nil outside vertico or when no row is
above."
  (when (and (bound-and-true-p vertico--input)
             (boundp 'vertico--index)
             (boundp 'vertico--candidates))
    (let ((idx vertico--index))
      (when (and (numberp idx) vertico--candidates (>= idx 1))
        (let ((display (nth (1- idx) vertico--candidates)))
          (plist-get (cdr (assoc display
                                 browsel-tab-manager--current-alist))
                     :id))))))

(defun browsel-tab-manager--jump-to-anchor (anchor-id)
  "Move vertico's highlight to the candidate whose tab has ANCHOR-ID.
ANCHOR-ID is a tab `:id'.  We look it up in the freshly-built
candidate alist to recover the (possibly changed) display string,
then locate that string in vertico's current candidates.  Runs as a
0-timer so it fires after vertico's first refresh.  No-ops outside
vertico, or when the tab is no longer present, or when the typed
filter has excluded it."
  (when anchor-id
    (run-at-time
     0 nil
     (lambda ()
       (when (and (bound-and-true-p vertico--input)
                  (fboundp 'vertico--goto)
                  (boundp 'vertico--candidates)
                  vertico--candidates)
         (let* ((entry (seq-find
                        (lambda (e)
                          (equal anchor-id
                                 (plist-get (cdr e) :id)))
                        browsel-tab-manager--current-alist))
                (display (and entry (car entry)))
                (idx (and display
                          (cl-position display
                                       vertico--candidates
                                       :test #'equal))))
           (when (and idx (>= idx 0))
             (vertico--goto idx))))))))

(defun browsel-tab-manager--run-prompt (client sort &optional initial-input anchor)
  "Run one jump-to-tab prompt under SORT for CLIENT and dispatch.
Each call fetches a fresh `GET_ALL_TABS' so closures and reorderings
between prompts (e.g. after `M-k') are reflected immediately.
INITIAL-INPUT, when a non-empty string, pre-fills the minibuffer.
ANCHOR, when a non-nil display string, becomes the candidate the
highlight lands on after vertico has refreshed — used by `M-k' to
keep the user one row above where the closed tab was.  When `M-k'
or `C-t' throw, the in-prompt command sends a plist
\(:sort :input :anchor) to `browsel-tab-manager--cycle' and this
function tail-recurses with it; otherwise it focuses the chosen
tab and returns."
  (let* ((tabs (browsel-request "GET_ALL_TABS" nil client)))
    (unless (and (listp tabs) tabs)
      (user-error "browsel-tab-manager: no tabs returned from %s" client))
    (let* ((sorted (browsel-tab-manager--sort-tabs tabs sort))
           (alist  (browsel-tab-manager--candidates sorted))
           (browsel-tab-manager--current-alist  alist)
           (browsel-tab-manager--current-client client)
           (browsel-tab-manager--current-sort   sort)
           (setup-fn (lambda ()
                       (browsel-tab-manager--install-keys)
                       (browsel-tab-manager--jump-to-anchor anchor)))
           (next
            ;; `catch' captures the non-local-exit signals from the
            ;; in-prompt action commands (M-k / M-RET / C-t); errors
            ;; raised inside the body go through the inner
            ;; `condition-case' and are reported explicitly so the
            ;; user always sees what went wrong rather than relying
            ;; on Emacs's top-level handler.
            (catch 'browsel-tab-manager--cycle
              (condition-case err
                  (let* ((pick (minibuffer-with-setup-hook (:append setup-fn)
                                 (completing-read
                                  (format "Tab [%s] (%s): " sort client)
                                  (browsel-tab-manager--completion-table alist)
                                  nil t
                                  (and (stringp initial-input)
                                       (not (string-empty-p initial-input))
                                       initial-input))))
                         ;; Some completion frontends strip text
                         ;; properties on exit (vertico) while others
                         ;; preserve them; look up under both.
                         (key  (and (stringp pick)
                                    (substring-no-properties pick)))
                         (tab  (or (cdr (assoc key  alist))
                                   (cdr (assoc pick alist)))))
                    (unless tab
                      (user-error "browsel-tab-manager: no tab matches %S"
                                  pick))
                    (browsel-request "FOCUS_TAB"
                                     `(:id ,(plist-get tab :id) :focusWindow t)
                                     client)
                    (browsel-activate-client client)
                    nil)
                (error
                 (message "browsel-tab-manager: %s"
                          (error-message-string err))
                 nil)))))
      (when (browsel-tab-manager--valid-next-p next)
        (browsel-tab-manager--run-prompt client
                                         (plist-get next :sort)
                                         (plist-get next :input)
                                         (plist-get next :anchor))))))

(defun browsel-tab-manager--valid-next-p (next)
  "Return non-nil when NEXT is a plist shaped like our throw protocol.
Belt-and-suspenders: ensures a stray `throw' to our tag with the
wrong payload cannot send the prompt loop recursing with junk.
Checks that NEXT is a non-empty list whose first element is a
keyword and that contains a `:sort' key our sort cycle recognizes."
  (and (listp next)
       next
       (keywordp (car next))
       (memq (plist-get next :sort) browsel-tab-manager--sort-cycle)))

;;;###autoload
(defun browsel-tab-manager ()
  "Focus a tab in the connected browser, picked via completion.
Lists every open tab from the resolved client (see
`browsel-default-client').
The initial sort order comes from `browsel-tab-manager-sort'
\(default `mru'); use `C-t' inside the prompt to cycle through
mru / title / domain / window orders.  RET focuses the chosen tab
and its parent window via the extension's FOCUS_TAB handler.

In-prompt keys (see also `?' inside the prompt):
  ?       legend + action-key help
  C-c c   copy the highlighted candidate's URL to the kill ring
  M-k     close the highlighted candidate's tab and stay in the prompt
  M-RET   show the highlighted tab in Chrome without raising its window
  C-t     cycle the sort order"
  (interactive)
  (browsel-tab-manager--run-prompt (browsel--read-client-interactive)
                                   browsel-tab-manager-sort))

(provide 'browsel-tab-manager)


;; Local Variables:
;; package-lint-main-file: "browsel.el"
;; End:

;;; browsel-tab-manager.el ends here
