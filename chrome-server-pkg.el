;;; chrome-server-pkg.el --- Package descriptor for chrome-server  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; This file declares chrome-server as a multi-file Emacs package.
;; The optional backends (-www, -chatgpt, -youtube, -babel) are part
;; of the same distribution but are loaded only when the user calls
;; (require ...) on each.

(define-package "chrome-server" "0.5"
  "Bidirectional bridge between Emacs and a Chrome MV3 extension"
  '((emacs "27.1") (websocket "1.13"))
  :url       "https://github.com/dmgerman/chrome-server"
  :keywords  '("comm" "tools" "browser" "org")
  :maintainer '("Daniel M. German" . "dmg@turingmachine.org")
  :authors   '(("Daniel M. German" . "dmg@turingmachine.org")))

;;; chrome-server-pkg.el ends here
