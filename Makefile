# Top-level Makefile for browsel.
#
# Drives both the elisp side (compile, lint) and delegates to the
# extension's own Makefile for the WebExtension builds.
#
# Targets:
#   make             — compile + extension (default)
#   make lint        — package-lint every browsel*.el file
#   make compile     — byte-compile every browsel*.el file (errors on warning)
#   make extension   — rebuild Chrome + Firefox extension targets
#                      (delegates to extension/Makefile's default target)
#   make clean       — remove every *.elc file
#   make check       — compile + lint
#   make all         — check + extension
#
# Override the Emacs binary by passing EMACS=path/to/emacs.

EMACS ?= emacs

# Foundational files first so follow-on files can (require 'browsel) without
# erroring when compiled in isolation.
EL_FILES = browsel.el \
           browsel-www.el \
           browsel-chatgpt.el \
           browsel-youtube.el \
           browsel-tab-manager.el \
           browsel-babel.el \
           browsel-url-handler.el

# Project-local ELPA so the user's personal package directory is not touched
# and CI starts from a clean slate every run.
ELPA_DIR = .elpa

# Dependencies installed into the project-local ELPA before lint/compile.
# `websocket' is the runtime dependency declared in browsel.el's
# Package-Requires; `package-lint' is the lint tool itself.
DEPS = websocket package-lint

# Common Emacs invocation header: project-local package-user-dir, MELPA in
# package-archives, package-initialize so installed packages are on load-path.
EMACS_BATCH = $(EMACS) -Q --batch \
  --eval "(setq package-user-dir (expand-file-name \"$(ELPA_DIR)\"))" \
  --eval "(require 'package)" \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
  --eval "(package-initialize)"

.PHONY: default lint compile clean check extension all

# Default target: byte-compile the elisp and rebuild the WebExtension
# bundles.  Lint is not included here so the common edit-then-`make' loop
# stays fast; run `make check' or `make all' before committing.
default: compile extension

$(ELPA_DIR):
	@mkdir -p $@

$(ELPA_DIR)/.installed: | $(ELPA_DIR)
	$(EMACS_BATCH) \
	  --eval "(unless package-archive-contents (package-refresh-contents))" \
	  $(foreach pkg,$(DEPS),--eval "(unless (package-installed-p '$(pkg)) (package-install '$(pkg)))")
	@touch $@

lint: $(ELPA_DIR)/.installed
	$(EMACS_BATCH) \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit $(EL_FILES)

# Compile each file in a fresh subprocess so a definition leaked by one file
# cannot mask a missing `require' in another.  Treats every byte-compile
# warning as a hard error so CI catches them before commit.  `-L .' puts the
# source tree on the load-path so files compile in order even though they
# (require 'browsel) before browsel.elc exists.
compile: $(ELPA_DIR)/.installed
	@set -e; \
	for f in $(EL_FILES); do \
	  echo "==> compiling $$f"; \
	  $(EMACS_BATCH) \
	    --eval "(setq byte-compile-error-on-warn t)" \
	    -L . \
	    -f batch-byte-compile $$f; \
	done

clean:
	rm -f *.elc

# Delegate to the extension's own Makefile.  Its default target builds
# both Chrome and Firefox bundles.
extension:
	$(MAKE) -C extension

check: compile lint

all: check extension
