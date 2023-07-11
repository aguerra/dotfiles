;; Look and feel
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(blink-cursor-mode -1)
(menu-bar-mode -1)
(set-frame-font "Monospace-12" nil t)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)

;; Mode line settings
(column-number-mode)
(size-indication-mode)

;; Misc settings
(delete-selection-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(prefer-coding-system 'utf-8)
(setq auto-save-default nil)
(setq confirm-kill-processes nil)
(setq gc-cons-threshold 50000000)
(setq load-prefer-newer t)
(setq make-backup-files nil)
(setq scroll-conservatively 100000
      scroll-preserve-screen-position t)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)

;; Setup package
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Key bindings
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Package list
(use-package anzu
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package cider
  :ensure t
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-save-file-on-load nil)
  :hook
  ((cider-repl-mode . paredit-mode)
   (cider-repl-mode . rainbow-delimiters-mode)))

(use-package clj-refactor
  :ensure t
  :defer t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package clojure-mode
  :ensure t
  :hook
  ((clojure-mode . paredit-mode)
   (clojure-mode . subword-mode)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (push '(company-capf :with company-yasnippet) company-backends)
  (global-company-mode))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  (global-set-key (kbd "C-c a") 'counsel-ag))

(use-package crux
  :ensure t
  :bind
  (("C-c o" . crux-open-with)
   ("C-c n" . crux-cleanup-buffer-or-region)
   ("C-c f" . crux-recentf-find-file)
   ("C-M-z" . crux-indent-defun)
   ("C-c u" . crux-view-url)
   ("C-c e" . crux-eval-and-replace)
   ("C-c w" . crux-swap-windows)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-buffer-and-file)
   ("C-c t" . crux-visit-term-buffer)
   ("C-c k" . crux-kill-other-buffers)
   ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-<backspace>" . crux-kill-line-backwards)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c j" . crux-top-join-line)
   ("C-c i" . crux-ispell-word-then-abbrev)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ([(shift return)] . crux-smart-open-line)
   ([(control shift return)] . crux-smart-open-line-above)))

(use-package diff-hl
  :ensure t
  :init
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t)
  (set-foreground-color "#d1d7e1"))

(use-package easy-kill
  :ensure t
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package elixir-mode
  :ensure t
  :hook
  (elixir-mode . subword-mode))

(use-package epa)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flyspell
  :config
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))

(use-package git-timemachine
  :ensure t
  :bind
  ("C-c g" . git-timemachine))

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  :hook
  (before-save . gofmt-before-save))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package imenu-anywhere
  :ensure t
  :bind
  ("C-c m" . ivy-imenu-anywhere))

(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 25)
  (setq ivy-use-virtual-buffers t))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((python-mode . lsp-deferred)
   (clojure-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc -f markdown_github -t html5 -s --mathjax"))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package org
  :config
  (setq org-directory "~/notes"))

(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode . paredit-mode)))

(use-package paren
  :config
  (show-paren-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/checkout"))
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-sort-order 'recentf)
  (projectile-mode)
  :bind
  (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (recentf-mode))

(use-package savehist
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-mode))

(use-package saveplace
  :config
  (save-place-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix nil))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package whitespace
  :bind
  ("C-c c" . whitespace-cleanup)
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

;; Hooks
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(defconst local-file (expand-file-name "local.el" user-emacs-directory)
  "Local customization.")

(when (file-exists-p local-file)
  (load local-file))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
