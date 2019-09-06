;;; init.el --- Alex's Emacs configuration

;;; Commentary:

;;; Code:

;; Look and feel
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(blink-cursor-mode -1)
(menu-bar-mode -1)
(set-frame-font "Ubuntu Mono-14" nil t)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)

;; Mode line settings
(column-number-mode)
(size-indication-mode)

;; Misc settings
(add-hook 'focus-out-hook 'garbage-collect)
(delete-selection-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(setq auto-save-default nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq load-prefer-newer t)
(setq make-backup-files nil)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(set-default-coding-systems 'utf-8)

;; Package settings
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Key bindings
(global-set-key (kbd "M-[") 'beginning-of-buffer)
(global-set-key (kbd "M-]") 'end-of-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-j") 'delete-indentation)
(global-set-key (kbd "<f4>") 'query-replace-from-beginning-of-buffer)
(global-set-key (kbd "<f5>") 'query-replace-regexp-from-beginning-of-buffer)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Package list
(use-package abbrev
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package bm
  :ensure t
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  :bind
  (("<f2>" . bm-toggle)
   ("<f3>" . bm-next)
   ("C-<f3>" . bm-previous)))

(use-package cider
  :ensure t
  :bind
  (:map cider-mode-map
   ([f8] . cider-show-repl-buffer))
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . paredit-mode)))

(use-package clj-refactor
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (setq cljr-eagerly-build-asts-on-startup nil)
  (setq cljr-ignore-analyzer-errors t)
  (setq cljr-warn-on-eval nil))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'clojure-jack-in)
  (define-clojure-indent
    (against-background 'defun)
    (alet 'defun)
    (as-customer 1)
    (as-of 1)
    (constraint-fn 'defun)
    (data-fn 'defun)
    (facts 'defun)
    (fact 'defun)
    (for-all 'defun)
    (flow 'defun)
    (future-fact 'defun)
    (let-entities 'defun)
    (log-messages 'defun)
    (mlet 'defun)
    (provided 'defun)
    (request-context 'defun)
    (tabular 'defun)
    (tabular-flow 'defun)
    (verify 'defun))
  :hook
  ((clojure-mode . paredit-mode)
   (clojure-mode . subword-mode)
   (clojure-mode . clj-refactor-mode)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2)
  (global-company-mode 1)
  :hook
  (prog-mode . (lambda () (set (make-local-variable 'company-backends)
                               '((company-dabbrev-code company-yasnippet))))))

(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

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
   ("s-k" . crux-kill-whole-line)
   ("C-<backspace>" . crux-kill-line-backwards)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("s-J" . crux-top-join-line)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([(shift return)] . crux-smart-open-line)
   ([(control shift return)] . crux-smart-open-line-above)))

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package easy-kill
  :ensure t
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package elixir-mode
  :ensure t
  :hook
  ((elixir-mode . subword-mode)))

(use-package epa)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package fill-column-indicator
  :ensure t
  :bind
  ([f7] . fci-mode)
  :config
  (setq fci-rule-width 5))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1))

(use-package flyspell
  :config
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package imenu-anywhere
  :ensure t
  :bind
  ("s-i" . imenu-anywhere))

(use-package ivy
  :ensure t
  :bind
  ([f6] . ivy-resume)
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t))

(use-package linum
  :hook
  (prog-mode . linum-mode))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc -f markdown_github -t html5 -s --mathjax"))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package org
  :config
  (setq org-directory "~/git/notes"))

(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . paredit-mode))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "M-p s") 'counsel-ag)
  (projectile-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (recentf-mode 1))

(use-package savehist
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package smart-mode-line
   :ensure t
   :config
   (setq sml/no-confirm-load-theme t)
   (sml/setup))

(use-package super-save
  :ensure t
  :config
  (super-save-mode 1))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package whitespace
  :bind
  ([f1] . whitespace-cleanup)
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

;; Custom functions
(defun cider-show-repl-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (cider-switch-to-repl-buffer)
    (pop-to-buffer buffer)))

(defun clojure-jack-in ()
  "Start the repl on demand."
  (unless (cider-current-connection)
    (cider-jack-in '())))

(defun query-replace-from-beginning-of-buffer ()
  "Call 'query-replace' from beginning of buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'query-replace)))

(defun query-replace-regexp-from-beginning-of-buffer ()
  "Call 'query-replace-regexp' from beginning of buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'query-replace-regexp)))

(defconst custom-file (expand-file-name "custom.el" user-emacs-directory)
  "Changes from the customize user interface.")
(when (file-exists-p custom-file)
  (load custom-file))

(defconst local-file (expand-file-name "local.el" user-emacs-directory)
  "Local customization.")
(when (file-exists-p local-file)
  (load local-file))

;;; init.el ends here
