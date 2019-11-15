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
(setq load-prefer-newer t)
(setq make-backup-files nil)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(set-default-coding-systems 'utf-8)
(setq package-check-signature nil)

;; Package settings
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Key bindings
(global-set-key (kbd "M-[") 'beginning-of-buffer)
(global-set-key (kbd "M-]") 'end-of-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-j") 'delete-indentation)
(global-set-key (kbd "<f4>") 'query-replace-buffer)
(global-set-key (kbd "<f5>") 'query-replace-regexp-buffer)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; We need this function earlier
(defun call-and-go-to-previous-buffer (func)
  "Call FUNC then go to the previous buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (call-interactively func)
    (pop-to-buffer buffer)))

;; Package list
(use-package abbrev
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package anzu
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package avy
  :ensure t
  :bind
  (("s-." . avy-goto-word-or-subword-1)
   ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package beacon
  :ensure t
  :config
  (beacon-mode))

(use-package bm
  :ensure t
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  :bind
  (("<f2>" . bm-toggle)
   ("<f3>" . bm-next)
   ("C-<f3>" . bm-previous)))

(use-package browse-kill-ring
  :ensure t
  :bind
  ([f10] . browse-kill-ring))

(use-package cider
  :ensure t
  :bind
  (:map cider-mode-map
   ([f9] . cider-show-repl-buffer))
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . paredit-mode)))

(use-package cider-eval-sexp-fu
  :ensure t)

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
  ((clojure-mode . maybe-cider-jack-in)
   (clojure-mode . paredit-mode)
   (clojure-mode . subword-mode)
   (clojure-mode . clj-refactor-mode)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2)
  (global-company-mode)
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
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package diminish
  :ensure t)

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
  (exec-path-from-shell-copy-env "NU_HOME")
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package fill-column-indicator
  :ensure t
  :bind
  ([f8] . fci-mode)
  :config
  (setq fci-rule-width 5))

(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-joker
  :ensure t)

(use-package flyspell
  :config
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))

(use-package git-timemachine
  :ensure t
  :bind
  ("s-g" . git-timemachine))

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
  ("s-i" . ivy-imenu-anywhere))

(use-package ivy
  :ensure t
  :bind
  ([f7] . ivy-resume)
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
  (show-paren-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "M-p s") 'counsel-ag)
  (setq projectile-switch-project-action 'my-projectile-switch-project-action)
  (projectile-mode))

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

(use-package smart-mode-line
   :ensure t
   :config
   (setq sml/no-confirm-load-theme t)
   (setq sml/shortener-func 'sml/not-shorten-directory)
   (sml/setup))

(use-package super-save
  :ensure t
  :config
  (super-save-mode))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package treemacs
  :ensure t
  :bind
  ([f6] . treemacs)
  :config
  (setq treemacs-project-follow-cleanup t))

(use-package treemacs-projectile
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode))

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
  ([f1] . whitespace-cleanup)
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
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package zop-to-char
  :ensure t
  :bind
  (([remap zap-to-char] . zop-to-char)
   ([remap zap-up-to-char] . zop-up-to-char)))

;; Functions
(defun cider-show-repl-buffer ()
  "Show cider repl buffer."
  (interactive)
  (call-and-go-to-previous-buffer 'cider-switch-to-repl-buffer))

(defun maybe-cider-jack-in ()
  "Start the repl if there is no current connection."
  (unless (cider-current-connection)
    (cider-jack-in '())))

(defun call-on-buffer (func)
  "Call FUNC on buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively func)))

(defun query-replace-buffer ()
  "Call 'query-replace' on buffer."
  (interactive)
  (call-on-buffer 'query-replace))

(defun query-replace-regexp-buffer ()
  "Call 'query-replace-regexp' on buffer."
  (interactive)
  (call-on-buffer 'query-replace-regexp))

(defun projectile-recentf-or-find-file ()
  "Call 'projectile-recentf' if there are recent files, else 'projectile-find-file'."
  (interactive)
  (let ((files (projectile-recentf-files)))
    (if (> (length files) 0)
        (projectile-recentf)
      (projectile-find-file))))

(defun my-projectile-switch-project-action ()
  "Projectile default action when switching projects."
  (interactive)
  (projectile-recentf-or-find-file)
  (call-and-go-to-previous-buffer 'treemacs-select-window))

;; Hooks
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(defconst local-file (expand-file-name "local.el" user-emacs-directory)
  "Local customization.")

(when (file-exists-p local-file)
  (load local-file))

;; Local Variables:
;; byte-compile-warnings: (not noruntime redefine)
;; End:

;;; init.el ends here
