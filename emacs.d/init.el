;; init.el

;; Look and feel
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(blink-cursor-mode -1)
(set-frame-font "Ubuntu Mono-14" nil t)
(setq inhibit-startup-screen t
      linum-format "%4d "
      ring-bell-function 'ignore)
(tool-bar-mode -1)

;; Mode line settings
(column-number-mode 1)
(size-indication-mode 1)

;; Misc settings
(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'prog-mode-hook (lambda () (linum-mode)))
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(prefer-coding-system 'utf-8)
(setq auto-save-default nil
      gc-cons-threshold 50000000
      load-prefer-newer t
      make-backup-files nil
      split-width-threshold 9999
      tab-always-indent 'complete)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq-default indent-tabs-mode nil)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Package settings
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Key bindings
(global-set-key (kbd "M-<up>") #'beginning-of-buffer)
(global-set-key (kbd "M-<down>") #'end-of-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f6>") 'fci-mode)
(global-set-key (kbd "<f3>") 'jump-to-mark)
(global-set-key (kbd "<f2>") 'push-mark-no-activate)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

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

(use-package anzu
  :ensure t
  :bind
  (([f4] . anzu-query-replace)
   ([f5] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode 1))

(use-package cider
  :ensure t
  :bind
  (:map cider-mode-map
   ([f9] . cider-show-repl-buffer))
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . paredit-mode)))

(use-package clj-refactor
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (setq cljr-warn-on-eval nil))

(use-package clojure-mode
  :ensure t
  :bind
  (:map clojure-mode-map
   ([f8] . cider-jack-in-clj))
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
  ((clojure-mode . paredit-mode)
   (clojure-mode . subword-mode)
   (clojure-mode . clj-refactor-mode)
   (clojure-mode . yas-minor-mode)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2)
  (global-company-mode 1))

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
  (("C-c t" . crux-visit-term-buffer)
   ("s-j" . crux-top-join-line)
   ("C-<backspace>" . crux-kill-line-backwards)))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 5))

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

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package yaml-mode
  :ensure t)

;; Custom functions
(defun cider-show-repl-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (cider-switch-to-repl-buffer)
    (pop-to-buffer buffer)))

;; From https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region"
  (interactive)
  (push-mark (point) t nil))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order"
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region"
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun projectile-ag-regex ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'projectile-ag)))

;; Changes from the customize UI
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Local file
(setq local-file (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-file)
  (load local-file))
