;; init.el

;; Look and feel
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(blink-cursor-mode -1)
(set-frame-font "Ubuntu Mono-15" nil t)
(setq inhibit-startup-screen t
      linum-format "%4d "
      ring-bell-function 'ignore)
(tool-bar-mode -1)

;; Mode line settings
(column-number-mode 1)
(size-indication-mode 1)

;; Misc settings
(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode)
            (hl-line-mode)
            (set-face-attribute hl-line-face nil :underline nil)))
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(prefer-coding-system 'utf-8)
(setq auto-save-default nil
      gc-cons-threshold 50000000
      load-prefer-newer t
      make-backup-files nil
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

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Package list
(use-package cider
  :after (clojure-mode)
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.3)
  (global-company-mode 1))

(use-package company-go
  :after (go-mode)
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 5))

(use-package go-mode
  :ensure t
  :config
  (setq exec-path (append '("~/go/bin") exec-path)
        gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package ido
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-ignore-extensions t)
  (ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode 1))

(use-package rainbow-delimiters
  :ensure t)

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-default-separator 'bar)
  (spaceline-spacemacs-theme))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing)))

;; Changes from the customize UI
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Local file
(setq local-file (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-file)
  (load local-file))
