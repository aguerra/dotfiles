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

(use-package cider
  :after (clojure-mode)
  :ensure t
  :bind
  (:map cider-mode-map
   ([f9] . cider-show-repl-buffer))
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (setq cider-prompt-for-symbol nil
        cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package clj-refactor
  :after (clojure-mode)
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-n")
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'yas-minor-mode))

(use-package clojure-mode
  :ensure t
  :bind
  (:map clojure-mode-map
   ([f8] . cider-jack-in-clj))
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
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
    (verify 'defun)))

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

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "<f6>") 'ivy-resume))

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
  (setq markdown-fontify-code-blocks-natively t
        markdown-command
        "pandoc -f markdown_github -t html5 -s --mathjax"))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

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
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring))
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

(use-package zop-to-char
  :ensure t
  :bind
  (("M-z" . zop-up-to-char)
   ("M-Z" . zop-to-char)))

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
