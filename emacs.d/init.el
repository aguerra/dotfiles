;; init.el

;; Look and feel
(load-theme 'wombat)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(set-frame-font "Ubuntu Mono-15" nil t)
(setq inhibit-startup-screen t
      linum-format "%4d "
      ring-bell-function 'ignore)
(tool-bar-mode -1)

;; Mode line settings
(column-number-mode 1)
(size-indication-mode 1)


;; Misc settings
(fset 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Package stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Key bindings
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Packages
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines)))

;;(use-package ivy
;;  :ensure t
;;  :config
;;  (ivy-mode 1)
;;  (setq ivy-use-virtual-buffers t)
;;  (setq enable-recursive-minibuffers t)
;;  (global-set-key (kbd "<f6>") 'ivy-resume))

;;(use-package clojure-mode
;;  :ensure t
;;  :config
;;  ;;(add-hook 'clojure-mode-hook #'paredit-mode)
;;  (add-hook 'clojure-mode-hook #'subword-mode))
;;  ;;(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;;(use-package cider
;;  :ensure t
;;  :config
;;  (setq nrepl-log-messages t)
;;  (add-hook 'cider-mode-hook #'eldoc-mode)
;;  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
;;  (setq cider-repl-history-file
;;    (expand-file-name "cider-history" user-emacs-directory)))
  ;;(add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq local-file (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-file)
    (load local-file))

;;(add-to-list 'load-path "~/.emacs.d/languages")
