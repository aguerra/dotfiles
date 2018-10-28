;; init.el

;; Look and feel
(load-theme 'wombat)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(set-face-attribute hl-line-face nil :underline nil)
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

;; Package settings
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Package list
(use-package paren
  :config
  (show-paren-mode 1))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; Changes from the customize UI
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Local hook
(setq local-hook (expand-file-name "local.el" user-emacs-directory))
(when (file-exists-p local-hook)
  (load local-hook))
