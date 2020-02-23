;; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-j") 'delete-indentation)
(global-set-key (kbd "<f4>") 'query-replace-buffer)
(global-set-key (kbd "<f5>") 'query-replace-regexp-buffer)

;; Package list
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
  :bind
  ("s-D" . paredit-forward-down)
  :hook
  ((emacs-lisp-mode lisp-interaction-mode) . paredit-mode))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map [remap projectile-ag] 'counsel-ag)
  (setq projectile-switch-project-action 'projectile-commander)
  (projectile-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode))

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

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

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

;; Functions
(defun call-and-go-to-previous-buffer (func)
  "Call FUNC then go to the previous buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (call-interactively func)
    (pop-to-buffer buffer)))

(defun cider-toggle-show-repl ()
  "Toggle to show repl."
  (interactive)
  (let ((state (get 'cider-toggle-show-repl 'state)))
    (if state
        (progn
          (cider-switch-to-repl-buffer)
          (delete-window))
      (call-and-go-to-previous-buffer 'cider-switch-to-repl-buffer))
    (put 'cider-toggle-show-repl 'state (not state))))

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
  (call-on-buffer 'anzu-query-replace))

(defun query-replace-regexp-buffer ()
  "Call 'query-replace-regexp' on buffer."
  (interactive)
  (call-on-buffer 'anzu-query-replace-regexp))

(defun cider-namespace-refresh ()
  "Call 'clojure.tools.namespace.repl/refresh'."
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))
