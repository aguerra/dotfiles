;; spacemacs

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-configuration-layers
   '(auto-completion
     c-c++
     clojure
     emacs-lisp
     git
     go
     helm
     markdown
     org
     python
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     yaml)))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-default-font '("Ubuntu Mono"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-maximized-at-startup t
   dotspacemacs-line-numbers '(:relative nil
                               :disabled-for-modes dired-mode
                                                   markdown-mode
                                                   org-mode
                                                   text-mode)
   dotspacemacs-whitespace-cleanup 'changed))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yapfify yaml-mode xterm-color smeargle shell-pop pyvenv pytest pyenv-mode py-isort pip-requirements orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup live-py-mode hy-mode dash-functional htmlize helm-pydoc helm-gitignore helm-company helm-c-yasnippet go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit transient git-commit with-editor eshell-z eshell-prompt-extras esh-help disaster cython-mode company-statistics company-go go-mode company-c-headers company-anaconda company cmake-mode clojure-snippets clang-format auto-yasnippet auto-dictionary anaconda-mode pythonic ac-ispell auto-complete clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
