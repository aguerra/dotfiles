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

(defun dotspacemacs/user-config ()
  (setq custom-file "~/.spacemacs.custom")
  (when (file-exists-p custom-file)
    (load custom-file)))
