
;; ;; set keybindings
(load-file "~/.emacs.d/scripts/keybindings.el")
(load-file "~/.emacs.d/scripts/elegance.el")


(load-file "~/.emacs.d/scripts/styling.el")
(load-file "~/.emacs.d/scripts/fira-code-mode.el")
(put 'scroll-left 'disabled nil)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/"))


(setq user-full-name "Sreejith Sreekumar")
(setq user-mail-address "sreejith2904@gmail.com")

(add-hook 'python-mode-hook 'fira-code-mode)
(xclip-mode 1)
(undo-tree-mode 1)

(require 'undo-tree)
(setenv "SHELL" "/bin/zsh")
(setq shell-file-name "/bin/zsh")

(defun ipython ()
  (interactive)
  (term "/Users/sreejithsreekumar/venvs/mllib2/bin/ipython"))
(setq python-shell-interpreter "/Users/sreejithsreekumar/venvs/mllib2/bin/ipython")





(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(setq venv-location "~/venvs")

;; (load-file "~/.emacs.d/scripts/ox-ipynb.el")
;; (display-time-mode 1)
;; (display-battery-mode 1)


(setq company-idle-delay 0)
(setq recentf-max-menu-items 10)

(load-file "~/.emacs.d/scripts/orgmode.el")



(getenv "PATH")
 (setenv "PATH"
(concat
 "/Library/TeX/texbin/" ":"

(getenv "PATH")))



;; (load-file "~/.emacs.d/scripts/nano-sidebar.el")
;; (load-file "~/.emacs.d/scripts/nano-sidebar-ibuffer.el")

;; Uncomment the line below to enable jupyter notebook via org mode
;; (load-file "~/.emacs.d/scripts/ob-jupyter.el") 
;; (load-file "~/.emacs.d/scripts/org-ipython.el") 



(setq initial-major-mode 'org-mode)

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))




(setq load-prefer-newer t)
(setq mu4e-mu-binary "/opt/homebrew/bin/mu")
(add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.6.10/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(load-file "~/.emacs.d/scripts/mu4e-dashboard.el")

(provide 'custom)
;;;  custom.el ends here
 
