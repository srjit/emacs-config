;;; contents for writing jupyter type org files
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sql . t)
   (ipython . t)
   (jupyter . t)))

;; put <py and press tab in org mode

(setq org-structure-template-alist
      '(("py" . "SRC jupyter-python :session py")
        ("i" . "SRC ipython :session")
        ("r" . "SRC R")
        ("s" . "SRC sql")
        ))


(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)
