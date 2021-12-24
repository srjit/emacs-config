
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta))


;; Sreejith's custom keybindings
(defun remap-up-key-in-shell ()
  (local-set-key (kbd "<up>") 'comint-previous-input))
(global-set-key (kbd "C-x <up>") 'previous-multiframe-window)
(global-set-key (kbd "C-x <down>") 'next-multiframe-window)
;; open recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;Duplicate lines
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-t") 'duplicate-line)

(global-set-key (kbd "M-c") 'capitalize-word)


(define-key global-map (kbd "RET") 'newline-and-indent)
(setq dired-dwim-target t)


;; Moving text around Up and Down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))


(defun move-text-down (arg)
       "Move region (transient-mark-mode active) or current line
  arg lines down."
       (interactive "*p")
       (move-text-internal arg))


(defun move-text-up (arg)
       "Move region (transient-mark-mode active) or current line
  arg lines up."
       (interactive "*p")
       (move-text-internal (- arg)))

(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)




(global-set-key (kbd "C-g") 'fiplr-find-file)

(global-set-key (kbd "C-x .") 'next-buffer)
(global-set-key (kbd "C-x ,") 'previous-buffer)
(global-set-key (kbd "C-x m") 'magit-status)


(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" "target" ".ensime_cache" "wordnet"))
                            (files ("*.jpg" "*.png" "*.zip" "*.pyc" "*.class" "*~"))))






;;  Cutting and pasting without copying to clipboard

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))
(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))



; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)



;; project explorer toggle
(global-set-key (kbd "C-x f") 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))



;; other links
(global-set-key (kbd "C-c l") 'org-store-link)


;; enable this for case sensitive find and replace
;;(customize-set-variable case-fold-search  nil)


;;(global-set-key "C-x C-f" 'helm-project-find-file)


(global-set-key (kbd "A-<down>") 'enlarge-window)
(global-set-key (kbd "A-<up>") 'shrink-window)
(global-set-key (kbd "A-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "A-<right>") 'shrink-window-horizontally)




;; Python Ide Keybinding
(global-set-key (kbd "C-x p") (lambda () (interactive) (load-file "~/.emacs.d/scripts/pyide.el")))
(global-set-key (kbd "M-.") 'jedi:goto-definition)
