;;; ob-jupyter.el --- org-babel functions for Jupyter evaluation    -*- lexical-binding: t; -*-

;; Author: Corey Ducharme, based on ob-ipython by Grey Sexton
;; Keywords: literate programming, reproducible research
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (dash-functional "1.2.0") (f "0.17.2") (emacs "24"))

;; The MIT License (MIT)

;; Copyright (c) 2019 Corey Ducharme

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; Org-Babel support for evaluating source blocks using Jupyter.

;;; Code:

(require 'ob)
(require 'org-id)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'f)
(require 'json)
(require 'python)
(require 'cl)

;; variables

(defcustom ob-jupyter-kernel-extra-args '()
  "List of extra args to pass when creating a kernel."
  :group 'ob-jupyter)

(defcustom ob-jupyter-client-path
  (f-expand "./client.py"
            (or (-when-let (f load-file-name) (f-dirname f)) default-directory))
  "Path to the client script."
  :group 'ob-jupyter)

(defcustom ob-jupyter-command "jupyter"
  "Command to launch jupyter. Usually ipython or jupyter."
  :group 'ob-jupyter)

(defcustom ob-jupyter-resources-dir "./obipy-resources/"
  "Directory where resources (e.g images) are stored so that they
can be displayed."
  :group 'ob-jupyter)

(defcustom ob-jupyter-output-exec-count t
  "When non-nil decorate results with execution count metadata."
  :group 'ob-jupyter)

(defcustom ob-jupyter-output-example nil
  "When non-nil wrap multiline results in a BEGIN_EXAMPLE org block."
  :group 'ob-jupyter)

;; utils

(defun ob-jupyter--write-string-to-file (file string)
  (if string
      (with-temp-buffer
        (let ((require-final-newline nil)
              (before-save-hook nil))
          (insert string)
          (write-file file)))
    (error "No output was produced to write to a file.")))

(defun ob-jupyter--write-base64-string (file b64-string)
  (if b64-string
      (with-temp-buffer
        (let ((buffer-file-coding-system 'binary)
              (require-final-newline nil)
              (before-save-hook nil))
          (insert b64-string)
          (base64-decode-region (point-min) (point-max))
          (write-file file)))
    (error "No output was produced to write to a file.")))

(defun ob-jupyter--create-traceback-buffer (traceback)
  (let ((buf (get-buffer-create "*ob-jupyter-traceback*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (-each traceback
          (lambda (line) (insert (format "%s\n" line))))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (pop-to-buffer buf)))

(defun ob-jupyter--create-inspect-buffer (doc)
  (let ((buf (get-buffer-create "*ob-jupyter-inspect*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert doc)
        (ansi-color-apply-on-region (point-min) (point-max))
        (whitespace-cleanup)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun ob-jupyter--clear-output-buffer ()
  (let ((buf (get-buffer-create "*ob-jupyter-out*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun ob-jupyter--output (output append-p)
  (when (not (s-blank? output))
    (let ((buf (get-buffer-create "*ob-jupyter-out*")))
      (with-current-buffer buf
        (special-mode)
        (let ((inhibit-read-only t))
          (unless append-p (erase-buffer))
          (when (s-blank? (buffer-string)) (pop-to-buffer buf))
          (let ((p (point)))
            (if (= p (point-max))     ;allow tailing
                (progn (insert output)
                       (-when-let (w (get-buffer-window buf 'visible))
                         (set-window-point w (point-max))))
              (save-excursion
                (goto-char (point-max))
                (insert output)))
            (ansi-color-apply-on-region p (point-max))
            ;; this adds some support for control chars
            (comint-carriage-motion p (point-max)))
          (unless append-p (goto-char (point-min))))))))

(defun ob-jupyter--dump-error (err-msg)
  (with-current-buffer (get-buffer-create "*ob-jupyter-debug*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert err-msg)
      (goto-char (point-min))))
  (error "There was a fatal error trying to process the request. See *ob-jupyter-debug*"))

(defun ob-jupyter--generate-file-name (suffix)
  (s-concat (make-temp-name ob-jupyter-resources-dir) suffix))

;; process management

(defun ob-jupyter--kernel-file (name)
  (if (s-ends-with-p ".json" name)
      name
    (format "emacs-%s.json" name)))

(defun ob-jupyter--kernel-repl-cmd (name)
  (list ob-jupyter-command "console" "--simple-prompt" "--existing"
        (ob-jupyter--kernel-file name)))

;;; TODO: could setup a default sentinel that outputs error on process
;;; early termination
(defun ob-jupyter--create-process (name cmd)
  (let ((buf (get-buffer-create (format "*ob-jupyter-%s*" name))))
    (with-current-buffer buf (erase-buffer))
    (apply 'start-process name buf (car cmd) (cdr cmd))))

(defun ob-jupyter--get-python ()
  (locate-file (if (eq system-type 'windows-nt)
                   "python.exe"
                 (or python-shell-interpreter "python"))
               exec-path))

(defun ob-jupyter--create-kernel (name &optional kernel)
  (when (and (not (ignore-errors (process-live-p (get-process (format "kernel-%s" name)))))
             (not (s-ends-with-p ".json" name)))
    (ob-jupyter--create-process
     (format "kernel-%s" name)
     (append
      (list ob-jupyter-command "console" "--simple-prompt")
      (list "-f" (ob-jupyter--kernel-file name))
      (if kernel (list "--kernel" kernel) '())
      ;;should be last in the list of args
      ob-jupyter-kernel-extra-args))
    (sleep-for 1)))

(defun ob-jupyter--get-kernel-processes ()
  (let ((procs (-filter (lambda (p)
                          (s-starts-with? "kernel-" (process-name p)))
                        (process-list))))
    (-zip (-map (-compose (-partial 's-replace "kernel-" "")
                          'process-name)
                procs)
          procs)))

(defun ob-jupyter--create-repl (name)
  (let ((python-shell-completion-native-enable nil)
        (cmd (s-join " " (ob-jupyter--kernel-repl-cmd name))))
    (if (string= "default" name)
        (progn
          (run-python cmd nil nil)
          (format "*%s*" python-shell-buffer-name))
      (let ((process-name (format "Python:%s" name)))
        (get-buffer-process
         (python-shell-make-comint cmd process-name nil))
        (format "*%s*" process-name)))))

;; kernel management

(defun ob-jupyter--choose-kernel ()
  (let ((procs (ob-jupyter--get-kernel-processes)))
    (-> (ido-completing-read "kernel? " (-map 'car procs) nil t)
        (assoc procs)
        cdr
        list)))

;;; TODO: make this work on windows
;;; NOTE: interrupting remote kernel not currently possible, cf https://github.com/jupyter/jupyter_console/issues/150
(defun ob-jupyter-interrupt-kernel (proc)
  "Interrupt a running kernel. Useful for terminating infinite
loops etc. If things get really desparate try `ob-jupyter-kill-kernel'."
  (interactive (ob-jupyter--choose-kernel))
  (when proc
    ;; send SIGINT to "python -m ipykernel_launcher", a child of proc
    (let ((proc-name (process-name proc)))
      (accept-process-output
       ;; get the child pid with pgrep -P
       ;; NOTE assumes proc has only 1 child (seems to be true always)
       (make-process
        :name (concat proc-name "-child")
        :command (list "pgrep" "-P" (number-to-string
                                     (process-id proc)))
        ;; send SIGINT to child-proc
        :filter
        (lambda (proc child-proc-id)
          (make-process
           :name (concat "interrupt-" proc-name)
           :command (list "kill" "-2"
                          (string-trim child-proc-id)))))))))

(defun ob-jupyter-kill-kernel (proc)
  "Kill a kernel process. If you then re-evaluate a source block
a new kernel will be started."
  (interactive (ob-jupyter--choose-kernel))
  (when proc
    (delete-process proc)
    (message (format "Killed %s" (process-name proc)))))

;; evaluation

(defvar ob-jupyter--async-queue nil)

(defun ob-jupyter--enqueue (q x)
  (set q (append (symbol-value q) (list x))))

(defun ob-jupyter--dequeue (q)
  (let ((ret (car (symbol-value q))))
    (set q (cdr (symbol-value q)))
    ret))

(defun ob-jupyter--collect-json ()
  ;; this function assumes that we're in a buffer with the json lines
  (let ((json-array-type 'list))
    (let (acc)
      (while (not (= (point) (point-max)))
        (setq acc (cons (json-read) acc))
        (forward-line))
      (nreverse acc))))

(defun ob-jupyter--running-p ()
  (get-process "execute"))

(defun ob-jupyter--run-async (code name callback args)
  (let ((proc (ob-jupyter--create-process
               "execute"
               (list (ob-jupyter--get-python)
                     "--" ob-jupyter-client-path "--conn-file" name "--execute"))))
    ;; TODO: maybe add a way of disabling streaming output?
    ;; TODO: cleanup and break out - we parse twice, can we parse once?
    (set-process-filter
     proc
     (lexical-let ((parse-pos 0))
       (lambda (proc output)
         ;; not guaranteed to be given lines - we need to handle buffering
         (with-current-buffer (process-buffer proc)
           (goto-char (point-max))
           (insert output)
           (let ((json-array-type 'list))
             (goto-char parse-pos)
             (while (not (= (point) (point-max)))
               (condition-case nil
                   (progn (-> (json-read)
                              list
                              ob-jupyter--extract-output
                              (ob-jupyter--output t))
                          (forward-line)
                          (setq parse-pos (point)))
                 (error (goto-char (point-max))))))))))
    (set-process-sentinel
     proc
     (lexical-let ((callback callback)
                   (args args))
       (lambda (proc state)
         (when (not (process-live-p proc))
           (with-current-buffer (process-buffer proc)
             (goto-char (point-min))
             (apply callback (-> (ob-jupyter--collect-json)
                                 ob-jupyter--eval
                                 (cons args))))
           (ob-jupyter--maybe-run-async)))))
    (process-send-string proc code)
    (process-send-string proc "\n")
    (process-send-eof proc)))

(defun ob-jupyter--maybe-run-async ()
  (when (not (ob-jupyter--running-p))
    (-when-let (val (ob-jupyter--dequeue 'ob-jupyter--async-queue))
      (cl-destructuring-bind (code name callback args) val
        (ob-jupyter--run-async code name callback args)))))

(defun ob-jupyter--execute-request-async (code name callback args)
  (ob-jupyter--enqueue 'ob-jupyter--async-queue (list code name callback args))
  (ob-jupyter--maybe-run-async))

(defun ob-jupyter--execute-request (code name)
  (with-temp-buffer
    (let ((ret (apply 'call-process-region code nil
                      (ob-jupyter--get-python) nil t nil
                      (list "--" ob-jupyter-client-path "--conn-file" name "--execute"))))
      (if (> ret 0)
          (ob-jupyter--dump-error (buffer-string))
        (goto-char (point-min))
        (ob-jupyter--collect-json)))))

(defun ob-jupyter--extract-output (msgs)
  (->> msgs
       (-filter (lambda (msg) (string= "stream" (cdr (assoc 'msg_type msg)))))
       (-filter (lambda (msg) (-contains? '("stdout" "stderr")
                                          (->> msg (assoc 'content)
                                               (assoc 'name)
                                               cdr))))
       (-map (lambda (msg) (->> msg (assoc 'content) (assoc 'text) cdr)))
       (-reduce 's-concat)))

(defun ob-jupyter--extract-result (msgs)
  `((:value . ,(->> msgs
                    (-filter (lambda (msg)
                               (s-equals? "execute_result"
                                          (cdr (assoc 'msg_type msg)))))
                    (-mapcat (lambda (msg)
                               (->> msg (assoc 'content) (assoc 'data) cdr)))))
    (:display . ,(->> msgs
                      (-filter (lambda (msg)
                                 (s-equals? "display_data"
                                            (cdr (assoc 'msg_type msg)))))
                      (-mapcat (lambda (msg)
                                 (->> msg (assoc 'content) (assoc 'data) cdr)))))))

(defun ob-jupyter--extract-error (msgs)
  (let ((error-content
         (->> msgs
              (-filter (lambda (msg) (-contains? '("execute_reply" "inspect_reply")
                                                 (cdr (assoc 'msg_type msg)))))
              car
              (assoc 'content)
              cdr)))
    ;; TODO: this doesn't belong in this abstraction
    (ob-jupyter--create-traceback-buffer (cdr (assoc 'traceback error-content)))
    (format "%s: %s" (cdr (assoc 'ename error-content)) (cdr (assoc 'evalue error-content)))))

(defun ob-jupyter--extract-status (msgs)
  (->> msgs
       (-filter (lambda (msg) (-contains? '("execute_reply" "inspect_reply" "complete_reply")
                                          (cdr (assoc 'msg_type msg)))))
       car
       (assoc 'content)
       (assoc 'status)
       cdr))

(defun ob-jupyter--extract-execution-count (msgs)
  (->> msgs
       (-filter (lambda (msg) (-contains? '("execute_reply")
                                          (cdr (assoc 'msg_type msg)))))
       car
       (assoc 'content)
       (assoc 'execution_count)
       cdr))

(defun ob-jupyter--eval (service-response)
  (let ((status (ob-jupyter--extract-status service-response)))
    (cond ((string= "ok" status) `((:result . ,(ob-jupyter--extract-result service-response))
                                   (:output . ,(ob-jupyter--extract-output service-response))
                                   (:exec-count . ,(ob-jupyter--extract-execution-count service-response))))
          ((string= "abort" status) (error "Kernel execution aborted."))
          ((string= "error" status) (error (ob-jupyter--extract-error service-response))))))

;; inspection

(defun ob-jupyter--inspect-request (code &optional pos detail)
  (let ((input (json-encode `((code . ,code)
                              (pos . ,(or pos (length code)))
                              (detail . ,(or detail 0)))))
        (args (list "--" ob-jupyter-client-path
                    "--conn-file"
                    (ob-jupyter--get-session-from-edit-buffer (current-buffer))
                    "--inspect")))
    (with-temp-buffer
      (let ((ret (apply 'call-process-region input nil
                        (ob-jupyter--get-python) nil t nil
                        args)))
        (if (> ret 0)
            (ob-jupyter--dump-error (buffer-string))
          (goto-char (point-min))
          (ob-jupyter--collect-json))))))

(defun ob-jupyter--inspect (code pos)
  "Given a piece of code and a point position, return inspection results."
  (let* ((resp (ob-jupyter--inspect-request code pos 0))
         (status (ob-jupyter--extract-status resp)))
    (if (string= "ok" status)
        (->> resp
             (-filter (lambda (msg)
                        (-contains? '("execute_result" "display_data" "inspect_reply")
                                    (cdr (assoc 'msg_type msg)))))
             (-mapcat (lambda (msg)
                        (->> msg
                             (assoc 'content)
                             (assoc 'data)
                             cdr))))
      (error (ob-jupyter--extract-error resp)))))

(defun ob-jupyter-inspect (buffer pos)
  "Ask a kernel for documentation on the thing at POS in BUFFER."
  (interactive (list (current-buffer) (point)))
  (let ((code (with-current-buffer buffer
                (buffer-substring-no-properties (point-min) (point-max)))))
    (-if-let (result (->> (ob-jupyter--inspect code pos)
                          (assoc 'text/plain)
                          cdr))
        (ob-jupyter--create-inspect-buffer result)
      (message "No documentation was found."))))

;; completion

(defun ob-jupyter--complete-request (code &optional pos)
  (let ((input (json-encode `((code . ,code)
                              (pos . ,(or pos (length code))))))
        (args (list "--" ob-jupyter-client-path "--conn-file"
                    (ob-jupyter--get-session-from-edit-buffer (current-buffer))
                    "--complete")))
    (with-temp-buffer
      (let ((ret (apply 'call-process-region input nil
                        (ob-jupyter--get-python) nil t nil
                        args)))
        (if (> ret 0)
            (ob-jupyter--dump-error (buffer-string))
          (goto-char (point-min))
          (ob-jupyter--collect-json))))))

(defun ob-jupyter-completions (buffer pos)
  "Ask a kernel for completions on the thing at POS in BUFFER."
  (let* ((code (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
         (resp (ob-jupyter--complete-request code pos))
         (status (ob-jupyter--extract-status resp)))
    (if (not (string= "ok" status))
        '()
      (->> resp
           (-filter (lambda (msg)
                      (-contains? '("complete_reply")
                                  (cdr (assoc 'msg_type msg)))))
           (-mapcat (lambda (msg)
                      (->> msg
                           (assoc 'content)
                           cdr)))))))

(defun ob-jupyter--company-doc-buffer (doc)
  "Make company-suggested doc-buffer with ansi-color support."
  (let ((buf (company-doc-buffer doc)))
    (with-current-buffer buf
      (ansi-color-apply-on-region (point-min) (point-max)))
    buf))

(defun company-ob-jupyter (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ob-jupyter))
    (prefix (and ob-jupyter-mode
                 (let ((res (ob-jupyter-completions (current-buffer) (1- (point)))))
                   (substring-no-properties (buffer-string)
                                            (cdr (assoc 'cursor_start res))
                                            (cdr (assoc 'cursor_end res))))))
    (candidates (cons :async (lambda (cb)
                               (let ((res (ob-jupyter-completions
                                           (current-buffer) (1- (point)))))
                                 (funcall cb (cdr (assoc 'matches res)))))))
    (sorted t)
    (doc-buffer (ob-jupyter--company-doc-buffer
                 (cdr (assoc 'text/plain (ob-jupyter--inspect arg (length arg))))))))

;; mode

(define-minor-mode ob-jupyter-mode
  ""
  nil
  " ipy"
  '())

;; babel framework

;; double check
(add-to-list 'org-src-lang-modes '("jupyter" . python))
(add-hook 'org-mode-hook 'ob-jupyter-auto-configure-kernels)

(defvar ob-jupyter-configured-kernels nil)

(defun ob-jupyter--get-kernels ()
  "Return a list of available jupyter kernels and their corresponding languages.
The elements of the list have the form (\"kernel\" \"language\")."
  (and ob-jupyter-command
       (let*
           ((kernelspecs-cmd (s-concat ob-jupyter-command " kernelspec list --json"))
            (kernelspecs-text (shell-command-to-string kernelspecs-cmd))
            (kernelspecs
             (condition-case-unless-debug nil
                 (cdar (json-read-from-string kernelspecs-text))
               (json-readtable-error
                (message "Failed to list jupyter kernels with: %s, got:\n%s" kernelspecs-cmd kernelspecs-text)
                nil))))
         (when kernelspecs
           (-map (lambda (spec)
                   (cons (symbol-name (car spec))
                         (->> (cdr spec)
                              (assoc 'spec)
                              cdr
                              (assoc 'language)
                              cdr)))
                 kernelspecs)))))

(defun ob-jupyter--configure-kernel (kernel-lang)
  "Configure org mode to use specified kernel."
  (let* ((kernel (car kernel-lang))
         (language (cdr kernel-lang))
         (jupyter-lang (concat "jupyter-" language))
         (mode (or (cdr (assoc language org-src-lang-modes))
                   (intern (replace-regexp-in-string "[0-9]*" "" language))))
         (header-args (intern (concat "org-babel-default-header-args:" jupyter-lang))))
    (add-to-list 'org-src-lang-modes `(,jupyter-lang . ,mode))
    ;; Only set defaults if the corresponding variable is nil or does not
    ;; exist yet.
    (unless (and (boundp header-args) (symbol-value header-args))
      (set (intern (concat "org-babel-default-header-args:" jupyter-lang))
           `((:session . ,language)
             (:kernel . ,kernel)
	     (:language . ,language))))
    (defalias (intern (concat "org-babel-execute:" jupyter-lang))
      'org-babel-execute:jupyter)
    (defalias (intern (concat "org-babel-" jupyter-lang "-initiate-session"))
      'org-babel-jupyter-initiate-session)
    kernel-lang))

(defun ob-jupyter-auto-configure-kernels (&optional replace)
  "Auto-configure kernels for use with org-babel based on the
available kernelspecs of the current jupyter installation. If
REPLACE is non-nil, force configuring the kernels even if they
have previously been configured."
  (interactive (list t))
  (when (or replace (not ob-jupyter-configured-kernels))
    (setq ob-jupyter-configured-kernels
          (-map 'ob-jupyter--configure-kernel (ob-jupyter--get-kernels)))))

(defvar org-babel-default-header-args:jupyter '())

(defun org-babel-edit-prep:jupyter (info)
  ;; TODO: based on kernel, should change the major mode
  (ob-jupyter--create-kernel (->> info (nth 2) (assoc :session) cdr
                                  ob-jupyter--normalize-session)
                             (->> info (nth 2) (assoc :kernel) cdr))
  (ob-jupyter-mode +1))

(defun ob-jupyter--normalize-session (session)
  (if (string= "default" session)
      (error "default is reserved for when no name is provided. Please use a different session name.")
    (or session "default")))

(defun ob-jupyter--get-session-from-edit-buffer (buffer)
  (with-current-buffer buffer
    (->> org-src--babel-info
         (nth 2)
         (assoc :session)
         cdr
         ob-jupyter--normalize-session)))

(defun org-babel-execute:jupyter (body params)
  "Execute a block of Jupyter code with Babel.
This function is called by `org-babel-execute-src-block'."
  (ob-jupyter--clear-output-buffer)
  (if (cdr (assoc :async params))
      (ob-jupyter--execute-async body params)
    (ob-jupyter--execute-sync body params)))

(defun ob-jupyter--execute-async (body params)
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
         (result-params (cdr (assoc :result-params params)))
         (sentinel (jupyter--async-gen-sentinel))
	 (kernel (cdr (assoc :kernel params)))
	 (language (cdr (assoc :language params))))
    (ob-jupyter--create-kernel (ob-jupyter--normalize-session session) kernel)
    (ob-jupyter--execute-request-async
     (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                    params
				    (funcall (ob-jupyter-babel-variable-assignments language) params))
     (ob-jupyter--normalize-session session)
     (lambda (ret sentinel buffer file result-type result-params)
       (unless (member "silent" result-params)
         (let ((replacement (ob-jupyter--process-response ret file result-type)))
           (jupyter--async-replace-sentinel sentinel buffer replacement))))
     (list sentinel (current-buffer) file result-type result-params))
    (format "%s - %s" (length ob-jupyter--async-queue) sentinel)))

(defun ob-jupyter--execute-sync (body params)
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
	 (kernel (cdr (assoc :kernel params)))
	 (language (cdr (assoc :language params))))
    (ob-jupyter--create-kernel (ob-jupyter--normalize-session session) kernel)
    (-when-let (ret (ob-jupyter--eval
                     (ob-jupyter--execute-request
                      (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                     params
						     (funcall (ob-jupyter-babel-variable-assignments language) params))
                      (ob-jupyter--normalize-session session))))
      (ob-jupyter--process-response ret file result-type))))

(defmacro ob-jupyter-babel-variable-assignments (arg)
  `(intern (concat "org-babel-variable-assignments:" ,arg)))

(defun ob-jupyter--process-response (ret file result-type)
  (let ((result (cdr (assoc :result ret)))
        (output (cdr (assoc :output ret))))
    (if (eq result-type 'output)
        output
      (ob-jupyter--output output nil)
      (s-concat
       (if ob-jupyter-output-exec-count
           (format "# Out[%d]:\n" (cdr (assoc :exec-count ret))))
       (s-join "\n" (->> (-map (-partial 'ob-jupyter--render file)
                               (list (cdr (assoc :value result))
                                     (cdr (assoc :display result))))
                         (remove-if-not nil)))))))

(defun ob-jupyter--render (file-or-nil values)
  (let ((org (lambda (value) value))
        (png (lambda (value)
               (let ((file (or file-or-nil (ob-jupyter--generate-file-name ".png"))))
                 (ob-jupyter--write-base64-string file value)
                 (format "[[file:%s]]" file))))
        (svg (lambda (value)
               (let ((file (or file-or-nil (ob-jupyter--generate-file-name ".svg"))))
                 (ob-jupyter--write-string-to-file file value)
                 (format "[[file:%s]]" file))))
        (html (lambda (value)
                ;; ((eq (car value) 'text/html)
                ;;  (let ((pandoc (executable-find "pandoc")))
                ;;    (and pandoc (with-temp-buffer
                ;;                  (insert value)
                ;;                  (shell-command-on-region
                ;;                   (point-min) (point-max)
                ;;                   (format "%s -f html -t org" pandoc) t t)
                ;;                  (s-trim (buffer-string))))))
                ))
        (txt (lambda (value)
               (let ((lines (s-lines value)))
                 (if (and (cdr lines) ob-jupyter-output-example)
                     (->> lines
                          (-map 's-trim-right)
                          (s-join "\n  ")
                          (s-concat "  ")
                          (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"))
		   value)))))
    (or (-when-let (val (cdr (assoc 'text/org values))) (funcall org val))
        (-when-let (val (cdr (assoc 'image/png values))) (funcall png val))
        (-when-let (val (cdr (assoc 'image/svg+xml values))) (funcall svg val))
        (-when-let (val (cdr (assoc 'text/plain values))) (funcall txt val)))))

(defun org-babel-prep-session:jupyter (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references"
  ;; c-u c-c c-v c-z
  (error "Currently unsupported."))

(defun org-babel-load-session:jupyter (session body params)
  "Load BODY into SESSION."
  ;; c-c c-v c-l
  (error "Currently unsupported."))

(defun org-babel-jupyter-initiate-session (&optional session params)
  "Create a session named SESSION according to PARAMS."
  (if (string= session "none")
      (error "ob-jupyter currently only supports evaluation using a session.
Make sure your src block has a :session param.")
    (when (not (s-ends-with-p ".json" session))
      (ob-jupyter--create-kernel (ob-jupyter--normalize-session session)
                                 (cdr (assoc :kernel params))))
    (ob-jupyter--create-repl (ob-jupyter--normalize-session session))))

;; async

(defalias 'jupyter--async-gen-sentinel 'org-id-uuid)

(defun jupyter--async-replace-sentinel (sentinel buffer replacement)
  (save-window-excursion
    (save-excursion
      (save-restriction
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward sentinel)
          (re-search-backward "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)")
          (org-babel-remove-result)
          (org-babel-insert-result
           replacement
           (cdr (assoc :result-params (nth 2 (org-babel-get-src-block-info)))))
          (org-redisplay-inline-images))))))

(provide 'ob-jupyter)

;;; ob-jupyter.el ends here
