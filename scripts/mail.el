(require 'mu4e)

;; default
(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-trash-folder  "/trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/Gmail" . ?i)
        ("/sent" . ?s)
        ("/trash" . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "sreejith2904@gmail.com"
 user-full-name  "Sreejith Sreekumar"
 ;; message-signature
 ;;  (concat
 ;;    "Foo X. Bar\n"
 ;;    "http://www.example.com\n")
 )

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
