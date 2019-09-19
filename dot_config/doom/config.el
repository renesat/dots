;;; config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; General settings ;;
;;;;;;;;;;;;;;;;;;;;;;

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))

;; Keys
(after! evil
  (setq doom-leader-key "SPC")
  (setq doom-leader-alt-key "M-SPC")
  (setq doom-localleader-key ",")
  (setq doom-localleader-alt-key "M-,"))

;;; Fonts
(setq
 doom-font (font-spec :family "mononoki" :size 14)
 doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14))

;; Pretty symbol
(global-prettify-symbols-mode +1)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(after! python
  (add-hook 'before-save-hook 'py-isort-before-save)

  (add-hook 'python-mode-hook 'py-yapf-enable-on-save)

  ;; Poetry
  (map! :map python-mode-map
        :localleader
        :desc "poetry" "p" #'poetry))

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(after! org
  ;; Src blocks
  (map! :map org-mode-map
        :localleader
        (:prefix "e"
          (:prefix ("p" . "latex")
            :desc "to latex" "l" #'org-pandoc-export-to-latex
            :desc "to latex & open" "L" #'org-pandoc-export-to-latex-and-open
            :desc "to latex pdf" "p" #'org-pandoc-export-to-latex-pdf
            :desc "to latex pdf & open" "P" #'org-pandoc-export-to-latex-pdf-and-open))
        (:prefix ("o" . "src")
          :desc "previous block" "p" #'org-babel-previous-src-block
          :desc "next block" "n" #'org-babel-next-src-block
          :desc "execute block" "e" #'org-babel-execute-src-block)))

;;;;;;;;;;;;
;; Ledger ;;
;;;;;;;;;;;;

(after! ledger-mode
  (defvar finance-path
    "~/Finance")
  (defvar finance-get-current-listing-file
    (concat finance-path
            "/"
            (format-time-string "%Y" (current-time))
            ".journal"))
  (defun org-capture-finance-template ()
    (concat (format-time-string "%Y/%m/%d" (current-time))
            " * "
            "%^{Transaction title}\n"
            "    %^{From|Assets:Card:Sberbank}\n"
            "    %?"))
  (defvar org-capture-finance-headline
    (concat "# -*- mode: ledger -*-\n"
            "# Year "
            (format-time-string "%Y" (current-time))
            "\n\n"))
  (after! org
    (push '("l"
            "Ledger entry"
            plain
            (file finance-get-current-listing-file)
            (function org-capture-finance-template)
            :kill-buffer t
            :empty-lines 1)
          org-capture-templates)))

;;;;;;;;;;;
;; Email ;;
;;;;;;;;;;;

(after! mu4e
  (setq +mu4e-backend 'offlineimap)
  ;; (set-email-account! "EduPolitech"
  ;;   `((mu4e-sent-folder       . "/edu-politech/Sent Mail")
  ;;     (mu4e-drafts-folder     . "/edu-politech/Drafts")
  ;;     (mu4e-trash-folder      . "/edu-politech/Trash")
  ;;     (mu4e-refile-folder     . "/edu-politech/All Mail")
  ;;     (smtpmail-smtp-user     . ,(password-store-get "mail/edu-politech"))
  ;;     (user-mail-address      . ,(password-store-get "mail/edu-politech"))
  ;;     (mu4e-compose-signature . "---\nEdu Politech"))
  ;;   t)
  (set-email-account! "MainMail"
    `((mu4e-sent-folder       . "/mainmail/Sent Mail")
      (mu4e-drafts-folder     . "/mainmail/Drafts")
      (mu4e-trash-folder      . "/mainmail/Trash")
      (mu4e-refile-folder     . "/mainmail/All Mail")
      (smtpmail-smtp-user     . ,(auth-source-pass-get "user" "mail/mainmail"))
      (user-mail-address      . ,(auth-source-pass-get "user" "mail/mainmail"))
      (mu4e-compose-signature . "---\nMain Mail")))
  (set-email-account! "Paradox"
    `((mu4e-sent-folder       . "/paradox/Sent Mail")
      (mu4e-drafts-folder     . "/paradox/Drafts")
      (mu4e-trash-folder      . "/paradox/Trash")
      (mu4e-refile-folder     . "/paradox/All Mail")
      (smtpmail-smtp-user     . ,(auth-source-pass-get "user" "mail/paradox"))
      (user-mail-address      . ,(auth-source-pass-get "user" "mail/paradox"))
      (mu4e-compose-signature . "---\nParadox"))
    t))


;;;;;;;;;;;;;;;;;;;
;; Lecture notes ;;
;;;;;;;;;;;;;;;;;;;

(after! ox-pandoc
  (setq org-latex-to-pdf-process
        (setq org-pandoc-options-for-latex-pdf
              '((pdf-engine-opt . "-shell-escape")
                (pdf-engine . "xelatex")))))
