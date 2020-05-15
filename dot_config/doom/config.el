;;; config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; General settings ;;
;;;;;;;;;;;;;;;;;;;;;;
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))

;; Keys
(after! evil
  (setq doom-leader-key "SPC")
  (setq doom-leader-alt-key "M-SPC")
  (setq doom-localleader-key ",")
  (setq doom-localleader-alt-key "M-,"))

;; Theme
(load-theme 'nord t)
;; (set-face-attribute 'font-lock-comment-face nil
                    ;; :foreground "#81A1C1") ; nord9
;; (set-face-attribute 'vertical-border nil
                    ;; :foreground "#EBCB8B") ; nord13
;; (load-theme 'base16-nord t)

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
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-process-alist
        '((dvipng :programs
                  ("xelatex" "dvipng")
                  :description "dvi > png" :message "you need to install the programs: latex and dvipng."
                  :image-input-type "xdv" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("dvipng -D %D -T tight -o %O %f"))
          (dvisvgm :programs
                   ("xelatex" "dvisvgm")
                   :description "dvi > svg"
                   :message "you need to install the programs: latex and dvisvgm."
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler
                   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("xelatex" "convert")
                       :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O")))))

;;;;;;;;;;;
;; Julia ;;
;;;;;;;;;;;



(setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                    (:session . "jl")
                                                    (:kernel . "julia-1.4")))
;; (setq lsp-julia-default-environment "~/.julia/environments/v1.4")

;; (after! (:and julia-mode julia-formatter)
;;   (add-hook 'julia-mode-hook
;;             '(lambda() (julia-formatter-server-start))))

;;;;;;;;
;; JS ;;
;;;;;;;;

(defun my-js-mode-hook ()
  (setq indent-tabs-mode nil
        ;; tab-width 2
        js-indent-level 4))
(add-hook 'js-mode-hook 'my-js-mode-hook)

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

;; (after! mu4e
;;   (setq +mu4e-backend 'offlineimap)
;;   ;; (set-email-account! "EduPolitech"
;;   ;;   `((mu4e-sent-folder       . "/edu-politech/Sent Mail")
;;   ;;     (mu4e-drafts-folder     . "/edu-politech/Drafts")
;;   ;;     (mu4e-trash-folder      . "/edu-politech/Trash")
;;   ;;     (mu4e-refile-folder     . "/edu-politech/All Mail")
;;   ;;     (smtpmail-smtp-user     . ,(password-store-get "mail/edu-politech"))
;;   ;;     (user-mail-address      . ,(password-store-get "mail/edu-politech"))
;;   ;;     (mu4e-compose-signature . "---\nEdu Politech"))
;;   ;;   t)
;;   (set-email-account! "MainMail"
;;     `((mu4e-sent-folder       . "/mainmail/Sent Mail")
;;       (mu4e-drafts-folder     . "/mainmail/Drafts")
;;       (mu4e-trash-folder      . "/mainmail/Trash")
;;       (mu4e-refile-folder     . "/mainmail/All Mail")
;;       (smtpmail-smtp-user     . ,(auth-source-pass-get "user" "mail/mainmail"))
;;       (user-mail-address      . ,(auth-source-pass-get "user" "mail/mainmail"))
;;       (mu4e-compose-signature . "---\nMain Mail")))
;;   (set-email-account! "Paradox"
;;     `((mu4e-sent-folder       . "/paradox/Sent Mail")
;;       (mu4e-drafts-folder     . "/paradox/Drafts")
;;       (mu4e-trash-folder      . "/paradox/Trash")
;;       (mu4e-refile-folder     . "/paradox/All Mail")
;;       (smtpmail-smtp-user     . ,(auth-source-pass-get "user" "mail/paradox"))
;;       (user-mail-address      . ,(auth-source-pass-get "user" "mail/paradox"))
;;       (mu4e-compose-signature . "---\nParadox"))
;;     t))


;;;;;;;;;;;;;;;;;;;
;; Lecture notes ;;
;;;;;;;;;;;;;;;;;;;

(after! ox-latex
  (add-to-list 'org-latex-classes
              '("empty"
                "\\documentclass{article}
                [NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("empty-beamer" "\\documentclass[presentation]{beamer}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted")))
  (setq org-latex-pdf-process
      (mapcar
       (lambda (s)
         (replace-regexp-in-string "%latex " "%latex -shell-escape " s))
       org-latex-pdf-process))
  (setq org-latex-to-pdf-process
       (setq org-pandoc-options-for-latex-pdf
             '((pdf-engine-opt . "-shell-escape")
               (filter . "pandoc-minted")
               (pdf-engine . "xelatex")))))

;;;;;;;;;;;;;;
;; Snippets ;;
;;;;;;;;;;;;;;

(after! snippets
  (setq yas-indent-line 'fixed))

(defun r-env ()
  (setq exec-path
        (append exec-path
                '("/home/renesat/Apps/MyR/bin")))
  (setenv "R_HOME"
          "/home/renesat/Apps/MyR/lib64/R")
  (setenv "PATH"
          (concat
           "/home/renesat/Apps/MyR/bin:"
           (getenv "PATH"))))
(add-hook 'org-mode-hook 'r-env)
