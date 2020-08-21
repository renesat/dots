;; -*- no-byte-compile: t; -*-
;;; packages.el

;;; Code:

;;;;;;;;;
;; Org ;;
;;;;;;;;;

(package! org-ref)
(package! reftex)
(package! org-pretty-tags)
(package! org-super-agenda)
(package! org-ql)
(package! org-roam-server)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! ranger
          :recipe (:host github :repo "ralesi/ranger.el"))
(package! doct)
(package! org-alert)
(package! org-chef)



;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

;; (package! base16-theme)
(package! nord-theme)
(package! zenburn-theme)


;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(package! poetry) ; :recipe (:host github :repo "renesat/poetry.el")
(package! py-yapf)
(package! py-isort)

;;;;;;;;;;;
;; Julia ;;
;;;;;;;;;;;

;;;;;;;;;;
;; SCAD ;;
;;;;;;;;;;

(package! scad-mode)

;;;;;;;;;;;;;;;
;; GhostText ;;
;;;;;;;;;;;;;;;

(package! atomic-chrome)

;;;;;;;;;;;
;; Lines ;;
;;;;;;;;;;;

(package! linum-relative)

;;;;;;;;;;
;; Mail ;;
;;;;;;;;;;

(package! mu4e-alert)
(package! khardel)

;;;;;;;;;;;;;;;;;;;;
;; Activity watch ;;
;;;;;;;;;;;;;;;;;;;;

(package! activity-watch-mode)

;;;;;;;;;;;;;;;;
;; Web search ;;
;;;;;;;;;;;;;;;;

;; (package! engine-mode)

;;;;;;;;;;;;;;;;
;; Large file ;;
;;;;;;;;;;;;;;;;

(package! vlf)
