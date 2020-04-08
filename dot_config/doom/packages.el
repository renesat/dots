;; -*- no-byte-compile: t; -*-
;;; packages.el

;;; Code:

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(package! base16-theme)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;
(package! poetry) ; :recipe (:host github :repo "renesat/poetry.el")
(package! py-yapf)
(package! py-isort)

;;;;;;;;;;;
;; Julia ;;
;;;;;;;;;;;

(package! ob-julia :recipe
  (:host github
   :repo "phrb/ob-julia"
   :files ("*.el")))
