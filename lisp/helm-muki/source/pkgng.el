;;; pkgng -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:


(setq helm-muki-pkgng
      (helm-build-sync-source "helm-muki-pkgng"
        :candidates
        (lambda ()
          (shell-command-to-string "pkg query %n"))
        :candidate-transformer
        (lambda (candidates _source)
          (seq-map
           (lambda (cand)
             (cons (propertize cand 'face 'font-lock-variable-name-face)
                   cand))
           candidates))))

;; (helm :sources (helm-build-async-source "test2"
;;                  :candidates-process
;;                  (lambda ()
;;                    (start-process "echo" nil "echo" "a\nb\nc\nd\ne")))
;;       :buffer "*helm test2*")

(cl-defun helm-muki-pkgng ()
  (interactive)
  (helm :sources 'helm-muki-pkgng
        :buffer "*helm muki pkgng*"))

;;; pkgng.el ends here
