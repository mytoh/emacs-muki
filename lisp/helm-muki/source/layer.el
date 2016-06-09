;;; layer -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'helm-muki-util "helm-muki/util")

(defface helm-muki-layer-category
    '((t :inherit font-lock-variable-name-face))
  "face for helm muki directory name"
  :group 'helm-muki)

(defface helm-muki-layer-name
    '((t :inherit font-lock-function-name-face))
  "face for helm muki directory name"
  :group 'helm-muki)


(cl-defun helm-muki-layer-init ()
  (setq helm-muki-layer-candidates
        (helm-muki-layer-create-candidates
         muki-layer:root)))

(cl-defun helm-muki-layer-create-candidates (root)
  (cl-letf* ((cats (helm-muki-directory-files root)))
    (seq-mapcat
     (lambda (c)
       (seq-map
        (lambda (l)
          (cons (concat
                 (propertize c 'face 'helm-muki-layer-category)
                 "/"
                 (propertize l 'face 'helm-muki-layer-name))
                (expand-file-name
                 "init.el"
                 (expand-file-name l
                                   (expand-file-name c muki-layer:root)))))
        (helm-muki-layer-get-layers c)))
     cats)))

(cl-defun helm-muki-layer-get-layers (category)
  (cl-letf* ((layers (helm-muki-directory-files
                      (expand-file-name category muki-layer:root))))
    layers))

(cl-defun helm-muki-layer-action-open (candidate)
  (switch-to-buffer
   (find-file
    candidate)))

(defclass helm-muki-layer-source (helm-source-sync)
  ((init :initform #'helm-muki-layer-init)
   (candidates :initform 'helm-muki-layer-candidates)
   (action :initform
           (helm-make-actions
            "Open file" #'helm-muki-layer-action-open))))

(defvar helm-source-muki-layer
  (helm-make-source (helm-muki-source-name/mark  "Configuration layers" "🍰")
      'helm-muki-layer-source))

(provide 'helm-muki-layer)

;;; layer.el ends here
