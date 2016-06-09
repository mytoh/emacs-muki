;;; ääliö -*- lexical-binding: t -*-

;;; Code:

(require 'seq)
(require 'helm-files)
(require 'helm-muki-util "helm-muki/util")

(defvar helm-muki-ääliö-candidates nil)

(defface helm-muki-ääliö-name
    '((t :inherit font-lock-variable-name-face))
  "face for helm ääliö directory name"
  :group 'helm-muki-ääliö)

(cl-defun helm-muki-ääliö-create-candidates ()
  (cl-letf ((root (string-trim (shell-command-to-string "pikkukivi ääliö root"))))
    (seq-map
     (lambda (dir)
       (cons (string-remove-prefix root dir)  dir))
     (split-string (shell-command-to-string "pikkukivi ääliö list -p")))))

(cl-defun helm-muki-ääliö-init ()
  (setq helm-muki-ääliö-candidates
        (helm-muki-ääliö-create-candidates)))

(cl-defun helm-muki-ääliö-action-open-find-file (candidate)
  (cl-letf ((helm-ff-transformer-show-only-basename t))
    (thread-first candidate
      file-name-as-directory
      helm-find-files-1)))

(defclass helm-muki-ääliö-source (helm-source-sync)
  ((init :initform #'helm-muki-ääliö-init)
   (candidates :initform 'helm-muki-ääliö-candidates)
   (action :initform
           (helm-make-actions
            "Open with helm ff" #'helm-muki-ääliö-action-open-find-file))))

(defvar helm-source-muki-ääliö
  (helm-make-source (helm-muki-source-name/mark "Project" "📁")
      'helm-muki-ääliö-source))


(provide 'helm-muki-ääliö)

;;; ääliö.el ends here
