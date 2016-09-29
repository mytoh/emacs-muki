;;; directory.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(require 'helm-files)
(require 'helm-muki-util "helm-muki/util")

(defcustom helm-muki-directory-list
  '(("Home" . "~"))
  "default directory list"
  :type '(alist :key-type string :value-type string)
  :group 'helm-muki)

(defvar helm-muki-directory-candidates nil)

(defface helm-muki-directory-name
    '((t :inherit font-lock-variable-name-face))
  "face for helm muki directory name"
  :group 'helm-muki)

(cl-defun helm-muki-directory-create-candidates (init-list)
  (cl-letf* ((longest (helm-muki-string-longest (seq-map #'car init-list)))
             (longest-width (string-width longest)))
    (seq-map
     (lambda (lst)
       (cl-letf* ((dir (thread-first lst
                         cdr
                         expand-file-name
                         file-name-as-directory))
                  (disp (format "%s%s"
                                (helm-muki-string-pad
                                 (propertize (car lst)
                                             'face 'helm-muki-directory-name)
                                 longest-width)
                                dir))
                  (real dir))
         (cons disp real)))
     init-list)))

(cl-defun helm-muki-directory-init ()
  (setq helm-muki-directory-candidates
        (helm-muki-directory-create-candidates
         helm-muki-directory-list)))

(cl-defun helm-muki-directory-action-open-find-file (candidate)
  (cl-letf ((helm-ff-transformer-show-only-basename t))
    (helm-find-files-1 candidate)))

(cl-defun helm-muki-directory-add-subdirectories (path)
  (cl-letf* ((files (directory-files (expand-file-name path) t "[.][^.]+\\|^[^.].*"))
             (subs (cl-member-if #'file-directory-p files))
             (dirs (seq-map (lambda (d) (cons (file-name-base d) d)) subs)))
    (setq helm-muki-directory-list
          (append helm-muki-directory-list dirs))))

(defclass helm-muki-directory-source (helm-source-sync)
  ((init :initform #'helm-muki-directory-init)
   (candidates :initform 'helm-muki-directory-candidates)
   (action :initform
           (helm-make-actions
            "Open with helm ff" #'helm-muki-directory-action-open-find-file))))

(defvar helm-source-muki-directory
  (helm-make-source (helm-muki-source-name/mark "Directory" "📁")
      'helm-muki-directory-source))

(provide 'helm-muki-directory)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
