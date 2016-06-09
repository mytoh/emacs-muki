;;; vihko.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)
(require 'helm)
(require 'helm-muki-util "helm-muki/util")

(defcustom helm-muki-vihko-directory
  "~/.org"
  "default directory"
  :type 'string
  :group 'helm-muki)

(defvar helm-muki-vihko-default-extension
  ".org"
  "default extension for note files")

(defcustom helm-muki-vihko-extension
  helm-muki-vihko-default-extension
  "extension for files"
  :type 'string
  :group 'helm-muki)

(defface helm-muki-vihko-file
    '((t :inherit font-lock-variable-name-face))
  "face for helm muki vihko file name"
  :group 'helm-muki)

(defface helm-muki-vihko-directory
    '((t :inherit font-lock-builtin-face))
  "face for helm muki vihko directory name"
  :group 'helm-muki)

(cl-defun helm-muki-vihko-init ()
  (setq helm-muki-vihko-candidates
        (helm-muki-vihko-create-candidates
         helm-muki-vihko-directory)))

(defface helm-muki-vihko-name
    '((t :inherit font-lock-builtin-face))
  "face for helm muki vihko name"
  :group 'helm-muki)

(cl-defun helm-muki-vihko-create-candidates (dir)
  (append
   (helm-muki-vihko-candidate-directory-file (helm-muki-vihko-filter-directory-file dir))
   (helm-muki-vihko-candidate-file (helm-muki-vihko-filter-file dir))))

(cl-defun helm-muki-vihko-candidate-file (files)
  (seq-map
   (lambda (file)
     (thread-first file
       file-name-base
       (propertize 'face 'helm-muki-vihko-file)
       (cons file)))
   files))

(cl-defun helm-muki-vihko-candidate-directory-file (files)
  (seq-map
   (lambda (file)
     (cl-letf ((subdir (thread-last (file-name-directory file)
                         (string-remove-prefix
                          (expand-file-name helm-muki-vihko-directory))
                         (string-remove-prefix "/")
                         (string-remove-suffix"/")))
               (notefile (file-name-base file)))
       (cons (seq-concatenate 'string
                              (propertize
                               subdir
                               'face 'helm-muki-vihko-directory)
                              "/"
                              (propertize
                               notefile
                               'face 'helm-muki-vihko-file))
             file)))
   files))

(cl-defun helm-muki-vihko-filter-file (dir)
  (directory-files dir 'absolute
                   (rx (: (* anything)
                          (eval helm-muki-vihko-default-extension))
                       line-end)))

(cl-defun helm-muki-vihko-filter-directory-file (dir)
  (cl-letf* ((all-files (directory-files helm-muki-vihko-directory 'absolute
                                         "^[^.]\\|^[^.]"))
             (directories (seq-filter #'file-directory-p all-files)))
    (apply #'seq-concatenate 'list
           (seq-map
            (lambda (d)
              (seq-filter
               (lambda (file)
                 (and (file-exists-p file)
                    (cl-equalp helm-muki-vihko-default-extension
                               (seq-concatenate 'string
                                                "."
                                                (file-name-extension file)))))

               (directory-files d 'absolute "^[^.]\\|^[^.]")))
            directories))))

(cl-defun helm-muki-vihko-action-not-found (candidate)
  (switch-to-buffer
   (find-file
    (format "%s/%s%s"
            helm-muki-vihko-directory candidate
            helm-muki-vihko-default-extension))))

(defclass helm-muki-vihko-source (helm-source-sync)
  ((init :initform #'helm-muki-vihko-init)
   (candidates :initform 'helm-muki-vihko-candidates)
   (action :initform
           (helm-make-actions
            "Open"  #'find-file))))

(defvar helm-source-muki-vihko
  (helm-make-source (helm-muki-source-name/mark "Vihko" "📓")
      'helm-muki-vihko-source))

(defclass helm-muki-vihko-not-found-source (helm-source-dummy)
  ((action :initform #'helm-muki-vihko-action-not-found)))

(defvar helm-source-muki-vihko-not-found
  (helm-make-source "Create org file"
      'helm-muki-vihko-not-found-source))

(provide 'helm-muki-vihko)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
