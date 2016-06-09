;;; program.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)
(require 'helm-external)
(require 'helm-muki-util "helm-muki/util")

(defcustom helm-muki-program-list
  '("conkeror")
  "default program list of
\"program-name\" or (\"program-name\" . \"full-path\")"
  :type 'list
  :group 'helm-muki)

(defface helm-muki-program-name
    '((t :inherit font-lock-variable-name-face))
  "face for helm muki program name"
  :group 'helm-muki)

(cl-defun helm-muki-program-create-candidates (init-list)
  (cl-letf* ((longest (helm-muki-string-longest
                       (seq-map (lambda (l) (if (listp l) (car l) l)) init-list)))
             (longest-width (string-width longest)))
    (seq-map
     (lambda (com)
       (if (stringp com)
           (helm-muki-program-format com :width longest-width)
         (helm-muki-program-format (car com)
                                   :path (expand-file-name (cdr com))
                                   :width longest-width)))
     init-list)))

(cl-defun helm-muki-program-format (program &key (path nil) width)
  (if path
      (cons (format "%s%s"
                    (helm-muki-string-pad
                     (propertize program
                                 'face 'helm-muki-program-name)
                     width)
                    path)
            path)
    (propertize program
                'face 'helm-muki-program-name)))

(cl-defun helm-muki-program-action-run (candidate)
  (cl-letf ((com (format "%s &" candidate)))
    (message "starting %s..." candidate)
    (start-process-shell-command com nil com)
    (message "started %s" candidate)))

(cl-defun helm-muki-program-init ()
  (setq helm-muki-program-candidates
        (helm-muki-program-create-candidates
         helm-muki-program-list)))

(defclass helm-muki-program-source (helm-source-sync)
  ((init :initform #'helm-muki-program-init)
   (candidates :initform 'helm-muki-program-candidates)
   (action :initform
           (helm-make-actions
            "Run" #'helm-muki-program-action-run))))

(defvar helm-source-muki-program
  (helm-make-source (helm-muki-source-name/mark "Program" "⚒")
      'helm-muki-program-source))

(provide 'helm-muki-program)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
