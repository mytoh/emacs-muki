;;; pocket -- pocket -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'helm)
(require 'pocket-lib)
(require 'pocket-reader)
(require 'map)
(require 'subr-x)

(require 'helm-muki-util "helm-muki/util")

(defun helm-muki-pocket-candidates (&rest _)
  (cl-letf* ((items (helm-muki-pocket-get-items 50))
             (longest
              (apply #'max
                     (mapcar
                      (lambda (i)  (length (map-elt (cdr i) 'given_title)))
                      items))))
    (colle:map
     (pcase-lambda ((and item `(,_id . ,data)))
       (cons (helm-muki-pocket-format-candidate-display
              longest data)
             item))
     items)))

(defun helm-muki-pocket-format-candidate-display (longest data)
  (format (concat
           "%-" (number-to-string (+ 3 longest)) "s"
           (make-string 10 ?\ )
           "%s")
          (propertize (map-elt data 'given_title)
                      'face 'font-lock-type-face)
          (propertize (pocket-reader--url-domain
                       (or (map-elt data 'resolved_url)
                          (map-elt data 'amp_url)
                          (map-elt data 'given_url)))
                      'face 'font-lock-builtin-face
                      'help-echo (map-elt data 'resolved_url))))

(defun helm-muki-pocket-get-items (limit)
  (map-elt (pocket-lib-get :count limit) 'list))

(defvar helm-muki-pocket-candidates nil)

(defun helm-muki-pocket-init ()
  (setq helm-muki-pocket-candidates
        (helm-muki-pocket-candidates)))

(defun helm-muki-pocket-action-open-url (item)
  (message "Opening '%s'"
           (map-elt (cdr item) 'resolved_url))
  (browse-url (map-elt (cdr item)  'resolved_url)))

(defun helm-muki-pocket-action-archive (item)
  (pocket-api-archive (car item))
  (message "Item '%s (%s)' was archived."
           (map-elt (cdr item) 'given_title)
           (map-elt (cdr item) 'resolved_url)))

(defun helm-muki-pocket-action-copy-url (item)
  (kill-new (map-elt (cdr item) 'resolved_url)))

(defclass helm-muki-pocket-source (helm-source-sync)
  ((init :initform #'helm-muki-pocket-init)
   (candidates :initform 'helm-muki-pocket-candidates)
   (action :initform
     (helm-make-actions
      "Open with default browser" #'helm-muki-pocket-action-open-url
      "Archive" #'helm-muki-pocket-action-archive
      "Copy URL" #'helm-muki-pocket-action-copy-url))))

(defvar helm-source-muki-pocket
  (helm-make-source (helm-muki-source-name/mark "pocket" "👝")
      'helm-muki-pocket-source))

(provide 'helm-muki-pocket)

;;; pocket.el ends here
