;;; util.el -*- lexical-binding: t -*-

(require 'cl-lib) ; don't use cl.el
(require 'seq)

(defun helm-muki-string-longest (strs)
  (seq-reduce
   (lambda (a b)
     (cl-letf* ((al (string-width a))
                (bl (string-width b)))
       (if (< al bl)  b a)))
   strs ""))

(defun helm-muki-string-pad (str longest)
  (cl-letf ((len (- longest (string-width str)))
            (offset 4))
    (seq-concatenate 'string
                     str
                     (make-string (+ offset len) ?\ ))))

(defun helm-muki-source-name/mark (name mark)
  (cond ((window-system)
         (format " %s %s" mark name))
        (t
         name)))

(cl-defun helm-muki-directory-files (dir &optional full)
  (directory-files dir full "^[^.]"))

(provide 'helm-muki-util)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
