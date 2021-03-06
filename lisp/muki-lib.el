;;; muki-lib -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

;; http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
(cl-defmacro req (lib . body)
  "load library if file is exits"
  (declare (debug t)
           (indent 1))
  `(cond
    ((locate-library (symbol-name ,lib))
     (require ,lib nil 'noerror)
     (muki:log "loading " (propertize (symbol-name ,lib)
                                    'face 'font-lock-variable-name-face))
     ,@body
     (muki:log "loaded " (propertize (symbol-name ,lib)
                                   'face 'font-lock-keyword-face)))
    (t (message "%s not loaded" (symbol-name ,lib)))))

(cl-defmacro pak (package . body)
  "execute body when package is installed"
  (declare (debug t)
           (indent 1))
  `(cond
    ((or (package-installed-p ,package)
        (locate-library (symbol-name ,package)))
     (muki:log "found package " (propertize (symbol-name ,package)
                                          'face 'font-lock-variable-name-face))
     ,@body)
    (t (message "%s not found" (symbol-name ,package)))))

(cl-defmacro liby (library . body)
  "execute body when library found"
  (declare (debug t)
           (indent 1))
  `(cond
    ((locate-library (symbol-name ,library))
     (muki:log "found library " (propertize (symbol-name ,library)
                                          'face 'font-lock-variable-name-face))
     ,@body)
    (t (message "%s not found" (symbol-name ,library)))))

(cl-defmacro command (funcs lib)
  (declare (debug t))
  `(cl-locally
    ,@(seq-map (lambda (f) `(autoload ',f ,lib nil t))
               funcs)))

(cl-defun defery (library)
  (run-with-idle-timer 2 nil
                       (lambda ()
                         (message "loading %s" library)
                         (require library nil t))))

(provide 'muki-lib)

;;; muki-lib.el ends here
