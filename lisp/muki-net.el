;;; muki-net -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(cl-defun muki:shutup-stop (where)
  (interactive "sWhere?: ")
  (cl-labels ((get-id (elm) (xml-get-children elm 'id))
              (get-div (elm) (xml-get-children elm 'div))
              (node-filter (elms target attr)
                (seq-filter
                 (lambda (elm) (string-equal target
                                             (xml-get-attribute elm attr)))
                 elms)))
    (cl-letf* ((html (with-current-buffer
                         (url-retrieve-synchronously
                          where)
                       (libxml-parse-html-region
                        (point-min) (point-max))))
               (root html)
               (divs (thread-first root
                       (xml-get-children 'body)
                       car
                       get-div))
               (my-body (thread-first divs
                          (node-filter "my_body" 'id)
                          car
                          get-div))
               (main (thread-first my-body
                       (node-filter "main" 'id)
                       car
                       get-div))
               (my-footer (thread-first main
                            (node-filter "my_footer" 'id)
                            car
                            get-div))
               (box02 (car (node-filter my-footer "box02" 'class)))
               (links (xml-get-children box02 'a))
               (url (xml-get-attribute (seq-elt links 1)
                                       'href)))
      (message "playing %s" where)
      (require 'async)
      (muki:play-mpv url))))

(cl-defun muki:play-mpv (url)
  (interactive "sUrl: ")
  (cl-letf ((cleaned-url (string-trim url)))
    (message "playing %s" cleaned-url)
    (start-process-shell-command "muki:play-mpv" cleaned-url
                                 (concat "nohup " "mpv "
                                         "\'" cleaned-url "\'"
                                         " &"))))
(cl-defun muki:tumblr-image-url (where)
  (interactive "sWhere?: ")
  (cl-labels ((get-id (elm) (xml-get-children elm 'id))
              (get-div (elm) (xml-get-children elm 'div))
              (get-img (elm) (xml-get-children elm 'img))
              (node-filter (elms target attr)
                (seq-filter
                 (lambda (elm) (string-equal target
                                        (xml-get-attribute elm attr)))
                 elms)))
    (cl-letf* ((doc (with-current-buffer
                        (url-retrieve-synchronously
                         where)
                      (libxml-parse-html-region
                       (point-min) (point-max))))
               (divs (thread-first doc
                       (xml-get-children 'body)
                       car
                       get-div))
               (url
                (thread-first divs
                  (seq-elt 2)
                  get-div
                  car
                  get-img
                  car
                  (xml-get-attribute 'data-src))))
      (kill-new url)
      (message "copied %s to kill ring" url))))

(provide 'muki-net)
;;; muki-net.el ends here
