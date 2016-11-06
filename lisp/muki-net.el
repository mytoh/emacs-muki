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

(cl-defun muki:backspacefm ()
  (interactive)
  (cl-labels ((format-title (title mx)
                            (pcase-let ((`(,num ,title)
                                         (split-string title ":")))
                              (concat
                               (propertize (concat num
                                                   (make-string (1+ (abs (- mx (length num))))
                                                                ?\ ))
                                           'face 'font-lock-keyword-face)
                               (propertize title 'face 'font-lock-string-face))))
              (items->candidates(items mx)
                                (colle:map
                                 (lambda (item)
                                   (pcase-let ((`(item ,_ (title ,_ ,title) . ,_)
                                                item)
                                               (`((enclosure ((url . ,url) . ,_) . ,_) . ,_)
                                                (thread-first item
                                                  (xml-get-children 'enclosure))))
                                     (cons
                                      (format-title title mx)
                                      url)))
                                 items)))
    (pcase-let* ((root (with-current-buffer
                           (url-retrieve-synchronously
                            "http://feeds.backspace.fm/backspacefm")
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(,body . ,_) (thread-first root
                                  (xml-get-children 'body)))
                 (`(,p . ,_) (thread-first body
                               (xml-get-children 'p)))
                 (`(,rss . ,_) (thread-first p
                                 (xml-get-children 'rss)))
                 (`(,channel . ,_) (thread-first rss
                                     (xml-get-children 'channel)))
                 (items (thread-first channel
                          (xml-get-children 'item)))
                 (maxitemlength (apply #'max
                                       (mapcar
                                        (lambda (item)
                                          (thread-first item
                                            (xml-get-children 'title)
                                            car cdr cdr car
                                            (split-string ":")
                                            car
                                            length))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "backspacefm")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*backspacem fm*"))))

(cl-defun muki:yatteikifm ()
  (interactive)
  (cl-labels ((format-title (title subtitle mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 5 (- mx (length title)))
                                          ?\ )
                             (propertize subtitle 'face 'font-lock-doc-face)))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`(item ,_ (title ,_ ,title) . ,_)
                                                 item)
                                                (`((enclosure ((url . ,url) . ,_) . ,_) . ,_)
                                                 (thread-first item
                                                   (xml-get-children 'enclosure)))
                                                (`((subtitle ,_ ,subtitle))
                                                 (thread-first item
                                                   (xml-get-children 'subtitle))))
                                      (cons
                                       (format-title title subtitle mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "https://yatteiki.fm/feed.xml")
                 (root (with-current-buffer
                           (url-retrieve-synchronously
                            feedurl)
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               (p ,_ ,_ ,_
                                  (rss ,_
                                       ,channel))))
                  root)
                 (items (thread-first channel
                          (xml-get-children 'item)))
                 (maxitemlength (apply #'max
                                       (mapcar
                                        (lambda (item)
                                          (thread-first item
                                            (xml-get-children 'title)
                                            car cdr cdr car
                                            length))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "yatteikifm")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*yatteiki fm*")
      )))

(provide 'muki-net)
;;; muki-net.el ends here
