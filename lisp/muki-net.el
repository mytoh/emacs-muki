;;; muki-net -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'xml)
(declare-function colle:map "colle")
(declare-function helm "helm")

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
            :buffer "*yatteiki fm*"))))

(cl-defun muki:sore-ha-sou ()
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
    (pcase-let* ((feedurl "http://feeds.feedburner.com/BiaccoRadio.xml")
                 (root (with-current-buffer
                           (url-retrieve-synchronously
                            feedurl)
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               (p ,_ ,_ ,_ ,_ ,_
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
      (helm :sources `((name . "sore-ha-sou")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "sore ha sou"))))

(cl-defun muki:mozaicfm ()
  (interactive)
  (cl-labels ((format-title (title subtitle mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             (propertize subtitle 'face 'font-lock-doc-face)))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
                                                (`((enclosure ((url . ,url) . ,_) . ,_) . ,_)
                                                 (thread-first item
                                                   (xml-get-children 'enclosure)))
                                                (`((subtitle ,_ ,subtitle))
                                                 (thread-first item
                                                   (xml-get-children 'subtitle))))
                                      (cons
                                       (format-title (car (split-string title "|")) subtitle mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://feed.mozaic.fm")
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
                                            (split-string "|")
                                            car
                                            length))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "mozaicfm")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*mozaic.fm*"))))

(cl-defun muki:dexfm ()
  (interactive)
  (cl-labels ((format-title (title subtitle mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             (propertize subtitle 'face 'font-lock-doc-face)))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
                                                (`((enclosure ((url . ,url) . ,_) . ,_) . ,_)
                                                 (thread-first item
                                                   (xml-get-children 'enclosure)))
                                                (`((subtitle ,_ ,subtitle))
                                                 (thread-first item
                                                   (xml-get-children 'subtitle))))
                                      (cons
                                       (format-title title
                                                     (concat (car (split-string subtitle "。" ))
                                                             "。")
                                                     mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://feeds.feedburner.com/dexfm")
                 (root (with-current-buffer
                           (url-retrieve-synchronously
                            feedurl)
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               (p ,_ ,_ ,_ ,_ ,_
                                  (rss ,_
                                       ,channel))))
                  root)
                 (items (thread-first channel
                          (xml-get-children 'item)))
                 (maxitemlength (apply #'max
                                       (mapcar
                                        (lambda (item)
                                          (pcase-let ((`((title ,_ ,title))
                                                       (xml-get-children item 'title)))
                                            (length title)))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "dexfm")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*dex.fm*"))))

(cl-defun muki:bootfm ()
  (interactive)
  (cl-labels ((format-title (title subtitle mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             (propertize subtitle 'face 'font-lock-doc-face)))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
                                                (`((enclosure ((url . ,url) . ,_) . ,_) . ,_)
                                                 (thread-first item
                                                   (xml-get-children 'enclosure)))
                                                (`((subtitle ,_ ,subtitle))
                                                 (thread-first item
                                                   (xml-get-children 'subtitle))))
                                      (cons
                                       (format-title title
                                                     subtitle mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "https://bootfm.github.io/feed.xml")
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
                                          (pcase-let ((`((title ,_ ,title))
                                                       (xml-get-children item 'title)))
                                            (length title)))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "bootfm")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*boot.fm*"))))

(cl-defun muki:functional-geekery ()
  (interactive)
  (cl-labels ((format-title (title summary mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             (if summary
                                 (propertize summary 'face 'font-lock-keyword-face))
                             ""))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
                                                (`((enclosure ((url . ,url) . ,_) . ,_) . ,_)
                                                 (thread-first item
                                                   (xml-get-children 'enclosure)))
                                                (`((summary ,_ ,summary))
                                                 (thread-first item
                                                   (xml-get-children 'summary))))
                                      (cons
                                       (format-title title summary mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "https://www.functionalgeekery.com/feed/mp3")
                 (root (with-current-buffer
                           (url-retrieve-synchronously
                            feedurl)
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               . ,body))
                  root)
                 (items (thread-first body
                          (xml-get-children 'item)))
                 (maxitemlength (apply #'max
                                       (mapcar
                                        (lambda (item)
                                          (pcase-let ((`((title ,_ ,title))
                                                       (xml-get-children item 'title)))
                                            (length title)))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))

      ;; (cl-letf ((buf (get-buffer-create "testthechagelog")))
      ;;   (with-current-buffer buf
      ;;     (delete-region (point-min) (point-max))
      ;;     (prin1 (car items)(current-buffer))
      ;;     (write-file "testchangelog"))
      ;;   (switch-to-buffer buf))

      (helm :sources `((name . "functional-geekery")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*functional geekery*")
      )))
(defun muki:nhk-world ()
  "[[http://askubuntu.com/questions/704612/nhk-live-streaming-not-available-on-this-browser][firefox - NHK Live streaming not available on this browser - Ask Ubuntu]]"
  (interactive)
  (pcase-let* ((xmlurl "http://www3.nhk.or.jp/nhkworld/app/tv/hlslive_tv.xml")
               (root (with-current-buffer
                         (url-retrieve-synchronously
                          xmlurl)
                       (libxml-parse-html-region
                        (point-min) (point-max))))
               (`(html ,_
                       (body ,_
                             (p ,_ ,_ ,_
                                ,urls)))
                root)
               (url (thread-first urls
                      (xml-get-children 'main_url)
                      car
                      (xml-get-children 'jstrm)
                      car
                      cl-caddr)))
    (muki:play-mpv url)))

(defun muki:defn-audio ()
  (interactive)
  (cl-labels ((format-title (title mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
                                                (url (thread-first item
                                                       (xml-get-children 'enclosure)
                                                       car
                                                       (xml-get-attribute 'url)))
                                                )
                                      (cons
                                       (format-title title mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://feeds.soundcloud.com/users/soundcloud:users:220484243/sounds.rss")
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
                                          (pcase-let ((`((title ,_ ,title))
                                                       (xml-get-children item 'title)))
                                            (length title)))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "defn")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*defn*"))))

(defun muki:nyanyanya-radio ()
  (interactive)
  (cl-labels ((format-title (title mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
                                                (url (thread-first item
                                                       (xml-get-children 'enclosure)
                                                       car
                                                       (xml-get-attribute 'url)))
                                                )
                                      (cons
                                       (format-title title mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://feeds.soundcloud.com/users/soundcloud:users:100121411/sounds.rss")
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
                                          (pcase-let ((`((title ,_ ,title))
                                                       (xml-get-children item 'title)))
                                            (length title)))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "nyanyanya-radio")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*nyanyanya radio*"))))

(cl-defun muki:cortex ()
  (interactive)
  (cl-labels ((format-title (title subtitle mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             (if subtitle
                                 (propertize subtitle 'face 'font-lock-keyword-face))
                             ""))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
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
    (pcase-let* ((feedurl "https://www.relay.fm/cortex/feed")
                 (root (with-current-buffer
                           (url-retrieve-synchronously
                            feedurl)
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               (p ,_ ,_ 
                                  (rss ,_
                                       ,channel))))
                  root)
                 (items (thread-first channel
                          (xml-get-children 'item)))
                 (maxitemlength (apply #'max
                                       (mapcar
                                        (lambda (item)
                                          (pcase-let ((`((title ,_ ,title))
                                                       (xml-get-children item 'title)))
                                            (length title)))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "cortex")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*cortex*"))))

(cl-defun muki:hello-internet()
  (interactive)
  (cl-labels ((format-title (title mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             ""))
              (items->candidates (items mx)
                                 (colle:map
                                  (lambda (item)
                                    (pcase-let ((`((title ,_ ,title))
                                                 (thread-first item
                                                   (xml-get-children 'title)))
                                                (`((enclosure ((url . ,url) . ,_) . ,_) . ,_)
                                                 (thread-first item
                                                   (xml-get-children 'enclosure)))
                                                )
                                      (cons
                                       (format-title title mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://www.hellointernet.fm/podcast?format=rss")
                 (root (with-current-buffer
                           (url-retrieve-synchronously
                            feedurl)
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               . ,rss))
                  root)
                 (items (thread-first rss
                          (xml-get-children 'item)))
                 (maxitemlength (apply #'max
                                       (mapcar
                                        (lambda (item)
                                          (pcase-let ((`((title ,_ ,title))
                                                       (xml-get-children item 'title)))
                                            (length title)))
                                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      (helm :sources `((name . "hellointernet")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*hellointernet*"))))

;; (cl-letf ((buf (get-buffer-create "testthechagelog")))
;;   (with-current-buffer buf
;;     (delete-region (point-min) (point-max))
;;     (prin1 (car items)(current-buffer))
;;     (write-file "testchangelog"))
;;   (switch-to-buffer buf))


(provide 'muki-net)
;;; muki-net.el ends here
