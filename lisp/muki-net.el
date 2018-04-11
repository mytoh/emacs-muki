;;; muki-net -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:
(require 'xml)
(require 'cl-lib)
(require 'colle)
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
  ;; (interactive (list (read-string "Url: " (pcase (current-kill 0)
  ;;                                           (`nil "")
  ;;                                           (default default))
  ;;                                 nil)))
  (interactive "sUrl: ")
  (cl-letf ((cleaned-url (string-trim url)))
    (message "playing %s" cleaned-url)
    (start-process-shell-command "muki:play-mpv" cleaned-url
                                 (concat "mpv "
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
  (cl-labels ((format-title (str mx)
                            ;; taken from s-contains?
                            (if (string-match-p "[：:]" str)
                                (pcase-let ((`(,num ,title)
                                             (split-string str "[：:]")))
                                  (concat

                                   (propertize (concat num
                                                       (make-string (1+ (abs (- mx (length num))))
                                                                    ?\ ))
                                               'face 'font-lock-keyword-face)
                                   (propertize title 'face 'font-lock-string-face)))
                              (propertize str 'face 'font-lock-string-face)))
              (items->candidates(items mx)
                                (colle:map
                                 (lambda (item)
                                   ;; ((title nil #241:Web 3.0を考える))
                                   (pcase-let ((`((title ,_ ,title))
                                                (xml-get-children item 'title))
                                               ;; ((enclosure ((url . http://tracking.feedpress.it/link/6091/8821701/backspace-241.mp3) (type . audio/mpeg) (length . 89194870))))
                                               (`((enclosure ((url . ,url) . ,_)))
                                                (xml-get-children item 'enclosure)))
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
                                            (split-string "[：:]")
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

(cl-defun muki:thechangelog ()
  (interactive)
  (cl-labels ((format-title (title mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             ))
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
    (pcase-let* ((feedurl "https://changelog.com/podcast/feed")
                 ;; (root (with-current-buffer
                 ;;           (url-retrieve-synchronously
                 ;;            feedurl)
                 ;;         (libxml-parse-html-region
                 ;;          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               (p ,_ ,_ ,_
                                  ,rss)))
                  testchangelog)
                 (fstitem (thread-first rss
                            (xml-get-children 'channel)
                            cl-first
                            (xml-get-children 'item)
                            cl-first))
                 (fstenclosure (thread-first rss
                                 (xml-get-children 'enclosure)))
                 ;; (maxitemlength (apply #'max
                 ;;                       (mapcar
                 ;;                        (lambda (item)
                 ;;                          (pcase-let ((`((title ,_ ,title))
                 ;;                                       (xml-get-children item 'title)))
                 ;;                            (length title)))
                 ;;                        items)))
                 (actions
                  `(("Open" . (lambda (url)
                                (muki:play-mpv url))))))
      ;; (cl-letf ((buf (get-buffer-create "testthechagelog")))
      ;;   (with-current-buffer buf
      ;;     (delete-region (point-min) (point-max))
      ;;     (prin1 root (current-buffer))
      ;;     (write-file "testchangelog")) )

      ;; (message "%s" (thread-first rss
      ;;                 (xml-get-children 'channel)))
      (message "%s" (car (thread-first rss
                           (xml-get-children 'item))))

      ;; (helm :sources `((name . "thechangelog")
      ;;                  (candidates . ,(items->candidates items maxitemlength))
      ;;                  (action . ,actions))
      ;;       :buffer "*thechangelog*")
      )))

;; (car testchangelog)
;; (setq testchangelog (with-current-buffer
;;                            (url-retrieve-synchronously
;;  "https://changelog.com/podcast/feed"
;;                             )
;;                          (libxml-parse-html-region
;;                           (point-min) (point-max))))

;; (request "http://feed.mozaic.fm"
;;         :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
;;         :success (cl-function
;;                   (lambda (&key data &allow-other-keys)
;;                     data)))

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
                      caddr)))
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

(cl-defun muki:soundcloud->rss (url)
  (pcase-let* ((root (with-current-buffer
                         (url-retrieve-synchronously
                          url)
                       (libxml-parse-html-region
                        (point-min) (point-max))))
               (`(html ,_
                       (body ,_
                             (p . ,_)
                             . ,infos))
                root)
               (metas (thread-first infos
                        (xml-get-children 'meta)))
               (meta (colle:filter
                      (lambda (m)
                        (cl-equalp "al:ios:url"
                                   (xml-get-attribute m 'property)))
                      metas))
               (id 
                (thread-first (car meta)
                  (xml-get-attribute  'content)
                  (split-string ":")
                  colle:last)))
    (concat
     "http://feeds.soundcloud.com/users/soundcloud:users:"   
     id
     "/sounds.rss")))

(defun muki:frontend-lunch ()
  (interactive)
  (cl-labels ((format-title (title subtitle mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             subtitle))
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
                                                (`((subtitle ,_ ,subtitle))
                                                 (thread-first item
                                                   (xml-get-children 'subtitle))))
                                      (cons
                                       (format-title title subtitle mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://feeds.soundcloud.com/users/soundcloud:users:266716117/sounds.rss")
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
      (helm :sources `((name . "frontend-lunch")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*nyanyanya radio*"))))

(cl-defun muki:bilingual-news ()
  (interactive)
  (cl-labels ((format-title (title desc mx)
                            (concat
                             (propertize title 'face 'font-lock-string-face)
                             (make-string (+ 3 (- mx (length title)))
                                          ?\ )
                             (if (stringp desc)
                                 (propertize desc 'face 'font-lock-keyword-face))
                             ""))
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
                                                (`((description ,_ ,desc))
                                                 (thread-first item
                                                   (xml-get-children 'description))))
                                      (cons
                                       (format-title title desc mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://bilingualnews.libsyn.com/rss")
                 (root (with-current-buffer
                           (url-retrieve-synchronously
                            feedurl)
                         (libxml-parse-html-region
                          (point-min) (point-max))))
                 (`(html ,_
                         (body ,_
                               . ,body))
                  root)
                 (`(p ,_ ,_ ,_
                      (rss ,_
                           ,channel))
                  (car body))
                 (firstenclosure (thread-first body
                                   (xml-get-children 'enclosure)
                                   car))
                 (firstitem (append (thread-first channel
                                      (xml-get-children 'item)
                                      car)
                                    (list firstenclosure)))
                 (items (cons firstitem
                              (thread-first body
                                (xml-get-children 'item))))
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
      (helm :sources `((name . "*bilingual news*")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*bilingual news*"))))

(cl-defun muki:clfreaks ()
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
                                    (pcase-let* ((`((title ,_ ,title))
                                                  (thread-first item
                                                    (xml-get-children 'title)))
                                                 (url
                                                  (pcase-let ((`(html ,_
                                                                      (body ,_
                                                                            . ,body))
                                                               (with-temp-buffer
                                                                 (insert (colle:third (car (xml-get-children item 'description))))
                                                                 (libxml-parse-html-region
                                                                  (point-min) (point-max)))))
                                                    (thread-first (colle:find 
                                                                   (lambda (p)
                                                                     (if (listp (colle:third p))
                                                                         (string-match-p "mp3$"
                                                                                         (xml-get-attribute (colle:third p) 'href))
                                                                       nil))
                                                                   (xml-get-children body 'p))
                                                      colle:third
                                                      (xml-get-attribute 'href)))))
                                      (cons
                                       (format-title title mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://clfreaks.org/rss")
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
      (helm :sources `((name . "*clfreaks*")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*clfreaks*"))))

(cl-defun muki:washipo ()
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
                                                (url (thread-first item
                                                       (xml-get-children 'enclosure)
                                                       car
                                                       (xml-get-attribute 'url))))
                                      (cons
                                       (format-title title mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "https://washipo.nyoho.jp/podcast.rss")
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
      (helm :sources `((name . "*washipo*")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*washipo*"))))

(cl-defun muki:codelunchfm ()
  (interactive)
  (pcase-let*  ((feedurl "http://codelunch.fm/rss.xml")
                (root (with-current-buffer
                          (url-retrieve-synchronously
                           feedurl)
                        (mm-with-part (mm-dissect-buffer 'not-strict-mime)
                          (libxml-parse-xml-region (point) (point-max)))))
                
                ;; (`(html ,_
                ;;         (body ,_
                ;;               (p ,_ ,_ ,_
                ;;                  (rss ,_
                ;;                       ,channel))))
                ;;  root)
                ;; (items (thread-first channel
                ;;          (xml-get-children 'item)))
                ;; (maxitemlength (apply #'max
                ;;                       (mapcar
                ;;                        (lambda (item)
                ;;                          (pcase-let ((`((title ,_ ,title))
                ;;                                       (xml-get-children item 'title)))
                ;;                            (length title)))
                ;;                        items)))
                ;; (actions
                ;;  `(("Open" . (lambda (url)
                ;;                (muki:play-mpv url)))))
                )
    (message "%s" root)
    )
  )

(cl-defun muki:wadafm ()
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
                                                (url (thread-first item
                                                       (xml-get-children 'enclosure)
                                                       car
                                                       (xml-get-attribute 'url))))
                                      (cons
                                       (format-title title mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://feed.wada.fm/wadafm")
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
      (helm :sources `((name . "*wadafm*")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*wadafm*"))))

(cl-defun muki:dandyfm ()
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
                                                (url (thread-first item
                                                       (xml-get-children 'enclosure)
                                                       car
                                                       (xml-get-attribute 'url))))
                                      (cons
                                       (format-title title mx)
                                       url)))
                                  items)))
    (pcase-let* ((feedurl "http://feed.dandy.fm/dandyfm")
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
      (helm :sources `((name . "*dandyfm*")
                       (candidates . ,(items->candidates items maxitemlength))
                       (action . ,actions))
            :buffer "*dandyfm*"))))

(cl-defun muki:nixers ()
  "https://podcast.nixers.net/feed/feed.xml")

(cl-defun muki:atp ()
  "http://atp.fm/episodes?format=rss")

(cl-defun muki:roguelike-radio ()
  "http://feeds.feedburner.com/RoguelikeRadio")

(cl-defun muki:gnuworldorder ()
  "http://gnuworldorder.info/spx.atom.xml")

(cl-defun muki:norfolk-winters ()
  "https://media.norfolkwinters.com/podcast-ogg.xml")

(cl-defun muki:late-night-linux ()
  "https://latenightlinux.com/feed/ogg")

(cl-defun muki:kumocast ()
  "http://feeds.feedburner.com/tumblr/IkZP")

(cl-defun muki:turingcompletefm ()
  "https://turingcomplete.fm/")

(provide 'muki-net)
;;; muki-net.el ends here
