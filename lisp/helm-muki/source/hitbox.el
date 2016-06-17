;;; hitbox -*- lexical-binding: t; coding: utf-8; -*-


(cl-defun muki.hitbox:mpv (url)
  (start-process-shell-command
   "muki-hitbox" nil
   (seq-concatenate 'string
                    "mpv "
                    " --ytdl=no "
                    " '"
                    url
                    "'"
                    " &")))


(cl-defun muki.hitbox:play (player)
  (muki.hitbox:mpv
   (concat
    "http://api.hitbox.tv/player/hls/"
    player ".m3u8")))

(cl-defun muki.hitbox:api-json (url)
  (cl-letf ((content nil))
    (with-current-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies)
      (save-mark-and-excursion
       (goto-char (1+ url-http-end-of-headers))
       (setq content (buffer-substring-no-properties
                      (point) (point-max)))
       (kill-buffer)))
    (json-read-from-string
     content)))

(cl-defun muki.hitbox:api-games (&optional (liveonlyp t))
  (cl-letf ((url (if liveonlyp
                     "http://api.hitbox.tv/games?liveonly=true&limit=500"
                   "http://api.hitbox.tv/games&limit=500")))
    (let-alist (muki.hitbox:api-json url)
      (cl-letf ((categories .categories))
        (seq-map
         (lambda (g)
           (cons
            (format "%s %s"
                    (cdr (assoc 'category_name g))
                    (cdr (assoc 'category_viewers g)))
            g))
         categories)))))

(cl-defun muki.hitbox:api-lives (game)
  (cl-letf ((url (concat
                  "http://api.hitbox.tv/media/live/list?limit=200&game="
                  (cdr (assoc 'category_seo_key game)))))
    (let-alist (muki.hitbox:api-json url)
      (cl-letf ((lives .livestream))
        (seq-map
         (lambda (l)
           (cons (cdr (assoc 'media_user_name l))
                 l))
         lives)))))

(cl-defun muki.hitbox:action-open-game (candidate)
  (helm :sources
        (muki.hitbox:build-source-lives candidate)
        :buffer "*hitbox lives*"
        :prompt "Lives: "))

(cl-defun muki.hitbox:action-play-live (candidate)
  (muki.hitbox:play
   (cdr (assoc 'media_user_name candidate))))


(cl-defun muki.hitbox:build-source-lives (candidate)
  (helm-build-sync-source "muki.hitbox:lives"
    :candidates
    (lambda ()
      (muki.hitbox:api-lives candidate))
    :action
    (helm-make-actions
     "Open live" #'muki.hitbox:action-play-live)))


(defvar muki.hitbox:source-games
  (helm-build-sync-source "muki.hitbox:games"
    :candidates
    #'muki.hitbox:api-games
    :action
    (helm-make-actions
     "open" #'muki.hitbox:action-open-game)))

;;;###autoload
(cl-defun muki-hitbox ()
  (interactive)
  (helm :sources '(muki.hitbox:source-games)
        :buffer "*muki hitbox games*"
        :prompt "Game: "))

;;;###autoload
(cl-defun muki-hitbox-open-user (user)
  (interactive "sUser: ")
  (muki.hitbox:play user))

;; (muki.hitbox "ninjaikotoba")
;; (muki.hitbox:api-json "http://api.hitbox.tv/games?liveonly")
;; (helm muki.hitbox:source-games)
;; (muki.hitbox:api-json "http://api.hitbox.tv/media/live/list?game=minecraft&limit=500")

;; (muki.hitbox:api-lives '((category_seo_key ."h1z1-king-of-the-kill")))

;; curl -x GET https://api.hitbox.tv/game/dota-2
;; curl 'https://api.hitbox.tv/games?liveonly'
;; http --print b --json 'https://api.hitbox.tv/games?liveonly'
;;http://api.hitbox.tv/player/hls/grossgore.m3u8


(provide 'muki.hitbox)
