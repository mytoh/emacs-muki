;;; twitch -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'map)

(defgroup helm-muki-twitch nil
  "personal helm command"
  :group 'helm)

(defcustom helm-muki-twitch-games-limit 100
  "limit number of games candidate")

(defcustom helm-muki-twitch-streams-limit 100
  "limit number of streams candidate")

(defcustom helm-muki-twitch-additional-games
  ()
  "names of game always shown in the games list")

(defface helm-muki-twitch-stream
    '((t :inherit font-lock-variable-name-face))
  "face for helm muki twitch stream name"
  :group 'helm-muki-twitch)

(defface helm-muki-twitch-game
    '((t :inherit font-lock-function-name-face))
  "face for helm muki twitch game name"
  :group 'helm-muki-twitch)

(cl-defun helm-muki-twitch-api-url-base (&optional part)
  (cl-letf ((base "https://api.twitch.tv/kraken/"))
    (if part
        (seq-concatenate 'string
                         base
                         (string-remove-prefix "/" part))
      base)))

(cl-defun helm-muki-twitch-game-create-url ()
  (helm-muki-twitch-api-url-base
   (seq-concatenate 'string
                    "games/top?"
                    (url-build-query-string
                     `((limit ,helm-muki-twitch-games-limit))))))

(cl-defun helm-muki-twitch-game-names ()
  (cl-letf* ((games-json-alist
              (helm-muki-twitch-api-get-json
               (helm-muki-twitch-game-create-url)))
             (games (map-elt games-json-alist 'top)))
    (seq-map
     (lambda (alist)
       (let-alist alist
         (cl-letf ((channels .channels)
                   (viewers .viewers)
                   (name .game.name))
           (cons
            (format "%s  %s %s"
                    (propertize name 'face 'helm-muki-twitch-game)
                    (if channels (number-to-string channels) channels)
                    (if viewers (number-to-string viewers) viewers))
            name))))
     games)))

(cl-defun helm-muki-twitch-game-additional-game-names ()
  (seq-map
   (lambda (name)
     (cons
      (format "%s"
              (propertize name 'face 'helm-muki-twitch-game))
      name))
   helm-muki-twitch-additional-games))

(cl-defun helm-muki-twitch-game-create-candidates ()
  (if helm-muki-twitch-additional-games
      (append (helm-muki-twitch-game-names)
              (helm-muki-twitch-game-additional-game-names))
    (helm-muki-twitch-game-names)))

(defvar helm-muki-twitch-game-candidates nil)

(cl-defun helm-muki-twitch-game-init ()
  (setq helm-muki-twitch-game-candidates
        (helm-muki-twitch-game-create-candidates)))

(defvar helm-muki-twitch-selected-game-name "")

(cl-defun helm-muki-twitch-game-action-view-streams (candidate)
  (setq helm-muki-twitch-selected-game-name candidate)
  (helm :sources '(helm-source-muki-twitch-streams)
        :buffer "*helm twitch*"
        :prompt "Stream: "))

(defclass helm-muki-twitch-game-source (helm-source-sync)
  ((init :initform #'helm-muki-twitch-game-init)
   (candidates :initform 'helm-muki-twitch-game-candidates)
   (action :initform
           (helm-make-actions
            "View Streams" #'helm-muki-twitch-game-action-view-streams))))

(defvar helm-source-muki-twitch
  (helm-make-source (helm-muki-source-name/mark "Game" "🎮")
      'helm-muki-twitch-game-source))

(defvar helm-muki-twitch-stream-candidates nil)

(cl-defun helm-muki-twitch-stream-init ()
  (setq helm-muki-twitch-stream-candidates
        (helm-muki-twitch-stream-create-candidates
         helm-muki-twitch-selected-game-name)))

(cl-defun helm-muki-twitch-stream-create-url (game-name)
  (helm-muki-twitch-api-url-base
   (seq-concatenate 'string
                    "streams?"
                    (url-build-query-string
                     `((game ,game-name)
                       (limit ,helm-muki-twitch-streams-limit))))))

(cl-defun helm-muki-twitch-api-get-json (url)
  (cl-letf ((url-request-extra-headers
             '(("Accept" . "application/vnd.twitchtv.v3+json")))
            (response-string nil))
    (with-current-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (setq response-string
            (buffer-substring-no-properties
             (point) (point-max)))
      (kill-buffer))
    (json-read-from-string
     response-string)))

(cl-defun helm-muki-twitch-stream-create-candidates (game-name)
  (cl-letf* ((streams-json-alist
              (helm-muki-twitch-api-get-json
               (helm-muki-twitch-stream-create-url game-name)))
             (streams (map-elt streams-json-alist 'streams)))
    (seq-map
     (lambda (alist)
       (let-alist alist
         (cons (format "%s %s  %s %s %s"
                       (propertize .channel.display_name
                                   'face 'helm-muki-twitch-stream)
                       .viewers
                       .channel.followers
                       .channel.views
                       (if (null .channel.broadcaster_language)
                           ""
                         .channel.broadcaster_language))
               .channel.url)))
     streams)))

(cl-defun helm-muki-twitch-stream-action-open (candidate)
  (cl-letf ((url candidate))
    (message "playng %s" url)
    (cl-letf ((command (seq-concatenate 'string
                                        "mpv "
                                        " --ytdl=yes "
                                        " '" url "'"
                                        " &")))
      (start-process-shell-command "helm-muki-twitch" nil command))))

(defclass helm-muki-twitch-stream-source (helm-source-sync)
  ((init :initform #'helm-muki-twitch-stream-init)
   (candidates :initform 'helm-muki-twitch-stream-candidates)
   (action :initform
           (helm-make-actions
            "Open" #'helm-muki-twitch-stream-action-open))))


(defvar helm-source-muki-twitch-streams
  (helm-make-source (helm-muki-source-name/mark "Game" "🎮")
      'helm-muki-twitch-stream-source))

(provide 'helm-muki-twitch)

;;; twitch.el ends here

;; get games
;; curl -H 'Accept: application/vnd.twitchtv.v3+json' -X GET 'https://api.twitch.tv/kraken/games/top?limit=50'
;; get channels
;;  "https://api.twitch.tv/kraken/search/channels?limit=10&offset=0&q=Clicker%2520Heroes"
;; get live channels
;; curl -H 'Accept: application/vnd.twitchtv.v3+json' -X GET https://api.twitch.tv/kraken/streams?game=StarCraft+II%3A+Heart+of+the+Swarm&channel=test_channel,test_channel2

;; get streams from game name
;; curl -H 'Accept: application/vnd.twitchtv.v3+json' 
;;-X GET https://api.twitch.tv/kraken/streams?game=StarCraft+II%3A+Heart+of+the+Swarm&channel=test_channel,test_channel2

;;(helm-muki-twitch-stream-create-url "Dark Souls III")
;; (helm-muki-twitch-api-get-json "https://api.twitch.tv/kraken/channels/hebo" )
