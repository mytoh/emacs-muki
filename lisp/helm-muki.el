;;; helm-muki.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'helm)
(require 'helm-bookmark)
(require 'helm-files)

(require 'helm-muki-directory "helm-muki/source/directory")
(require 'helm-muki-program "helm-muki/source/program")
(require 'helm-muki-vihko "helm-muki/source/vihko")
(require 'helm-muki-ääliö "helm-muki/source/ääliö")
(require 'helm-muki-layer "helm-muki/source/layer")
(require 'helm-muki-twitch"helm-muki/source/twitch")
(require 'muki.hitbox "helm-muki/source/hitbox")

;;;; group
(defgroup helm-muki nil
  "personal helm command"
  :group 'helm)

;;;; helm sources
(defcustom helm-muki-sources
  '(helm-source-muki-directory
    helm-source-bookmarks
    helm-source-muki-program
    helm-source-recentf)
  "helm muki sources"
  :type 'list
  :group 'helm-muki)

;;;; helm
;;;###autoload
(cl-defun helm-muki ()
  "personal helm command : [\\[helm-muki]]"
  (interactive)
  (cl-letf ((helm-ff-transformer-show-only-basename nil))
    (helm :sources helm-muki-sources
          :buffer "*helm muki*"
          :prompt "Muki: ")))

;;;###autoload
(cl-defun helm-muki-directory ()
  "Preconfigured `helm' for muki-directory source."
  (interactive)
  (helm :sources '(helm-source-muki-directory)
        :buffer "*helm muki directory*"
        :prompt "Dir: "))

;;;###autoload
(cl-defun helm-muki-program ()
  "Preconfigured `helm' for muki-program source."
  (interactive)
  (helm :sources '(helm-source-muki-program)
        :buffer "*helm muki program*"
        :prompt "Program: "))

;;;###autoload
(cl-defun helm-muki-vihko ()
  "Preconfigured `helm' for muki-vihko source."
  (interactive)
  (helm :sources '(helm-source-muki-vihko
                   helm-source-muki-vihko-not-found)
        :buffer "*helm muki vihko*"
        :prompt "Vihko: "))

;;;###autoload
(cl-defun helm-muki-ääliö ()
  "personal helm command : [\\[helm-muki-ääliö]]"
  (interactive)
  (cl-letf ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(helm-source-muki-ääliö)
          :buffer "*helm ääliö*"
          :prompt "Project: ")))

;;;###autoload
(cl-defun helm-muki-layer ()
  "personal helm command"
  (interactive)
  (helm :sources '(helm-source-muki-layer)
        :buffer "*helm layer*"
        :prompt "Layer: "))

;;;###autoload
(cl-defun helm-muki-twitch ()
  (interactive)
  (helm :sources '(helm-source-muki-twitch)
        :buffer "*helm twitch*"
        :prompt "Game: "))

;;; provide
(provide 'helm-muki)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
