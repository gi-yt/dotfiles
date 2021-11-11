;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! goggles)
(package! esup)
(package! hungry-delete)
(package! expand-region)
(package! super-save)
(package! keycast)
(package! activity-watch-mode)
(package! 0x0)
(package! multi-vterm)
(package! imgur :recipe
  (:host github :repo "larsmagne/imgur.el"))
(package! meme :recipe
  (:host github :repo "larsmagne/meme"))
(package! ox-twbs)
(package! toc-org)
(package! org-auto-tangle)
(package! aggressive-indent)
(package! company-box)
(package! exwm)
(package! desktop-environment)
(package! exwm-firefox-core)
(package! counsel)
(when (package! eaf :recipe (:host github
                             :repo "manateelazycat/emacs-application-framework"
                             :files ("*.el" "*.py" "app" "core")
                             :build (:not compile)))
  (package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
  (package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
  (package! epc :recipe (:host github :repo "kiwanami/emacs-epc")))
(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))

(package! ement
  :recipe (:host github :repo "alphapapa/ement.el"))
(package! shrface)
