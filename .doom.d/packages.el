;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! hungry-delete)
(package! super-save)
(package! keycast)
(package! 0x0)
(package! multi-vterm)
(package! ox-twbs)
(package! toc-org)
(package! org-auto-tangle)
(package! aggressive-indent)
(package! company-box)
(package! eaf :recipe (:host github
                       :repo "manateelazycat/emacs-application-framework"
                       :files ("*.el" "*.py" "app" "core")
                       :build (:not compile)))
(package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
(package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
(package! epc :recipe (:host github :repo "kiwanami/emacs-epc"))
(package! modus-themes)
(package! pomm)
(package! info-colors)
(package! lorem-ipsum)
(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))

(package! ement
  :recipe (:host github :repo "alphapapa/ement.el"))
