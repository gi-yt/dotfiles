;;; Magit
;; It can do more complex git tasks
;; I am trying to use vc.el for less complex ones
(use-package magit
  :straight t
  :defer t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)
    ))

(setq magit-status-margin
      '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(use-package git-gutter
  :straight t
  :init
  (global-git-gutter-mode +1))
(use-package forge
  :after magit)
(use-package git-timemachine
  :straight t
  )
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))
;; Git Keybindings With VC and Magit
(general-define-key
 :prefix "C-c g"
 "g d" '(vc-root-diff :which-key "Give a diff for all modified files")
 "l d" '(vc-diff :which-key "Give a diff for modifications in current file")
 "d" 'vc-next-action
 "l s" '(magit-stage :which-key "Stage current file if not staged")
 "l l"   '(vc-print-log :which-key "Show log for current vc file")
 "r l" '(vc-region-history :which-key "Show the vc history of the current region")
 "g l"   '(magit-log-all :which-key "Show log for the whole project")
 "p" '(vc-push :which-key "Push Commits to remote")
 "s" '(magit-status :which-key "Show status")
 "c" '(magit-commit :which-key "Commit") ;; Need a better way to commit with vc.el until then stickin to magit
 "l a" '(vc-annotate :which-key "Show annotations"))
;;; Best Terminal Emulator For Emacs
;; Vterm can do alot of cool stuff
(use-package vterm :straight t :defer t)
(setq vterm-eval-cmds '(("magit-status-setup-buffer" magit-status-setup-buffer)
                        ("find-file" find-file)
                        ("message" message)
                        ("vterm-clear-scrollback" vterm-clear-scrollback)))
(use-package multi-vterm :straight t :defer t
  :bind ("s-<return>" . multi-vterm))
;;; Access 0x0.st from the comfort of your emacs
(use-package 0x0 :straight t :defer t)
;;; Cool startpage
(use-package page-break-lines)
(use-package dashboard :after page-break-lines
  :straight t
  :init                                                                      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)                                       ;; add icons for headings
  (setq dashboard-set-file-icons t)                                          ;; add icons for files
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")     ;; set a title to be displayed under the banner
  (setq dashboard-center-content nil)                                          ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)                                      ;; show some items on dashboard
                          (bookmarks . 5)
                          (registers . 5)))
  (setq dashboard-page-separator "\n\f\n")    ;; <-----
  (global-page-break-lines-mode)
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :custom-face
  (dashboard-items-face ((t (:inherit widget-button :weight normal))))
  (dashboard-heading ((t (:inherit font-lock-keyword-face :weight semi-bold)))))

;;; EAF
;; Do everything in emacs from browser to pdf reader to video player
(add-to-list 'load-path "~/.emacs.mine/site-lisp/emacs-application-framework/")
(require 'eaf)
(load "~/.emacs.mine/site-lisp/emacs-application-framework/app/browser/eaf-browser.el")
(load "~/.emacs.mine/site-lisp/emacs-application-framework/app/camera/eaf-camera.el")
(load "~/.emacs.mine/site-lisp/emacs-application-framework/app/video-player/eaf-video-player.el")
(load "~/.emacs.mine/site-lisp/emacs-application-framework/app/pdf-viewer/eaf-pdf-viewer.el")
(load "~/.emacs.mine/site-lisp/emacs-application-framework/app/org-previewer/eaf-org-previewer.el")
(setq eaf-pdf-dark-mode t)
(setq eaf-browser-enable-scrollbar t)
(setq eaf-browser-pc-user-agent "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:83.0) Gecko/20100101 Firefox/83.0")
(setq eaf-browser-dark-mode nil)
(setq eaf-pdf-dark-exclude-image t)
(defun browser ()
  (interactive)
  (eaf-open-browser-with-history))
(use-package epc :defer t)
(use-package ctable :defer t)
(use-package s :defer t)
(use-package deferred :defer t)
;;; Meme.el
;; The Greatest meme generator
(use-package imgur
  :ensure t
  :straight (imgur
             :type git
             :host github
             :repo "myuhe/imgur.el"))

(use-package meme
  :ensure t
  :straight (meme
             :type git
             :host github
             :repo "larsmagne/meme"))
