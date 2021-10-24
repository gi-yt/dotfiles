(setq org-ellipsis "▾")
(defun ak-org-hooks ()
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src bash"))
  (my/org-mode/load-prettify-symbols)
  (setq org-hide-emphasis-markers t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (org-indent-mode 1)
  )
(defun up-n-fold ()
  (interactive)
  (progn
    (outline-previous-visible-heading 1)
    (org-cycle)))
;; (add-hook 'org-mode-hook 'ak-org-hooks)
(use-package org
  :straight nil
  :bind (:map org-mode-map
              ("<C-tab>" . up-n-fold)
              )
  :hook (org-mode . ak-org-hooks))
(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode))
(use-package toc-org :defer t
  :hook (org-mode . toc-org-mode)
  )
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))
(use-package ox-twbs :defer t
  :straight t)
(defun my/org-mode/load-prettify-symbols () "Prettify org mode keywords"
       (interactive)
       (setq prettify-symbols-alist
             (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                     '(
                       ;;            ("#+begin_src" . ?🔜)
                       ;; ("#+end_src" . ?🔝)
                       ("#+begin_quote" . ?💭)
                       ("#+end_quote" . ?🗯)
                                        ;("#+begin_example" . ?)
                                        ;("#+end_example" . ?)
                       ("#+OPTIONS:" . ?⚙)
                       ("#+startup:" . ?🏁)
                       ("#+DATE:" . ?📅)
                       ("#+AUTHOR:" . ?✍)
                       ("#+TITLE:" . ?📖)
                       ("#+language:" . ?🔤)
                       ("[ ]" .  ?☐)
                       ("[X]" . ?☑)
                       ("[-]" . ?❍)
                                        ;("lambda" . ?λ)
                                        ;("#+header:" . ?)
                                        ;("#+name:" . ?﮸)
                       ("#+results:" . ?🏁)
                                        ;("#+call:" . ?)
                       (":properties:" . ?)
                                        ;(":logbook:" . ?)
                       )))
       (prettify-symbols-mode 1))
(use-package org-roam ;; Package is on melpa
  :straight t
  :defer t
  :custom
  (make-directory "~/org-roam") ;; The dir all notes are gonna be stored
  (setq org-roam-directory (file-truename "~/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle) ;; Binds
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph) ;; Graph i was talking about.
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (setq org-roam-completion-everywhere t)
  (org-roam-setup))
(setq org-roam-v2-ack t)
(setq-default ispell-program-name    "hunspell"
              ispell-really-hunspell t
              ispell-check-comments  t
              ispell-extra-args      '("-i" "utf-8") ;; produce a lot of noise, disable?
              ispell-dictionary      "en_US")


(defun joe-turn-on-spell-check ()
  (flyspell-mode 1))

;; enable spell-check in certain modes
(add-hook 'markdown-mode-hook 'joe-turn-on-spell-check)
(add-hook 'text-mode-hook     'joe-turn-on-spell-check)
(add-hook 'org-mode-hook      'joe-turn-on-spell-check)
