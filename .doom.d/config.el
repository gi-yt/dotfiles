;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Arya K"
      user-mail-address "aryakiran@zohomail.eu")
(setq doom-theme 'doom-one)
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setf use-default-font-for-symbols nil)
(set-fontset-font t 'unicode "Joypixels" nil 'append)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;; Disable auto vscroll (makes scrolling down a bit faster?)
(setq auto-window-vscroll nil)
;; Select
(use-package! select
  :config
  ;; Use clipboard and primary selection for copy/paste
  (setq select-enable-primary t)

  (defun select-add-selection-to-kill-ring ()
    "Add clipboard and primary selection to the kill ring."
    (interactive)
    (when-let* ((primary (gui-get-primary-selection))
                (not-empty? (not (string-empty-p primary))))
      (kill-new primary))
    (when-let* ((clipboard (gui-backend-get-selection 'CLIPBOARD 'STRING))
                (not-empty? (not (string-empty-p clipboard)))))))
;; Save Where you were
(use-package! saveplace
  :unless noninteractive
  :config
  (setq save-place-limit 1000)
  (save-place-mode))
;; Click On links
(define-globalized-minor-mode my/global-goto-addr-mode goto-address-mode
  (lambda () (goto-address-mode 1)))
(my/global-goto-addr-mode)
;;; Movement
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(global-set-key (kbd "C-s") 'consult-line)
(use-package! hungry-delete
  :config (global-hungry-delete-mode))
;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
 See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)
;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
 See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-S-o") 'open-previous-line)
;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")
(global-set-key [S-return]   'open-next-line)
(global-set-key [C-S-return] 'open-previous-line)


;; Autisave
(use-package! super-save
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))
;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)

(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)
(global-set-key (kbd "M-/") 'comment-line)
(straight-use-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;;; KeyCast
(after! keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update))))
(add-to-list 'global-mode-string '(" " mode-line-keycast))
(keycast-mode) ;; or run keycast-mode by demand
;; Magit Gravatars
(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
(setq which-key-idle-delay 0.8)
;; Vterm
(setq vterm-eval-cmds '(("magit-status-setup-buffer" magit-status-setup-buffer)
                        ("find-file" find-file)
                        ("message" message)
                        ("vterm-clear-scrollback" vterm-clear-scrollback)))
(after! vterm
  (define-key vterm-mode-map (kbd "<C-backspace>") (lambda () (interactive) (vterm-send-key (kbd "C-w")))))


;;; Org
(setq org-ellipsis "▾")
(defun up-n-fold ()
  (interactive)
  (progn
    (outline-previous-visible-heading 1)
    (org-cycle)))
(add-hook 'org-mode-hook
  (lambda ()
   (local-set-key [(control tab)] 'up-n-fold)))
(use-package! toc-org
  :hook (org-mode . toc-org-mode)
  )
(use-package! org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))
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
;;; Aggressive indent
(use-package! aggressive-indent :hook (prog-mode . aggressive-indent-mode))
(use-package! company-box
  :hook (company-mode . company-box-mode))

;; (use-package! eaf
;;   :commands (eaf-open-browser eaf-open find-file)
;;   :config
;;   (use-package! ctable)
;;   (use-package! deferred)
;;   (use-package! epc))
(global-set-key (kbd "C-x b") 'consult-buffer)

(setq fancy-splash-image "~/.doom.d/stallman.png")
(setq-hook! 'python-mode-hook +format-with 'html-tidy)

;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)
(use-package! modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts t
        modus-themes-subtle-line-numbers t
        modus-themes-intense-markup t
        modus-themes-tabs-accented t
        modus-themes-fringes nil ; {nil,'subtle,'intense}
        modus-themes-mode-line '(3d accented)
        modus-themes-hl-line '(intense)
        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)
        modus-themes-completions 'moderate ;; {nil,'moderate,'opinionated}
        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        modus-themes-region '(bg-only no-extend))
  ;; (set-face-attribute 'vertico-current nil :underline t :background "#191AB")
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))
(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))
(setq  doom-font (font-spec :family "Fira Code" :size 13))
;; Bug with Emacs 29 and general.el
(general-auto-unbind-keys :off)
(remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)

(lorem-ipsum-use-default-bindings)
(setq ispell-dictionary "en_GB-large")
