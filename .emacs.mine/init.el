;; This is not rust :( GC is Important.
(setq gc-cons-threshold (* 256 1024 1024))
;; Fringe is useful for diff-hl
;; It is the space between the line numbers and the edge of the window
(fringe-mode '(7 . 1))
;; Remove irritating stuff
(scroll-bar-mode -1)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "Welcome Back!")
(setq initial-buffer-choice t)
(setq initial-scratch-message nil)
;; Straight.el Initialization
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; See startup data.
;; Useful for debugging slow emacs
(use-package esup)
;; Do not litter .emacs.d with junk
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(use-package no-littering
  :demand t
  :config
  (setq no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; or
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
;; Treat all themes as safe
(use-package custom :straight nil
  :config
  ;; We don't use custom and don't have to set custom-file even
  ;; in the case when we "accidentally" click save in a custom buffer,
  ;; `init.el' would get modified which gets overwrite the next time
  ;; we run `make'.

  ;; Treat all themes as safe
  (setf custom-safe-themes t))
;; Fira code is my favourite font
(set-frame-font "FiraCode-9:slant=normal:weight=regular:width=normal" nil t)
;; (set-frame-font "Fira Code:weight=medium:width=normal" nil t)

(setf use-default-font-for-symbols nil)
(set-fontset-font t 'unicode "Joypixels" nil 'append)
(use-package emojify
  :straight t
  :commands emojify-mode)

;; Themes
(use-package one-themes)
(use-package theme-switcher
  :straight '(theme-switcher :host github :repo "hmatheisen/theme-switcher")
  :config
;; Set the light and dark theme
(setq day-theme 'one-light)
(setq night-theme 'one-dark)

;; Set the hour when you want the theme to switch
(setq day-hour 06)
(setq night-hour 18))
(use-package doom-modeline
  :config
  (doom-modeline-mode 1))
;; Basic Info
(setq user-full-name "Arya Kiran"
      user-mail-address "aryakiran@zohomail.eu")
;;; QOF Improvements

;; Increase the amount of data which Emacs reads from the process
;; (Useful for LSP where the LSP responses are in the 800k - 3M range)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Don't compact font caches during GC as it doesn't play too nice
;; with org-superstar-mode and some of my large org files (e.g. this file).
;; This might enlarge the Emacs memory footprint but I don't mind if Emacs
;; uses more memory but rather prefer speed.
(setq inhibit-compacting-font-caches t)

;; Always just use left-to-right text
;; This makes Emacs a bit faster for very long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default indent-tabs-mode nil)   ; don't use tabs to indent
(setq-default tab-width 4)            ; but maintain correct appearance
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
;; Newline at end of file
(setq require-final-newline t)
;; Default to utf-8 unix encoding
(prefer-coding-system 'utf-8-unix)
;; Delete the selection with a keypress
(delete-selection-mode t)
;; Activate character folding in searches i.e. searching for 'a' matches 'ä' as well
(setq search-default-mode 'char-fold-to-regexp)
;; Only split vertically on very tall screens
(setq split-height-threshold 120)
;; Only split horizontally if there are at least 90 chars column after splitting
(setq split-width-threshold 180)
;; Paste with middle mouse button doesn't move the cursor
(setq mouse-yank-at-point t)
;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)
;; Accept 'UTF-8' (uppercase) as a valid encoding in the coding header
(define-coding-system-alias 'UTF-8 'utf-8)

;; Put authinfo.gpg first so new secrets will be stored there by default and not in plain text
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
;; Don't ask to store credentials in .authinfo.gpg
(setq auth-source-save-behavior nil)
;; Use 'fancy' ellipses for truncated strings
(setq truncate-string-ellipsis "︙")
;; The blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
;; Disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; Nicer scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;; Disable auto vscroll (makes scrolling down a bit faster?)
(setq auto-window-vscroll nil)
;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; Some things don't work well with fish, just always use posix compatible shell (dash)
(setq shell-file-name "/bin/bash")
;; Select
(use-package select :straight nil
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
(use-package saveplace :straight nil
  :unless noninteractive
  :config
  (setq save-place-limit 1000)
  (save-place-mode))
;; Click On links
(use-package goto-addr :straight nil
  :hook ((compilation-mode prog-mode eshell-mode shell-mode) . goto-address-mode)
  :bind (:map goto-address-highlight-keymap
         ("<RET>" . goto-address-at-point)
         ("M-<RET>" . newline)))
(use-package dimmer
  :defer 10
  :config
  ;; Don't dim hydra, transient buffers or minibuffers
  (setq dimmer-buffer-exclusion-regexps '(" \\*\\(LV\\|transient\\)\\*"
                                          "^ \\*.*posframe.*buffer.*\\*$"
                                          "^\\*Minibuf-[0-9]+\\*"
                                          "^.\\*which-key\\*$"
                                          "^.\\*Echo.*\\*"))
  (setq dimmer-fraction 0.25)
  (setq dimmer-use-colorspace ':rgb)
  (dimmer-mode))
(use-package goggles
  :config (goggles-mode))
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; Individual history elements can be configured separately
;;(put 'minibuffer-history 'history-length 25)
;;(put 'evil-ex-history 'history-length 50)
;;(put 'kill-ring 'history-length 25))


(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
        folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main"
                      :files ("*.el" "extensions/*.el"))
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              :map minibuffer-local-map
              ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu")
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  :config
  (corfu-global-mode))


(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu")
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  :config
  (corfu-global-mode))


(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :demand t
    :init
    (global-unset-key (kbd "C-x b"))
    (global-set-key (kbd "C-x b") 'consult-buffer)
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :straight '(embark-consult :host github
                             :repo "oantolin/embark"
                             :files ("embark-consult.el"))
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))
(winner-mode 1)
(use-package popper
  :straight t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*rustic-compilation\\*"
          "\\*rustfmt\\*"
          "Output\\*$"
          "\\*Backtrace\\*"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (setq popper-mode-line nil)
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints
(defun doom-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively
     #'find-file
     )))


(global-set-key (kbd "<f1>") (lambda() (interactive) (doom-project-browse "~/.emacs.mine/*.el")))
(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)
;;; Navigation
(use-package hungry-delete
  :straight t
  :config (global-hungry-delete-mode))
(use-package expand-region
  :straight t
  :bind ("C-q" . er/expand-region)
  :defer t)

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

(use-package super-save
  :straight t
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package undo-fu :straight t)
(global-set-key (kbd "C-_")   'undo-fu-only-undo)
(global-set-key (kbd "M-_") 'undo-fu-only-redo)
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(global-undo-fu-session-mode)

(defun daedreth/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c k w") 'daedreth/kill-inner-word)
(defun daedreth/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))
(global-set-key (kbd "C-c c w") 'daedreth/copy-whole-word)
(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-c c l") 'daedreth/copy-whole-line)
(global-set-key (kbd "C-c k l") 'kill-whole-line)
(use-package exec-path-from-shell :straight t
  :config
  (exec-path-from-shell-initialize))
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(windmove-swap-states-default-keybindings)
(winner-mode t)
(use-package helpful :defer t)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)

(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)
(use-package general)
(general-evil-setup t)
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package evil-nerd-commenter
  :straight t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
(straight-use-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(straight-use-package 'solaire-mode)
(solaire-global-mode +1)
(load "~/.emacs.mine/programming.el")
(load "~/.emacs.mine/org.el")
(load "~/.emacs.mine/everything.el")
(load "~/.emacs.mine/exwm.el")
;;; Activity Watch
(use-package activity-watch-mode)
(global-activity-watch-mode)
;;; KeyCast
(use-package keycast)
(with-eval-after-load 'keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" mode-line-keycast)))
(keycast-mode)
