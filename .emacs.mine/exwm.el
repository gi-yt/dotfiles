(use-package exwm
  :init
  (setq mouse-autoselect-window nil
        focus-follows-mouse t
        exwm-workspace-warp-cursor t
        exwm-workspace-number 6)
                                        ;exwm-workspace-display-echo-area-timeout 5
                                        ;exwm-workspace-minibuffer-position 'bottom) ;; Annoying focus issues
  :config
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (pcase exwm-class-name
                ("Vimb" (exwm-workspace-rename-buffer (format "vimb: %s" exwm-title)))
                ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title))))))

  (exwm-enable))

(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/bind-function (key invocation &rest bindings)
  "Bind KEYs to FUNCTIONs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (funcall ',invocation)))
    (setq key (pop bindings)
          invocation (pop bindings))))

(defun exwm/bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (exwm/run-in-background ,command)))
    (setq key (pop bindings)
          command (pop bindings))))



(defun dw/exwm-init-hook ()
  (with-eval-after-load 'perspective
    ;; Set up perspective names on initial workspaces
    (exwm-workspace-switch-create 1)
    (exwm-workspace-switch-create 2)
    (exwm-workspace-switch-create 3)
    (exwm-workspace-switch-create 4)
    (exwm-workspace-switch-create 5)
    (exwm-workspace-delete 0)
    
    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 1))
  
  ;; Launch apps that will run in the background
  (exwm/run-in-background "dunst")
  (exwm/run-in-background "element-desktop --hidden")
  (exwm/run-in-background "/usr/lib/polkit-gnome/gnome-authentication-agent-1r")
  (exwm/run-in-background "eval $(gpg-agent daemon)")
  )

(add-hook 'exwm-mode-hook
          (lambda ()
            (evil-local-set-key 'motion (kbd "C-u") nil)))

(defun dw/setup-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Emacs" (call-interactively #'exwm-input-toggle-keyboard))
    ("Xephyr" (call-interactively #'exwm-input-toggle-keyboard))
    ("mpv" (exwm-floating-toggle-floating)
     (dw/exwm-floating-toggle-pinned))))

;; Do some post-init setup
(add-hook 'exwm-init-hook #'dw/exwm-init-hook)

;; Manipulate windows as they're created
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            ;; Send the window where it belongs
            (dw/setup-window-by-class)))

;; Hide the modeline on all X windows
(add-hook 'exwm-floating-setup-hook
          (lambda ()
            (exwm-layout-hide-mode-line)))

(use-package exwm-systemtray :straight nil
  :after (exwm)
  :config
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 35))

;; Misc Commands

(defun dw/run-xmodmap ()
  (interactive)
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.config/X11/Xmodmap"))

(defun dw/configure-desktop ()
  (interactive)
  (dw/run-xmodmap)
  (exwm/run-in-background "dm-setbg -i"))

(defun dw/on-exwm-init ()
  (dw/configure-desktop)
  )

;; Key Passthrough

;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-h
        ?\M-x
        ?\M-`
        ?\M-&
        ?\M-:
        ?\C-\M-j  ;; Buffer list
        ?\C-\M-k  ;; Browser list
        ?\C-\M-n  ;; Next workspace
        ?\C-\M-'  ;; Popper toggle
        ?\C-\     ;; Ctrl+Space
        ?\C-\;))

;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
;; QOF Keybindings
(exwm/bind-function
 "s-w" 'eaf-open-browser-with-history
 "s-q" 'kill-buffer)

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  (desktop-environment-screenshot-command "flameshot gui"))

;; This needs a more elegant ASCII banner
(use-package hydra)
(defhydra hydra-exwm-move-resize (:timeout 4)
  "Move/Resize Window (Shift is bigger steps, Ctrl moves window)"
  ("j" (lambda () (interactive) (exwm-layout-enlarge-window 10)) "V 10")
  ("J" (lambda () (interactive) (exwm-layout-enlarge-window 30)) "V 30")
  ("k" (lambda () (interactive) (exwm-layout-shrink-window 10)) "^ 10")
  ("K" (lambda () (interactive) (exwm-layout-shrink-window 30)) "^ 30")
  ("h" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 10)) "< 10")
  ("H" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 30)) "< 30")
  ("l" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 10)) "> 10")
  ("L" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 30)) "> 30")
  ("C-j" (lambda () (interactive) (exwm-floating-move 0 10)) "V 10")
  ("C-S-j" (lambda () (interactive) (exwm-floating-move 0 30)) "V 30")
  ("C-k" (lambda () (interactive) (exwm-floating-move 0 -10)) "^ 10")
  ("C-S-k" (lambda () (interactive) (exwm-floating-move 0 -30)) "^ 30")
  ("C-h" (lambda () (interactive) (exwm-floating-move -10 0)) "< 10")
  ("C-S-h" (lambda () (interactive) (exwm-floating-move -30 0)) "< 30")
  ("C-l" (lambda () (interactive) (exwm-floating-move 10 0)) "> 10")
  ("C-S-l" (lambda () (interactive) (exwm-floating-move 30 0)) "> 30")
  ("f" nil "finished" :exit t))

;; Workspace switching
(setq exwm-input-global-keys
      `(([?\s-R] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)
        ([?\s-x] . exwm-input-toggle-keyboard)
        ([?\s-r] . hydra-exwm-move-resize/body)
        ([?\s-e] . dired-jump)
        ([?\s-Q] . (lambda () (interactive) (kill-buffer)))
        ))

(exwm-input-set-key (kbd "<s-return>") 'multi-vterm)
(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)


;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)
;; IJKL movement keys
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-j") #'windmove-left)
(exwm-input-set-key (kbd "s-k") #'windmove-down)
(exwm-input-set-key (kbd "s-i") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)

;; The following can only apply to EXWM buffers, else it could have unexpected effects.
(push ?\s-  exwm-input-prefix-keys)
(define-key exwm-mode-map (kbd "s-SPC") #'exwm-floating-toggle-floating)

