;;; LSP Mode
;; Intelligent completions
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :defer t
  :bind-keymap ("C-c l" . lsp-command-map)
  :after (company company-box)
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  (lsp-mode . company-mode)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  ((let* (args)
     )lsp-enable-which-key-integration t))
(use-package lsp-ui :after lsp-mode
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  )
;;; Flycheck
;; Check code fast
(use-package flycheck :straight t :hook (prog-mode . flycheck-mode) :defer t)
(straight-use-package 'flycheck-popup-tip)
(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))
;;; Highlight the indentation
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  ;; Don't highlight first level (that would be a line at column 1)
  (defun my-highlighter (level responsive display)
    (if (> 1 level) ; replace `1' with the number of guides you want to hide
        nil
      (highlight-indent-guides--highlighter-default level responsive display)))

  (setq highlight-indent-guides-highlighter-function 'my-highlighter)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 20)

  (highlight-indent-guides-auto-set-faces))
;;; Aggressive Indent
;; Indentation is something I cannot live without
(use-package aggressive-indent :hook (prog-mode . aggressive-indent-mode))
;;; Smartparens
;; Smartly close brackets depending on mode
(use-package smartparens
  :defer 1
  :hook ((
          emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode hy-mode
          prolog-mode go-mode cc-mode python-mode
          typescript-mode json-mode javascript-mode java-mode
          ) . smartparens-strict-mode)
  ;; :hook (prog-mode . smartparens-strict-mode)
  :bind (:map smartparens-mode-map
              ;; This is the paredit mode map minus a few key bindings
              ;; that I use in other modes (e.g. M-?)
              ("C-M-f" . sp-forward-sexp) ;; navigation
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-p" . sp-backward-down-sexp)
              ("C-M-n" . sp-up-sexp)
              ("C-w" . whole-line-or-region-sp-kill-region)
              ("M-s" . sp-splice-sexp) ;; depth-changing commands
              ("M-r" . sp-splice-sexp-killing-around)
              ("M-(" . sp-wrap-round)
              ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-S" . sp-split-sexp) ;; misc
              ("C-M-s" . sp-join-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  ;; Always highlight matching parens
  (show-smartparens-global-mode +1)
  (setq blink-matching-paren nil)  ;; Don't blink matching parens
  (defun whole-line-or-region-sp-kill-region (prefix)
    "Call `sp-kill-region' on region or PREFIX whole lines."
    (interactive "*p")
    (whole-line-or-region-wrap-beg-end 'sp-kill-region prefix))
  ;; Create keybindings to wrap symbol/region in pairs
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
       (interactive "P")
       (sp-wrap-with-pair ,s)))
  (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
  (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
  (define-key prog-mode-map (kbd "M-{") (prelude-wrap-with "{"))
  (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))
  (define-key prog-mode-map (kbd "M-'") (prelude-wrap-with "'"))
  (define-key prog-mode-map (kbd "M-`") (prelude-wrap-with "`"))
  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "[" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "(" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  ;; Don't include semicolon ; when slurping
  (add-to-list 'sp-sexp-suffix '(java-mode regexp ""))
  ;; use smartparens-mode everywhere
  (smartparens-global-mode))


;;; Company
;; Beautiful Completion framework that looks like the one in BSCode
(use-package company
  :straight t
  :after (lsp-mode)
  :hook ((lsp-mode org-mode prog-mode) . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))

  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))
(use-package company-quickhelp :straight t :after company :hook (company-mode . company-quickhelp-mode) :defer t)
;;; Rust

(defun hrs/append-to-path (path)
  "Add a path both to the $PATH variable and to Emacs' exec-path."
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (add-to-list 'exec-path path))
(use-package rustic :after lsp
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))

  :config
  (hrs/append-to-path "~/.cargo/bin")
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'hrs/rustic-mode-hook))

(defun hrs/rustic-mode-hook ()
  "Don't prompt for confirmation before running `rustfmt'."
  (setq-local buffer-save-without-query t))

;;; C/C++
;; Very minimal configuration as I usually dont use it
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
;; Python
(use-package python-mode :straight nil :after lsp)
;;; Shell script
(use-package company-shell :straight t
  :hook ((sh-mode shell-mode) . sh-mode-init)
  :config
  (defun sh-mode-init ()
    (setq-local company-backends '((company-shell
                                    company-shell-env
                                    company-files
                                    company-dabbrev-code
                                    company-capf
                                    company-yasnippet)))))
(use-package sh-mode :straight nil
  :hook (sh-mode . lsp-deferred))
;;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("magit" (mode . magit-mode))
               ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ;; ("mu4e" (or

               ;;          (mode . mu4e-compose-mode)
               ;;          (name . "\*mu4e\*")
               ;;          ))
               ("programming" (or
                               (mode . clojure-mode)
                               (mode . clojurescript-mode)
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("eaf" (mode . eaf-mode))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
                                        ;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)
(use-package cloc
  :commands cloc)
(use-package define-word)
(straight-use-package '(apheleia :host github :repo "raxod502/apheleia"))
(apheleia-global-mode +1)
;;; Go Lang
(use-package go-mode)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)
;;; Line Numbers
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")
(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
      Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))
(global-display-line-numbers-mode)
;;; Compilation mode input support
;; (require 'cl-lib)
;; (defun endless/toggle-comint-compilation ()
;;   "Restart compilation with (or without) `comint-mode'."
;;   (interactive)
;;   (cl-callf (lambda (mode) (if (eq mode t) nil t))
;;       (elt compilation-arguments 1))
;;   (recompile))

;; (define-key compilation-mode-map (kbd "C-c i")
;;   #'endless/toggle-comint-compilation)
;; (define-key compilation-minor-mode-map (kbd "C-c i")
;;   #'endless/toggle-comint-compilation)
;; (define-key compilation-shell-minor-mode-map (kbd "C-c i")
;;   #'endless/toggle-comint-compilation)
(defun endless/send-input (input &optional nl)
  "Send INPUT to the current process.
Interactively also sends a terminating newline."
  (interactive "MInput: \nd")
  (let ((string (concat input (if nl "\n"))))
    ;; This is just for visual feedback.
    (let ((inhibit-read-only t))
      (insert-before-markers string))
    ;; This is the important part.
    (process-send-string
     (get-buffer-process (current-buffer))
     string)))
(define-key compilation-mode-map (kbd "i")
  #'endless/send-input)
(defun endless/send-self ()
  "Send the pressed key to the current process."
  (interactive)
  (endless/send-input
   (apply #'string
          (append (this-command-keys-vector) nil))))

(dolist (key '("\C-d" "\C-j" "y" "n"))
  (define-key compilation-mode-map key
    #'endless/send-self))
