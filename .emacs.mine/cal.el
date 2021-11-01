;;; Calendar and Diary (and prot-diary.el)
(setq calendar-mark-diary-entries-flag t)
(setq calendar-mark-holidays-flag t)
(setq calendar-mode-line-format nil)
(setq calendar-time-display-form
      '(12-hours ":" minutes
                 (when time-zone
                   (format "(%s)" time-zone))))
(setq calendar-week-start-day 1)      ; Monday
(setq calendar-date-style 'european)
(setq calendar-date-display-form calendar-iso-date-display-form)
(setq calendar-time-zone-style 'numeric) ; Emacs 28.1

(require 'solar)
(setq calendar-latitude 13.082680         ; Not my actual coordinates
      calendar-longitude 80.270721)

(require 'cal-dst)
(setq calendar-standard-time-zone-name "+0530")

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

(remove-hook 'calendar-mode-hook #'org--setup-calendar-bindings)

(let ((map calendar-mode-map))
  (define-key map (kbd "s") #'calendar-sunrise-sunset)
  (define-key map (kbd "l") #'lunar-phases)
  (define-key map (kbd "i") nil) ; Org sets this, much to my chagrin (see `remove-hook' above)
  (define-key map (kbd "M-n") #'calendar-forward-month)
  (define-key map (kbd "M-p") #'calendar-backward-month))

