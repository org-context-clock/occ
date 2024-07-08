;;; org-clock-daysummary.el --- Basic macros                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; put in .emacs
;; (org-work-day-clock-mode-line-add t)

;;

;;; Code:



(provide 'org-clock-daysummary)


;; (require 'mode-line-config)

(require 'spaceline)

(eval-when-compile
  '(require 'spaceline))

;; (require 'file-utils)

(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(defvar org-work-day-clock-hours 8 "work day hours")

(defvar org-work-day-clock-monitor-files nil
  "org clock monitor files")

(defvar org-work-day-clock-monitor-files-mins-aggregate-internal nil
  "org clock monitor files")

(defvar org-work-day-clock-start 10)
(defvar org-work-day-clock-lunch-break-hour 1)
(defvar org-work-day-clock-end (+ org-work-day-clock-start
                                  org-work-day-clock-lunch-break-hour
                                  org-work-day-clock-hours))

(defvar org-work-day-clock-msg nil)

(defvar org-work-day-clock-mode-line-map (make-sparse-keymap))
(defvar org-work-day-mode-mode-line-string "" "Hello")
(put 'org-work-day-mode-line-string 'risky-local-variable t)
;; (get 'org-work-day-mode-line-string 'risky-local-variable)
;; (get 'org-mode-line-string 'risky-local-variable)

;;;###autoload
(defvar org-work-day-get-clock-string-separator nil)
;;;###autoload
(defvar org-work-day-mode-line-timer nil)
;;;###autoload
(defvar org-work-day-mode-pause nil)
;;;###autoload
(defvar org-work-day-mode-display 'mode-line)

(defvar org-work-day-get-clock-start-timer nil)


(unless org-work-day-get-clock-start-timer
  (setq org-work-day-get-clock-start-timer
        (run-at-time "00:01am" (* 24 60 60) 'org-work-day-get-clock-string t)))

(defface org-work-day-mode-line
  '((t (:inherit mode-line)))
  "Face used for clock display in mode line."
  :group 'org-faces)

(defface org-work-day-mode-line-underrun
    '((t (:inherit mode-line :foreground "Green")))
  "Face used for clock display for overrun tasks in mode line."
  :group 'org-faces)

(defface org-work-day-mode-line-overrun
  '((t (:inherit mode-line :background "red")))
  "Face used for clock display for overrun tasks in mode line."
  :group 'org-faces)


(defvar org-work-day-face 'org-work-day-mode-line)
(defvar org-work-day-face-overrun 'org-work-day-mode-line-overrun)
(defvar org-work-day-face-underrun 'org-work-day-mode-line-underrun)


;; (setq
;;  org-work-day-face          'org-mode-line-clock
;;  org-work-day-face-underrun 'org-work-day-mode-line-overrun
;;  org-work-day-face-overrun  'org-mode-line-clock-overrun)

(defun org-clock-unclocked-files-mins-today (files)
  (let* ((totalmins 0)
         file)
    (with-timeout (3 nil)
      (org-agenda-prepare-buffers files))
    (while (setq file (pop files))
      (with-current-buffer (find-buffer-visiting file)
        (save-excursion
          (save-restriction
            (cl-incf totalmins (org-clock-sum-today))))))
    totalmins))

(defun org-clock-clocked-secs-today ()
  "Get the clocked time for the current item in minutes.
The time returned includes the time spent on this task in
previous clocking intervals."
  (if (not (org-clock-is-active))
      0
    (let ((currently-clocked-time (floor (- (org-float-time)
                                            (org-float-time org-clock-start-time)))))
      currently-clocked-time)))

(defun org-work-day-clock-files-secs (files &optional all)
  (+ (org-clock-clocked-secs-today)
     (* 60
        (if (or all
                (null org-work-day-clock-monitor-files-mins-aggregate-internal))
            (setq org-work-day-clock-monitor-files-mins-aggregate-internal
                  (org-clock-unclocked-files-mins-today files))
          org-work-day-clock-monitor-files-mins-aggregate-internal))))

(defun org-work-day-clock-files-min-today (&optional force)
  (interactive "P")
  (if org-work-day-clock-monitor-files
      (let* ((today-clock-secs (org-work-day-clock-files-secs org-work-day-clock-monitor-files force))
             (secs             (- (* org-work-day-clock-hours 60 60) today-clock-secs))
             (remiain-today-clock-hms (org-timer-secs-to-hms secs)))
        (message "%s left for today." (format "%s" remiain-today-clock-hms)))))


(defun org-work-day-clock-start-secs ()
  (floor (org-float-time
            (apply 'encode-time
                   (append '(0 0 0) (cdddr (decode-time)))))))
;; (org-float-time org-work-day-clock-end)

(define-key org-work-day-clock-mode-line-map [mode-line mouse-2] 'org-work-day-clock-files-min-today)
(define-key org-work-day-clock-mode-line-map [mode-line mouse-1] 'org-work-day-clock-menu)

(defun org-work-day-clock-menu ()
  (interactive)
  (popup-menu
   '("Clock"
     ["Clock out" org-clock-out t]
     ["Change effort estimate" org-clock-modify-effort-estimate t]
     ["Go to clock entry" org-clock-goto t]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"])))

(defun org-timer-secs-to-hm (s)
  "Convert integer S into h:mm.
If the integer is negative, the string will start with \"-\"."
  (let (sign m h)
    (setq sign (if (< s 0) "-" "")
          s (abs s)
          m (/ s 60) s (- s (* 60 m))
          h (/ m 60) m (- m (* 60 h)))
    (format "%s%d:%02d" sign h m)))

(defun org-timer-secs-to-intelligent (s)
  "Convert integer S into h:mm.
If the integer is negative, the string will start with \"-\"
it omit prefixed 0s."
  (let (sign m h)
    (setq sign (if (< s 0) "-" "")
          s (abs s)
          m (/ s 60) s (- s (* 60 m))
          h (/ m 60) m (- m (* 60 h)))
    (format
     (concat "%s%d" (unless (= m 0) ":%02d"))
     sign h m)))

(defvar org-timer-secs-to-time-fmt-fn #'org-timer-secs-to-intelligent
  "could be org-timer-secs-to-intelligent or org-timer-secs-to-hm or org-timer-secs-to-hms")

;; TODO: optimize it.
;;;###autoload
(defun org-work-day-get-clock-string (&optional force)
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
  (if org-work-day-clock-monitor-files
      (let* ((now-sec             (floor (org-float-time)))
             (day-start-secs      (org-work-day-clock-start-secs))
             (work-day-start-secs (+ day-start-secs (* org-work-day-clock-start 60 60)))
             (work-day-end-secs   (+ day-start-secs (* org-work-day-clock-end 60 60)))
             (work-day-over-secs  (- now-sec work-day-start-secs))
             (work-day-left-secs  (- work-day-end-secs now-sec))
             (work-day-over-str   (funcall org-timer-secs-to-time-fmt-fn work-day-over-secs))
             (work-day-left-str   (funcall org-timer-secs-to-time-fmt-fn work-day-left-secs))
             (today-clocked-secs  (org-work-day-clock-files-secs org-work-day-clock-monitor-files force))
             (today-dur-left-sec  (- (* org-work-day-clock-hours 60 60) today-clocked-secs))
             (today-dur-left-str  (funcall org-timer-secs-to-time-fmt-fn today-dur-left-sec))
             (work-done-str
              (org-propertize
               today-dur-left-str
               'face (if (< today-dur-left-sec work-day-left-secs) ;; t ;; org-clock-task-overrun
                         org-work-day-face-underrun
                       org-work-day-face-overrun)))
             (work-day-time-str
              ;; (org-minutes-to-clocksum-string (* org-work-day-clock-hours 60))
              (funcall org-timer-secs-to-time-fmt-fn (* org-work-day-clock-hours 60 60)))
             (clockstr (org-propertize
                        (concat (if org-work-day-get-clock-string-separator " " "")
                                "[" "%s %s/%s %s" "]")
                         ;; (if org-work-day-clock-msg
                         ;;     (concat " (" (replace-regexp-in-string "%" "%%" org-work-day-clock-msg) ")"))
                        'face org-work-day-face)))
        (format clockstr
                work-day-over-str
                work-done-str
                work-day-time-str
                work-day-left-str))
    (message "org-work-day-clock-monitor-files is not set")))

(defun org-work-day-clock-monitor-files-set-from-dir (monitor-dir)
  (interactive "Dset org clock monitor dir: ")
  (setq org-work-day-clock-monitor-files
        (directory-files-recursive monitor-dir "\\.org$" 2 "\\(rip\\|stage\\)")))

(defun org-work-day-clock-monitor-files-add-from-dir (monitor-dir)
  (interactive "Dadd org clock monitor dir: ")
  (setq org-work-day-clock-monitor-files
        (append
         org-work-day-clock-monitor-files
         (directory-files-recursive monitor-dir "\\.org$" 2 "\\(rip\\|stage\\)"))))

;;;###autoload
(defun org-work-day-clock-monitor-files-add-files (&rest monitor-files)
  ;; (interactive "Dadd org clock monitor files: ")
  (dolist (f monitor-files)
    (cl-pushnew f org-work-day-clock-monitor-files)))

;;;###autoload
(defun org-work-day-clock-update-mode-line-internal (&optional force)
  ;; (defun org-work-day-clock-update-mode-line ()
  (if org-work-day-clock-monitor-files
      (progn
        (setq org-work-day-mode-mode-line-string
              (org-propertize
               (let ((clock-string (org-work-day-get-clock-string force))
                     (help-text
                      ;; "Org-mode clock is running.\nmouse-1 shows a menu\nmouse-2 will jump to task"
                      "Today's work clocks."))
                 (if (and (> org-clock-string-limit 0)
                          (> (length clock-string) org-clock-string-limit))
                     (org-propertize
                      (substring clock-string 0 org-clock-string-limit)
                      'help-echo (concat help-text ": " org-clock-heading))
                   (org-propertize clock-string 'help-echo help-text)))

               'local-map org-work-day-clock-mode-line-map
               'mouse-face (if (featurep 'xemacs) 'highlight 'mode-line-highlight)))
        (force-mode-line-update))
    (message "org-work-day-clock-monitor-files is not set")))

;;;###autoload
(defun org-work-day-clock-update-mode-line (&optional force)
  "Update the timer time in the mode line."
  (if org-work-day-clock-monitor-files
      (if org-work-day-mode-pause
          nil
        (org-work-day-clock-update-mode-line-internal force)
        (force-mode-line-update))
    (message "org-work-day-clock-monitor-files is not set")))

;;;###autoload
(defun org-work-day-clock-mode-line-add (force)
  (interactive "P")
  ;; (or global-mode-string (setq global-mode-string '("")))
  ;; (or (memq 'org-work-day-mode-mode-line-string global-mode-string)
  ;;     (setq global-mode-string
  ;;           (append global-mode-string
  ;;                   '(org-work-day-mode-mode-line-string))))

  (when (and (boundp 'global-mode-line-list)
             global-mode-line-list)
    (progn
      (or global-mode-line-list
          (setq global-mode-string '("")))
      (or (memq 'org-work-day-mode-mode-line-string
                global-mode-line-list)
          (setq global-mode-line-list
                (append global-mode-line-list
                        '(org-work-day-mode-mode-line-string))))))

  (when (fboundp 'spaceline-define-segment)
    (spaceline-define-segment workdaysummary
      ;; "Test"
      (powerline-raw (s-trim org-work-day-mode-mode-line-string)))
    (spaceline-toggle-workdaysummary-on)
    (spaceline-spacemacs-theme 'workdaysummary))

  (when (eq org-work-day-mode-display 'mode-line)
    (org-work-day-clock-update-mode-line force)
    (when org-work-day-mode-line-timer
      (cancel-timer org-work-day-mode-line-timer)
      (setq org-work-day-mode-line-timer nil))
    (setq org-work-day-mode-line-timer
          (run-with-timer 20 20 'org-work-day-clock-update-mode-line))))

;;;###autoload
(defun org-work-day-clock-mode-line-remove ()
  (interactive)
  (if (and (boundp 'global-mode-line-list)
           global-mode-line-list)
      (progn
        ;; (setq global-mode-string
        ;;       (delq 'org-work-day-mode-mode-line-string global-mode-string))
        (setq global-mode-line-list
              (delq 'org-work-day-mode-mode-line-string global-mode-line-list))))

  (when org-work-day-mode-line-timer
    (cancel-timer org-work-day-mode-line-timer)
    (setq org-work-day-mode-line-timer nil)))

;;; org-clock-daysummary.el ends here
