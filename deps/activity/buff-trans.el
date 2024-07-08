;;; buff-trans.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(provide 'buff-trans)


(require 'switch-buffer-functions)
(require 'org-uninteractive-log-note)


(eval-when-compile
  (require 'activity-macro))
(require 'activity-base)
(require 'activity)


(drive-extended@ @defautl-note-in-org-current-clock (@note-class)
  "org log note"
  (def@ @@ :dispatch (prev curr time-spent)
    (ignore time-spent)
    (@! @org-clock-uninteractive-log-note :send "Changed to buffer %s from %s spent time %s"
        curr prev time-spent)))

(drive-extended@ @buffer-transition (@transition-class)
  (def@ @@ :initialize ()
    (setf @:transition @defautl-note-in-org-current-clock))

  (def@ @@ :dispatch (prev
                      curr
                      time-spent)
    (if (and (bufferp curr)
             (buffer-live-p curr))
        (@! @:transition :dispatch prev curr time-spent))))


(drive-extended@ @buffer-transition-span-detector (@transition-span-dectector-class)
  "Detector"
  (def@ @@ :ptrace (&optional msg)
    (let ((msg (or msg "ptrace"))
          (trace (with-temp-buffer
                   (backtrace)
                   (buffer-string))))
      (@:message "%s: %s" msg trace)))

  (def@ @@ :notify-buf-chg (fmt &rest args)
    (let ((msg (concat (current-time-string)
                       ": "
                       (apply #'format fmt args))))
      (@:message msg)))

  (def@ @@ :get-timer ()
    (@:message "Timer %s" @:timer))

  (def@ @@ :get-idle-times ()
    (@:message "Idle Times %s" @:idle-times))

  (def@ @@ :buffer-chg-print-info (&optional msg)
    (let ((msg         (or msg "info"))
          (time-passed (- (float-time (current-time))
                          (float-time @:time-start))))
      (when @:debug-switch-buf
        (@:notify-buf-chg "%s: prev currbuf-detect-buffer-chg-use %s, currbuf-run-detect-buffer-chg currbuf %s, (current-buffer) %s, (window-buffer) %s, idle-times %s, time-passed %d, Idle timer %s"
                          msg
                          @:currbuf-detect-buffer-chg-use
                          @:currbuf-run-detect-buffer-chg
                          (current-buffer)
                          (window-buffer)
                          @:idle-times
                          time-passed
                          (if @:timer t)))))

  (def@ @@ :buffer-chg-action (prevbuf currbuf time-spent)
    (@:buffer-chg-print-info "inaction")
    (@! @:transition :dispatch prevbuf currbuf time-spent)
    (@:buffer-chg-print-info "done inaction"))


  (def@ @@ :add-idle-timer-hook ()
    (let* ((idle-time-internal (current-idle-time))
           (idle-time (if idle-time-internal
                          (float-time idle-time-internal)
                        0)))
      (when (> idle-time @:idle-thresh-hold)
        (push idle-time @:idle-times))))

  (def@ @@ :cancel-detect-buffer-chg-use ()
    (when @:timer
      (cancel-timer @:timer)
      (setf @:timer nil))
    (setf @:idle-times nil
          @:time-start (current-time)))


  (def@ @@ :detect-buffer-chg-use (prev curr)
    (let* ((cumulatibe-idle-time (cl-reduce #'+ @:idle-times))
           (time-passed (- (float-time (current-time))
                           (float-time @:time-start)))
           (time-spent  (- time-passed cumulatibe-idle-time)))
      (when @:debug-switch-buf
        (@:message "detect-buffer-chg-use: (>= time-spent time-threshold-gap) %s" (>= time-spent @:time-threshold-gap))
        (@:message "detect-buffer-chg-use: (is-run-detect-buffer-chg-use) %s" (@:is-run-detect-buffer-chg-use))
        (@:message "detect-buffer-chg-use: (not (eq currbuf-detect-buffer-chg-use (current-buffer))) %s" (not (eq @:currbuf-detect-buffer-chg-use (current-buffer)))))

      (if (and (>= time-spent @:time-threshold-gap)
               (@:is-run-detect-buffer-chg-use)
               (not (eq @:currbuf-detect-buffer-chg-use (current-buffer))))
          (progn
            (@:cancel-detect-buffer-chg-use)
            (@:buffer-chg-action prev curr time-spent)
            (@:cancel-detect-buffer-chg-use)
            (setf @:currbuf-detect-buffer-chg-use curr)
            (@:buffer-chg-print-info "detect-buffer-chg-use total stop timer"))
        (progn
          (@:buffer-chg-print-info "detect-buffer-chg-use: else ")
          (when @:timer
            (cancel-timer @:timer)
            (setf @:timer
                  (run-with-timer @:timer-gap
                                  nil
                                  @:detect-buffer-chg-use @@ prev curr))) ;todo
          (@:buffer-chg-print-info "detect-buffer-chg-use reschd timer")))))

  (def@ @@ :is-run-detect-buffer-chg-use ()
    (and (not (or (string-match "^*helm" (buffer-name))
                  ;; (current-idle-time)
                  (string-match "^*Minibuf-" (buffer-name))
                  (minibufferp)))
         (eq (current-buffer)
             (window-buffer))))
  ;; (current-idle-time)

  (def@ @@ :run-detect-buffer-chg (prev curr)
    (ignore prev)
    (@:buffer-chg-print-info "run-detect-buffer-chg1")
    (if (and (@:is-run-detect-buffer-chg-use)
             (not (eq @:currbuf-run-detect-buffer-chg (current-buffer)))
             (not (eq @:currbuf-detect-buffer-chg-use (current-buffer))))
        (progn
          (@:buffer-chg-print-info "run-detect-buffer-chg")
          (@:cancel-detect-buffer-chg-use)
          (setf @:timer (run-with-timer @:timer-gap
                                        nil
                                        @:detect-buffer-chg-use @@ @:currbuf-run-detect-buffer-chg curr))
          (setf @:currbuf-run-detect-buffer-chg curr))
      (when (eq @:currbuf-detect-buffer-chg-use (current-buffer))
        (when @:debug-switch-buf (@:message "cancel timer"))
        (@:cancel-detect-buffer-chg-use))))

  (def@ @@ :initialize ()
    ;; (let ((trans-event (or trans-event))))
    ;; transition to be used
    (setf @:transition @buffer-transition)
    ;; debug
    (setf @:debug-switch-buf nil)
    ;; threshold
    (setf @:time-threshold-gap 45)      ;; for production
    ;; (setf @:time-threshold-gap 10) ;; -- for debug
    (setf @:timer-gap @:time-threshold-gap)
    (setf @:idle-thresh-hold 5)
    (setf @:time-start (current-time))
    (setf @:timer nil)
    (setf @:idle-times nil)
    ;; ?
    (setf @:currbuf-detect-buffer-chg-use (current-buffer))
    ;; ?
    (setf @:currbuf-run-detect-buffer-chg (current-buffer))
    ;; (@:enable-detect-buffer-chg-use)
    ;; prefix
    (@:buffer-chg-print-info "run-detect-buffer-chg1")
    t)

  (def@ @@ :uninitialize ()
    (@:disable-detect-buffer-chg-use)
    t))


(defun buffer-transition-span-detector-add-idle-timer-hook ()
  (@! @buffer-transition-span-detector :add-idle-timer-hook))

(defun buffer-transition-span-detector-run-detect-buffer-chg (prev curr)
  (@! @buffer-transition-span-detector :run-detect-buffer-chg prev curr))


(drive-extended@ @buff-trans-activity (@activity-interface) "buff-trans-activity"
  (def@ @@ :key ()
    "buff-trans-activity"
    "buff-trans-activity")
  (def@ @@ :activate ()
    (@! @buffer-transition-span-detector :cancel-detect-buffer-chg-use)
    (@! @buffer-transition-span-detector :initialize)
    (add-hook 'post-command-hook
              #'buffer-transition-span-detector-add-idle-timer-hook)
    (add-hook 'switch-buffer-functions
              #'buffer-transition-span-detector-run-detect-buffer-chg))
  (def@ @@ :deactivate ()
    (@! @buffer-transition-span-detector :cancel-detect-buffer-chg-use)
    (remove-hook 'post-command-hook
                 #'buffer-transition-span-detector-add-idle-timer-hook)
    (remove-hook 'switch-buffer-functions
                 #'buffer-transition-span-detector-run-detect-buffer-chg)))

;;;###autoload
(defun activity-register-buff-trans ()
  (interactive)
  (activity-register @buff-trans-activity))

;;;###autoload
(add-hook 'activity-register-hook
          #'activity-register-buff-trans)

(when nil
  ;; https://stackoverflow.com/questions/32878675/using-elisp-local-variables-instead-of-global-variables-to-add-a-function-into-a
  (progn

    (nth 11  (default-value 'post-command-hook))

    (setq-default post-command-hook '())

    (setq-default switch-buffer-functions nil)))

;;; buff-trans.el ends here
