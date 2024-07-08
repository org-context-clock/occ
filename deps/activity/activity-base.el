;;; activity-base.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

(provide 'activity-base)


(require '@)


(eval-when-compile
  (require 'activity-macro))


(drive-extended@ @activity-base (@)
  "Activity Base"

  (setf @:activation-list nil)

  (setf @:activity-debug nil)

  (def@ @@ :keyp (key)
    (memq key (@:keys)))

  (def@ @@ :finalize ()
    ())

  (def@ @@ :message (&rest args)
    (apply #'message args))

  (def@ @@ :error (&rest args)
    (apply #'error args))

  (def@ @@ :debug (level &rest args)
    (when @:activity-debug
      (when (cl-first args)
        (apply #'format args)
        (when (member level '(:emergency :error :warning :debug))
          (apply #'lwarn 'activity level args))
        (unless (eq level :nodisplay)
          (apply #'message args)))))

  (def@ @@ :init ()
    (@^:init)
    (@:debug :warning "@activity-base :init")
    (setf @:_occuredon (current-time)))

  (def@ @@ :occuredon ()
    (format-time-string "%Y-%m-%d %H:%M:%S" @:_occuredon))

  (def@ @@ :dispatch ()
    (@:init))

  (@:dispatch))


(drive-extended@ @dest-class (@activity-base)
  "Destination Base Class"

  (defobjgen@ @@ :gen-builder ()
                 (def@ @@ :receive (fmt &rest args)
                   (apply #'format
                          fmt args)))

  (defobjgen@ @@ :gen-msg ()
                 (def@ @@ :receive (fmt &rest args)
                   (apply #'message
                          fmt args)))

  (defobjgen@ @@ :gen-warning ()
                 (def@ @@ :receive (fmt &rest args)
                   (apply #'lwarn
                          'activity
                          'warning
                          fmt args)))

  (defobjgen@ @@ :gen-error ()
                 (def@ @@ :receive (fmt &rest args)
                   (apply #'lwarn
                          'activity
                          'error
                          fmt args)))

  (def@ @@ :dispatch ()
    (@:init))

  (@:dispatch))


(drive-extended@ @note-class (@activity-base)
  "Note Base Class"

  (setf @:dests '())

  (def@ @@ :send (&rest args)
    "Node send method"
    (if (and
         ;; (memq :dests (@:keys))
         @:dests
         (consp @:dests))
        (dolist (dest @:dests)
          (if dest
              (if (@! dest :keyp :receive)
                  ;; (@! dest :receive fmt args)
                  (apply (@ dest :receive) dest args)
                (@:message
                 "for %s dest %s [%s] not has :receive method, not sending msg."
                 @:name
                 (@ dest :name)
                 (@! dest :keys)))
            (@:message "dest is nil")))
      (error "%s has No @:dests %d boundp(%s) consp(%s) present."
             @:name
             (length @:dests)
             (boundp '@:dests)
             (consp @:dests))))

  ;; (defobjgen@ @@ :gen-format-msg ()
  ;;                      "Generator for format message note"
  ;;   (push
  ;;    (@! @dest-class :gen-msg "msg")
  ;;    @:dests))

  ;; (defobjgen@ @@ :gen-org-log-note ()
  ;;                      "Generator for org log note"
  ;;   (push
  ;;    (@! @dest-class :gen-msg "msg")
  ;;    @:dests))

  ;; (defobjgen@ @@ :gen-org-dual-log-note ()
  ;;                      "Generator for dual org log note"
  ;;   (push
  ;;    (@! @dest-class :gen-msg "msg")
  ;;    @:dests))

  ;; (defobjgen@ @@ :gen-org-intreactive-log-note ()
  ;;                      "Generator for Interactive org log note"
  ;;   (push
  ;;    (@! @dest-class :gen-msg "msg")
  ;;    @:dests))

  (def@ @@ :dispatch ()
    (@:init))

  (@:dispatch))


;; activity
(drive-extended@ @activity-class (@activity-base)
  "Activity class"
  (def@ @@ :init ()
    (@^:init)
    (@:message "@activity-class :init")
    (setf @:_occuredon (current-time))))

(drive-extended@ @event-class (@activity-class)
  "Event class"
  (def@ @@ :note ()
    ))

(drive-extended@ @transition-class (@event-class)
  "Transition class"
  (def@ @@ :note ()
    ))

;; detectors
(drive-extended@ @activity-dectector-class (@activity-base)
  "Activity detector class"
  (def@ @@ :note ()
    ))

(drive-extended@ @event-dectector-class (@activity-dectector-class)
  "Event detector class"
  (def@ @@ :note ()
    ))


(drive-extended@ @transition-dectector-class (@event-dectector-class)
  "Transition detector class"
  (def@ @@ :note ()
    ))

(drive-extended@ @event-span-dectector-class (@event-dectector-class) ;TODO START
  "Duration detector class"
  (def@ @@ :note ()
    )
  (def@ @@ :dispatch ()
    (setf
     @:start-time    0
     @:stop-time     0
     @:active-time   0
     @:inactive-time 0)))


(drive-extended@ @transition-span-dectector-class (@transition-dectector-class) ;TODO START
  "Duration detector class"
  (def@ @@ :note ()
    )
  (def@ @@ :dispatch ()
    (setf
     @:start-time    0
     @:stop-time     0
     @:active-time   0
     @:inactive-time 0)))


(drive-extended@ @postpone-event-class (@activity-base)
  "Activity detector class"
  (def@ @@ :note ()
    ))

(drive-extended@ @save-event-class (@activity-base)
  "Activity detector class"
  (def@ @@ :note ()
    ))

(drive-extended@ @idleness-dectector-class (@activity-base)
  ;; set pre-command-hook
  ;; and measure time
  ;; collect in list
  ;; provide list return-reset functions
  "Activity detector class"
  (def@ @@ :note ()
    ))


;; based on note type correct destination should be chosen.
;; objects
;; 1. activity
;;   event
;;     mail send
;;     mail read
;;   transition
;;     buffer transition
;;     clock transition
;; note
;; note-destination
;;

;; just write prototype code
;; like sending function and their parameters etc
;; later it could be found to manage it to implement.

;; how I can pass different types of value e.g. (fmt arg1 arg2 ) and sexp
;; and it will be handled by corresponding handler ?

;;
;; * Do not forget about pragmatism
;; ** First complete working model, somehow
;; ** Things should be definable outside of this library based on framework provided here.

(when nil
  (progn
    (string-match "(\\<\\(def@\\)\\> +\\([^ ()]+\\)" "(def@ x")

    (string-match "(\\<\\(def@\\) +\\([^ ()]+\\)" "(def@ x")

    (string-match "\\<\\(@\\^?:[^ ()]+\\)\\>" "@:aa")

    (string-match "\\(@\\^?:[^ ()]+\\)\\>" "@:aa")))

;;; activity-base.el ends here
