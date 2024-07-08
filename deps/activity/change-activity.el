;;; change-activity.el --- Emacs Change-Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

;; This package meant to log, analyze and report all emacs change-activity of
;; user which could further utilized to visualize change-activity of user
;; during period of time or editing session.

;; Enable Change-Activity for the current buffer by invoking
;; `change-activity-mode'. If you wish to activate it globally, use
;; `global-change-activity-mode'.

;; Set variable `change-activity-api-key' to your API key. Point
;; `change-activity-cli-path' to the absolute path of the CLI script
;; (change-activity-cli.py).

;;; Code:

(provide 'change-activity)


(require 'org-insert-utils)


(eval-when-compile
  (require 'activity-macro))
(require 'activity-base)
(require 'org-interactive-note)


(drive-extended@ @change-tansition (@transition-class))
(drive-extended@ @change-dectector (@transition-dectector-class))
(drive-extended@ @change-span-detector (@transition-span-dectector-class)
  (setf @:note @org-interactive-note-dest)
  (setf @:minimum-char-changes 70) ; "minimum char changes"
  (setf @:minimum-changes      70) ; "minimum changes"
  (setf @:minimal-char-changes @:minimum-changes)
  (setf @:idle-timeout         10)
  (setf @:win-timeout          7)
  (def@ @@ :register-in-session ()
    (@:error "Implement it."))
  (def@ @@ :unregister-in-session ()
    (@:error "Implement it."))
  (def@ @@ :note-send (win-timeout
                       &key
                       fail-quietly
                       buff
                       chgcount
                       success
                       fail
                       run-before)
    (@! @:note :send win-timeout
        :fail-quietly fail-quietly
        :buff buff
        :chgcount chgcount
        :success success
        :fail fail
        :run-before run-before)))



(drive-extended@ @undo-tree-change-span-detector (@change-span-detector)
  (def@ @@ :initialize ()
    (defvar activity-buff-local-change-last-buffer-undo-tree-count 0) ;internal add in session and desktop
    (make-variable-buffer-local 'activity-buff-local-change-last-buffer-undo-tree-count))

  (def@ @@ :register-in-session ()
    (when (featurep 'desktop)
      (add-hook 'desktop-locals-to-save
                'activity-buff-local-change-last-buffer-undo-tree-count))
    (when (featurep 'session)
      (add-hook 'session-locals-include
                'activity-buff-local-change-last-buffer-undo-tree-count)))

  (def@ @@ :unregister-in-session ()
    (when (featurep 'desktop)
      (remove-hook 'desktop-locals-to-save
                   'activity-buff-local-change-last-buffer-undo-tree-count))
    (when (featurep 'session)
      (remove-hook 'session-locals-include
                   'activity-buff-local-change-last-buffer-undo-tree-count)))


  (def@ @@ :buffer-changes-count ()
    (let ((changes 0))
      (when buffer-undo-tree
        (undo-tree-mapc #'(lambda (node)
                            (ignore node)
                            (setq changes (+ changes 1)));; (length (undo-tree-node-next node))
                        (undo-tree-root buffer-undo-tree)))
      changes))

  (def@ @@ :detect (buff)
   (if (eq buff (current-buffer))
       (with-current-buffer buff
         (let* ((minimal-changes (or @:minimum-changes
                                     @:minimum-char-changes))
                (win-timeout (or @:win-timeout 7))
                (totalchgcount (@:buffer-changes-count))
                (chgcount (- totalchgcount
                             activity-buff-local-change-last-buffer-undo-tree-count)))
           (if (>= chgcount
                   minimal-changes)
               (if (@:note-send (+ @:idle-timeout @:win-timeout)
                                :buff
                                buff
                                :chgcount
                                chgcount
                                :success
                                #'(lambda ()
                                    (with-current-buffer buff
                                      (setq activity-buff-local-change-last-buffer-undo-tree-count totalchgcount)))
                                :fail
                                #'(lambda ()
                                    (with-current-buffer buff
                                      (setq activity-buff-local-change-last-buffer-undo-tree-count totalchgcount)))
                                :run-before nil)
                   (message "Lunched noter ret t")
                 (message "Lunched noter ret nil"))
             (message "HELLO: buffer-undo-tree-change: only %d changes not more than %d" chgcount minimal-changes))))
     (message "HELLO Current buffer %s is not same as %s"
              (current-buffer)
              buff))))

(drive-extended@ @undo-list-change-span-detector (@change-span-detector)
  (def@ @@ :initialize ()
    (defvar activity-buff-local-change-last-buffer-undo-list-pos 0) ;internal add in session and desktop
    (make-variable-buffer-local 'activity-buff-local-change-last-buffer-undo-list-pos))

  (def@ @@ :register-in-session ()
    (when (featurep 'desktop)
      (add-to-list 'desktop-locals-to-save
                   'activity-buff-local-change-last-buffer-undo-list-pos))
    (when (featurep 'session)
      (add-to-list 'session-locals-include
                   'activity-buff-local-change-last-buffer-undo-list-pos)))

  (def@ @@ :unregister-in-session ()
    (when (featurep 'desktop)
      (remove-hook 'desktop-locals-to-save
                   'activity-buff-local-change-last-buffer-undo-list-pos))
    (when (featurep 'session)
      (remove-hook 'session-locals-include
                   'activity-buff-local-change-last-buffer-undo-list-pos)))

  (def@ @@ :detect (buff)
   "Set point to the position of the last change.
  Consecutive calls set point to the position of the previous change.
  With a prefix arg (optional arg MARK-POINT non-nil), set mark so \
  \\[exchange-point-and-mark]
  will return point to the current position."
  ;; (interactive "P")
  ;; (unless (buffer-modified-p)
  ;;   (error "Buffer not modified"))
   (let ((win-timeout (or @:win-timeout 7)))
     (when (eq buffer-undo-list t)
       (error "No undo information in this buffer"))
     ;; (when mark-point (push-mark))
     ;; (unless @:minimal-char-changes
     ;;   (setq minimal-char-changes 10))
     (let ((char-changes 0)
           (undo-list (if activity-buff-local-change-last-buffer-undo-list-pos
                          (cl-rest (memq activity-buff-local-change-last-buffer-undo-list-pos
                                         buffer-undo-list))
                          buffer-undo-list))
           undo)
       (while (and undo-list
                   (cl-first undo-list)
                   (< char-changes
                      @:minimal-char-changes))
         (setq undo (cl-first undo-list))
         (cond ((and (consp undo) (integerp (cl-first undo)) (integerp (cl-rest undo)))
                ;; (BEG . END)
                (setq char-changes (+ char-changes (abs (- (cl-first undo) (cl-rest undo))))))
               ((and (consp undo) (stringp (cl-first undo))) ; (TEXT . POSITION)
                (setq char-changes (+ char-changes (length (cl-first undo)))))
               ((and (consp undo) (eq (cl-first undo) t))) ; (t HIGH . LOW)
               ((and (consp undo) (null (cl-first undo))))
            ;; (nil PROPERTY VALUE BEG . END)
            ;; (setq position (rest (last undo)))

           ((and (consp undo) (markerp (cl-first undo)))) ; (MARKER . DISTANCE)
           ((integerp undo))               ; POSITION
           ((null undo))               ; nil
           (t (error "Invalid undo entry: %s" undo)))
         (setq undo-list (cl-rest undo-list)))
       (when (>= char-changes
                 @:minimal-char-changes)
         (if (@:note-send (+ @:idle-timeout
                             @:win-timeout)
                          :buff     buff
                          :chgcount char-changes
                          :success  #'(lambda ()
                                        (with-current-buffer buff
                                          (setq activity-buff-local-change-last-buffer-undo-list-pos undo)))
                          :fail     #'(lambda ()
                                        (with-current-buffer buff
                                          (setq activity-buff-local-change-last-buffer-undo-list-pos undo)))
                          :run-before nil)
             (setq activity-buff-local-change-last-buffer-undo-list-pos undo)))))))


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


(drive-extended@ @change-activity (@activity-interface)

  (def@ @@ :initialize ()
    (setf @:detect-periodic-fn-timer nil) ;"Time for on change log note."
    (setf @:idle-timeout 10)
    (setf @:win-timeout (@ @change-span-detector :win-timeout)))

  (def@ @@ :key ()
    "chnage-activity"
    "change-activity")

  (def@ @@ :detect-periodic-fn ()
    ;; (when (or t (eq buffer (current-buffer)))
    (let ((buff     (current-buffer))
          (detector (if (and (consp buffer-undo-list)
                             (cl-first buffer-undo-list))
                        @undo-list-change-span-detector
                      @undo-tree-change-span-detector)))
      (@! detector :detect buff)))

  (def@ @@ :detect-periodic-fn-start-timer ()
    (if @:detect-periodic-fn-timer
        (progn
          (cancel-timer @:detect-periodic-fn-timer)
          (setf @:detect-periodic-fn-timer nil)))
    (setf @:detect-periodic-fn-timer (run-with-idle-timer @:idle-timeout
                                                          @:idle-timeout
                                                          @:detect-periodic-fn @@)))

 (def@ @@ :detect-periodic-fn-stop-timer ()
   (interactive)
   (if @:detect-periodic-fn-timer
       (progn
         (cancel-timer @:detect-periodic-fn-timer)
         (setf @:detect-periodic-fn-timer nil))))

 (def@ @@ :activate ()
   (@! @undo-list-change-span-detector :register-in-session)
   (@! @undo-tree-change-span-detector :register-in-session)
   (@:detect-periodic-fn-start-timer))

 (def@ @@ :deactivate ()
   (@:detect-periodic-fn-stop-timer)
   (@! @undo-list-change-span-detector :unregister-in-session)
   (@! @undo-tree-change-span-detector :unregister-in-session)))

;;;###autoload
(defun activity-register-change-activity ()
  (interactive)
  (activity-register @change-activity))

;;;###autoload
(add-hook 'activity-register-hook
          #'activity-register-change-activity)

;;; change-activity.el ends here
