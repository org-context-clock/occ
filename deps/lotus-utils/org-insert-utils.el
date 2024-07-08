;;; org-insert-utils.el --- Org insert utils for inserting log note, entry etc  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(provide 'org-insert-utils)


(require 'subr-x)
(require 'desktop)
(require 'session)

(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(eval-when-compile
  (require 'org-macs))
(eval-when-compile
  (require 'org-clock))
(require 'org-clock)
(require 'time-stamp)
(require 'undo-tree)
(require 'message)


;;;###autoload
(defun org-insert-log-note (marker
                            txt
                            &optional
                            purpose
                            effective-time
                            state
                            previous-state)
  "Finish taking a log note, and insert it to where it belongs.
It is non-interactive re-implementation of org-store-log-note here note is taken from TXT"
  (let* ((note-marker marker)
         (txt txt)
         (note-purpose (or purpose 'note))
         (effective-time (or effective-time (org-current-effective-time)))
         (note-state state)
         (note-previous-state previous-state))
    (if (marker-buffer marker)
        (let ((note (cl-rest (assq note-purpose org-log-note-headings)))
              lines)
          (while (string-match "\\`# .*\n[ \t\n]*" txt)
            (setq txt (replace-match "" t t txt)))
          (when (string-match "\\s-+\\'" txt)
            (setq txt (replace-match "" t t txt)))
          (setq lines (org-split-string txt "\n"))
          (when (org-string-nw-p note)
            (setq note
                  (org-replace-escapes
                   note
                   (list (cons "%u" (user-login-name))
                         (cons "%U" user-full-name)
                         (cons "%t" (format-time-string
                                     (org-time-stamp-format 'long 'inactive)
                                     effective-time))
                         (cons "%T" (format-time-string
                                     (org-time-stamp-format 'long nil)
                                     effective-time))
                         (cons "%d" (format-time-string
                                     (org-time-stamp-format nil 'inactive)
                                     effective-time))
                         (cons "%D" (format-time-string
                                     (org-time-stamp-format nil nil)
                                     effective-time))
                         (cons "%s" (cond
                                     ((not note-state) "")
                                     ((string-match-p org-ts-regexp note-state)
                                      (format "\"[%s]\""
                                              (substring note-state 1 -1)))
                                     (t (format "\"%s\"" note-state))))
                         (cons "%S"
                               (cond
                                ((not note-previous-state) "")
                                ((string-match-p org-ts-regexp
                                                 note-previous-state)
                                 (format "\"[%s]\""
                                         (substring
                                          note-previous-state 1 -1)))
                                (t (format "\"%s\""
                                           note-previous-state)))))))
            (when lines (setq note (concat note " \\\\")))
            (push note lines))

          (when lines ;; (and lines (not (or current-prefix-arg org-note-abort)))
            (with-current-buffer (marker-buffer note-marker)
              (org-with-wide-buffer
               ;; Find location for the new note.
               (goto-char note-marker)
               ;; (set-marker note-marker nil)

               ;; Note associated to a clock is to be located right after
               ;; the clock.  Do not move point.
               (unless (eq note-purpose 'clock-out)
                 (goto-char (org-log-beginning t)))
               ;; Make sure point is at the beginning of an empty line.
               (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
                     ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
               ;; In an existing list, add a new item at the top level.
               ;; Otherwise, indent line like a regular one.
               (let ((itemp (org-in-item-p)))
                 (if itemp
                     (indent-line-to
                      (let ((struct (save-excursion
                                      (goto-char itemp) (org-list-struct))))
                        (org-list-get-ind (org-list-get-top-point struct) struct)))
                   (org-indent-line)))
               (insert (org-list-bullet-string "-") (pop lines))
               (let ((ind (org-list-item-body-column (line-beginning-position))))
                 (dolist (line lines)
                   (insert "\n")
                   (indent-line-to ind)
                   (insert line)))
               (org-lotus-modification-post-action)
               (message "Note stored")
               (org-back-to-heading t)
               (org-cycle-hide-drawers 'children))
              ;; Fix `buffer-undo-list' when `org-store-log-note' is called
              ;; from within `org-add-log-note' because `buffer-undo-list'
              ;; is then modified outside of `org-with-remote-undo'.
              (when (eq this-command 'org-agenda-todo)
                (setcdr buffer-undo-list (nthcdr 2 buffer-undo-list))))))
      (error "merker %s buffer is nil" marker))))
;; Org insert log note un-interactively:1 ends here

;; Clock out with NOTE
;; *** Clock out with NOTE

;; [[file:org-onchange.org::*Clock out with NOTE][Clock out with NOTE:1]]
;;;###autoload
(defun org-clock-out-with-note (note
                                &optional
                                switch-to-state
                                fail-quietly
                                at-time) ;BUG TODO will it work or save-excursion save-restriction also required
  "org-clock-out-with-note"
  (interactive
   (let ((note (read-from-minibuffer "Closing notes: "))
         (switch-to-state current-prefix-arg))
     (list note switch-to-state)))

  (let ((org-log-note-clock-out t))
    (move-marker org-log-note-return-to nil)
    (move-marker org-log-note-marker nil)
    (org-clock-out switch-to-state
                   fail-quietly
                   at-time)
    (remove-hook 'post-command-hook
                 'org-add-log-note)
    (org-insert-log-note org-clock-marker
                         note)))
;; Clock out with NOTE:1 ends here

;; Org add log note with-timed-new-win
;; background in name is misleading it at present log-note show org file buffer to
;; add note but in this case it is not shown so background word is used.

;; *Note:* these function prepare buffer or window (timed) to take log note
;;  main work is only done by _org-store-log-note_


;; [[file:org-onchange.org::*Org add log note with-timed-new-win][Org add log note with-timed-new-win:1]]
;; copy of org-add-log-note

(cl-defun org-build-org-store-log-note-function (&key
                                                 success-fun
                                                 fail-fun
                                                 run-before)
  #'(lambda ()
      (let ((org-note-abort-before org-note-abort))
        (if run-before
            (unwind-protect
                (if org-note-abort-before
                    (and fail-fun
                         (funcall fail-fun))
                  (and success-fun
                       (funcall success-fun)))
              (funcall #'org-store-log-note))
            (unwind-protect
                (funcall #'org-store-log-note)
              (if org-note-abort-before
                  (and fail-fun
                       (funcall fail-fun))
                (and success-fun
                     (funcall success-fun))))))))

;; (defmacro org-build-org-store-log-note-function (&rest args)
;;   (let ((success-body (plist-get args :success))
;;         (fail-body    (plist-get args :fail))
;;         (run-before   (plist-get args :run-before)))
;;      `(org-build-org-store-log-note-function-1 :success-fun
;;                                                #'(lambda () ,@success-body)
;;                                               :fail-fun
;;                                               #'(lambda () ,@fail-body)
;;                                               :run-before
;;                                               run-before)))

;; *** Org add log note with-timed-new-win
;; background in name is misleading it at present log-note show org file buffer to
;; add note but in this case it is not shown so background word is used.

;; *Note:* these function prepare buffer or window (timed) to take log note
;; main work is only done by _org-store-log-note_

(defvar org-store-log-note-local-function nil)
(make-variable-buffer-local 'org-store-log-note-local-function)

(defun org-store-log-note-invoke-local-fun ()
  (funcall org-store-log-note-local-function))

(cl-defun org-add-log-note-buffer (target-buffer
                                   &key
                                   buff
                                   chgcount
                                   success
                                   fail
                                   run-before)
  "Prepare buffer for taking a note, to add this note later."
  ;; (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
  ;; (goto-char org-log-note-marker)
  ;; (org-switch-to-buffer-other-window "*Org Note*")

  (switch-to-buffer target-buffer 'norecord)
  ;; (set-buffer target-buffer)
  (erase-buffer)

  (let ((store-log-note-function (org-build-org-store-log-note-function :success-fun success
                                                                        :fail-fun fail
                                                                        :run-before run-before)))
    (if (memq org-log-note-how '(time state))
        (let (current-prefix-arg)
          ;; (org-store-log-note)
          (funcall store-log-note-function))
      (let ((org-inhibit-startup t))
        (org-mode))
      (let ((note-for (cond
                       ((eq org-log-note-purpose 'clock-out) "stopped clock")
                       ((eq org-log-note-purpose 'done)  "closed todo item")
                       ((eq org-log-note-purpose 'state)
                        (format "state change from \"%s\" to \"%s\""
                                (or org-log-note-previous-state "")
                                (or org-log-note-state "")))
                       ((eq org-log-note-purpose 'reschedule)
                        "rescheduling")
                       ((eq org-log-note-purpose 'delschedule)
                        "no longer scheduled")
                       ((eq org-log-note-purpose 'redeadline)
                        "changing deadline")
                       ((eq org-log-note-purpose 'deldeadline)
                        "removing deadline")
                       ((eq org-log-note-purpose 'refile)
                        "refiling")
                       ((eq org-log-note-purpose 'note)
                        "this entry")
                       (t (error "This should not happen")))))
        (insert (format (string-join
                         '("# Insert note for %s."
                           "# and %d changes in  buffer %s"
                           "# Finish with C-c C-c, or cancel with C-c C-k.\n\n")
                         "\n")
                        note-for
                        chgcount
                        (buffer-name buff))))
      (when org-log-note-extra (insert org-log-note-extra))
      ;; (setq-local org-finish-function 'org-store-log-note)
      (setq-local org-store-log-note-local-function store-log-note-function)
      (setq-local org-finish-function #'org-store-log-note-invoke-local-fun)
      (run-hooks 'org-log-buffer-setup-hook))))

;; (defun abcd (win-timeout &optional _purpose &key success fail run-before)
;;   (list :win-timeout win-timeout :_purpose _purpose :success success :fail fail :run-before run-before))



(cl-defun org-add-log-note-with-timed-new-win (win-timeout
                                               &key
                                               npurpose
                                               buff
                                               chgcount
                                               success
                                               fail
                                               run-before)
  "Pop up a window for taking a note, and add this note later."
  ;; (remove-hook 'post-command-hook 'org-add-log-note-background)
  ;; (setq org-log-note-window-configuration (current-window-configuration))
  ;; (delete-other-windows)

  ;; (move-marker org-log-note-return-to (point))
  (lotus-with-no-active-minibuffer-if
      (progn                            ;could schedule in little further.
        (lwarn 'org-onchange :debug "org-add-log-note-with-timed-new-win: [minibuff body] lotus-with-no-active-minibuffer-if")
        (lwarn 'org-onchange :debug "add-log-note-background: minibuffer already active quitting")
        (message "add-log-note-background: minibuffer already active quitting")
        (message nil))
    (lwarn 'org-onchange :debug "org-add-log-note-with-timed-new-win: [body] lotus-with-no-active-minibuffer-if")
    (let ((win-timeout (or win-timeout 7))
          (cleanupfn-local nil))
      (setq org-log-note-window-configuration (current-window-configuration))
      (lotus-with-timed-new-win
          win-timeout timer cleanupfn-newwin cleanupfn-local win
          (condition-case nil
              (let ((target-buffer (get-buffer-create "*Org Note*")))
                (org-add-log-note-buffer target-buffer
                                         :buff buff
                                         :chgcount chgcount
                                         :success success
                                         :fail fail
                                         :run-before run-before))
            ((quit)
             (progn
               (funcall cleanupfn-newwin win cleanupfn-local)
               (if timer (cancel-timer timer))
               (signal (cl-first err) (cl-rest err)))))))))

(cl-defun org-add-log-setup-with-timed-new-win (win-timeout
                                                &key
                                                purpose
                                                state
                                                prev-state
                                                how
                                                extra
                                                buff
                                                chgcount
                                                success
                                                fail
                                                run-before)
  "Set up the post command hook to take a note.
      If this is about to TODO state change, the new state is expected in STATE.
      HOW is an indicator what kind of note should be created.
      EXTRA is additional text that will be inserted into the notes buffer."
  (let ((win-timeout (or win-timeout 7)))
    (move-marker org-log-note-marker (point))
    (setq org-log-note-purpose purpose
          org-log-note-state state
          org-log-note-previous-state prev-state
          org-log-note-how how
          org-log-note-extra extra
          org-log-note-effective-time (org-current-effective-time))
    ;; (add-hook 'post-command-hook 'org-add-log-note-background 'append)
    (org-add-log-note-with-timed-new-win win-timeout
                                         :npurpose nil
                                         :buff buff
                                         :chgcount chgcount
                                         :success success
                                         :fail fail
                                         :run-before run-before)))

  ;;;##autoload
(cl-defun org-clock-lotus-log-note-current-clock-with-timed-new-win (win-timeout
                                                                     &key
                                                                     fail-quietly
                                                                     buff
                                                                     chgcount
                                                                     success
                                                                     fail
                                                                     run-before)
  (interactive)
  (ignore win-timeout)
  (let ((win-timeout  (or win-timeout  7)))
      (when (org-clocking-p)
        (move-marker org-log-note-return-to (point))
        (org-clock-lotus-with-current-clock
            (org-add-log-setup-with-timed-new-win win-timeout
                                                  :purpose 'note
                                                  :state nil
                                                  :prev-state nil
                                                  :how nil
                                                  :extra (concat "# Task: " (org-get-heading t) "\n\n")
                                                  :buff buff
                                                  :chgcount chgcount
                                                  :success success
                                                  :fail fail
                                                  :run-before run-before)))))

;; (defun org-clock-lotus-log-note-current-clock-with-timed-new-win (&optional fail-quietly)
;;   (interactive)
;;   (if (org-clocking-p)
;;       (org-clock-lotus-with-current-clock
;;        (org-add-log-setup-background
;;         'note nil nil nil
;;         (concat "# Task: " (org-get-heading t) "\n\n")))
;;       (if fail-quietly (throw 'exit t) (user-error "No active clock"))))

;; Org add log note with-timed-new-win:1 ends here

;;; org-insert-utils.el ends here
