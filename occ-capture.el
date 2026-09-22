;;; occ-capture.el --- occ capture                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  s

;; Author: s <>
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

(provide 'occ-capture)



(defvar occ-capture-buffer-name "*Occ Capture*")

(defun occ-capture-store-entry (org-marker
                                return-to-marker
                                win-config
                                lines)
  ;; taken from org-capture-place-entry
  (lotus-with-marker org-marker
    (let ((org-text (string-join lines "\n"))
          (level    (org-get-valid-level (if (org-at-heading-p)
                                             (org-outline-level)
                                           1)
                                         1)))
      (org-end-of-meta-data t)
      (let ((origin (point-marker)))
        (unless (bolp) (insert "\n"))
        (org-capture-empty-lines-before)
        (let ((beg (point)))
          (save-restriction
            ;; (when insert-here? (narrow-to-region beg beg))
            (org-paste-subtree level
                               org-text
                               'for-yank))
          ;; (org-capture-position-for-last-stored beg)
          (org-capture-empty-lines-after)
          ;; (unless (org-at-heading-p) (outline-next-heading))
          ;; (org-capture-mark-kill-region origin (point))
          ;; (org-capture-narrow beg (if (eobp) (point) (1- (point))))
          ;; (org-capture--position-cursor beg (point))
          t)))))

(defun occ-capture-store-plain (org-marker
                                return-to-marker
                                win-config
                                lines)
  (lotus-with-marker org-marker
    (if t ;; (org-capture-get :prepend)
        ;; Skip meta data and drawers.
        (org-end-of-meta-data t)
      ;; Go to end of the entry text, before the next headline.
      (outline-next-heading))

    (let ((origin (point-marker)))
      (unless (bolp) (insert "\n"))
      (org-capture-empty-lines-before)
      (org-capture-position-for-last-stored (point))
      (let ((beg (point)))
        (insert (org-capture-get :template))
        (unless (bolp) (insert "\n"))
        ;; Ignore the final newline character so as to not alter data
        ;; after inserted text.  Yet, if the template is empty, make
        ;; sure END matches BEG instead of pointing before it.
        (let ((end (max beg (1- (point)))))
	        (org-capture-empty-lines-after)
	        (org-capture-mark-kill-region origin (point))
	        (org-capture-narrow beg end)
	        (org-capture--position-cursor beg end))))))

(defun occ-capture-store-note (org-marker
                               return-to-marker
                               win-config
                               lines)
  "Finish taking a log note, and insert it to where it belongs."
  (let ((txt (prog1 (buffer-string)
               (kill-buffer)))
        (note (cdr (assq org-log-note-purpose org-log-note-headings)))
        lines)
    (while (string-match "\\`# .*\n[ \t\n]*" txt)
      (setq txt (replace-match "" t t txt)))
    (when (string-match "\\s-+\\'" txt)
      (setq txt (replace-match "" t t txt)))
    (setq lines (and (not (equal "" txt)) (org-split-string txt "\n")))
    (when (org-string-nw-p note)
      (setq note
            (org-replace-escapes
             note
             (list (cons "%u" (user-login-name))
                   (cons "%U" user-full-name)
                   (cons "%t" (format-time-string
                               (org-time-stamp-format 'long 'inactive)
                               org-log-note-effective-time))
                   (cons "%T" (format-time-string
                               (org-time-stamp-format 'long nil)
                               org-log-note-effective-time))
                   (cons "%d" (format-time-string
                               (org-time-stamp-format nil 'inactive)
                               org-log-note-effective-time))
                   (cons "%D" (format-time-string
                               (org-time-stamp-format nil nil)
                               org-log-note-effective-time))
                   (cons "%s" (cond
                               ((not org-log-note-state) "")
                               ((string-match-p org-ts-regexp
                                                org-log-note-state)
                                (format "\"[%s]\""
                                        (substring org-log-note-state 1 -1)))
                               (t (format "\"%s\"" org-log-note-state))))
                   (cons "%S"
                         (cond
                          ((not org-log-note-previous-state) "")
                          ((string-match-p org-ts-regexp
                                           org-log-note-previous-state)
                           (format "\"[%s]\""
                                   (substring
                                    org-log-note-previous-state 1 -1)))
                          (t (format "\"%s\""
                                     org-log-note-previous-state))))))))
      (when lines (setq note (concat note " \\\\")))
      (push note lines))
    (when (and lines
               (not org-note-abort))
      (with-current-buffer (marker-buffer org-marker)
        (org-fold-core-ignore-modifications
          (org-with-wide-buffer
           ;; Find location for the new note.
           (goto-char org-marker)
           (when nil
             (set-marker org-marker nil))
           ;; Note associated to a clock is to be located right after
           ;; the clock.  Do not move point.
           (unless (eq org-log-note-purpose 'clock-out)
             (goto-char (org-log-beginning t)))
           ;; Make sure point is at the beginning of an empty line.
           (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert-and-inherit "\n"))
                  ((looking-at "[ \t]*\\S-") (save-excursion (insert-and-inherit "\n")))))
           ;; In an existing list, add a new item at the top level.
           ;; Otherwise, indent line like a regular one.
           (let ((itemp (org-in-item-p)))
             (if itemp
                 (let ((column (let ((struct (save-excursion
                                               (goto-char itemp)
                                               (org-list-struct))))
                                 (org-list-get-ind (org-list-get-top-point struct) struct))))
                   (indent-line-to column))
               (org-indent-line)))
           (insert-and-inherit (org-list-bullet-string "-") (pop lines))
           (let ((ind (org-list-item-body-column (line-beginning-position))))
             (dolist (line lines)
               (insert-and-inherit "\n")
               (unless (string-empty-p line)
                 (indent-line-to ind)
                 (insert-and-inherit line)))
             (insert-and-inherit "\n")
             (org-lotus-modification-post-action))
           (message "Note stored")
           (org-back-to-heading t)))))
  ;; (when org-log-post-message (message "%s" org-log-post-message))
  t)


;; (defun occ-capture-capture ()
;;   (let ((txt (prog1 (buffer-string)
;;                (kill-buffer)))
;;         lines)
;;     (while (string-match "\\`# .*\n[ \t\n]*" txt)
;;       (setq txt (replace-match "" t t txt)))
;;     (when (string-match "\\s-+\\'" txt)
;;       (setq txt (replace-match "" t t txt)))
;;     (setq lines (and (not (equal "" txt)) (org-split-string txt "\n")))))
(defun occ-capture-capture ()
  (let ((txt (prog1 (buffer-string)))
        lines)
    (while (string-match "\\`# .*\n[ \t\n]*" txt)
      (setq txt (replace-match "" t t txt)))
    (when (string-match "\\s-+\\'" txt)
      (setq txt (replace-match "" t t txt)))
    (setq lines (and (not (equal "" txt)) (org-split-string txt "\n")))))


(defun occ-capture-set-type (type)
  (plist-put occ-capture-cmd-local-plist
             :type type))
(defun occ-capture-get-type ()
  (plist-get occ-capture-cmd-local-plist
             :type))


(defun occ-capture-star ()
  (interactive)
  (if (= 0
         (current-column))
      (let ((template (occ-obj-capture+-helm-select-template)))
        (if template
            (progn
              (occ-capture-set-type 'entry)
              (insert (org-capture-plus-fill-template template)))
          (self-insert-command 1 ?\*)))
    (self-insert-command 1 ?\*)))

(defun occ-capture-finalize (org-marker
                             return-to-marker
                             win-config)
  "Finish taking a log note, and insert it to where it belongs."
  (occ-assert return-to-marker)
  (occ-assert (marker-buffer return-to-marker))
  (occ-message "finalize: return-to-marker = %s" return-to-marker)
  (let ((type (occ-capture-get-type))
        (lines (occ-capture-capture)))
    (cond ((eq type 'entry)
           (occ-capture-store-entry org-marker
                                    return-to-marker
                                    win-config
                                    lines))
          ((eq type 'plain)
           (occ-capture-store-plain org-marker
                                    return-to-marker
                                    win-config
                                    lines))
          (t
           (occ-capture-store-note org-marker
                                   return-to-marker
                                   win-config
                                   lines)))
    (set-window-configuration win-config)
    (with-current-buffer (marker-buffer return-to-marker)
      (goto-char return-to-marker)))
  ;; (move-marker return-to-marker nil)
  t)

(defun occ-capture-kill (org-marker
                         win-config)
  (if win-config
      (progn
        (set-window-configuration win-config))
    (occ-error "win-config is nil"))
  (kill-buffer (get-buffer occ-capture-buffer-name)))

(defun occ-capture-refile ()
  (interactive))

(defun occ-capture-replace-template ()
  (interactive))


(defvar occ-capture-cmd-local-plist nil)
(make-variable-buffer-local 'occ-capture-cmd-local-plist)
(defun occ-capture-cmd (cmd)
  (let ((cmd-fn (plist-get occ-capture-cmd-local-plist cmd)))
    (if cmd-fn
        (funcall cmd-fn))))
(defun occ-capture-cmd-finalize ()
  (interactive)
  (occ-capture-cmd :finalize))
(defun occ-capture-cmd-kill ()
  (interactive)
  (occ-capture-cmd :kill))
(defun occ-capture-cmd-refile ()
  (interactive)
  (occ-capture-cmd :refile))
(defun occ-capture-cmd-replace-template ()
  (interactive)
  (occ-capture-cmd :replace-template))
(defvar occ-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "*" #'occ-capture-star)
    (define-key map "\C-c\C-c" #'occ-capture-cmd-finalize)
    (define-key map "\C-c\C-k" #'occ-capture-cmd-kill)
    (define-key map "\C-c\C-w" #'occ-capture-cmd-refile)
    (define-key map "\C-c\C-r" #'occ-capture-cmd-replace-template)
    map)
  "Keymap for `occ-capture-mode', a minor mode.
      Use this map to set additional keybindings for when Org mode is used
      for a capture buffer.")

(define-minor-mode occ-capture-mode
  "Minor mode for special key bindings in a capture buffer.

      Turning on this mode runs the normal hook `occ-capture-mode-hook'."
  nil " Cap" occ-capture-mode-map
  (setq-local occ-capture-mode t)
  (setq-local header-line-format
              (substitute-command-keys
               "\\<occ-capture-mode-map>Capture buffer.  Finish \
      `\\[occ-capture-cmd-finalize]', refile `\\[occ-capture-cmd-refile]', \
      abort `\\[occ-capture-cmd-kill]', recapture `\\[occ-capture-cmd-replace-template]'.")))

(cl-defun occ-build-functions (&key
                               org-marker
                               return-to-marker
                               win-config
                               success-fun
                               fail-fun
                               run-before)
  (occ-assert return-to-marker)
  (occ-assert (marker-buffer return-to-marker))
  (occ-message "build: return-to-marker = %s" return-to-marker)
  (let ((win-config (or win-config
                        (current-window-configuration))))
   (let ((finalize #'(lambda ()
                       (occ-capture-finalize org-marker
                                             return-to-marker
                                             win-config)))
         (kill     #'(lambda ()
                       (occ-capture-kill org-marker
                                        win-config))))
     (list :finalize finalize
           :kill     kill))))

(defvar occ-capture-buffer-setup-hook nil)

(cl-defun occ-add-capture-buffer (target-buffer
                                 &key
                                 org-marker
                                 return-to-marker
                                 win-config
                                 chgcount
                                 success
                                 fail
                                 run-before)
  "Prepare buffer for taking a note, to add this note later."
  (switch-to-buffer target-buffer 'norecord)
  (erase-buffer)


  (occ-assert return-to-marker)
  (occ-assert (marker-buffer return-to-marker))

  (let ((functions (occ-build-functions :org-marker       org-marker
                                        :return-to-marker return-to-marker
                                        :win-config       win-config
                                        :success-fun      success
                                        :fail-fun         fail
                                        :run-before       run-before)))
    (if nil ;; (memq org-capture-how '(time state))
        (let (current-prefix-arg)
          ;; (occ-capture-finalize-internal)
          (funcall (plist-get functions :finalize)))
      (let ((org-inhibit-startup t))
        (org-mode))
      (occ-capture-mode t)
      (goto-char (point-max))
      (insert "HHHH")
      (setq-local occ-capture-cmd-local-plist functions)
      (run-hooks 'occ-capture-buffer-setup-hook))))

(cl-defun occ-do-capture-add (marker
                             &key
                             win-config)
  (occ-assert marker)
  (occ-assert (marker-buffer marker))
  (let ((win-timeout     7)
        (cleanupfn-local nil))
    (lotus-with-timed-new-win win-timeout timer cleanupfn-newwin cleanupfn-local win
      (condition-case nil
          (let ((target-buffer (get-buffer-create occ-capture-buffer-name)))
            (occ-add-capture-buffer target-buffer
                                   :org-marker       marker
                                   :return-to-marker (point-marker)
                                   :win-config       win-config
                                   :chgcount         nil
                                   :success          nil
                                   :fail             nil
                                   :run-before       nil))
        ((quit)
         (progn
           (funcall cleanupfn-newwin win cleanupfn-local)
           (if timer (cancel-timer timer))
           (signal (cl-first err)
                   (cl-rest err))))))))

;;; occ-capture.el ends here
