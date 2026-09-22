;;; occ-property-editor.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'occ-property-editor)


(require 'time-stamp)
(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'lotus-utils)
(eval-when-compile
  (require 'lotus-utils))
(require 'lotus-idle-utils)
(eval-when-compile
  (require 'lotus-idle-utils))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-method)
(require 'occ-obj-utils)
(require 'occ-prop)
(require 'occ-assert)


(cl-defgeneric occ-do-select-propetry (obj
                                       &optional prompt)
  "occ-do-select-propetry")

(cl-defmethod occ-do-select-propetry ((obj occ-obj-ctx-tsk)
                                      &optional prompt)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore ctx)
    (occ-debug "%s" (occ-obj-Format tsk))
    ;; (occ-message "%s" (occ-obj-Format tsk))
    (let ((prompt     (or prompt
                          (format "%s proptery: "
                                  (occ-obj-Format tsk))))
          (fixed-keys '(edit
                        done))
          (keys       (occ-obj-properties-to-edit obj)))
      (if keys
          (let ((maxkeylen (apply #'max
                                  (mapcar #'(lambda (sym) ;https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
                                              (length (symbol-name sym)))
                                          (append keys
                                                  fixed-keys))))
                (key-vals  (occ-obj-get-properties tsk
                                                   keys)))
            (ignore maxkeylen)
            (occ-debug "for %s with keys =%s got key-vals = %s"
                       (occ-obj-Format tsk)
                       keys
                       key-vals)
            (if key-vals
                (let* ((key-val-collection (mapcar #'(lambda (key-val)
                                                       (let ((key-prompt (if (cl-rest key-val)
                                                                             (format "%s: %s" (cl-first key-val) (cl-rest key-val))
                                                                           (symbol-name (cl-first key-val)))))
                                                         (cons key-prompt
                                                               (cl-first key-val))))
                                                   key-vals))
                       (key-val-collection (append key-val-collection
                                                   (mapcar #'(lambda (fk) (cons (symbol-name fk) fk))
                                                           fixed-keys))))
                  (occ-assert key-val-collection)
                  (if key-val-collection
                      (let* ((key-sel (occ-completing-read prompt
                                                           key-val-collection
                                                           nil
                                                           t))
                             (sel (assoc key-sel
                                         key-val-collection)))
                        (occ-debug "selected option %s" sel)
                        (cl-rest sel))
                    (occ-error "Not Keys Vals Collection %s for %s" key-val-collection (occ-obj-Format tsk))))
              (occ-error "Not Keys Vals for %s" (occ-obj-Format tsk))))
        (occ-debug "Not Keys for %s" (occ-obj-Format tsk))))))


(defun org-get-flag-property-drawer (&optional force)
  (let ((range (org-get-property-block (point) force)))
    (when range
      org-cycle-subtree-status)))

(defun org-flag-property-drawer (flag
                                 &optional
                                 force)
  "NIL to open drawer T to close drawer"
  ;; https://orgmode.org/worg/org-hacks.html
  ;; https://orgmode.org/worg/org-hacks.html#org6d4906f
  ;; (recenter-top-bottom 2)
  ;; (unless flag                  ;; creating issue in cleanupfn error as display buffer and current buffer is not same.
  ;;   (recenter-top-bottom 2))
  (let ((heading (org-get-heading 'notags))
        (prop-range (org-get-property-block (point)
                                            force)))
    (ignore prop-range)
    ;; first show heading
    (when (eq org-cycle-subtree-status 'folded)
      (unless flag
        ;; https://lists.gnu.org/archive/html/emacs-orgmode/2015-02/msg00573.html
        (progn
          (when (or (org-invisible-p)
                    (org-invisible-p2))
            (org-show-context 'org-goto)))
        (progn                                        ; changed from org-show-tsk to org-show-entry
          (org-show-entry)
          (occ-debug "did %s entry `%s'"
                     (if flag "close" "open")
                     heading)
          (org-unlogged-message "CHILDREN")
          (setq org-cycle-subtree-status 'children))))
    ;; show expand property if flag is nil, else hide
    (let* ((prop-range (org-get-property-block (point)
                                               force))
           (prop-loc   (when (consp prop-range)
                         (1- (cl-first prop-range)))))
      (when prop-range
        (occ-debug "pos %d before jumping to %s drawer, will jump to pos %d"
                   (point)
                   (if flag "close" "open")
                   prop-loc)
        (goto-char prop-loc)
        (occ-debug "reached to %s drawer"
                   (if flag "close" "open"))
        (if (org-at-drawer-p)
            ;; show drawer
            (let ((drawer (org-element-at-point)))
              (when (memq (org-element-type drawer)
                          '(node-property drawer property-drawer))
                (occ-debug "trying to %s drawer %s current pos %d"
                           (if flag "close" "open")
                           drawer
                           (point))
                ;; (org-flag-drawer flag
                ;;                  drawer)
                (org-hide-drawer-toggle flag
                                        drawer)
                ;; Make sure to skip drawer entirely or we might flag
                ;; it another time when matching its ending line with
                ;; `org-drawer-regexp'.
                (when nil       ;;BUG ?? what
                  (goto-char (org-element-property :end drawer)))))
          (occ-debug "not at drawer to %s current pos is %s"
                     (if flag "close" "open")
                     (point)))
        (goto-char prop-loc)
        (occ-debug "reached to %s drawer1 current pos %d"
                   (if flag "close" "open")
                   (point))
        prop-range))))


(defun org-get-flag-property-drawer-at-marker (marker
                                               &optional
                                               force)
  (let ((buff (marker-buffer marker))
        (loc  (marker-position marker)))
    (when (and buff
               loc)
      (with-current-buffer buff
        (when (goto-char loc)
          (org-get-flag-property-drawer force))))))

(defun org-flag-property-drawer-at-marker (marker
                                           flag
                                           &optional
                                           force)
  "NIL to open drawer T to close drawer"
  ;; https://orgmode.org/worg/org-hacks.html
  ;; https://orgmode.org/worg/org-hacks.html#org6d4906f
  (let ((buff    (marker-buffer marker))
        (loc     (marker-position marker))
        (heading (org-get-heading 'notags)))
    (when (and buff
               loc)
      (with-current-buffer buff
        (let ((currloc (point)))
          (ignore currloc)
          (goto-char loc)
          (occ-debug "%s: org-flag-property-drawer-at-marker: called to %s drawer of heading `%s' in file %s loc %d"
                     (time-stamp-string)
                     (if flag "close" "open")
                     heading
                     (buffer-file-name buff)
                     loc)
          (org-flag-property-drawer flag
                                    force))))))


(cl-defmethod occ-do-open-property-block ((obj marker))
  ;; Find better name
  (let ((mrk              obj)
        (buffer-read-only nil))
    ;; (occ-debug "timer started for win %s" win)
    (let ((buff (marker-buffer   mrk))
          (pos  (marker-position mrk)))
      ;; show proptery drawer
      (if buff
          (progn
            (switch-to-buffer buff)
            (goto-char pos)
            (set-marker mrk (point))
            (recenter-top-bottom 2)
            (let* ((prop-range (org-flag-property-drawer-at-marker mrk nil))
                   (prop-loc   (when (consp prop-range) (1- (cl-first prop-range)))))
              (outline-show-all)
              (if (numberp prop-loc)
                  (goto-char prop-loc)
                (if nil
                    (occ-error "no prop-loc % for buff %s marker %s"
                               prop-loc buff mrk)))
              t))
        (occ-error "no buff %s found for object %s"
                   (occ-obj-Format obj))))))

(cl-defmethod occ-do-open-property-block ((obj null))
  (ignore obj)
  (occ-do-open-property-block (point-marker)))


(cl-defmethod occ-do-properties-editor ((obj occ-obj-ctx-tsk))
  (occ-debug "occ-do-properties-editor: begin %s"
             (occ-obj-Format obj))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk ctx)
    (let ((prop nil))
      (while (and (not (member (setq prop (occ-do-select-propetry obj))
                               '(edit done)))
                  prop)
        ;; TODO: handle (occ-do-select-propetry obj ctx) return NIL
        ;; No prop or NIL value can happen in CTX like *scratch*
        ;; (occ-assert prop)
        (let ((retval (occ-do-op-prop-edit obj
                                           prop
                                           nil
                                           nil)))
          (when retval
            ;; (occ-tsk-update-tsks t)
            (occ-debug "done with retval %s" retval)
            retval))))))


(cl-defmethod occ-do-properties-editor-in-cloned-buffer ((obj occ-obj-ctx-tsk))
  (occ-debug "occ-do-properties-editor-in-cloned-buffer: begin")
  ;; (message "message")
  ;; (message (occ-obj-format (occ-obj-tsk obj)))
  (let ((mrk (occ-obj-marker obj)))
    (org-with-cloned-marker mrk "<proptree>"
      (let ((cloned-mrk (point-marker)))
        (org-with-narrow-to-marker mrk
          (if (occ-do-open-property-block cloned-mrk)
              (occ-do-properties-editor obj)
            (occ-error "occ-do-properties-editor-in-cloned-buffer: can not edit props for %s"
                       (occ-obj-Format obj))))))))


(defun occ-do-properties-editor-handle-response (prop
                                                 timeout
                                                 timer
                                                 cleanup
                                                 local-cleanup
                                                 win)
  (ignore timeout)
  (cond ((eql 'done prop)
         (funcall cleanup
                  win
                  local-cleanup)
         (when timer
           (cancel-timer timer)))
        ((eql 'edit prop)
         ;; (funcall cleanup win local-cleanup)
         (occ-debug "occ-obj-prop-edit: debug editing")
         (when timer
           (cancel-timer timer))
         (when (and win
                    (windowp win)
                    (window-valid-p win))
           (select-window win 'norecord)))
        (t
         (funcall cleanup
                  win
                  local-cleanup)
         (when timer
           (cancel-timer timer)))))

(cl-defmethod occ-do-properties-window-editor ((obj occ-obj-ctx-tsk)
                                               &key
                                               return-transform
                                               timeout)
  (let* ((timeout (or timeout
                      occ-idle-timeout)))
    (let* ((local-cleanup #'(lambda ()
                              (occ-debug :warning "occ-do-properties-window-editor(obj occ-obj-ctx-tsk): local-cleanup called")
                              (occ-debug "occ-do-properties-window-editor(obj occ-obj-ctx-tsk): local-cleanup called")
                              (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-do-select-propetry
                                (abort-recursive-edit)))))
        (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
            timeout timer cleanup local-cleanup win
            (condition-case-control err
              (let ((prop (occ-do-properties-editor-in-cloned-buffer obj)))
                (occ-do-properties-editor-handle-response prop
                                                          timeout
                                                          timer
                                                          cleanup
                                                          local-cleanup
                                                          win)
                (occ-debug "occ-do-properties-window-editor(obj occ-obj-ctx-tsk) noquit: label %s value %s"
                           occ-return-true-label obj)
                (if return-transform ;Here caller know if return value is going to be used.
                    (occ-obj-make-return occ-return-true-label
                                         obj)
                  obj))
              ((quit)
               (progn
                 (occ-debug :warning "occ-do-properties-window-editor(obj occ-obj-ctx-tsk): canceling timer")
                 (occ-debug "occ-do-properties-window-editor(obj occ-obj-ctx-tsk): canceling timer")
                 (funcall cleanup
                          win
                          local-cleanup)
                 (if timer
                     (cancel-timer timer))
                 (signal (cl-first err)
                         (cl-rest  err))
                 (occ-debug "occ-do-properties-window-editor(obj occ-obj-ctx-tsk) quit: label %s value %s"
                            occ-return-quit-label
                            nil)
                 (when return-transform ;Here caller know if return value is going to be used.
                   (occ-obj-make-return occ-return-quit-label
                                        nil)))))))))

(cl-defmethod occ-do-properties-window-editor ((obj occ-ctx)
                                               &key
                                               filters
                                               builder
                                               ap-normal
                                               ap-transf
                                               return-transform ;Here caller know if return value is going to be used.
                                               timeout)
  (ignore ap-normal)
  (ignore ap-transf)
  (let* ((filters   (or filters nil))
         (builder   (or builder #'occ-obj-build-ctsk-with))
         (ap-normal (or ap-normal '(t actions general)))
         (ap-transf (or ap-transf '(t actions general edit)))
         (timeout   (or timeout occ-idle-timeout)))
    (occ-debug "occ-select-obj-prop-edit((obj occ-ctx)): begin")
    (occ-debug "occ-select-obj-prop-edit((obj occ-ctx)): begin")
    (let ((buff (occ-ctx-buffer obj)))
      (if (and (buffer-live-p buff)
               (not (occ-helm-buffer-p buff)))
          (let ((retval-ctx-tsk (occ-obj-select obj
                                                (occ-collections-default)
                                                :filters          filters
                                                :builder          builder
                                                :ap-normal        ap-normal
                                                :ap-transf        ap-transf
                                                :return-transform return-transform ;Here caller know if return value is going to be used.
                                                :timeout          timeout)))
            ;; (occ-debug "occ-do-properties-window-editor((obj occ-ctx)): ap-transf: %s action %s"
            ;;                   ap-transf action)
            (ignore ap-normal)
            (ignore ap-transf)
            (occ-debug "occ-do-properties-window-editor((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                       retval-ctx-tsk
                       (occ-obj-format (occ-obj-obj retval-ctx-tsk) 'capitalize)
                       (occ-obj-return-get-label retval-ctx-tsk))
            ;; BUG: will do run recursively as another method with (obj null) is define below.
            (when (and (occ-obj-return-in-labels-p retval-ctx-tsk
                                                   occ-return-select-label)
                       (occ-obj-obj retval-ctx-tsk))
              (occ-do-properties-window-editor (occ-obj-obj retval-ctx-tsk)
                                               :return-transform return-transform
                                               :timeout          timeout)
              (occ-debug "occ-do-properties-window-editor((obj occ-ctx)): No selection"))
            (occ-debug "occ-do-properties-window-editor((obj occ-ctx)): returning original: %s, retval: %s with label %s operate: %s"
                              retval-ctx-tsk
                              (occ-obj-Format (occ-obj-obj retval-ctx-tsk))
                              (occ-obj-return-get-label retval-ctx-tsk)
                              (occ-obj-return-in-labels-p retval-ctx-tsk
                                                          occ-return-select-label))
            retval-ctx-tsk)
        (occ-debug "occ-do-properties-window-editor((obj occ-ctx)): not running  as context buff is deleted or not live 1 %s, 2 %s"
                   (buffer-live-p buff)
                   (not (occ-helm-buffer-p buff)))
        (occ-debug "occ-do-properties-window-editor((obj occ-ctx)): not running  as context buff is deleted or not live 1 %s, 2 %s"
                   (buffer-live-p buff)
                   (not (occ-helm-buffer-p buff)))
        (when return-transform ;Here caller know if return value is going to be used.
          (occ-obj-make-return occ-return-false-label
                               nil))))))

(cl-defmethod occ-do-properties-window-editor ((obj null)
                                               &key
                                               filters
                                               builder
                                               ap-normal
                                               ap-transf
                                               return-transform
                                               timeout)
  (ignore obj)
  (ignore ap-normal)
  (ignore ap-transf)
  (occ-debug "occ-select-obj-prop-edit((obj null)):")
  (let ((filters   (or filters nil))
        (builder   (or builder #'occ-obj-build-ctsk-with))
        (ap-normal (or ap-normal '(t actions general)))
        (ap-transf (or ap-transf '(t actions general edit)))
        (timeout   (or timeout occ-idle-timeout)))
    (occ-do-properties-window-editor (occ-obj-make-ctx-at-point)
                                  :filters          filters
                                  :builder          builder
                                  :ap-normal        ap-normal
                                  :ap-transf        ap-transf
                                  :return-transform return-transform
                                  :timeout          timeout)))


(cl-defmethod occ-do-safe-properties-window-editor ((obj occ-ctx)
                                                    &key
                                                    filters
                                                    builder
                                                    ap-normal
                                                    ap-transf
                                                    return-transform
                                                    timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  ;; (interactive '((occ-obj-make-ctx-at-point) occ-idle-timeout))
  (ignore ap-normal)
  (ignore ap-transf)
  (occ-debug "occ-do-safe-properties-window-editor((obj occ-ctx)): begin")
  (occ-debug "occ-do-safe-properties-window-editor((obj[%s] occ-ctx)): begin"
               (occ-obj-Format obj))
  (let ((filters   (or filters nil))
        (builder   (or builder #'occ-obj-build-ctsk-with))
        (ap-normal (or ap-normal '(t actions general checkout)))
        (ap-transf (or ap-transf '(t actions general edit checkout)))
        (timeout   (or timeout occ-idle-timeout)))
    (occ-debug "begin occ-do-safe-properties-window-editor")
    (occ-debug "begin occ-do-safe-properties-window-editor")
    (occ-debug-return "occ-do-safe-properties-window-editor((obj occ-ctx)) no-active"
      (lotus-with-no-active-minibuffer-if
          (progn
            (occ-debug "occ-do-safe-properties-window-editor: [minibuffer-body] lotus-with-no-active-minibuffer-if")
            (occ-debug "occ-do-safe-properties-window-editor: minibuffer already active quitting")
            (occ-debug nil))
        ;;; TODO: extend lotus-with-other-frame-event-debug it to include elscreen change also.
        (occ-debug-return "occ-do-safe-properties-window-editor((obj occ-ctx)) frame-event-debug"
          (lotus-with-other-frame-event-debug "occ-do-safe-properties-window-editor" :cancel
            (occ-debug "occ-do-safe-properties-window-editor: lotus-with-other-frame-event-debug")
            (occ-debug "occ-do-safe-properties-window-editor: lotus-with-other-frame-event-debug obj[%s]" (occ-obj-Format obj))
            (prog1
                (let ((buff (occ-ctx-buffer obj)))
                  (if (eq (current-buffer)
                          buff)
                      (occ-debug-return "occ-do-safe-properties-window-editor((obj occ-ctx)) direct"
                        (occ-do-properties-window-editor obj
                                                         :filters          filters
                                                         :builder          builder
                                                         :ap-normal        ap-normal
                                                         :ap-transf        ap-transf
                                                         :return-transform return-transform
                                                         :timeout          timeout))
                   (progn
                     (occ-debug "context is not for current buffer.")
                     nil)))
              (occ-debug "finished occ-do-safe-properties-window-editor"))))))))

(cl-defmethod occ-do-safe-properties-window-editor ((obj marker)
                                                    &key
                                                    filters
                                                    builder
                                                    ap-normal
                                                    ap-transf
                                                    return-transform
                                                    timeout)
  (ignore obj)
  (occ-debug "occ-do-safe-properties-window-editor((obj marker)): begin")
  (let ((selected (occ-do-safe-properties-window-editor (occ-obj-make-ctx obj)
                                                        :filters          filters
                                                        :builder          builder
                                                        :return-transform return-transform
                                                        :ap-normal        ap-normal
                                                        :ap-transf        ap-transf
                                                        :timeout          timeout)))
    (occ-debug "occ-do-safe-properties-window-editor((obj marker)): returning %s" selected)
    selected))

(cl-defmethod occ-obj-safe-ignore-quit-properties-window-editor ((obj occ-ctx)
                                                                 &key
                                                                 filters
                                                                 builder
                                                                 return-transform
                                                                 ap-normal
                                                                 ap-transf
                                                                 timeout)
  "Return value is important to decide next action to (create unnamed tsk.)"
  (ignore ap-normal)
  (ignore ap-transf)
  (occ-debug "occ-obj-safe-ignore-quit-properties-window-editor((obj occ-ctx)): begin")
  (occ-debug "occ-obj-safe-ignore-quit-properties-window-editor((obj[%s] occ-ctx)): begin" (occ-obj-Format obj))
  (let ((filters   (or filters nil))
        (builder   (or builder #'occ-obj-build-ctsk-with))
        (ap-normal (or ap-normal '(t actions general checkout)))
        (ap-transf (or ap-transf '(t actions general edit checkout)))
        (timeout   (or timeout occ-idle-timeout)))
    (occ-debug "called occ-obj-safe-ignore-quit-properties-window-editor")
    (occ-debug "%s: begin: occ-obj-safe-ignore-quit-properties-window-editor" (time-stamp-string))
    (occ-debug "occ-obj-safe-ignore-quit-properties-window-editor: calling occ-delayed-select-obj-prop-edit with this-command=%s"
               this-command)
    (prog1
        (occ-do-safe-properties-window-editor obj
                                              :filters          filters
                                              :builder          builder
                                              :ap-normal        ap-normal
                                              :ap-transf        ap-transf
                                              :return-transform return-transform
                                              :timeout          timeout)
      (occ-debug "%s: end: occ-obj-safe-ignore-quit-properties-window-editor(obj[%s])"
                   (time-stamp-string)
                   (occ-obj-Format obj)))))

;;; occ-property-editor.el ends here
