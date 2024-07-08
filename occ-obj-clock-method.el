;;; occ-obj-clock-method.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  Sharad Pratap

;; Author: Sharad Pratap
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

(provide 'occ-obj-clock-method)


(eval-when-compile
  (require 'occ-macros))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'timer-utils-lotus)


(require 'occ-config)
(require 'occ-cl-utils)
(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-prop)
(require 'occ-obj-simple)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-unnamed)
(require 'occ-helm)
(require 'occ-helm-actions-config)


(defcustom *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*"
  :type '(list)
  :group 'occ)
(defvar    *occ-buff-sel-timer*                nil)
(defvar    *occ-tsk-current-ctx-time-interval* 7)
(defvar    *occ-tsk-previous-ctx*              nil)
(defvar    *occ-tsk-current-ctx*               nil)


(cl-defmethod occ-obj-ctxual-current-tsk ((obj occ-ctx)
                                          &key
                                          other-allowed)
  (let ((curr-tsk (occ-current-tsk :other-allowed other-allowed)))
    (when curr-tsk
      (occ-obj-build-ctxual-tsk-with curr-tsk
                                     obj))))

;; TODO: use TSK in place of CLOCK and make is general or CLOCK may be replaced with CURRENT

(cl-defmethod occ-obj-current-associated-p ((ctx occ-ctx))
  "current clock is associated to CTX"
  (and (occ-current-tsk)
       ;; (not (occ-clock-marker-unnamed-clock-p))
       (occ-obj-current-associable-p ctx)))
(cl-defmethod occ-obj-current-unassociated-p ((ctx occ-ctx))
  "current clock is unassociated to CTX"
  (not (occ-obj-current-associated-p ctx)))

(cl-defmethod occ-obj-try-until-associable-p ((obj occ-ctxual-tsk)
                                              (tries number))
  "Try three time to associated CTX with current TSK if succeed
then return t else nil"
  (occ-debug "(occ-obj-try-until-associable-p (obj occ-ctxual-tsk)[%s]) begin" (occ-obj-Format obj))
  (let ((retval  nil)
        (org-obj obj)
        (obj     obj))
    (occ-try-until tries (not (or (memq retval '(no-action skip))
                                  (occ-obj-associable-p obj)))
      (occ-debug "(occ-obj-try-until-associable-p (obj occ-ctxual-tsk)[%s]) ITERATION" (occ-obj-Format obj))
      ;; ;; BUG FIX
      ;; TODO: provision to pass prompt to describe why editor is called
      ;; note: it supposed to return t or nil
      ;; (setq retval
      ;;       (occ-do-op-prop-edit obj '(timebeing add 10)))
      (setq retval (occ-do-properties-editor-combined obj))
      (setq obj (occ-obj-build-ctxual-tsk-with (occ-obj-tsk org-obj)
                                               (occ-obj-make-ctx-at-point)))
      (occ-debug "(occ-obj-try-until-associable-p (obj occ-ctxual-tsk)[%s]): occ-try-until: (occ-obj-associable-p) %s retval %s"
                   (occ-obj-Format obj)
                   (occ-obj-associable-p obj)
                   retval))

    (occ-debug "(occ-obj-try-until-associable-p (obj occ-ctxual-tsk)[%s]) (occ-obj-associable-p) %s retval %s"
                 (occ-obj-Format obj)
                 (occ-obj-associable-p obj)
                 retval)
    (or (eq 'skip retval)
        (occ-obj-associable-p obj))))

(cl-defmethod occ-obj-try-if-unassociated-with-p ((tsk occ-tsk)
                                                  (ctx occ-obj-ctx)) ;; should handle occ-ctx
  "If clock in task is not unnamed clock then offer to increase clock time."
  (occ-debug "(occ-obj-try-if-unassociated-p (ctx occ-ctx)[%s]) begin" (occ-obj-Format ctx))
  (occ-debug "(occ-obj-try-if-unassociated-p (ctx occ-ctx)) (occ-obj-tsk tsk) %s" (occ-obj-Format (occ-obj-tsk tsk)))
  (if (and (occ-obj-tsk tsk)
           (not (occ-obj-unnamed-p tsk)))
      (if (occ-obj-associable-with-p tsk
                                     ctx)
          (progn
            (occ-debug "(occ-obj-try-if-unassociated-p (ctx occ-ctx)) ELSE need NO next clock-in")
            nil)
        (if (occ-obj-unnamed-p tsk)
            t
          (let* ((retval (not (occ-obj-try-until-associable-p (occ-obj-build-ctxual-tsk-with tsk ctx)
                                                              3))))
            (occ-debug "(occ-obj-try-if-unassociated-p (ctx occ-ctx)) IF occ-obj-try-until-associable-p: returned %s" retval)
            retval)))
    (progn
      (occ-debug "(occ-obj-try-if-unassociated-p (ctx occ-ctx)) ELSE No NAMED clock active need next clock-in")
      t)))

(cl-defmethod occ-obj-try-if-unassociated-p ((obj null)) ;; should handle occ-ctx
  "If clock in task is not unnamed clock then offer to increase clock time."
  (ignore obj)
  t)

(cl-defmethod occ-obj-try-if-unassociated-p ((obj occ-ctsk)) ;; should handle occ-ctx
  "If clock in task is not unnamed clock then offer to increase clock time."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-obj-try-if-unassociated-with-p tsk
                                        ctx)))

;; (cl-defmethod occ-obj-try-current-if-unassociated-p ((obj occ-obj-ctx)) ;; should handle occ-ctx
;;   "If clock in task is not unnamed clock then offer to increase clock time."
;;   (occ-debug "(occ-obj-try-current-if-unassociated-p (obj occ-ctx)[%s]) begin" (occ-obj-Format obj))
;;   (occ-debug "(occ-obj-try-current-if-unassociated-p (obj occ-ctx)) (occ-current-tsk) %s" (occ-obj-Format (occ-current-tsk)))
;;   (if (and (occ-current-tsk)
;;            (not (occ-clock-marker-unnamed-clock-p)))
;;       (if (occ-obj-current-associated-p obj)
;;           (progn
;;             (occ-debug "(occ-obj-try-current-if-unassociated-p (obj occ-ctx)) ELSE need NO next clock-in")
;;             nil)
;;         (if (occ-clock-marker-unnamed-clock-p)
;;             t
;;           (let* ((retval (not (occ-obj-try-until-associable-p (occ-obj-ctxual-current-tsk obj)
;;                                                               3))))
;;             (occ-debug "(occ-obj-try-current-if-unassociated-p (obj occ-ctx)) IF occ-obj-try-until-associable-p: returned %s" retval)
;;             retval)))
;;     (progn
;;       (occ-debug "(occ-obj-try-current-if-unassociated-p (obj occ-ctx)) ELSE No NAMED clock active need next clock-in")
;;       t)))

(cl-defmethod occ-obj-try-current-if-unassociated-p ((obj occ-obj-ctx)) ;; should handle occ-ctx
  (occ-obj-try-if-unassociated-p (occ-obj-ctxual-current-tsk obj :other-allowed t)))


(cl-defmethod occ-do-clock-in-if-not ((obj occ-ctx)
                                      &key
                                      filters
                                      builder
                                      ap-normal
                                      ap-transf
                                      auto-select-if-only
                                      timeout)
  (ignore ap-normal)
  (ignore ap-transf)
  (unless builder (occ-error "Builder can not be nil"))
  (let ((filters          (or filters (occ-match-filters)))
        (builder          (or builder #'occ-obj-build-ctxual-tsk-with))
        (return-transform t) ;as return value is going to be used.)
        (timeout          (or timeout occ-idle-timeout)))
    (let* ((ap-normal (occ-obj-list-select-ap-transf-keys)))
      (occ-debug "occ-do-clock-in-if-not((obj occ-ctx)): begin")
      (if (occ-obj-try-current-if-unassociated-p obj) ;; (occ-obj-current-unassociated-p obj) ;; (occ-do-edit-current-if-unassociated-p obj)
          (prog1                ;current clock is not matching
              t
            (occ-debug "occ-do-clock-in-if-not: Now really going to clock with this-command=%s"
                       this-command)
            ;; TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm
            (occ-debug "TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm")
            (let ((retval (occ-do-clock-in obj
                                           :filters             filters
                                           :builder             builder
                                           :return-transform    return-transform
                                           :ap-normal           ap-normal
                                           :ap-transf           ap-transf
                                           :auto-select-if-only auto-select-if-only
                                           :timeout             timeout)))
              (occ-debug "occ-do-clock-in-if-not: operate %s retval %s"
                         (occ-obj-return-in-labels-p retval
                                                     occ-return-quit-label
                                                     occ-return-timeout-label)
                         (occ-obj-obj retval))
              (if (occ-obj-return-in-labels-p retval
                                              occ-return-quit-label
                                              occ-return-timeout-label)
                  (unless (occ-obj-obj retval)
                    (if (occ-clock-marker-unnamed-clock-p)
                        (occ-debug "occ-do-clock-in-if-not: already clock-in into unnamed task ")
                      (if (occ-config-clock-in)
                          (progn
                            (occ-debug "trying to create unnamed tsk.")
                            (occ-debug "trying to create unnamed tsk.")
                            (occ-do-maybe-create-clockedin-unnamed-ctxual-tsk obj))
                        (occ-debug "occ-do-clock-in(obj occ-ctx): clock-in not allowed."))))
                (occ-debug "occ-do-clock-in-if-not: Can not operate on %s"
                           (occ-obj-format (occ-obj-obj retval)))))
            (occ-debug "occ-do-clock-in-if-not: Now really clock done."))
        (prog1
            nil
          (occ-debug "occ-do-clock-in-if-not: Current tsk already associate to %s"
                     (occ-obj-Format obj)))))))
;; occ-do-clock-in-if-not


(cl-defmethod occ-obj-consider-for-clockin-in-p ()
  (> (float-time (time-since *occ-last-buff-sel-time*))
     *occ-tsk-current-ctx-time-interval*))

(cl-defmethod occ-obj-try-to-clock-in-p ((curr occ-ctx)
                                         (prev occ-ctx))
  ;;BUG: Reconsider whether it is catching case after some delay.
  (not (equal curr
              prev)))

(cl-defmethod occ-obj-try-to-clock-in-p ((curr occ-ctx)
                                         (prev null))
  (ignore curr)
  (ignore prev)
  t)

(defun occ-obj-clock-in-ctx-auto-select-if-only ()
  "clock in ctx auto select if only"
  occ-clock-in-ctx-auto-select-if-only nil)

(cl-defmethod occ-do-clock-in-if-chg ((obj occ-ctx)
                                      &key
                                      filters
                                      builder
                                      ap-normal
                                      ap-transf
                                      auto-select-if-only
                                      timeout)
  (ignore ap-normal)
  (ignore ap-transf)
  (let ((filters (or filters (occ-match-filters)))
        (builder (or builder #'occ-obj-build-ctxual-tsk-with))
        (timeout (or timeout occ-idle-timeout)))
    (let* ((ap-normal (occ-obj-list-select-ap-transf-keys))
           ;;  ap-transf will reuse ap-normal if nil
           (ap-transf nil))
      (occ-debug "occ-do-clock-in-if-chg((obj occ-ctx)): begin")
      (if (occ-obj-consider-for-clockin-in-p)
          (progn
            (setq *occ-tsk-current-ctx* obj)

            (if (occ-obj-try-to-clock-in-p obj
                                           *occ-tsk-previous-ctx*)
                (when (occ-do-clock-in-if-not obj
                                              :filters             filters
                                              :builder             builder
                                              :ap-normal           ap-normal
                                              :ap-transf           ap-transf
                                              :auto-select-if-only auto-select-if-only
                                              :timeout             timeout)
                  (occ-debug "occ-do-clock-in-if-chg((obj occ-ctx)): calling occ-do-clock-in-if-not")
                  (setq *occ-tsk-previous-ctx* *occ-tsk-current-ctx*))
              (prog1
                  nil
                ;; BUG *occ-tsk-previous-ctx* *occ-tsk-current-ctx* not getting
                ;;     updated with simple buffer switch as idle tiem occur. IS IT CORRECT OR BUG
                ;; TODO: here describe reason for not trying properly, need to print where necessary.
                (occ-do-describe-try-to-clock-in *occ-tsk-current-ctx*
                                                 *occ-tsk-previous-ctx*))))
        (occ-nodisplay "occ-do-clock-in-if-chg: not enough time passed.")))))
;; occ-do-clock-in-if-chg


(defvar *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*")
(defvar *occ-buff-sel-timer*                nil)
(defvar *occ-tsk-current-ctx-time-interval* 7)
(defvar *occ-tsk-previous-ctx*              nil)
(defvar *occ-tsk-current-ctx*               nil)


(defvar occ-ignore-buffer-names   '("*scratch*" "*Org Note*" " *helm" "*Help*") "occ-ignore-buffer-names")
(defvar occ-ignore-buffer-regexps '(" *helm" "*Help*" "*helpful") "occ-ignore-buffer-names")

(defun occ-add-ignore-buffer-names ()
  (interactive)
  (let ((buffname (buffer-name (current-buffer))))
    (cl-pushnew buffname
                occ-ignore-buffer-names)))

(defun occ-obj-ignore-ctx-p (ctx-buff)
  (or (member (buffer-name ctx-buff)
              occ-ignore-buffer-names)
      (cl-some #'(lambda (re)
                   (string-match re (buffer-name ctx-buff)))
               occ-ignore-buffer-regexps)))

(cl-defmethod occ-do-describe-try-to-clock-in ((curr occ-ctx)
                                               (prev occ-ctx))
  (let ((buff (occ-ctx-buffer curr)))
    (let ((msg (cond
                 ((not (occ-chgable-p))
                  (format "clock is not changeable now."))
                 ((not buff)
                  (format "context buffer is null"))
                 ((not (buffer-live-p buff))
                  (format "context buffer is not live now."))
                 ((minibufferp buff)
                  (format "context buffer is minibuffer."))
                 ((occ-obj-ignore-p buff)
                  (format "context buffer is ignored buffer."))
                 ((equal prev curr)
                  (format "context is not changed."))
                 (t (format "Unknown reason.")))))
      (let ((full-msg (format "occ-do-clock-in-if-chg: ctx %s not suitable to associate as %s"
                              (occ-obj-Format curr)
                              msg)))
         ;; (occ-nodisplay full-msg)
        (occ-debug full-msg)))))


;;;###autoload
(defun occ-do-clock-in-curr-ctx (&optional force)
  (interactive "P")
  (ignore force)
  (let ((ctx (occ-obj-make-ctx-at-point)))
    (let ((filters             (occ-match-filters))
          (builder             #'occ-obj-build-ctxual-tsk-with)
          (auto-select-if-only nil) ; (occ-obj-clock-in-ctx-auto-select-if-only))
          (timeout             occ-idle-timeout))
      (let* ((ap-normal (occ-obj-list-select-ap-transf-keys))
             ;;  ap-transf will reuse ap-normal if nil
             (ap-transf nil))
        (occ-do-clock-in-if-not ctx
                                :filters             filters
                                :builder             builder
                                :ap-normal           ap-normal
                                :ap-transf           ap-transf
                                :auto-select-if-only auto-select-if-only
                                :timeout             timeout)))))

;;;###autoload
(defun occ-do-clock-in-curr-ctx-if-not (&optional force)
  (interactive "P")
  ;; TODO: Add code to which check if only focus present than only trigger else
  ;;       postpone it by calling run-with-idle-plus-timer
  (occ-debug "begin occ-do-clock-in-curr-ctx-if-not")
  ;;TODO: problem
  ;; (lotus-with-other-frame-event-debug "occ-do-clock-in-curr-ctx-if-not" :cancel
  (if (not (occ-obj-ignore-ctx-p (current-buffer)))
      (progn
        ;; (occ-message "buff: %s" (buffer-name (current-buffer)))
        (occ-debug "%s: occ-do-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug" (time-stamp-string))
        (if force
            (occ-do-clock-in-curr-ctx force)
          (let ((ctx (occ-obj-make-ctx-at-point)))
            (let ((filters             (occ-match-filters))
                  (builder             #'occ-obj-build-ctxual-tsk-with)
                  (auto-select-if-only (occ-obj-clock-in-ctx-auto-select-if-only))
                  (timeout             occ-idle-timeout))
              (let ((ap-normal '(t actions general edit))
                    ;;  ap-transf will reuse ap-normal if nil
                    (ap-transf nil))
                (occ-do-clock-in-if-chg ctx
                                        :filters             filters
                                        :builder             builder
                                        :ap-normal           ap-normal
                                        :ap-transf           ap-transf
                                        :auto-select-if-only auto-select-if-only
                                        :timeout             timeout)))))
        (occ-nodisplay "%s: end occ-do-clock-in-curr-ctx-if-not" (time-stamp-string)))
    (occ-message "Buffer %s not considered"
                 (buffer-name (current-buffer)))))


;;; Timers
;; TODO: Add method/function descriptions

(defun occ-run-curr-ctx-timer ()
  (occ-debug "occ-run-curr-ctx-chg-timer: begin")
  (occ-do-clock-in-curr-ctx nil))

(defun occ-run-curr-ctx-chg-timer ()
  (occ-debug "occ-run-curr-ctx-chg-timer: begin")
  (occ-do-clock-in-curr-ctx-if-not nil))


;; TODO: find some better name
;;;###autoload
(defun occ-do-clock-in-curr-ctx-if-not-timer-function (event)
  (occ-debug "occ-do-clock-in-curr-ctx-if-not-timer-function: begin")
  ;;BUG: could be the cause of high MEM usage
  (unwind-protect
      (if (occ-config-value-quiet)
          (occ-debug "Occ is quiet for some time.")
        (lotus-with-no-recursive-edit-if
            (occ-debug "(recursion-depth) [%d] > 0" (recursion-depth))
          (lotus-with-no-active-minibuffer-if
              (occ-debug "minibuffer active")
            (occ-cancel-timer)
            (if (eq 'buffer-switch
                    event)
                (occ-run-curr-ctx-chg-timer)
              (occ-run-curr-ctx-timer)))))
    ;; to bypass QUIT
    (occ-do-try-clock-schedule-next-timeout nil)))


(defun occ-cancel-timer ()
  (when *occ-buff-sel-timer*
    (cancel-timer *occ-buff-sel-timer*)
    (setq *occ-buff-sel-timer* nil)))

(cl-defmethod occ-obj-try-clock-in-next-timeout ()
  "Get next timeout to try clock-in"
  (occ-debug "begin")
  (let* ((ctx             (occ-obj-make-ctx-at-point))
         (ctxual-curr-tsk (occ-obj-ctxual-current-tsk ctx
                                                      :other-allowed t)))
    (cond
     ((null ctxual-curr-tsk)              3)
     ((occ-obj-unnamed-p ctxual-curr-tsk) (+ *occ-tsk-current-ctx-time-interval*
                                             10))
     (t                                   30))))

(cl-defmethod occ-do-try-clock-schedule-next-timeout (event)
  "Get next timeout to try clock-in"
  (occ-debug "begin")
  (occ-cancel-timer)
  (setq *occ-buff-sel-timer*
        ;; distrubing while editing.
        ;; run-with-timer
        (run-with-idle-plus-timer (occ-obj-try-clock-in-next-timeout)
                                  nil
                                  'occ-do-clock-in-curr-ctx-if-not-timer-function event)))


;;;###autoload
(defun occ-switch-buffer-run-curr-ctx-timer-function (prev
                                                      next)
  (ignore prev next)
  (occ-debug "occ-switch-buffer-run-curr-ctx-timer-function: begin")
  (setq *occ-last-buff-sel-time* (current-time))
  ;; (occ-do-try-clock-schedule-next-timeout 'buffer-switch)
  ;; avoiding direct run as failure will cause removal of switch-buffer-function from post-command-hook
  (run-with-timer nil nil
                  #'occ-do-try-clock-schedule-next-timeout
                  'buffer-switch))


(defvar occ-add-inquery        0)
(defvar occ-add-org-file-timer nil)

(cl-defmethod occ-do-add-org-buffer ((key symbol)
                                     buff)
  (ignore key)
  (occ-debug "occ-do-add-org-buffer: ignoring buff %s" buff))

(cl-defmethod occ-do-add-org-buffer ((key  symbol)
                                     (buff buffer))
  (if (and (buffer-live-p buff)
           (eql buff (current-buffer))())
      (when (< occ-add-inquery 3)
        (let ((file (buffer-file-name buff)))
          (unless (cl-member file
                             (occ-obj-files)
                             :key #'file-truename)
            (when (and file
                       (file-exists-p file)
                       (eq major-mode 'org-mode))
              (make-local-variable 'occ-add-inquery)
              (if (y-or-n-p-with-timeout (format "Do you want to add %s to occ: "
                                                 (file-name-nondirectory file))
                                         10
                                         nil)
                  (progn
                    (setq occ-add-inquery 3)
                    (occ-add-to-spec key file))
                (cl-incf occ-add-inquery)))))))
  (occ-add-org-file-timer key buff))

(defun occ-add-org-buffer (key buff)
  (if (and buff
           (buffer-live-p buff))
      (occ-do-add-org-buffer key buff)
    (occ-warn "occ-add-org-buff: ignoring %s to add to %s" buff key)))

(defun occ-add-org-file-timer (&optional key buffer)
  (ignore key)
  (occ-nodisplay "occ-add-org-file-timer: started for buff %s, occ-add-inquery %s, occ-add-org-file-timer %s"
                 buffer
                 occ-add-inquery
                 occ-add-org-file-timer)
  (let ((key    (occ-collector-default-key))
        (buffer (or buffer
                    (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (make-local-variable 'occ-add-org-file-timer)
        (when occ-add-org-file-timer
          (cancel-timer occ-add-org-file-timer)
          (setq occ-add-org-file-timer nil))
        (when (< occ-add-inquery 3)
          (setq occ-add-org-file-timer
                (run-with-idle-plus-timer (* (1+ occ-add-inquery) 20)
                                          nil
                                          #'occ-add-org-buffer (occ-collector-default-key) buffer)))))))

;;; occ-obj-clock-method.el ends here
