;;; occ-clock.el --- clock                           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

(provide 'occ-clock)


(require 'org-insert-utils) ;; for org-insert-log-note
(eval-when-compile
  (require 'org-misc-utils-lotus))


(require 'occ-obj-ctor)
(require 'occ-helm)
(require 'occ-obj-utils)
(require 'occ-select)
(require 'occ-prop)
(require 'occ-config)
(require 'occ)
(require 'occ-util-common)


(defvar *occ-clocked-ctxual-tsk-ctx-history* nil)
(defvar occ-do-clock-in-hooks nil "Hook to run on clockin with previous and next markers.")
(defvar *occ-update-current-ctx-msg* nil)

(cl-defmethod occ-do-clock-in ((obj null)
                               &key
                               filters
                               builder
                               ap-normal
                               ap-transf
                               timeout)
  (ignore obj)
  (ignore filters)
  (ignore builder)
  (ignore ap-normal)
  (ignore ap-transf)
  (ignore timeout)
  (error "Can not clock in NIL"))

(cl-defmethod occ-do-clock-in ((obj marker)
                               &key
                               filters
                               builder
                               ap-normal
                               ap-transf
                               timeout)
  (ignore filters)
  (ignore builder)
  (ignore ap-normal)
  (ignore ap-transf)
  (ignore timeout)
  ;; (occ-debug "occ-do-clock-in(marker=%s)" obj)
  (let ((org-log-note-clock-out nil))
    (when (marker-buffer obj)
      (with-current-buffer (marker-buffer obj)
        (let ((buffer-read-only nil))
          (condition-case-control err
            (progn
              (occ-straight-org-clock-clock-in (list obj)))
            ((error)
             (signal (cl-first err)
                     (cl-rest err)))))))))

(cl-defmethod occ-do-clock-in ((obj occ-tsk)
                               &key
                               filters
                               builder
                               ap-normal
                               ap-transf
                               timeout)
  (ignore filters)
  (ignore builder)
  (ignore ap-normal)
  (ignore ap-transf)
  (ignore timeout)
  ;; (occ-debug "occ-do-clock-in(occ-tsk=%s)" obj)
  (if (occ-config-clock-in)
      (occ-do-clock-in (occ-obj-marker obj))
    (occ-debug "occ-do-clock-in(obj occ-tsk): clock-in not allowed.")))

(cl-defmethod occ-do-clock-in ((obj occ-ctsk)
                               &key
                               filters
                               builder
                               ap-normal
                               ap-transf
                               timeout)
  ;; (occ-debug "occ-do-clock-in(occ-ctsk=%s)" obj)
  (if (or (occ-obj-unnamed-p    obj)
          (occ-obj-associable-p obj))
      (occ-do-clock-in (occ-ctsk-tsk obj)
                       :filters   filters
                       :builder   builder
                       :ap-normal ap-normal
                       :ap-transf ap-transf
                       :timeout   timeout)
    (occ-debug "occ-do-clock-in(occ-ctxual-tsk): not clocking in (occ-obj-unnamed-p obj)=%s (occ-obj-associable-p obj)=%s"
               (occ-obj-unnamed-p obj)
               (occ-obj-associable-p obj))))

(cl-defmethod occ-do-clock-in ((obj occ-ctxual-tsk)
                               &key
                               filters
                               builder
                               ap-normal
                               ap-transf
                               timeout)
  ;;TODO add org-insert-log-not
  "return "
  ;; (occ-debug "occ-do-clock-in(occ-ctxual-tsk=%s)" obj)
  (let* ((retval         nil)
         (old-ctxual-tsk (cl-first *occ-clocked-ctxual-tsk-ctx-history*))
         (old-tsk        (when old-ctxual-tsk (occ-ctxual-tsk-tsk old-ctxual-tsk)))
         (old-marker     (or (if old-tsk (occ-tsk-marker old-tsk)) org-clock-hd-marker))
         (old-heading    (if old-tsk (occ-tsk-heading old-tsk)))
         (obj-tsk        (occ-ctxual-tsk-tsk obj))
         (obj-ctx        (occ-ctxual-tsk-ctx obj))
         (new-marker     (if obj-tsk (occ-tsk-marker obj-tsk)))
         (new-heading    (if obj-tsk (occ-tsk-heading obj-tsk))))
    (ignore old-marker)
    (ignore obj-ctx)
    (when (and new-marker
               (marker-buffer new-marker))

      (let* ((org-log-note-clock-out nil)
             (old-marker             org-clock-marker)
             (old-buff               (marker-buffer old-marker)))

        (occ-debug "clocking in %s" new-marker)

        (let ((old-buff-read-only (when old-buff
                                    (with-current-buffer (marker-buffer old-marker)
                                      buffer-read-only))))
          (when old-buff
            (with-current-buffer old-buff
              (setq buffer-read-only nil)))

          (setq *occ-update-current-ctx-msg* old-marker)

          (run-hook-with-args 'occ-do-clock-in-hooks
                              old-marker
                              new-marker)

          (when (and new-heading
                     old-marker
                     (marker-buffer old-marker))
            (org-insert-log-note old-marker (format "clocking out to clockin to <%s>" new-heading)))
          (when old-heading
            (org-insert-log-note new-marker (format "clocking in to here from last clock <%s>" old-heading)))

          (if (or (occ-obj-unnamed-p obj)
                  (occ-obj-associable-p obj))
              (occ-do-clock-in obj-tsk
                               :filters   filters
                               :builder   builder
                               :ap-normal ap-normal
                               :ap-transf ap-transf
                               :timeout   timeout)
            (occ-debug "occ-do-clock-in(occ-ctxual-tsk): not clocking in (occ-obj-unnamed-p obj)=%s (occ-obj-associable-p obj)=%s"
                       (occ-obj-unnamed-p obj)
                       (occ-obj-associable-p obj)))

          (setq retval t)

          (push obj
                *occ-clocked-ctxual-tsk-ctx-history*)

          ;; check if current task is proper.
          ;;; if above (occ-do-clock-in obj-tsk) clocked in properly.
          (occ-assert (occ-obj-marker= obj org-clock-marker))
          (occ-current-tsk)

          (when old-buff
            (with-current-buffer old-buff
              (setq buffer-read-only old-buff-read-only)))
          retval)))))


(cl-defmethod occ-obj-ignore-p ((buff buffer))
  (ignore buff)
  nil)

(cl-defmethod occ-obj-ignore-p ((obj occ-ctx))
  (let ((buff (occ-ctx-buffer obj)))
    (and (occ-chgable-p)
         buff
         (buffer-live-p buff)
         (not (minibufferp buff))
         (not (occ-obj-ignore-p buff)))))


(cl-defmethod occ-obj-clockable-p ((obj occ-ctx))
  (let ((buff (occ-ctx-buffer obj)))
    (and (occ-chgable-p)
         buff
         (buffer-live-p buff)
         (not (minibufferp buff))
         (not (occ-obj-ignore-p buff)))))


;; THIS is the main method
(cl-defmethod occ-do-clock-in ((obj occ-ctx)
                               &key
                               filters
                               builder
                               return-transform
                               ap-normal
                               ap-transf
                               auto-select-if-only
                               timeout)
  "Clock-in selected CTXUAL-TSK for occ-ctx OBJ or open interface
for adding properties to heading."
  (ignore ap-normal)
  (ignore ap-transf)
  (unless builder (error "Builder can not be nil"))
  ;; (occ-debug "occ-do-clock-in(occ-ctx=%s)" obj)
  (if (occ-obj-clockable-p obj)
      (let ((filters   (or filters (occ-match-filters)))
            (builder   (or builder #'occ-obj-build-ctxual-tsk-with))
            (ap-normal '(t actions general checkout))
            (ap-transf '(t actions general edit checkout))
            (timeout   (or timeout occ-idle-timeout)))
        (occ-debug "occ-do-clock-in((obj occ-ctx)): begin")
        (let ((returned-ctxual-tsk (occ-obj-select obj ;TODO: if only one match then where it is selecting that.
                                                   (occ-collections-all)
                                                   :filters             filters
                                                   :builder             builder
                                                   :return-transform    t ;Here I know return value is going to be used, so passing t
                                                   :ap-normal           ap-normal ;as return value is going to be used.
                                                   :ap-transf           ap-transf
                                                   :auto-select-if-only auto-select-if-only
                                                   :timeout             timeout)))
          ;; (occ-debug "occ-do-clock-in((obj occ-ctx)): selected  returned-ctxual-tsk=%s ret-label=%s value=%s"
          ;;                   (occ-name returned-ctxual-tsk)
          ;;                   (occ-obj-return-in-labels-p returned-ctxual-tsk occ-return-select-label)
          ;;                   (occ-obj-format (occ-obj-obj returned-ctxual-tsk)))
          (if (occ-obj-return-in-labels-p returned-ctxual-tsk ;TODO: should return t if action were done than select[=identity] ;; occ-return-label
                                          occ-return-select-label)
              (let ((ctxual-tsk (occ-obj-obj returned-ctxual-tsk)))
                (prog1
                    (when return-transform ;Here caller know if return value is going to be used.
                      (occ-obj-make-return occ-return-true-label nil))
                  (if (occ-ctxual-tsk-p ctxual-tsk)
                      (occ-do-clock-in ctxual-tsk
                                       :filters   filters
                                       :builder   builder
                                       :ap-normal ap-normal
                                       :ap-transf ap-transf
                                       :timeout   timeout)
                    (occ-debug "%s is not ctxual-tsk" (occ-obj-format ctxual-tsk 'capitalize)))))
            (progn
              ;; here create unnamed tsk, no need
              (setq *occ-update-current-ctx-msg* "null clock")
              ;; (occ-delayed-select-obj-prop-edit-when-idle obj obj occ-idle-timeout)
              (occ-debug "No clock found please set a match for this ctx %s, add it using M-x occ-prop-edit-safe."
                         obj)
              (occ-debug "occ-do-clock-in(ctx):  with this-command=%s" this-command)
              ;; (occ-debug "occ-do-clock-in: Edit properties of a tsk %s to make associable to current context."
              ;;              (occ-obj-Format obj))
              (occ-obj-safe-ignore-quit-properties-window-editor obj
                                                                 :filters          (occ-list-filters)
                                                                 :builder          #'occ-obj-build-ctsk-with
                                                                 :return-transform return-transform ;Here caller know if return value is going to be used.
                                                                 :ap-normal        ap-normal
                                                                 :ap-transf        ap-transf
                                                                 :timeout          timeout)))))
    (occ-debug "ctx %s is not clockable." (occ-name obj))))


(cl-defmethod occ-do-clock-in-if-associable ((obj occ-obj-ctx-tsk)
                                             &key
                                             filters
                                             builder
                                             ap-normal
                                             ap-transf
                                             timeout)
  (let ((ctxtual-tsk (occ-obj-build-ctxual-tsk obj)))
    (when ctxtual-tsk
      (occ-do-clock-in ctxtual-tsk
                       :filters   filters
                       :builder   builder
                       :ap-normal ap-normal
                       :ap-transf ap-transf
                       :timeout   timeout))))


(cl-defmethod occ-do-try-clock-in ((obj marker)
                                   &key
                                   filters
                                   builder
                                   ap-normal
                                   ap-transf
                                   timeout)
  (occ-do-clock-in obj
                   :filters   filters
                   :builder   builder
                   :ap-normal ap-normal
                   :ap-transf ap-transf
                   :timeout   timeout))

(cl-defmethod occ-do-try-clock-in ((obj occ-obj-tsk)
                                   &key
                                   filters
                                   builder
                                   ap-normal
                                   ap-transf
                                   timeout)
  (occ-do-clock-in obj
                   :filters   filters
                   :builder   builder
                   :ap-normal ap-normal
                   :ap-transf ap-transf
                   :timeout   timeout))

(cl-defmethod occ-do-try-clock-in ((obj occ-ctsk)
                                   &key
                                   filters
                                   builder
                                   ap-normal
                                   ap-transf
                                   timeout)
  (let ((tsk         (occ-obj-tsk obj))
        (ctx         (occ-obj-ctx obj))
        (ctxtual-tsk (occ-obj-build-ctxual-tsk obj)))
    (when (occ-obj-try-if-unassociated-p ctxtual-tsk)
      (unless (occ-do-clock-in-if-associable ctxtual-tsk
                                             :filters   filters
                                             :builder   builder
                                             :ap-normal ap-normal
                                             :ap-transf ap-transf
                                             :timeout   timeout)
        (occ-debug "%s is not associable with %s not clocking-in."
                   (occ-obj-Format tsk)
                   (occ-obj-Format ctx))))))



(cl-defmethod occ-do-try-clock-in ((obj null)
                                   &key
                                   filters
                                   builder
                                   ap-normal
                                   ap-transf
                                   timeout)
  (occ-do-clock-in obj
                   :filters   filters
                   :builder   builder
                   :ap-normal ap-normal
                   :ap-transf ap-transf
                   :timeout   timeout))


(cl-defmethod occ-do-try-fast-clock-in ((obj marker)
                                        &key
                                        filters
                                        builder
                                        ap-normal
                                        ap-transf
                                        timeout)
  (occ-do-clock-in obj
                   :filters   filters
                   :builder   builder
                   :ap-normal ap-normal
                   :ap-transf ap-transf
                   :timeout   timeout))

(cl-defmethod occ-do-try-fast-clock-in ((obj occ-ctsk)
                                        &key
                                        filters
                                        builder
                                        ap-normal
                                        ap-transf
                                        timeout)
  (let ((tsk         (occ-obj-tsk obj))
        (ctx         (occ-obj-ctx obj))
        (ctxtual-tsk (occ-obj-build-ctxual-tsk obj)))
    (when (occ-obj-try-if-unassociated-p ctxtual-tsk)
      (unless (occ-do-clock-in-if-associable ctxtual-tsk
                                             :filters   filters
                                             :builder   builder
                                             :ap-normal ap-normal
                                             :ap-transf ap-transf
                                             :timeout   timeout)
        (occ-debug "%s is not associable with %s not clocking-in."
                   (occ-obj-Format tsk)
                   (occ-obj-Format ctx))))))

;;; occ-clock.el ends here
