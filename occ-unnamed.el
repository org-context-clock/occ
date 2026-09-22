;;; occ-unnamed.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-unnamed)


(require 'org-clock-utils-lotus)
(require 'org-clock-unnamed-task)


(require 'occ)
(require 'occ-obj)
(require 'occ-obj-ctor)
(eval-when-compile
  (require 'occ-macros))
(require 'occ-assert)
(require 'occ-clock)
(require 'occ-cl-utils)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-util-common)


(defvar +occ-unnamed-collection-depth+ 0)
(defvar +occ-unnamed-collection-limit+ 3)
(defvar *occ-unassociate-ctx-start-time*         nil)
(defvar *occ-swapen-unnamed-threashold-interval* (* 60 2)) ;2 mins
(defvar *occ-collector-unnamed-key* 'unnamed)

(defun occ-unassociate-ctx-start-time-reset ()
  (setq *occ-unassociate-ctx-start-time* nil))

(defun occ-can-create-unnamed-tsk-p ()
  (occ-debug "occ-can-create-unnamed-tsk-p: begin")
  (unless *occ-unassociate-ctx-start-time*
    (setq *occ-unassociate-ctx-start-time* (current-time)))
  (let ((unassociate-ctx-start-time *occ-unassociate-ctx-start-time*))
    (prog1
        (> (float-time (time-since unassociate-ctx-start-time))
           *occ-swapen-unnamed-threashold-interval*))))

(defun occ-clock-marker-unnamed-p (marker)
  (occ-debug "occ-clock-marker-is-unnamed-p: begin")
  (org-clock-marker-unnamed-p marker))

(defun occ-clock-marker-unnamed-clock-p (&optional clock)
  (let ((clock (or clock
                   org-clock-marker)))
    (occ-clock-marker-unnamed-p clock)))

;; defiend in occ-predicate.el
;; (defmethod occ-unnamed-p ((obj occ-tsk))
;;   (let ((mrk (occ-obj-marker (occ-obj-tsk obj))))
;;     (occ-clock-marker-unnamed-p mrk)))


(defun occ-maybe-create-clockedin-unnamed-heading ()
  (occ-debug "occ-maybe-create-clockedin-unnamed-heading: begin")
  (when (occ-can-create-unnamed-tsk-p)
    (let ((org-log-note-clock-out nil))
      (if (occ-clock-marker-unnamed-clock-p)
          (occ-debug "occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk")
        (prog1
            (org-without-org-clock-persist
              (lotus-org-create-unnamed-task-task-clock-in))
          (occ-unassociate-ctx-start-time-reset))))))

(defun occ-maybe-create-unnamed-heading ()
  (occ-debug "occ-maybe-create-unnamed-heading: begin")
  (when (occ-can-create-unnamed-tsk-p)
    (let ((org-log-note-clock-out nil))
      (if (occ-clock-marker-unnamed-clock-p)
          (occ-debug "occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk")
        (cl-rest (org-without-org-clock-persist
                (lotus-org-create-unnamed-task)))))))

(defun occ-maybe-create-unnamed-tsk ()
  ;; back
  (occ-debug "occ-maybe-create-unnamed-tsk: begin")
  (let ((unnamed-collection (occ-unnamed-collection)))
    (if unnamed-collection
        (let ((unnamed-heading-marker (cl-rest (org-without-org-clock-persist
                                                 (lotus-org-create-unnamed-task)))))
          (if unnamed-heading-marker
              (occ-obj-make-tsk-with unnamed-heading-marker
                                     (occ-collector-get *occ-collector-unnamed-key*))
            (occ-error "Failed o create unnamed task")))
      (occ-error "Unable to get unnamed collection."))))

(defun occ-build-unnamed-collection (&optional force-error)
  (let ((type (occ-collector-spec (occ-collector-default-key)))
        (file (lotus-org-unnamed-task-file)))
    (if (and type
             file)
        (progn
          (when (occ-collector-get *occ-collector-unnamed-key*)
            (unless (eq (cl-first (occ-collector-roots *occ-collector-unnamed-key*))
                        (lotus-org-unnamed-task-file))
              (occ-collector-remove *occ-collector-unnamed-key*)))
          (occ-collector-get-create *occ-collector-unnamed-key*
                                    "Unnamed"
                                    type
                                    (list file)
                                    :depth +occ-unnamed-collection-depth+
                                    :limit +occ-unnamed-collection-limit+))
        (if force-error
            (occ-error "error with type %s, file %s" type file)
          (occ-warn "error with type %s, file %s" type file)))))

(defun occ-unnamed-collection ()
  (unless (and (occ-collector-get *occ-collector-unnamed-key*)
               (eq (cl-first (occ-collector-roots *occ-collector-unnamed-key*))
                   (lotus-org-unnamed-task-file)))
    (occ-build-unnamed-collection))
  (occ-collector-get *occ-collector-unnamed-key*))

;;;###autoload
(defun occ-unnamed-initialize ()
  (interactive)
  (occ-unnamed-collection))

(occ-testing
 (ignore (occ-cl-inst-classname (occ-obj-make-tsk org-clock-hd-marker)))
 (let ((unnamed-test nil))
   (ignore (setq unnamed-test (occ-obj-make-tsk org-clock-hd-marker)))
   (ignore unnamed-test)
   (ignore (occ-tsk-marker unnamed-test))
   unnamed-test)
 (type-of (lotus-org-unnamed-task-clock-marker)))

(cl-defmethod occ-do-maybe-create-unnamed-ctxual-tsk ((ctx occ-ctx))
  ;; back
  (occ-debug "occ-do-maybe-create-unnamed-ctxual-tsk: begin")
  (let* ((unnamed-tsk        (occ-maybe-create-unnamed-tsk))
         (unnamed-ctxual-tsk (when unnamed-tsk
                               (occ-obj-build-ctxual-tsk-with unnamed-tsk
                                                              ctx))))
    (occ-assert unnamed-tsk)
    (occ-assert unnamed-ctxual-tsk)
    unnamed-ctxual-tsk))

(cl-defmethod occ-do-maybe-create-clockedin-unnamed-ctxual-tsk ((ctx occ-ctx))
  ;; back
  (occ-debug "occ-do-maybe-create-clockedin-unnamed-ctxual-tsk: begin")
  (when (occ-can-create-unnamed-tsk-p)
    (let ((org-log-note-clock-out nil))
      (if (occ-clock-marker-unnamed-clock-p)
          (occ-debug "occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk")
        (let* ((unnamed-ctxual-tsk (occ-do-maybe-create-unnamed-ctxual-tsk ctx))
               (unnamed-tsk        (when unnamed-ctxual-tsk
                                     (occ-ctxual-tsk-tsk unnamed-ctxual-tsk)))
               (unnamed-marker     (when unnamed-tsk
                                     (occ-tsk-marker unnamed-tsk))))
          (occ-assert unnamed-ctxual-tsk)
          (occ-assert unnamed-tsk)
          (if unnamed-marker
              (prog1
                  (occ-do-clock-in unnamed-ctxual-tsk)
                ;; id:x11 make org-ctx-clock version
                (lotus-org-unnamed-task-clock-marker unnamed-marker)
                (occ-debug "clockin to unnnamed tsk.")
                (occ-unassociate-ctx-start-time-reset))
              (occ-error "unnamed-marker is nil")))))))

;;; occ-unnamed.el ends here
