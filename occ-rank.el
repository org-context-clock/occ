;;; occ-rank.el --- occ rank                         -*- lexical-binding: t; -*-

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

(provide 'occ-rank)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-util-common)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-prop-base)

;; TODO: graded ranking where ranking will be under priority of properties, where one can not go beyond above one, normally

(defvar occ-rank-max-range 100)
(defvar occ-rank-quanta 1)


(defun occ-rank-percentage (num)
  num)


(cl-defgeneric occ-obj-calculate-rank (tsk
                                       ctx
                                       properties)
  "occ-obj-rank")


(cl-defmethod occ-obj-calculate-rank ((tsk occ-obj-tsk)
                                      (ctx occ-obj-ctx)
                                      (properties list))
  (/ (cl-reduce #'+
                (mapcar #'(lambda (prop)
                            (occ-obj-prop-rank-with tsk
                                                    ctx
                                                    prop)) ;;(downcase-sym prop)
                        properties))
     occ-rank-quanta))

(cl-defmethod occ-obj-calculate-rank ((tsk occ-obj-tsk)
                                      (ctx null)
                                      (properties list))
  (/ (cl-reduce #'+
                (mapcar #'(lambda (prop)
                            (occ-obj-prop-rank-with tsk
                                                    ctx
                                                    prop)) ;;(downcase-sym prop)
                        properties))
     occ-rank-quanta))


(cl-defmethod occ-obj-prop-rank ((obj  occ-ranktbl)
                                 (prop symbol))
  (let ((rplist (occ-ranktbl-plist obj)))
    (plist-get rplist
               prop)))
(cl-defmethod occ-obj-rank-inheritable ((obj occ-ranktbl))
  (occ-ranktbl-inheritable obj))
(cl-defmethod occ-obj-rank-nonheritable ((obj occ-ranktbl))
  (occ-ranktbl-nonheritable obj))
(cl-defmethod occ-obj-rank-max-decendent ((obj occ-ranktbl))
  (occ-ranktbl-max-decendent obj))
(cl-defmethod occ-obj-rank-acquired ((obj occ-ranktbl))
  (+ (occ-obj-rank-inheritable obj)
     (occ-obj-rank-nonheritable obj)))
(cl-defmethod occ-obj-rank ((obj occ-ranktbl))
  (occ-ranktbl-value obj))


(cl-defmethod occ-obj-reset-prop-rank ((obj  occ-ranktbl)
                                       (prop symbol))
  (let ((rplist (occ-ranktbl-plist obj)))
    (setf (occ-ranktbl-plist obj)
          (plist-put rplist prop nil))))
(cl-defmethod occ-obj-reset-rank-inheritable ((obj occ-ranktbl))
  (setf (occ-ranktbl-inheritable obj) nil))
(cl-defmethod occ-obj-reset-nonheritable-rank ((obj occ-ranktbl))
  (setf (occ-ranktbl-nonheritable obj) nil))
(cl-defmethod occ-obj-reset-rank-max-decendent ((obj occ-ranktbl))
  (setf (occ-ranktbl-max-decendent obj) nil))
(cl-defmethod occ-obj-reset-rank-acquired ((obj occ-ranktbl))
  (occ-error "Error"))
(cl-defmethod occ-obj-reset-rank ((obj occ-ranktbl))
  (setf (occ-ranktbl-value obj) nil))

(cl-defmethod (setf occ-obj-prop-rank) ((rank number)
                                        (obj  occ-ranktbl)
                                        (prop symbol))
  (let ((rplist (occ-ranktbl-plist obj)))
    (setf (occ-ranktbl-plist obj)
          (plist-put rplist prop rank))))
(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
                                               (obj occ-ranktbl))
  (setf (occ-ranktbl-inheritable obj) rank))
(cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number)
                                                (obj occ-ranktbl))
  (setf (occ-ranktbl-nonheritable obj) rank))
(cl-defmethod (setf occ-obj-rank-max-decendent) ((rank number)
                                                 (obj occ-ranktbl))
  (setf (occ-ranktbl-max-decendent obj) rank))
(cl-defmethod (setf occ-obj-rank-acquired) ((rank number)
                                            (obj occ-ranktbl))
  (occ-error "Error"))
(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-ranktbl))
  (setf (occ-ranktbl-value obj) rank))


(cl-defmethod occ-obj-prop-rank-with ((tsk  occ-obj-tsk)
                                      (ctx  occ-obj-ctx)
                                      (property symbol))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (unless (occ-obj-prop-rank rt
                               property)
      (setf (occ-obj-prop-rank rt
                               property)
            (occ-obj-priority-rank tsk
                                   ctx
                                   property)))
    (occ-obj-prop-rank rt
                       property)))

(cl-defmethod occ-obj-prop-rank-with ((tsk  occ-obj-tsk)
                                      (ctx  null)
                                      (property symbol))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (unless (occ-obj-prop-rank rt
                               property)
      (setf (occ-obj-prop-rank rt
                               property)
            (occ-obj-priority-rank tsk
                                   ctx
                                   property)))
    (occ-obj-prop-rank rt
                       property)))


(cl-defmethod occ-obj-reset-prop-rank-with ((tsk  occ-obj-tsk)
                                            (ctx  occ-obj-ctx)
                                            (property symbol))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (if (occ-obj-inheritable-p property)
        (occ-obj-reset-rank-inheritable-with tsk
                                             ctx)
      (occ-obj-reset-rank-nonheritable-with tsk
                                            ctx))
    (occ-obj-reset-prop-rank rt
                             property)))


(cl-defmethod occ-obj-reset-prop-rank-with ((tsk  occ-obj-tsk)
                                            (ctx  null)
                                            (property symbol))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (if (occ-obj-inheritable-p property)
        (occ-obj-reset-rank-inheritable-with tsk
                                             ctx)
      (occ-obj-reset-rank-nonheritable-with tsk
                                            ctx))
    (occ-obj-reset-prop-rank rt
                             property)))

(cl-defmethod (setf occ-obj-prop-rank-with) ((rank number)
                                             (tsk  occ-obj-tsk)
                                             (ctx  occ-obj-ctx)
                                             (property symbol))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-prop-rank rt
                             property) rank)))


(cl-defmethod (setf occ-obj-prop-rank-with) ((rank number)
                                             (tsk  occ-obj-tsk)
                                             (ctx  null)
                                             (property symbol))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-prop-rank rt
                             property) rank)))


(cl-defmethod occ-obj-prop-rank ((obj  occ-obj-tsk)
                                 (property symbol))
  (occ-obj-prop-rank-with (occ-obj-tsk obj)
                          (occ-obj-ctx obj)
                          property))
(cl-defmethod occ-obj-reset-prop-rank ((obj  occ-obj-tsk)
                                       (property symbol))
  (occ-obj-reset-prop-rank-with (occ-obj-tsk obj)
                                (occ-obj-ctx obj)
                                property))
(cl-defmethod (setf occ-obj-prop-rank) ((rank number)
                                        (obj  occ-obj-tsk)
                                        (property symbol))
  (setf (occ-obj-prop-rank-with (occ-obj-tsk obj)
                                (occ-obj-ctx obj)
                                property) rank))


(cl-defmethod occ-obj-ancestor-rank-with ((tsk null)
                                          (ctx occ-obj-ctx)
                                          (height number))
  0)

(cl-defmethod occ-obj-ancestor-rank-with ((tsk occ-obj-tsk)
                                          (ctx occ-obj-ctx)
                                          (height number))
  ;; TODO: sibling-count update parent with max child rank value
  (+ (occ-obj-rank-inheritable-with tsk
                                    ctx)
     (occ-obj-ancestor-rank-with (occ-tsk-parent tsk)
                                 ctx
                                 height)))
(cl-defmethod occ-obj-ancestor-rank-with ((tsk occ-obj-tsk)
                                          (ctx null)
                                          (height number))
  (+ (occ-obj-rank-inheritable-with tsk
                                    ctx)
     (occ-obj-ancestor-rank-with (occ-tsk-parent tsk)
                                 ctx
                                 height)))
(cl-defmethod occ-obj-ancestor-rank-with ((tsk null)
                                          (ctx null)
                                          (height number))
  0)


(cl-defmethod occ-obj-rank-inheritable-with ((tsk occ-obj-tsk)
                                             ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
   (unless (occ-obj-rank-inheritable rt)
     (let ((rank (occ-obj-calculate-rank tsk
                                         ctx
                                         (occ-obj-inheritable (occ-obj-properties-to-calculate-rank tsk
                                                                                                    ctx)))))
       (setf (occ-obj-rank-inheritable rt) rank)))
   (occ-obj-rank-inheritable rt)))
(cl-defmethod occ-obj-rank-nonheritable-with ((tsk occ-obj-tsk)
                                              ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
   (unless (occ-obj-rank-nonheritable rt)
     (let ((rank (occ-obj-calculate-rank tsk
                                         ctx
                                         (occ-obj-nonheritable (occ-obj-properties-to-calculate-rank tsk
                                                                                                     ctx)))))
       (setf (occ-obj-rank-nonheritable rt) rank)))
   (occ-obj-rank-nonheritable rt)))
(cl-defmethod occ-obj-rank-max-decendent-with ((tsk occ-obj-tsk)
                                               (ctx occ-obj-ctx))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (unless (occ-obj-rank-max-decendent rt)
      (setf (occ-obj-rank-max-decendent rt)
            (apply #'max
                   (occ-obj-rank-with tsk ctx)
                   (mapcar #'(lambda (xtsk) (occ-obj-rank-max-decendent-with xtsk ctx))
                           (occ-tree-tsk-subtree tsk)))))
    (occ-assert (occ-obj-rank-max-decendent rt))
    (occ-obj-rank-max-decendent rt)))
(cl-defmethod occ-obj-rank-acquired-with ((tsk occ-obj-tsk)
                                          ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (+ (/ (occ-obj-rank-inheritable-with tsk
                                         ctx)
          (occ-tsk-descendant-weight tsk))
       (occ-obj-rank-nonheritable-with tsk
                                       ctx))))
(cl-defmethod occ-obj-rank-with ((tsk occ-obj-tsk)
                                 ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (unless (occ-obj-rank rt)
      (let ((rank (+ (occ-obj-rank-acquired-with tsk
                                                 ctx)
                     (/ (occ-obj-ancestor-rank-with (occ-tsk-parent tsk)
                                                    ctx
                                                    0)
                        (occ-tsk-descendant-weight tsk)))))
        (setf (occ-obj-rank rt) rank)))
    (occ-obj-rank rt)))

(cl-defmethod occ-obj-reset-rank-inheritable-with ((tsk occ-obj-tsk)
                                                   ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-inheritable rt)
    (occ-obj-reset-rank-acquired-with tsk ctx)
    (occ-obj-tsk-do-descendant-with tsk
                                    ctx
                                    #'occ-obj-reset-rank-with)))
(cl-defmethod occ-obj-reset-rank-nonheritable-with ((tsk occ-obj-tsk)
                                                    ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-nonheritable rt)))
(cl-defmethod occ-obj-reset-rank-max-decendent-with ((tsk occ-obj-tsk)
                                                     (ctx occ-obj-ctx))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-max-decendent rt)))
(cl-defmethod occ-obj-reset-rank-acquired-with ((tsk occ-obj-tsk)
                                                ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-with tsk ctx)
    (when nil
      (occ-obj-reset-rank-acquired rt))))
(cl-defmethod occ-obj-reset-rank-with ((tsk occ-obj-tsk)
                                       ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-max-decendent-with tsk
                                           ctx)
    (occ-obj-tsk-do-ancestor-with tsk
                                  ctx
                                  #'occ-obj-reset-rank-max-decendent-with)
    (occ-obj-reset-rank rt)))

(cl-defmethod occ-obj-tsk-do-ancestor-with ((tsk occ-obj-tsk)
                                            ctx
                                            fun)
  (funcall fun tsk ctx)
  (if (occ-tsk-parent tsk)
      (occ-obj-tsk-do-ancestor-with (occ-tsk-parent tsk)
                                    ctx
                                    fun)))

(cl-defmethod occ-obj-tsk-do-descendant-with ((tsk occ-obj-tsk)
                                              ctx
                                              fun)
  (dolist (c (occ-tree-tsk-subtree tsk))
    (occ-obj-tsk-do-descendant-with c ctx fun))
  (funcall fun tsk ctx))

(cl-defmethod (setf occ-obj-rank-inheritable-with) ((rank number)
                                                    (tsk occ-obj-tsk)
                                                    ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-inheritable rt) rank)))
(cl-defmethod (setf occ-obj-rank-nonheritable-with) ((rank number)
                                                     (tsk occ-obj-tsk)
                                                     ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-nonheritable rt) rank)))
(cl-defmethod (setf occ-obj-rank-max-decendent-with) ((rank number)
                                                      (tsk occ-obj-tsk)
                                                      (ctx occ-obj-ctx))
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-max-decendent rt) rank)))
(cl-defmethod (setf occ-obj-rank-acquired-with) ((rank number)
                                                 (tsk occ-obj-tsk)
                                                 ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-acquired rt) rank)))
(cl-defmethod (setf occ-obj-rank-with) ((rank number)
                                        (tsk occ-obj-tsk)
                                        ctx)
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank rt) rank)))


(cl-defmethod occ-obj-rank-inheritable ((obj occ-obj-tsk))
  (occ-obj-rank-inheritable-with (occ-obj-tsk obj)
                                 (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank-nonheritable ((obj occ-obj-tsk))
  (occ-obj-rank-nonheritable-with (occ-obj-tsk obj)
                                  (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank-max-decendent ((obj occ-obj-tsk))
  (occ-obj-rank-max-decendent-with (occ-obj-tsk obj)
                                   (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank-acquired ((obj occ-obj-tsk))
  (occ-obj-rank-acquired-with (occ-obj-tsk obj)
                              (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank ((obj occ-obj-tsk))
  (occ-obj-rank-with (occ-obj-tsk obj)
                     (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank ((obj occ-ctxual-tsk))
  (occ-obj-rank-with (occ-obj-tsk obj)
                     (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank ((obj occ-ctsk))
  (occ-obj-rank-with (occ-obj-tsk obj)
                     nil))

(cl-defmethod occ-obj-reset-rank-inheritable ((obj occ-obj-tsk))
  (occ-obj-reset-rank-inheritable-with (occ-obj-tsk obj)
                                       (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank-nonheritable ((obj occ-obj-tsk))
  (occ-obj-reset-rank-nonheritable-with (occ-obj-tsk obj)
                                        (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank-max-decendent ((obj occ-obj-tsk))
  (occ-obj-reset-rank-max-decendent-with (occ-obj-tsk obj)
                                         (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank-acquired ((obj occ-obj-tsk))
  (occ-obj-reset-rank-acquired-with (occ-obj-tsk obj)
                                    (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank ((obj occ-obj-tsk))
  (occ-obj-reset-rank-with (occ-obj-tsk obj)
                           (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank ((obj occ-ctsk))
  (occ-obj-reset-rank-with (occ-obj-tsk obj)
                           nil))

(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
                                               (obj occ-obj-tsk))
  (setf (occ-obj-rank-inheritable-with (occ-obj-tsk obj)
                                       (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number)
                                                (obj occ-obj-tsk))
  (setf (occ-obj-rank-nonheritable-with (occ-obj-tsk obj)
                                        (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank-max-decendent) ((rank number)
                                                 (obj occ-obj-tsk))
  (setf (occ-obj-rank-max-decendent-with (occ-obj-tsk obj)
                                         (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank-acquired) ((rank number)
                                            (obj occ-obj-tsk))
  (setf (occ-obj-rank-acquired-with (occ-obj-tsk obj)
                                    (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-obj-tsk))
  (setf (occ-obj-rank-with (occ-obj-tsk obj)
                           (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-ctsk))
  (setf (occ-obj-rank-with (occ-obj-tsk obj)
                           nil)
        rank))


(cl-defmethod occ-obj-calculate-avgrank ((obj occ-ctx))
  (let* ((objs      (occ-obj-list obj
                                  :builder #'occ-obj-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         ;; BUG
         (avgrank   (occ-calculate-average rankslist)))
    avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-ctx))
  (occ-debug "occ-obj-calculate-varirank(occ-ctx=%s)"
             (occ-obj-format obj))
  (let* ((objs      (occ-obj-list obj
                                  :builder #'occ-obj-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         ;; BUG
         (varirank  (occ-calculate-variance rankslist)))
    varirank))


(cl-defmethod occ-obj-calculate-avgrank ((obj occ-collection))
  (let* ((objs      (occ-obj-list obj))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         (avgrank   (occ-calculate-average rankslist)))
       avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-collection))
  (let* ((objs      (occ-obj-list obj))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         (varirank  (occ-calculate-variance rankslist)))
      varirank))


;; (occ-obj-avgrank (occ-default-collection))
;; (occ-obj-varirank (occ-default-collection))

;;; occ-rank.el ends here
