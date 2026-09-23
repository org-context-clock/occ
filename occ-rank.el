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

;; occ-rank.el computes and caches context-dependent task ranks for the
;; filtering & ranking layer.  Ranks are cached per (task, context) in an
;; `occ-ranktbl' and filled lazily by the `-with' generics:
;; `occ-obj-prop-rank-with' (per-property rank),
;; `occ-obj-rank-inheritable-with' and `occ-obj-rank-nonheritable-with'
;; (sums over the inheritable / non-inheritable property sets),
;; `occ-obj-rank-acquired-with' and `occ-obj-rank-with' (acquired rank
;; plus inherited ancestor rank, damped by `occ-tsk-descendant-weight')
;; and `occ-obj-rank-max-decendent-with' (the subtree maximum, the default
;; display rank).  Inheritable matches propagate up the task tree via
;; `occ-obj-ancestor-rank-with' and the `occ-obj-tsk-do-ancestor-with' /
;; `occ-obj-tsk-do-descendant-with' walkers.  Every rank has reset and
;; `setf' counterparts (`occ-obj-reset-prop-rank-with' and friends) so
;; property writes invalidate the caches, and `occ-obj-calculate-avgrank'
;; / `occ-obj-calculate-varirank' give rank statistics over a context or
;; collection.
;;
;; See doc/occ-design.org for the full design.

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
  "Return NUM unchanged; percentage scaling is not implemented."
  num)


(cl-defgeneric occ-obj-calculate-rank (tsk
                                       ctx
                                       properties)
  "occ-obj-rank")


(cl-defmethod occ-obj-calculate-rank ((tsk occ-obj-tsk)
                                      (ctx occ-obj-ctx)
                                      (properties list))
  "Rank calculation method on occ-obj-tsk and occ-obj-ctx objects.
Return the sum of the property ranks of TSK for CTX over
PROPERTIES divided by occ-rank-quanta."
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
  "Rank calculation method on occ-obj-tsk with a null CTX.
Return the sum of the property ranks of TSK over PROPERTIES
divided by occ-rank-quanta."
  (/ (cl-reduce #'+
                (mapcar #'(lambda (prop)
                            (occ-obj-prop-rank-with tsk
                                                    ctx
                                                    prop)) ;;(downcase-sym prop)
                        properties))
     occ-rank-quanta))


(cl-defmethod occ-obj-prop-rank ((obj  occ-ranktbl)
                                 (prop symbol))
  "Rank accessor method on occ-ranktbl objects.
Return the cached rank of PROP from the plist of OBJ or nil
when PROP has no cached rank."
  (let ((rplist (occ-ranktbl-plist obj)))
    (plist-get rplist
               prop)))
(cl-defmethod occ-obj-rank-inheritable ((obj occ-ranktbl))
  "Rank accessor method on occ-ranktbl objects.
Return the cached inheritable rank of OBJ or nil."
  (occ-ranktbl-inheritable obj))
(cl-defmethod occ-obj-rank-nonheritable ((obj occ-ranktbl))
  "Rank accessor method on occ-ranktbl objects.
Return the cached nonheritable rank of OBJ or nil."
  (occ-ranktbl-nonheritable obj))
(cl-defmethod occ-obj-rank-max-decendent ((obj occ-ranktbl))
  "Rank accessor method on occ-ranktbl objects.
Return the cached subtree maximum rank of OBJ or nil."
  (occ-ranktbl-max-decendent obj))
(cl-defmethod occ-obj-rank-acquired ((obj occ-ranktbl))
  "Rank accessor method on occ-ranktbl objects.
Return the sum of the inheritable and nonheritable ranks of
OBJ."
  (+ (occ-obj-rank-inheritable obj)
     (occ-obj-rank-nonheritable obj)))
(cl-defmethod occ-obj-rank ((obj occ-ranktbl))
  "Rank accessor method on occ-ranktbl objects.
Return the cached total rank value of OBJ or nil."
  (occ-ranktbl-value obj))


(cl-defmethod occ-obj-reset-prop-rank ((obj  occ-ranktbl)
                                       (prop symbol))
  "Rank reset method on occ-ranktbl objects.
Clear the cached rank of PROP in the plist of OBJ."
  (let ((rplist (occ-ranktbl-plist obj)))
    (setf (occ-ranktbl-plist obj)
          (plist-put rplist prop nil))))
(cl-defmethod occ-obj-reset-rank-inheritable ((obj occ-ranktbl))
  "Rank reset method on occ-ranktbl objects.
Clear the cached inheritable rank of OBJ."
  (setf (occ-ranktbl-inheritable obj) nil))
(cl-defmethod occ-obj-reset-nonheritable-rank ((obj occ-ranktbl))
  "Rank reset method on occ-ranktbl objects.
Clear the cached nonheritable rank of OBJ."
  (setf (occ-ranktbl-nonheritable obj) nil))
(cl-defmethod occ-obj-reset-rank-max-decendent ((obj occ-ranktbl))
  "Rank reset method on occ-ranktbl objects.
Clear the cached subtree maximum rank of OBJ."
  (setf (occ-ranktbl-max-decendent obj) nil))
(cl-defmethod occ-obj-reset-rank-acquired ((obj occ-ranktbl))
  "Rank reset method on occ-ranktbl objects.
Stub: not yet implemented (signals occ-error)."
  (occ-error "Error"))
(cl-defmethod occ-obj-reset-rank ((obj occ-ranktbl))
  "Rank reset method on occ-ranktbl objects.
Clear the cached total rank value of OBJ."
  (setf (occ-ranktbl-value obj) nil))

(cl-defmethod (setf occ-obj-prop-rank) ((rank number)
                                        (obj  occ-ranktbl)
                                        (prop symbol))
  "Setf method for occ-obj-prop-rank on occ-ranktbl objects.
Store RANK as the cached rank of PROP in the plist of OBJ."
  (let ((rplist (occ-ranktbl-plist obj)))
    (setf (occ-ranktbl-plist obj)
          (plist-put rplist prop rank))))
(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
                                               (obj occ-ranktbl))
  "Setf method for occ-obj-rank-inheritable on occ-ranktbl objects.
Store RANK as the cached inheritable rank of OBJ."
  (setf (occ-ranktbl-inheritable obj) rank))
(cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number)
                                                (obj occ-ranktbl))
  "Setf method for occ-obj-rank-nonheritable on occ-ranktbl objects.
Store RANK as the cached nonheritable rank of OBJ."
  (setf (occ-ranktbl-nonheritable obj) rank))
(cl-defmethod (setf occ-obj-rank-max-decendent) ((rank number)
                                                 (obj occ-ranktbl))
  "Setf method for occ-obj-rank-max-decendent on occ-ranktbl objects.
Store RANK as the cached subtree maximum rank of OBJ."
  (setf (occ-ranktbl-max-decendent obj) rank))
(cl-defmethod (setf occ-obj-rank-acquired) ((rank number)
                                            (obj occ-ranktbl))
  "Setf method for occ-obj-rank-acquired on occ-ranktbl objects.
Stub: not yet implemented (signals occ-error)."
  (occ-error "Error"))
(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-ranktbl))
  "Setf method for occ-obj-rank on occ-ranktbl objects.
Store RANK as the cached total rank value of OBJ."
  (setf (occ-ranktbl-value obj) rank))


(cl-defmethod occ-obj-prop-rank-with ((tsk  occ-obj-tsk)
                                      (ctx  occ-obj-ctx)
                                      (property symbol))
  "Prop rank method on occ-obj-tsk and occ-obj-ctx objects.
Return the cached rank of PROPERTY for TSK in CTX; compute it
via occ-obj-priority-rank and cache it in the per-task rank
table when missing."
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
  "Prop rank method on occ-obj-tsk with a null CTX.
Return the cached rank of PROPERTY for TSK; compute it via
occ-obj-priority-rank and cache it in the per-task rank table
when missing."
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
  "Prop rank reset method on occ-obj-tsk and occ-obj-ctx objects.
Reset the cached rank of PROPERTY for TSK in CTX and also
reset the inheritable or nonheritable rank sum matching
PROPERTY."
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
  "Prop rank reset method on occ-obj-tsk with a null CTX.
Reset the cached rank of PROPERTY for TSK and also reset the
inheritable or nonheritable rank sum matching PROPERTY."
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
  "Setf method for occ-obj-prop-rank-with on occ-obj-tsk objects.
Store RANK as the cached rank of PROPERTY for TSK in CTX in
the per-task rank table."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-prop-rank rt
                             property) rank)))


(cl-defmethod (setf occ-obj-prop-rank-with) ((rank number)
                                             (tsk  occ-obj-tsk)
                                             (ctx  null)
                                             (property symbol))
  "Setf method for occ-obj-prop-rank-with with a null CTX.
Store RANK as the cached rank of PROPERTY for TSK in the
per-task rank table."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-prop-rank rt
                             property) rank)))


(cl-defmethod occ-obj-prop-rank ((obj  occ-obj-tsk)
                                 (property symbol))
  "Prop rank method on occ-obj-tsk objects.
Return the rank of PROPERTY for the task and context stored in
OBJ via occ-obj-prop-rank-with."
  (occ-obj-prop-rank-with (occ-obj-tsk obj)
                          (occ-obj-ctx obj)
                          property))
(cl-defmethod occ-obj-reset-prop-rank ((obj  occ-obj-tsk)
                                       (property symbol))
  "Prop rank reset method on occ-obj-tsk objects.
Reset the rank of PROPERTY for the task and context stored in
OBJ via occ-obj-reset-prop-rank-with."
  (occ-obj-reset-prop-rank-with (occ-obj-tsk obj)
                                (occ-obj-ctx obj)
                                property))
(cl-defmethod (setf occ-obj-prop-rank) ((rank number)
                                        (obj  occ-obj-tsk)
                                        (property symbol))
  "Setf method for occ-obj-prop-rank on occ-obj-tsk objects.
Store RANK as the rank of PROPERTY for the task and context
stored in OBJ via occ-obj-prop-rank-with."
  (setf (occ-obj-prop-rank-with (occ-obj-tsk obj)
                                (occ-obj-ctx obj)
                                property) rank))


(cl-defmethod occ-obj-ancestor-rank-with ((tsk null)
                                          (ctx occ-obj-ctx)
                                          (height number))
  "Ancestor rank method on null TSK and occ-obj-ctx objects.
Return 0 as there is no ancestor task to inherit rank from."
  0)

(cl-defmethod occ-obj-ancestor-rank-with ((tsk occ-obj-tsk)
                                          (ctx occ-obj-ctx)
                                          (height number))
  "Ancestor rank method on occ-obj-tsk and occ-obj-ctx objects.
Return the inheritable rank of TSK plus the ancestor rank of
its parent so inheritable matches propagate up the task tree."
  ;; TODO: sibling-count update parent with max child rank value
  (+ (occ-obj-rank-inheritable-with tsk
                                    ctx)
     (occ-obj-ancestor-rank-with (occ-tsk-parent tsk)
                                 ctx
                                 height)))
(cl-defmethod occ-obj-ancestor-rank-with ((tsk occ-obj-tsk)
                                          (ctx null)
                                          (height number))
  "Ancestor rank method on occ-obj-tsk with a null CTX.
Return the inheritable rank of TSK plus the ancestor rank of
its parent."
  (+ (occ-obj-rank-inheritable-with tsk
                                    ctx)
     (occ-obj-ancestor-rank-with (occ-tsk-parent tsk)
                                 ctx
                                 height)))
(cl-defmethod occ-obj-ancestor-rank-with ((tsk null)
                                          (ctx null)
                                          (height number))
  "Ancestor rank method on null TSK and null CTX.
Return 0 as there is no ancestor task to inherit rank from."
  0)


(cl-defmethod occ-obj-rank-inheritable-with ((tsk occ-obj-tsk)
                                             ctx)
  "Inheritable rank method on occ-obj-tsk objects.
Return the sum of the property ranks of TSK over the
inheritable properties selected for CTX; the value is computed
via occ-obj-calculate-rank and cached in the per-task rank
table."
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
  "Nonheritable rank method on occ-obj-tsk objects.
Return the sum of the property ranks of TSK over the
nonheritable properties selected for CTX; the value is
computed via occ-obj-calculate-rank and cached in the per-task
rank table."
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
  "Subtree maximum rank method on occ-obj-tsk objects.
Return the maximum of the total rank of TSK and the subtree
maximum ranks of its descendants for CTX; the value is the
default display rank and is cached in the per-task rank
table."
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
  "Acquired rank method on occ-obj-tsk objects.
Return the inheritable rank of TSK divided by its descendant
weight plus the nonheritable rank of TSK for CTX."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (+ (/ (occ-obj-rank-inheritable-with tsk
                                         ctx)
          (occ-tsk-descendant-weight tsk))
       (occ-obj-rank-nonheritable-with tsk
                                       ctx))))
(cl-defmethod occ-obj-rank-with ((tsk occ-obj-tsk)
                                 ctx)
  "Total rank method on occ-obj-tsk objects.
Return the acquired rank of TSK plus the ancestor rank of its
parent divided by the descendant weight of TSK for CTX; the
value is cached in the per-task rank table."
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
  "Inheritable rank reset method on occ-obj-tsk objects.
Clear the cached inheritable rank of TSK for CTX and reset the
total ranks of TSK and its descendants."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-inheritable rt)
    (occ-obj-reset-rank-acquired-with tsk ctx)
    (occ-obj-tsk-do-descendant-with tsk
                                    ctx
                                    #'occ-obj-reset-rank-with)))
(cl-defmethod occ-obj-reset-rank-nonheritable-with ((tsk occ-obj-tsk)
                                                    ctx)
  "Nonheritable rank reset method on occ-obj-tsk objects.
Clear the cached nonheritable rank of TSK for CTX."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-nonheritable rt)))
(cl-defmethod occ-obj-reset-rank-max-decendent-with ((tsk occ-obj-tsk)
                                                     (ctx occ-obj-ctx))
  "Subtree maximum rank reset method on occ-obj-tsk objects.
Clear the cached subtree maximum rank of TSK for CTX."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-max-decendent rt)))
(cl-defmethod occ-obj-reset-rank-acquired-with ((tsk occ-obj-tsk)
                                                ctx)
  "Acquired rank reset method on occ-obj-tsk objects.
Reset the total rank of TSK for CTX via occ-obj-reset-rank-with."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (occ-obj-reset-rank-with tsk ctx)
    (when nil
      (occ-obj-reset-rank-acquired rt))))
(cl-defmethod occ-obj-reset-rank-with ((tsk occ-obj-tsk)
                                       ctx)
  "Total rank reset method on occ-obj-tsk objects.
Clear the cached total rank of TSK for CTX and reset the
subtree maximum ranks of TSK and its ancestors."
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
  "Ancestor walker method on occ-obj-tsk objects.
Call FUN on TSK and CTX and then recurse on the parent of TSK
up the task tree."
  (funcall fun tsk ctx)
  (if (occ-tsk-parent tsk)
      (occ-obj-tsk-do-ancestor-with (occ-tsk-parent tsk)
                                    ctx
                                    fun)))

(cl-defmethod occ-obj-tsk-do-descendant-with ((tsk occ-obj-tsk)
                                              ctx
                                              fun)
  "Descendant walker method on occ-obj-tsk objects.
Recurse over the subtree of TSK calling FUN on each descendant
and finally on TSK itself passing CTX."
  (dolist (c (occ-tree-tsk-subtree tsk))
    (occ-obj-tsk-do-descendant-with c ctx fun))
  (funcall fun tsk ctx))

(cl-defmethod (setf occ-obj-rank-inheritable-with) ((rank number)
                                                    (tsk occ-obj-tsk)
                                                    ctx)
  "Setf method for occ-obj-rank-inheritable-with on occ-obj-tsk.
Store RANK as the cached inheritable rank of TSK for CTX in
the per-task rank table."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-inheritable rt) rank)))
(cl-defmethod (setf occ-obj-rank-nonheritable-with) ((rank number)
                                                     (tsk occ-obj-tsk)
                                                     ctx)
  "Setf method for occ-obj-rank-nonheritable-with on occ-obj-tsk.
Store RANK as the cached nonheritable rank of TSK for CTX in
the per-task rank table."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-nonheritable rt) rank)))
(cl-defmethod (setf occ-obj-rank-max-decendent-with) ((rank number)
                                                      (tsk occ-obj-tsk)
                                                      (ctx occ-obj-ctx))
  "Setf method for occ-obj-rank-max-decendent-with on occ-obj-tsk.
Store RANK as the cached subtree maximum rank of TSK for CTX
in the per-task rank table."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-max-decendent rt) rank)))
(cl-defmethod (setf occ-obj-rank-acquired-with) ((rank number)
                                                 (tsk occ-obj-tsk)
                                                 ctx)
  "Setf method for occ-obj-rank-acquired-with on occ-obj-tsk.
Store RANK as the cached acquired rank of TSK for CTX in the
per-task rank table."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank-acquired rt) rank)))
(cl-defmethod (setf occ-obj-rank-with) ((rank number)
                                        (tsk occ-obj-tsk)
                                        ctx)
  "Setf method for occ-obj-rank-with on occ-obj-tsk objects.
Store RANK as the cached total rank of TSK for CTX in the
per-task rank table."
  (let ((rt (occ-obj-ranktbl-with tsk
                                  ctx)))
    (setf (occ-obj-rank rt) rank)))


(cl-defmethod occ-obj-rank-inheritable ((obj occ-obj-tsk))
  "Inheritable rank method on occ-obj-tsk objects.
Return the inheritable rank of the task and context stored in
OBJ via occ-obj-rank-inheritable-with."
  (occ-obj-rank-inheritable-with (occ-obj-tsk obj)
                                 (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank-nonheritable ((obj occ-obj-tsk))
  "Nonheritable rank method on occ-obj-tsk objects.
Return the nonheritable rank of the task and context stored in
OBJ via occ-obj-rank-nonheritable-with."
  (occ-obj-rank-nonheritable-with (occ-obj-tsk obj)
                                  (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank-max-decendent ((obj occ-obj-tsk))
  "Subtree maximum rank method on occ-obj-tsk objects.
Return the subtree maximum rank of the task and context stored
in OBJ via occ-obj-rank-max-decendent-with."
  (occ-obj-rank-max-decendent-with (occ-obj-tsk obj)
                                   (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank-acquired ((obj occ-obj-tsk))
  "Acquired rank method on occ-obj-tsk objects.
Return the acquired rank of the task and context stored in OBJ
via occ-obj-rank-acquired-with."
  (occ-obj-rank-acquired-with (occ-obj-tsk obj)
                              (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank ((obj occ-obj-tsk))
  "Total rank method on occ-obj-tsk objects.
Return the total rank of the task and context stored in OBJ
via occ-obj-rank-with."
  (occ-obj-rank-with (occ-obj-tsk obj)
                     (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank ((obj occ-ctxual-tsk))
  "Total rank method on occ-ctxual-tsk objects.
Return the total rank of the task and context stored in OBJ
via occ-obj-rank-with."
  (occ-obj-rank-with (occ-obj-tsk obj)
                     (occ-obj-ctx obj)))
(cl-defmethod occ-obj-rank ((obj occ-ctsk))
  "Total rank method on occ-ctsk objects.
Return the total rank of the task stored in OBJ with a null
context via occ-obj-rank-with."
  (occ-obj-rank-with (occ-obj-tsk obj)
                     nil))

(cl-defmethod occ-obj-reset-rank-inheritable ((obj occ-obj-tsk))
  "Inheritable rank reset method on occ-obj-tsk objects.
Reset the inheritable rank of the task and context stored in
OBJ via occ-obj-reset-rank-inheritable-with."
  (occ-obj-reset-rank-inheritable-with (occ-obj-tsk obj)
                                       (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank-nonheritable ((obj occ-obj-tsk))
  "Nonheritable rank reset method on occ-obj-tsk objects.
Reset the nonheritable rank of the task and context stored in
OBJ via occ-obj-reset-rank-nonheritable-with."
  (occ-obj-reset-rank-nonheritable-with (occ-obj-tsk obj)
                                        (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank-max-decendent ((obj occ-obj-tsk))
  "Subtree maximum rank reset method on occ-obj-tsk objects.
Reset the subtree maximum rank of the task and context stored
in OBJ via occ-obj-reset-rank-max-decendent-with."
  (occ-obj-reset-rank-max-decendent-with (occ-obj-tsk obj)
                                         (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank-acquired ((obj occ-obj-tsk))
  "Acquired rank reset method on occ-obj-tsk objects.
Reset the acquired rank of the task and context stored in OBJ
via occ-obj-reset-rank-acquired-with."
  (occ-obj-reset-rank-acquired-with (occ-obj-tsk obj)
                                    (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank ((obj occ-obj-tsk))
  "Total rank reset method on occ-obj-tsk objects.
Reset the total rank of the task and context stored in OBJ via
occ-obj-reset-rank-with."
  (occ-obj-reset-rank-with (occ-obj-tsk obj)
                           (occ-obj-ctx obj)))
(cl-defmethod occ-obj-reset-rank ((obj occ-ctsk))
  "Total rank reset method on occ-ctsk objects.
Reset the total rank of the task stored in OBJ with a null
context via occ-obj-reset-rank-with."
  (occ-obj-reset-rank-with (occ-obj-tsk obj)
                           nil))

(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
                                               (obj occ-obj-tsk))
  "Setf method for occ-obj-rank-inheritable on occ-obj-tsk objects.
Store RANK as the inheritable rank of the task and context
stored in OBJ via occ-obj-rank-inheritable-with."
  (setf (occ-obj-rank-inheritable-with (occ-obj-tsk obj)
                                       (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number)
                                                (obj occ-obj-tsk))
  "Setf method for occ-obj-rank-nonheritable on occ-obj-tsk objects.
Store RANK as the nonheritable rank of the task and context
stored in OBJ via occ-obj-rank-nonheritable-with."
  (setf (occ-obj-rank-nonheritable-with (occ-obj-tsk obj)
                                        (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank-max-decendent) ((rank number)
                                                 (obj occ-obj-tsk))
  "Setf method for occ-obj-rank-max-decendent on occ-obj-tsk objects.
Store RANK as the subtree maximum rank of the task and context
stored in OBJ via occ-obj-rank-max-decendent-with."
  (setf (occ-obj-rank-max-decendent-with (occ-obj-tsk obj)
                                         (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank-acquired) ((rank number)
                                            (obj occ-obj-tsk))
  "Setf method for occ-obj-rank-acquired on occ-obj-tsk objects.
Store RANK as the acquired rank of the task and context stored
in OBJ via occ-obj-rank-acquired-with."
  (setf (occ-obj-rank-acquired-with (occ-obj-tsk obj)
                                    (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-obj-tsk))
  "Setf method for occ-obj-rank on occ-obj-tsk objects.
Store RANK as the total rank of the task and context stored in
OBJ via occ-obj-rank-with."
  (setf (occ-obj-rank-with (occ-obj-tsk obj)
                           (occ-obj-ctx obj))
        rank))
(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-ctsk))
  "Setf method for occ-obj-rank on occ-ctsk objects.
Store RANK as the total rank of the task stored in OBJ with a
null context via occ-obj-rank-with."
  (setf (occ-obj-rank-with (occ-obj-tsk obj)
                           nil)
        rank))


(cl-defmethod occ-obj-calculate-avgrank ((obj occ-ctx))
  "Average rank method on occ-ctx objects.
Return the average of the total ranks of the ctxual tasks
built from OBJ."
  (let* ((objs      (occ-obj-list obj
                                  :builder #'occ-obj-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         ;; BUG
         (avgrank   (occ-calculate-average rankslist)))
    avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-ctx))
  "Variance rank method on occ-ctx objects.
Return the variance of the total ranks of the ctxual tasks
built from OBJ."
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
  "Average rank method on occ-collection objects.
Return the average of the total ranks of the tasks listed in
OBJ."
  (let* ((objs      (occ-obj-list obj))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         (avgrank   (occ-calculate-average rankslist)))
       avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-collection))
  "Variance rank method on occ-collection objects.
Return the variance of the total ranks of the tasks listed in
OBJ."
  (let* ((objs      (occ-obj-list obj))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         (varirank  (occ-calculate-variance rankslist)))
      varirank))


;; (occ-obj-avgrank (occ-default-collection))
;; (occ-obj-varirank (occ-default-collection))

;;; occ-rank.el ends here
