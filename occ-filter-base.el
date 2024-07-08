;;; occ-filter-base.el --- list filter               -*- lexical-binding: t; -*-

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

(provide 'occ-filter-base)


(require 'occ-obj-common)
(require 'occ-tree)
(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-accessor)
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-rank)
(require 'occ-statistics)



(defvar occ-obj-static-filters nil)
(defun occ-obj-static-filter-add (static-filter)
  (cl-pushnew static-filter
              occ-obj-static-filters))
(defun occ-obj-static-filter-get (key)
  (cl-first (cl-remove-if-not #'(lambda (filter)
                                  (eq key
                                      (occ-static-filter-keyword filter)))
                              occ-obj-static-filters)))
(defun occ-obj-static-filters-get (keylist)
  (mapcan #'(lambda (key)
              (let ((fkey (or (car-safe key)
                              key)))
                (cl-remove-if-not #'(lambda (filter)
                                      (eq fkey
                                          (occ-static-filter-keyword filter)))
                                  occ-obj-static-filters)))
          keylist))


(cl-defmethod occ-obj-get-static-filters ((obj occ-obj-ctx)
                                          keylist)
  ;; TODO: do we require (apply #'append ...)
  (ignore obj)
  (occ-debug "(OCC-OBJ-GET-FILTERS OCC-OBJ-TSK): called")
  (occ-obj-static-filters-get keylist))

;; (occ-obj-get-static-filters (occ-obj-make-ctx-at-point) (occ-match-filters))
;; (occ-obj-static-filters-get (occ-match-filters))
;; (occ-obj-static-filters-get '(:identity :positive))


(cl-defmethod occ-obj-average ((obj occ-stat)
                               sequence)
  (unless (occ-stat-average obj)
    (setf (occ-stat-average obj) (apply #'occ-stats-average sequence)))
  (occ-stat-average obj))

(cl-defmethod occ-obj-stddev ((obj occ-stat)
                              sequence)
  (unless (occ-stat-stddev obj)
    (setf (occ-stat-stddev obj) (apply #'occ-stats-stddev sequence)))
  (occ-stat-stddev obj))

(cl-defmethod occ-obj-variance ((obj occ-stat)
                                sequence)
  (unless (occ-stat-variance obj)
    (setf (occ-stat-variance obj) (apply #'occ-stats-variance sequence)))
  (occ-stat-variance obj))


(cl-defmethod occ-obj-ctx-stat ((obj occ-obj-ctx)
                                stat)
  (unless (plist-get (occ-obj-ctx-stat-plist obj)
                     stat)
    (plist-put (occ-obj-ctx-stat-plist obj) stat
               (occ-obj-make-stat)))
  (plist-get (occ-obj-ctx-stat-plist obj) stat))


(cl-defmethod occ-obj-static-filter-points ((static-filter occ-static-filter)
                                            (obj occ-ctx)
                                            sequence
                                            &key rank)
  (let ((points-gen-fn (occ-static-filter-points-gen-fn static-filter)))
    (funcall points-gen-fn
             obj
             sequence
             :rank rank)))

(cl-defmethod occ-obj-static-filter-default-pivot ((static-filter occ-static-filter)
                                                   (obj occ-ctx)
                                                   points)
  (let ((default-pivot-fn (occ-static-filter-default-pivot-fn static-filter)))
    (funcall default-pivot-fn obj
             points)))


(cl-defmethod occ-obj-dyn-filter-init ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-init-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-seq ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-seq-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-selectable-filter ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-selectable-filter-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-display-filter ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-display-filter-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-increment ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-increment-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-decrement ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-decrement-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-reset ((dyn-filter occ-obj-dyn-filter))
  (funcall (occ-obj-dyn-filter-reset-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-next-closure-fn ((dyn-filter occ-combined-dyn-filter))
  (occ-combined-dyn-filter-next-closure-fn dyn-filter))

(cl-defmethod occ-obj-dyn-filter-prev-closure-fn ((dyn-filter occ-combined-dyn-filter))
  (occ-combined-dyn-filter-prev-closure-fn dyn-filter))


(defun occ-obj-filter-comparator (compare-fn
                                  rank
                                  pivot
                                  dir)
  (if dir
      (funcall compare-fn
               rank
               pivot)
    (funcall compare-fn
             pivot
             rank)))
(defun occ-obj-filter-incrementor (pivot
                                   length
                                   dir)
  (mod (if dir
           (1+ pivot)
         (1- pivot))
       length))
(defun occ-obj-filter-decrementor (pivot
                                   length
                                   dir)
  (mod (if dir
           (1- pivot)
         (1+ pivot))
       length))
(cl-defmethod occ-obj-static-to-dyn-filter ((static-filter occ-static-filter)
                                            (obj occ-ctx)
                                            (sequence list)
                                            prev ;; (prev occ-dyn-filter)
                                            &key
                                            filter-dir
                                            rank-select-fn
                                            rank-display-fn)
  (occ-debug "occ-obj-static-to-dyn-filter in %s 1" (occ-obj-name static-filter))
  (let ((rank-display-fn  (or (occ-static-filter-rank-display-fn static-filter)
                              rank-display-fn
                              #'occ-obj-rank-max-decendent))
        (rank-select-fn   (or (occ-static-filter-rank-select-fn  static-filter)
                              rank-select-fn
                              #'occ-obj-rank))
        (points-fn        (occ-static-filter-points-gen-fn    static-filter))
        (default-pivot-fn (occ-static-filter-default-pivot-fn static-filter))
        (compare-fn       (occ-static-filter-compare-fn       static-filter))
        (points           nil)
        (default-pivot    nil)
        (pivot            nil))
    (let* ((seq-closure-fn               (if prev
                                             (occ-obj-dyn-filter-selectable-filter-closure-fn prev)
                                           #'(lambda () sequence)))
           (selectable-filter-closure-fn #'(lambda ()
                                             (when points
                                               (cl-remove-if-not #'(lambda (ctsk)
                                                                     (let ((rank (funcall rank-select-fn ctsk)))
                                                                       (occ-obj-filter-comparator compare-fn
                                                                                                  rank
                                                                                                  (nth pivot points)
                                                                                                  filter-dir)))
                                                                 (funcall seq-closure-fn)))))
           (init-closure-fn              #'(lambda ()
                                             (setf points        (funcall points-fn obj
                                                                          (funcall seq-closure-fn)
                                                                          :rank rank-select-fn))
                                             (setf default-pivot (funcall default-pivot-fn obj
                                                                          points))
                                             (setf pivot         default-pivot))))
      (funcall init-closure-fn)
      (occ-obj-build-dyn-filter (occ-obj-name static-filter)
                                :init-closure-fn      init-closure-fn
                                :seq-closure-fn       seq-closure-fn
                                :display-filter-closure-fn    #'(lambda ()
                                                                  (dolist (ctsk sequence)
                                                                    (setf (occ-obj-tsk-selectable ctsk) nil))
                                                                  (dolist (ctsk (funcall selectable-filter-closure-fn))
                                                                    (setf (occ-obj-tsk-selectable ctsk) t))
                                                                  (when points
                                                                    (cl-remove-if-not #'(lambda (ctsk)
                                                                                          (let ((rank (funcall rank-display-fn ctsk)))
                                                                                            (occ-obj-filter-comparator compare-fn
                                                                                                                       rank
                                                                                                                       (nth pivot points)
                                                                                                                       filter-dir)))
                                                                                      sequence)))
                                :selectable-filter-closure-fn selectable-filter-closure-fn
                                :increment-closure-fn #'(lambda ()
                                                          (setf pivot (occ-obj-filter-incrementor pivot
                                                                                                  (length points)
                                                                                                  filter-dir)))
                                :decrement-closure-fn #'(lambda ()
                                                          (setf pivot (occ-obj-filter-decrementor pivot
                                                                                                  (length points)
                                                                                                  filter-dir)))
                                :reset-closure-fn     #'(lambda ()
                                                          (setf pivot default-pivot))
                                :prev                 prev))))


(cl-defmethod occ-obj-build-dyn-filters-recursive ((obj occ-ctx)
                                                   (static-filter-methods list)
                                                   (sequence list)
                                                   &key
                                                   (filter-dir t)
                                                   rank-select-fn
                                                   rank-display-fn)
  ;; (occ-message "len(static-filter-methods) = %d" (length static-filter-methods))
  (let ((static-filterkw-rank (cl-first static-filter-methods)))
    (if (or (consp static-filterkw-rank)
            (keywordp static-filterkw-rank))
        (let ((static-filter        (occ-obj-static-filter-get (or (car-safe static-filterkw-rank)
                                                                   static-filterkw-rank)))
              (rank-select-fn       (if (consp static-filterkw-rank)
                                        (nth 1 static-filterkw-rank)
                                      (or rank-select-fn
                                          #'occ-obj-rank))))
          (occ-assert static-filter)
          (occ-debug "occ-obj-build-dyn-filters-recursive in")
          (let* ((prev (if (cdr static-filter-methods)
                           (occ-obj-build-dyn-filters-recursive obj
                                                                (cdr static-filter-methods)
                                                                sequence
                                                                :filter-dir filter-dir
                                                                :rank-select-fn rank-select-fn
                                                                :rank-display-fn rank-display-fn)
                         nil)))
            (occ-obj-static-to-dyn-filter static-filter
                                          obj
                                          sequence
                                          prev
                                          :filter-dir filter-dir
                                          :rank-select-fn rank-select-fn
                                          :rank-display-fn rank-display-fn)))
      (when (cdr static-filter-methods)
        (occ-obj-build-dyn-filters-recursive obj
                                             (cdr static-filter-methods)
                                             sequence
                                             :filter-dir static-filterkw-rank
                                             :rank-select-fn rank-select-fn
                                             :rank-display-fn rank-display-fn)))))

;; (cl-defmethod xyz ((x symbol)
;;                    &key
;;                    (dir t)
;;                    ris)
;;   (list x dir ris))


;; (xyz 'a :dir nil)

(cl-defmethod occ-obj-combined-dyn-filter ((obj occ-ctx)
                                           (static-filter-methods list)
                                           (sequence list)
                                           &key
                                           rank-select-fn
                                           rank-display-fn)
  (occ-debug "occ-obj-combined-dyn-filter: Going in")
  (let ((curr-dyn-filter (occ-obj-build-dyn-filters-recursive obj
                                                              static-filter-methods ;; (list :incremental);; static-filter-methods
                                                              sequence
                                                              :rank-select-fn rank-select-fn
                                                              :rank-display-fn rank-display-fn))
        (stack nil))
    (if curr-dyn-filter
        (progn
          (occ-debug "occ-obj-combined-dyn-filter: Coming out")
          (occ-debug "occ-obj-combined-dyn-filter: curr-dyn-filter %s" (occ-obj-name curr-dyn-filter))
          (occ-obj-build-combined-dyn-filter "CTX"
                                             :curr-closure-fn      #'(lambda () curr-dyn-filter)
                                             :prev-closure-fn      #'(lambda ()
                                                                       (let ((prev (occ-dyn-filter-prev curr-dyn-filter)))
                                                                         (if prev
                                                                             (progn
                                                                               (occ-message "Setting prev %s" (occ-obj-name prev))
                                                                               (push curr-dyn-filter stack)
                                                                               (setf curr-dyn-filter prev))
                                                                           (ding t)
                                                                           (occ-message "No prev (current: %s)" (occ-obj-name curr-dyn-filter)))))
                                             :next-closure-fn      #'(lambda ()
                                                                       (if stack
                                                                           (let ((next (pop stack)))
                                                                             (occ-message "Setting next %s" (occ-obj-name next))
                                                                             ;; regenerate points, default-pivot, pivot.
                                                                             (occ-obj-dyn-filter-init next)
                                                                             (setf curr-dyn-filter next))
                                                                         (ding t)
                                                                         (occ-message "No next (current: %s)" (occ-obj-name curr-dyn-filter))))
                                             :seq-closure-fn       #'(lambda ()
                                                                       (occ-assert curr-dyn-filter)
                                                                       (occ-obj-dyn-filter-seq curr-dyn-filter))
                                             :display-filter-closure-fn #'(lambda () (occ-obj-dyn-filter-display-filter    curr-dyn-filter))
                                             :selectable-filter-closure-fn #'(lambda () (occ-obj-dyn-filter-selectable-filter    curr-dyn-filter))
                                             :increment-closure-fn #'(lambda () (occ-obj-dyn-filter-increment curr-dyn-filter))
                                             :decrement-closure-fn #'(lambda () (occ-obj-dyn-filter-decrement curr-dyn-filter))
                                             :reset-closure-fn     #'(lambda () (occ-obj-dyn-filter-reset     curr-dyn-filter))))
      (occ-error "No filter to build combined dynamic filter."))))
    

;;; occ-filter-base.el ends here
