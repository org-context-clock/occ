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

;; occ-filter-base.el implements the filter machinery of the filtering &
;; ranking layer.  Filters are interactive rank-threshold mechanisms, not
;; boolean task predicates: registered `occ-static-filter' specs are kept
;; in `occ-obj-static-filters' (`occ-obj-static-filter-add',
;; `occ-obj-static-filter-get', `occ-obj-static-filters-get') and are
;; compiled by `occ-obj-static-to-dyn-filter' into per-session
;; `occ-obj-dyn-filter' closure bundles (init/seq/selectable/display/
;; increment/decrement/reset) chained through `prev'.  The recursive
;; builder `occ-obj-build-dyn-filters-recursive' turns a filter spec list
;; into a chain and `occ-obj-combined-dyn-filter' wraps it with a prev/next
;; stack so the user can switch filter sets live.  Pivot movement helpers
;; are `occ-obj-filter-comparator', `occ-obj-filter-incrementor' and
;; `occ-obj-filter-decrementor'; ranks default to `occ-obj-rank' (select)
;; and `occ-obj-rank-max-decendent' (display), and passing tasks are
;; flagged with `occ-obj-tsk-selectable'.  Context-cached statistics
;; (`occ-obj-ctx-stat', `occ-obj-average', `occ-obj-stddev',
;; `occ-obj-variance') round out the file.
;;
;; See doc/occ-design.org for the full design.

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
  "Register STATIC-FILTER in the global static filter registry."
  (cl-pushnew static-filter
              occ-obj-static-filters))
(defun occ-obj-static-filter-get (key)
  "Return the registered occ-static-filter whose keyword is KEY."
  (cl-first (cl-remove-if-not #'(lambda (filter)
                                  (eq key
                                      (occ-static-filter-keyword filter)))
                              occ-obj-static-filters)))
(defun occ-obj-static-filters-get (keylist)
  "Return the registered static filters matching any key of KEYLIST.
Keys may be plain keywords or conses whose car is the keyword."
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
  "Return the static filters of KEYLIST for the OCC-OBJ-CTX OBJ."
  ;; TODO: do we require (apply #'append ...)
  (ignore obj)
  (occ-debug "(OCC-OBJ-GET-FILTERS OCC-OBJ-TSK): called")
  (occ-obj-static-filters-get keylist))

;; (occ-obj-get-static-filters (occ-obj-make-ctx-at-point) (occ-match-filters))
;; (occ-obj-static-filters-get (occ-match-filters))
;; (occ-obj-static-filters-get '(:identity :positive))


(cl-defmethod occ-obj-average ((obj occ-stat)
                               sequence)
  "Method of occ-obj-average for OCC-STAT OBJ.
Return the average of SEQUENCE and cache it in OBJ when unset."
  (unless (occ-stat-average obj)
    (setf (occ-stat-average obj) (apply #'occ-stats-average sequence)))
  (occ-stat-average obj))

(cl-defmethod occ-obj-stddev ((obj occ-stat)
                              sequence)
  "Method of occ-obj-stddev for OCC-STAT OBJ.
Return the standard deviation of SEQUENCE and cache it in OBJ."
  (unless (occ-stat-stddev obj)
    (setf (occ-stat-stddev obj) (apply #'occ-stats-stddev sequence)))
  (occ-stat-stddev obj))

(cl-defmethod occ-obj-variance ((obj occ-stat)
                                sequence)
  "Method of occ-obj-variance for OCC-STAT OBJ.
Return the variance of SEQUENCE and cache it in OBJ."
  (unless (occ-stat-variance obj)
    (setf (occ-stat-variance obj) (apply #'occ-stats-variance sequence)))
  (occ-stat-variance obj))


(cl-defmethod occ-obj-ctx-stat ((obj occ-obj-ctx)
                                stat)
  "Method of occ-obj-ctx-stat for OCC-OBJ-CTX OBJ.
Return the cached occ-stat for STAT in the OBJ stat plist.
Create and store a fresh occ-stat when none is cached yet."
  (unless (plist-get (occ-obj-ctx-stat-plist obj)
                     stat)
    (plist-put (occ-obj-ctx-stat-plist obj) stat
               (occ-obj-make-stat)))
  (plist-get (occ-obj-ctx-stat-plist obj) stat))


(cl-defmethod occ-obj-static-filter-points ((static-filter occ-static-filter)
                                            (obj occ-ctx)
                                            sequence
                                            &key rank)
  "Method of occ-obj-static-filter-points for OCC-STATIC-FILTER.
Call the STATIC-FILTER points-gen-fn on CTX and SEQUENCE with RANK
to compute the threshold points of the filter."
  (let ((points-gen-fn (occ-static-filter-points-gen-fn static-filter)))
    (funcall points-gen-fn
             obj
             sequence
             :rank rank)))

(cl-defmethod occ-obj-static-filter-default-pivot ((static-filter occ-static-filter)
                                                   (obj occ-ctx)
                                                   points)
  "Method of occ-obj-static-filter-default-pivot for OCC-STATIC-FILTER.
Call the STATIC-FILTER default-pivot-fn on CTX and POINTS to get the
initial pivot index of the filter."
  (let ((default-pivot-fn (occ-static-filter-default-pivot-fn static-filter)))
    (funcall default-pivot-fn obj
             points)))


(cl-defmethod occ-obj-dyn-filter-init ((dyn-filter occ-obj-dyn-filter))
  "Method of occ-obj-dyn-filter-init for OCC-OBJ-DYN-FILTER DYN-FILTER.
Invoke the init closure which computes the threshold points and the
default pivot of the filter session."
  (funcall (occ-obj-dyn-filter-init-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-seq ((dyn-filter occ-obj-dyn-filter))
  "Method of occ-obj-dyn-filter-seq for OCC-OBJ-DYN-FILTER DYN-FILTER.
Invoke the seq closure and return the candidate sequence the filter
operates on."
  (funcall (occ-obj-dyn-filter-seq-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-selectable-filter ((dyn-filter occ-obj-dyn-filter))
  "Method of occ-obj-dyn-filter-selectable-filter for DYN-FILTER.
Invoke the selectable-filter closure of the OCC-OBJ-DYN-FILTER
DYN-FILTER and return the tasks whose selected rank passes the
current threshold."
  (funcall (occ-obj-dyn-filter-selectable-filter-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-display-filter ((dyn-filter occ-obj-dyn-filter))
  "Method of occ-obj-dyn-filter-display-filter for DYN-FILTER.
Invoke the display-filter closure of the OCC-OBJ-DYN-FILTER
DYN-FILTER and return the tasks to show at the current threshold
ranked by the display rank function."
  (funcall (occ-obj-dyn-filter-display-filter-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-increment ((dyn-filter occ-obj-dyn-filter))
  "Method of occ-obj-dyn-filter-increment for OCC-OBJ-DYN-FILTER DYN-FILTER.
Invoke the increment closure which steps the threshold pivot one
rank point."
  (funcall (occ-obj-dyn-filter-increment-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-decrement ((dyn-filter occ-obj-dyn-filter))
  "Method of occ-obj-dyn-filter-decrement for OCC-OBJ-DYN-FILTER DYN-FILTER.
Invoke the decrement closure which steps the threshold pivot one
rank point back."
  (funcall (occ-obj-dyn-filter-decrement-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-reset ((dyn-filter occ-obj-dyn-filter))
  "Method of occ-obj-dyn-filter-reset for OCC-OBJ-DYN-FILTER DYN-FILTER.
Invoke the reset closure which restores the pivot to the default
pivot of the filter session."
  (funcall (occ-obj-dyn-filter-reset-closure-fn dyn-filter)))

(cl-defmethod occ-obj-dyn-filter-next-closure-fn ((dyn-filter occ-combined-dyn-filter))
  "Method of occ-obj-dyn-filter-next-closure-fn for DYN-FILTER.
Return the next closure of the OCC-COMBINED-DYN-FILTER DYN-FILTER."
  (occ-combined-dyn-filter-next-closure-fn dyn-filter))

(cl-defmethod occ-obj-dyn-filter-prev-closure-fn ((dyn-filter occ-combined-dyn-filter))
  "Method of occ-obj-dyn-filter-prev-closure-fn for DYN-FILTER.
Return the prev closure of the OCC-COMBINED-DYN-FILTER DYN-FILTER."
  (occ-combined-dyn-filter-prev-closure-fn dyn-filter))


(defun occ-obj-filter-comparator (compare-fn
                                  rank
                                  pivot
                                  dir)
  "Compare RANK with PIVOT using COMPARE-FN in direction DIR.
Call COMPARE-FN with RANK then PIVOT when DIR is non-nil and with
PIVOT then RANK otherwise."
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
  "Return the pivot index after one increment step modulo LENGTH.
Direction DIR moves the index up or down before the wrap."
  (mod (if dir
           (1+ pivot)
         (1- pivot))
       length))
(defun occ-obj-filter-decrementor (pivot
                                   length
                                   dir)
  "Return the pivot index after one decrement step modulo LENGTH.
Direction DIR moves the index down or up before the wrap."
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
  "Compile the OCC-STATIC-FILTER STATIC-FILTER into a dynamic filter.
The returned occ-obj-dyn-filter is instantiated over candidate
SEQUENCE for context CTX and chained onto PREV through the prev slot
so its seq closure consumes the selectable filter of PREV when
present.  Its init closure computes the threshold points with the
STATIC-FILTER points-gen-fn and the default pivot with default-pivot-fn.
Ranks come from RANK-SELECT-FN and RANK-DISPLAY-FN and are compared
against the pivot with COMPARE-FN in FILTER-DIR order."
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
  "Build the dynamic filter chain for CTX over candidate SEQUENCE.
STATIC-FILTER-METHODS lists filter specs as keywords or as conses of
keyword and rank function; a plain non-keyword element sets the
filter direction for the remaining specs.  Specs are built so that
the last one is the innermost filter and earlier ones consume its
output through the prev slot.  RANK-SELECT-FN and RANK-DISPLAY-FN
default the rank functions of the compiled filters.  Returns the
outermost occ-obj-dyn-filter or nil when no spec remains."
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
  "Build the combined dynamic filter bundle for CTX over SEQUENCE.
Builds the filter chain from STATIC-FILTER-METHODS with
occ-obj-build-dyn-filters-recursive and wraps it in a combined
occ-combined-dyn-filter whose prev and next closures keep a stack so
the user can move between filter sets live.  RANK-SELECT-FN and
RANK-DISPLAY-FN give the default rank functions.  Signals occ-error
when no dynamic filter could be built."
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
