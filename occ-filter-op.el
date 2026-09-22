;;; occ-filter-op.el --- occ filter operations       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

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


(provide 'occ-filter-op)


(require 'occ-filter-base)


(cl-defmethod occ-obj-filter-mutual-deviation-points ((obj occ-ctx)
                                                      sequence
                                                      &key rank) ;TODO: make it after method
  (ignore obj)
  (ignore sequence)
  (ignore rank)
  (let* ((rank (or rank
                   #'occ-obj-rank))
         (rankslist  (mapcar rank       sequence))
         (avgrank    (apply  #'occ-stats-average  rankslist))
         (varirank   (apply  #'occ-stats-variance avgrank rankslist)))
    (delete-dups (mapcar rank sequence))))

(cl-defmethod occ-obj-filter-mutual-deviation-points ((obj occ-ctx)
                                                      sequence
                                                      &key rank) ;TODO: make it after method
  (ignore obj)
  (ignore sequence)
  (ignore rank)
  (delete-dups (mapcar rank sequence)))


(cl-defmethod occ-obj-filter-mutual-deviation ((obj occ-ctx)
                                               sequence
                                               &key rank) ;TODO: make it after method
  "Return matched Sequence for context CTX"
  (ignore obj)
  (if (occ-default-collection)
      (let* ((rankslist  (mapcar rank       sequence))
             (avgrank    (apply  #'occ-stats-average  rankslist))
             (varirank   (apply  #'occ-stats-variance avgrank rankslist)))
        ;; (occ-debug "occ-collection-obj-matches :around finish")
        (occ-debug "matched ctxtsks %s" (length sequence))
        (occ-debug "occ-filter-mutual-deviation: avgrank = %d varirank = %d"
                   avgrank varirank)
        (cl-remove-if-not #'(lambda (tsk)
                              (>= (funcall rank tsk)
                                  avgrank))
                          sequence))
    (occ-error "(occ-default-collection) returned nil")))


(cl-defmethod occ-obj-filter-positive ((obj occ-ctx)
                                       sequence
                                       &key rank)
  (ignore obj)
  (cl-remove-if-not #'(lambda (tsk)
                        (> (funcall rank tsk)
                           0))
                    sequence))


(cl-defmethod occ-obj-filter-nonnegative ((obj occ-ctx)
                                          sequence
                                          &key rank)
  (ignore obj)
  (cl-remove-if-not #'(lambda (tsk)
                        (>= (funcall rank tsk)
                            0))
                    sequence))


(cl-defmethod occ-obj-filter-identity ((obj occ-ctx)
                                       sequence
                                       &key rank)
  (ignore obj)
  sequence)


(defvar occ-filter-min 0)
(cl-defmethod occ-obj-filter-min ((obj occ-ctx)
                                  sequence
                                  &key rank)
  (ignore obj)
  (cl-remove-if-not #'(lambda (tsk)
                        (>= (funcall rank tsk)
                            occ-filter-min))
                    sequence))


(defvar occ-filter-max 0)
(cl-defmethod occ-obj-filter-max ((obj occ-ctx)
                                  sequence
                                  &key rank)
  (ignore obj)
  (cl-remove-if-not #'(lambda (tsk)
                        (>= (funcall rank tsk)
                            occ-filter-max))
                    sequence))

;;; occ-filter-op.el ends here
