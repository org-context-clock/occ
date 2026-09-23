;;; occ-clock-report.el --- OCC Clock Report           -*- lexical-binding: t; -*-

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

;; occ-clock-report.el provides a thin OCC wrapper around the
;; vendored org-clock-report package for inserting clock reports.
;; Its single function occ-clock-plain-report-tree inserts a plain
;; clock report tree at MARKER via org-clock-plain-report-tree, using
;; "*" headline characters and inserting both content and notes, with
;; the report block chosen interactively by
;; occ-util-select-from-sym-list over the standard org clock ranges
;; (today, thisweek, thismonth, thisyear, lastweek, lastmonth,
;; lastyear, untilnow, interactive).  It is used by the Report action
;; in occ-obj-simple.el.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-clock-report)


(require 'org-clock-report)


(require 'occ-util-common)



(defun occ-clock-plain-report-tree (marker)
  (let ((range '(today ;check org-clock-special-range
                 thisweek
                 thismonth
                 thisyear
                 lastweek
                 lastmonth
                 lastyear
                 untilnow
                 interactive)))
    (org-clock-plain-report-tree marker
                                 :headline-char  "*" ;; "•"
                                 :insert-content t
                                 :insert-notes   t
                                 :block          (occ-util-select-from-sym-list "Block: " range))))

;;; occ-clock-report.el ends here
