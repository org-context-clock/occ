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

;;

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
