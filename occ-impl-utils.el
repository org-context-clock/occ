;;; occ-impl-utils.el --- impl utils                 -*- lexical-binding: t; -*-

;; Copyright (C) 2023  s

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; occ-impl-utils.el provides the comparison helpers used by the
;; per-property implementation methods: `occ-pu-safe-file-truename',
;; `occ-pu-file=' (truename file equality), `occ-pu-file-in-dir-p'
;; (directory containment) and `occ-pu-string=' (nil-safe string
;; equality).  They back the semantic prop= comparisons of the currfile,
;; root and git-branch properties in occ-property-methods.el (support
;; layer).
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-impl-utils)


;; (require 'subr)


(defun occ-pu-safe-file-truename (file)
  "Return the truename of FILE or nil when FILE is nil."
  (and file
       (file-truename file)))

(defun occ-pu-file-in-dir-p (dir file)
  "Return non-nil when the truename of FILE is inside DIR.
Both paths go through occ-pu-safe-file-truename before the prefix
test."
  (let ((dir  (occ-pu-safe-file-truename dir))
        (file (occ-pu-safe-file-truename file)))
    (and dir
         file
         (string-prefix-p (directory-file-name dir)
                          file))))

(defun occ-pu-file= (&rest files)
  "Return t when all FILES resolve to the same truename."
  (let ((files (mapcar #'occ-pu-safe-file-truename files)))
    (cl-every #'string=
              files
              (cdr-safe files))))

(defun occ-pu-string= (str1 str2)
  "Return t when STR1 and STR2 are both non-nil and string equal."
  (and str1
       str2
       (string= str1 str2)))

;;; occ-impl-utils.el ends here
