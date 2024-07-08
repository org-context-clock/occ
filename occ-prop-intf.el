;;; occ-prop-intf.el --- occ property interface      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

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

;; This file should only has interface and default interface methods which is
;; implemented by occ-property-methods.org

;;; Code:

(provide 'occ-prop-intf)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-util-common)
(require 'occ-obj)
(require 'occ-prop-utils)
(require 'occ-normalize-ineqs)



;;; occ-prop-intf.el ends here
;;
;;
;;
;; * read prop value from user
;; * read prop value from ctx
;; * read prop value from tsk
;; * write prop value to tsk
;; * write prop value to ctx error
;; * write to user means print
;; * checkout prop value from tsk
;; (org-read-date) (org--deadline-or-schedule arg 'scheduled tim)e
