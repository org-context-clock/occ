;;; occ-prop-op-checkout.el --- property checkout code  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  sharad

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

(provide 'occ-prop-op-misc)


(require 'occ-prop-base)
(eval-when-compile
  (require 'occ-macros))


(cl-defgeneric occ-do-op-prop-misc (obj
                                    prop)
  "Misc property PROP for forced clock-in.")

(cl-defmethod occ-do-op-prop-misc ((obj  occ-obj-tsk)
                                   (prop symbol))
  "Misc property PROP for forced clock-in."
  ;; BUG: Fix
  (occ-do-checkout obj
                   prop))


(cl-defgeneric occ-do-op-props-misc (obj)
  "Misc all property for forced clock-in.")

(cl-defmethod occ-do-op-props-misc ((obj occ-obj-tsk))
  "Misc all property for forced clock-in."
  ;; BUG: Fix
  (dolist (prop (occ-obj-properties-to-misc obj))
    (occ-debug "occ-do-op-props-misc: checkout prop %s" prop)
    (occ-do-op-prop-misc obj
                         prop)))

;;; occ-prop-op-checkout.el ends here
