;;; occ-prop-checkout.el --- property checkout code  -*- lexical-binding: t; -*-

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

(provide 'occ-prop-op-checkout)


(require 'occ-prop-base)
(eval-when-compile
  (require 'occ-macros))


(cl-defgeneric occ-do-op-prop-checkout (obj
                                        prop
                                        vdirector)
  "Checkout property PROP for forced clock-in.")

(cl-defmethod occ-do-op-prop-checkout ((obj  occ-obj-tsk)
                                       (prop symbol)
                                       vdirector)
  "Checkout property PROP for forced clock-in."
  (occ-do-checkout obj
                   prop
                   vdirector))


(cl-defgeneric occ-do-op-props-checkout (obj)
  "Checkout all property for forced clock-in.")

(cl-defmethod occ-do-op-props-checkout ((obj occ-obj-tsk))
  "Checkout all property for forced clock-in."
  (dolist (prop (occ-obj-properties-to-checkout obj))
    (occ-debug "occ-do-op-props-checkout: checkout prop %s" prop)
    (let ((vdir (implement-select-one (occ-obj-vdirectors obj
                                                          prop))))
      (occ-do-op-prop-checkout obj
                               prop
                               vdir))))

;;; occ-prop-checkout.el ends here
