;;; occ-prop-utils.el --- occ property utils         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  sharad

;; Author: s <>
;; Keywords: convenience, abbrev

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

;; occ-prop-utils.el holds small helpers for the OCC property protocol.
;;
;; It defines occ-obj-format-prop (printable form of a property value,
;; currently returning the value unchanged), the rank hierarchy hooks
;; occ-property-rank-hierarchy and occ-obj-set-rank-hierarchy (a stub),
;; and the stub occ-prop-util-readprop-list-from-user, which signals
;; "Implement it".
;;
;; NOTE: the file also redeclares the generic occ-obj-has-p, which is
;; likewise declared in occ-prop-base.el.
;;
;; Layer: support/utility layer of the property protocol, required by
;; occ-prop-intf.el.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-prop-utils)


(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-prop-intf)
(require 'occ-intf)


(defvar occ-property-rank-hierarchy '(t))

(cl-defmethod occ-obj-set-rank-hierarchy ((property symbol)
                                          &key
                                          pos)
  (ignore property)
  (ignore pos)
  (cond))

;; (cl-defmethod)


(defun occ-prop-util-readprop-list-from-user (obj
                                              property)
  (ignore obj)
  (ignore property)
  (occ-error "Implement it, try with (occ-readprop-elem-from-user obj property)"))



















;; (cl-defmethod occ-obj-get-property ((obj occ-obj-ctx)
;;                                 (property symbol))
;;   "must return occ compatible value."
;;   ;; (occ-error "must return occ compatible value2.")
;;   (occ-debug "occ-obj-get-property: property %s obj %s" property obj)
;;   (occ-obj-get-property-value-from-ctx obj property))

(cl-defmethod occ-obj-format-prop ((obj occ-obj-tsk)
                                   (property symbol)
                                   value)
  "Should return format printable value"
  (ignore obj)
  (ignore property)
  value)


(cl-defgeneric occ-obj-has-p (obj
                              property
                              value)
  "occ-obj-has-p")

;;; occ-prop-utils.el ends here
