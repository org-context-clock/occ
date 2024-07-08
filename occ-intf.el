;;; occ-intf.el --- occ intf                         -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'occ-intf)


(require 'occ-impl)


(cl-defgeneric occ-obj-intf-rank (tsk
                                  ctx
                                  property)
  "Return the RANK (number) for OBJ based on the property PROPERTY")
(cl-defmethod occ-obj-intf-rank (tsk
                                 ctx
                                 property)
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-obj-impl-rank tsk
                     ctx
                     property))
(cl-defmethod occ-obj-intf-rank ((tsk  occ-obj-tsk)
                                 (ctx  occ-obj-ctx)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-obj-impl-rank tsk
                     ctx
                     property))
(cl-defmethod occ-obj-intf-rank ((tsk  occ-obj-tsk)
                                 (ctx  null)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-obj-impl-rank tsk
                     ctx
                     property))


;; adding base propetry NIL for helping to add relative properties
(cl-defmethod occ-obj-intf-rank ((tsk  occ-obj-tsk)
                                 (ctx  occ-obj-ctx)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-obj-impl-rank tsk
                     ctx
                     property))
(cl-defmethod occ-obj-intf-rank ((tsk  occ-obj-tsk)
                                 (ctx  occ-obj-ctx)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-obj-impl-rank tsk
                     ctx
                     property))


(cl-defmethod occ-obj-intf-occ-prop-p ((prop symbol))
  (occ-obj-impl-occ-prop-p prop))


(cl-defgeneric occ-obj-intf-prop= (property
                                   prop-value
                                   value)
  "OBJ has property VALUE for PROPERTY")
(cl-defmethod occ-obj-intf-prop= ((property symbol)
                                  prop-value
                                  value)
  "OBJ has property VALUE for PROPERTY"
  (occ-obj-impl-prop= property
                      prop-value
                      value))


(cl-defgeneric occ-obj-intf-matches (obj
                                     property
                                     value)
  "VALUE equal prop-value of OBJ for PROPERTY")
(cl-defmethod occ-obj-intf-matches ((obj occ-obj-tsk)
                                    (prop symbol)
                                    value)
  "VALUE equal prop-value of OBJ for PROPERTY"
  (occ-obj-impl-matches obj
                        prop
                        value))


;; (cl-defgeneric occ-obj-intf-match (obj
;;                                    property
;;                                    value)
;;   "VALUE equal prop-value of OBJ for PROPERTY")
;; (cl-defmethod occ-obj-intf-match ((obj occ-obj-tsk)
;;                                   (prop symbol)
;;                                   value)
;;   "VALUE equal prop-value of OBJ for PROPERTY"
;;   (let ((matches (occ-obj-intf-matches obj
;;                                        prop
;;                                        value)))
;;     (if (occ-obj-list-p obj prop)
;;         (cl-first matches)
;;       matches)))


(cl-defgeneric occ-obj-intf-has-p (obj
                                   property
                                   value)
  "OBJ has property VALUE for PROPERTY")
(cl-defmethod occ-obj-intf-has-p ((obj occ-obj-tsk)
                                  (prop symbol)
                                  value)
  "OBJ has property VALUE for PROPERTY"
  (occ-obj-impl-has-p obj
                      prop
                      value))
(cl-defmethod occ-obj-intf-has-p ((obj occ-obj-ctx)
                                  (prop symbol)
                                  value)
  "OBJ has property VALUE for PROPERTY"
  (occ-obj-impl-has-p obj
                      prop
                      value))


(cl-defgeneric occ-obj-intf-require-p (obj
                                       operation
                                       property
                                       values)
  "Used by OCC-OBJ-INTF-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (occ-obj-impl-require-p obj
                          operation
                          property
                          values))


(cl-defgeneric occ-obj-intf-get (ctx
                                 property
                                 arg)
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ.")
  ;; (occ-error "must return occ compatible value.")
(cl-defmethod occ-obj-intf-get ((ctx occ-ctx)
                                (property symbol)
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (occ-obj-impl-get ctx
                    property
                    arg))


(cl-defmethod occ-obj-intf-get ((user occ-user-agent)
                                (property symbol)
                                (ctsk occ-obj-ctx-tsk))
  "Read value of list of elements if (occ-obj-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (occ-obj-impl-get user
                    property
                    ctsk))


(cl-defgeneric occ-obj-intf-propfmt (obj
                                     property
                                     value)
  "Return format printable value of property PROPERTY."
  (ignore obj)
  (ignore property)
  value)
(cl-defmethod occ-obj-intf-propfmt (obj
                                    property
                                    value)
  "Return format printable value of property PROPERTY."
  (occ-obj-impl-propfmt obj property value))


(cl-defgeneric occ-obj-intf-list-p (obj
                                    property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-obj-intf-list-p (obj
                                   property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  (occ-obj-impl-list-p obj
                       property))


(cl-defgeneric occ-obj-intf-to-org (property
                                    value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation.")
(cl-defmethod occ-obj-intf-to-org ((property symbol)
                                   value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation."
  (occ-obj-impl-to-org property
                       value))


(cl-defgeneric occ-obj-intf-from-org (property
                                      value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation.")
(cl-defmethod occ-obj-intf-from-org ((property symbol)
                                     value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  (occ-obj-impl-from-org property
                         value))
(cl-defmethod occ-obj-intf-from-org ((property symbol)
                                     (value string))
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  (occ-obj-impl-from-org property
                         value))


(cl-defmethod occ-obj-intf-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation symbol))
  (occ-obj-impl-values tsk
                       ctx
                       property
                       operation))

(cl-defmethod occ-obj-intf-values ((tsk occ-obj-tsk)
                                   (ctx null)
                                   (property symbol)
                                   (operation symbol))
  (occ-obj-impl-values tsk
                       ctx
                       property
                       operation))


(cl-defgeneric occ-do-intf-operation (obj
                                      operation
                                      prop
                                      value)
  "Do the actual OPERATION.")
(cl-defmethod occ-do-intf-operation (obj
                                     operation
                                     prop
                                     value)
  "Do the actual OPERATION."
  (occ-do-impl-operation obj
                         operation
                         prop
                         value))


(cl-defgeneric occ-obj-intf-checkout-p (obj
                                        prop
                                        value)
  "Return if OBJ support checking-out PROP with VALUE")

(cl-defmethod occ-obj-intf-checkout-p ((obj occ-obj-ctx)
                                       (prop symbol)
                                       value)
  (occ-obj-impl-checkout-p obj
                           prop
                           value))


(cl-defgeneric occ-do-intf-checkout (obj
                                     property
                                     vdirector)
  "Checkout property PROPERTY in case of force clock-in.")
(cl-defmethod occ-do-intf-checkout ((obj occ-obj-tsk)
                                    (property symbol)
                                    vdirector)
  "Checkout property in case of force clock-in."
  (occ-do-impl-checkout obj
                        property
                        vdirector))


(cl-defmethod occ-obj-intf-inheritable-p ((property symbol))
  (occ-obj-impl-inheritable-p property))


;; (org-read-date) (org--deadline-or-schedule arg 'scheduled tim)e

;;; occ-intf.el ends here
