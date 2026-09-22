;;; occ-impl.el --- OCC implementation generics and general default methods  -*- lexical-binding: t; -*-

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

(provide 'occ-impl)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)


(cl-defgeneric occ-obj-impl-rank (tsk
                                  ctx
                                  property)
  "Return the RANK (number) for OBJ based on the property PROPERTY")
(cl-defmethod occ-obj-impl-rank (tsk
                                 ctx
                                 property)
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; too much output
  ;; (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)" (occ-obj-format obj) property)
  0)
(cl-defmethod occ-obj-impl-rank ((tsk occ-obj-tsk)
                                 (ctx occ-obj-ctx)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)"
  ;;            (occ-obj-format obj)
  ;;            property)
  0)
(cl-defmethod occ-obj-impl-rank ((tsk occ-obj-tsk)
                                 (ctx null)
                                 (property symbol))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)" (occ-obj-format obj) property)
  0)


;; adding base propetry NIL for helping to add relative properties
(cl-defmethod occ-obj-impl-rank ((tsk  occ-obj-tsk)
                                 (ctx  occ-obj-ctx)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)"
             (occ-obj-format obj)
             property)
  0)
(cl-defmethod occ-obj-impl-rank ((tsk  occ-obj-tsk)
                                 (ctx  null)
                                 (property (eql nil)))
  "Return the RANK (number) for OBJ based on the property PROPERTY"
  ;; (occ-debug "occ-obj-impl-rank(obj=%s symbol=%s)" (occ-obj-format obj) property)
  0)


(cl-defmethod occ-obj-impl-occ-prop-p ((prop symbol))
  "Return t if PROP is introduced by OCC, else nil"
  nil)


(cl-defgeneric occ-obj-impl-prop= (property
                                   prop-value
                                   value)
  "OBJ has property PROPERTY")
(cl-defmethod occ-obj-impl-prop= ((prop symbol)
                                  prop-value
                                  value)
  "OBJ has property PROPERTY"
  (equal prop-value
         value))


(cl-defgeneric occ-obj-impl-matches (obj
                                     property
                                     value)
  "VALUE equal prop-value of OBJ for PROPERTY")
(cl-defmethod occ-obj-impl-matches ((obj occ-obj-tsk)
                                    (prop symbol)
                                    value)
  "VALUE equal prop-value of OBJ for PROPERTY"
  (let* ((tsk        (occ-obj-tsk obj))
         (tsk-prop-value (occ-obj-get-property tsk prop)))
    (if (occ-obj-list-p obj prop)
        (cl-remove-if-not #'(lambda (pvalue)
                              (occ-obj-prop= prop
                                             pvalue
                                             value))
                          tsk-prop-value)
      (when (occ-obj-prop= prop
                           tsk-prop-value
                           value)
        prop-value))))


(cl-defgeneric occ-obj-impl-has-p (obj
                                   property
                                   value)
  "OBJ has property PROPERTY")
(cl-defmethod occ-obj-impl-has-p ((obj occ-obj-tsk)
                                  (prop symbol)
                                  value)
  "OBJ has property PROPERTY"
  (let* ((tsk        (occ-obj-tsk obj))
         (tsk-prop-value (occ-obj-get-property tsk prop)))
    (if (occ-obj-list-p tsk prop)
        (cl-some #'(lambda (pvalue)
                     (occ-obj-prop= prop
                                    pvalue
                                    value))
                 tsk-prop-value)
      (occ-obj-prop= prop
                     tsk-prop-value
                     value))))
(cl-defmethod occ-obj-impl-has-p ((obj occ-obj-ctx)
                                  (prop symbol)
                                  value)
  "OBJ has property PROPERTY"
  (let* ((ctx            (occ-obj-ctx obj))
         (ctx-prop-value (occ-obj-get-property ctx prop)))
    (if (occ-obj-list-p ctx prop)
        (cl-some #'(lambda (pvalue)
                     (occ-obj-prop= prop
                                    pvalue
                                    value))
                 ctx-prop-value)
      (occ-obj-prop= prop
                     ctx-prop-value
                     value))))


(cl-defgeneric occ-obj-impl-require-p (obj
                                       operation
                                       property
                                       values)
  "Used by OCC-OBJ-IMPL-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
  (ignore obj)
  (ignore operation)
  (ignore property)
  (ignore values)
  (occ-debug "occ-obj-impl-require-p0 is called")
  nil)


(cl-defgeneric occ-obj-impl-get (ctx
                                 property
                                 obj)
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ.")


(cl-defgeneric occ-obj-impl-propfmt (obj
                                     property
                                     value)
  "Return format printable value of property PROPERTY."
  (ignore obj)
  (ignore property)
  value)


(cl-defgeneric occ-obj-impl-list-p (mrk
                                    property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-obj-impl-list-p ((mrk marker)
                                   (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  (occ-debug "occ-obj-impl-list-p: no method for property %s using default."
             property)
  nil)

(cl-defmethod occ-obj-impl-list-p ((ctx occ-obj-ctx)
                                   (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  (occ-debug "occ-obj-impl-list-p: no method for property %s using default."
             property)
  nil)


(cl-defgeneric occ-obj-impl-to-org (property
                                    value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation.")
(cl-defmethod occ-obj-impl-to-org ((property symbol)
                                   value)
  "Return string representation for property PROPERTY, Method
convert value VALUE of property PROPERTY from occ to org string
representation."
  (occ-debug "occ-obj-impl-to-org: no method for property %s using default."
             property)
  value)


(cl-defgeneric occ-obj-impl-from-org (property
                                      value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation.")
(cl-defmethod occ-obj-impl-from-org ((property symbol)
                                     value)
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  (occ-debug "occ-obj-impl-from-org: no method for property %s using default." property)
  value)
(cl-defmethod occ-obj-impl-from-org ((property symbol)
                                     (value string))
  "Return the Actual Object representation for property
PROPERTY, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  (occ-debug "occ-obj-impl-from-org: no method for property %s using default." property)
  value)


(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (property symbol)
                                (ctsk occ-obj-tsk))
  "Read value of list of elements if (occ-obj-impl-list-p PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (ignore obj)
  (occ-error "Implement method occ-obj-impl-readprop-from-user for property %s" property))


(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation symbol))
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property ctx
                                     property)))
    ;; (occ-error "Define for op %s and prop %s"
    ;;            operation
    ;;            property)
    nil))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx null)
                                   (property symbol)
                                   (operation symbol))
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx)))
    ;; (occ-error "Define for op %s and prop %s"
    ;;            operation
    ;;            property)
    nil))


(cl-defgeneric occ-do-impl-operation (obj
                                      operation
                                      property
                                      value)
  "Do the actual OPERATION.")

(cl-defmethod occ-do-impl-operation (obj
                                     operation
                                     property
                                     value)
  ;; NOTE: obj == marker -- present in builtin.el
  "Do the actual OPERATION."
  (occ-error "Implement it for obj=%s, operation %s, property %s, value %s."
             (occ-obj-format obj)
             operation
             property
             value))


(cl-defgeneric occ-obj-impl-checkout-p (obj
                                   prop
                                   value)
  "Return if OBJ support checking-out PROP with VALUE")

(cl-defmethod occ-obj-impl-checkout-p ((obj occ-obj-ctx)
                                       (prop symbol)
                                       value)
  t)


(cl-defgeneric occ-do-impl-checkout (obj
                                     property
                                     vdirector)
  "Checkout property PROPERTY in case of force clock-in.")
(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (property symbol)
                                    (vdirector number))
  "Checkout property in case of force clock-in."
  (ignore obj)
  (occ-error "Implement it for %s: Checkout property in case of force clock-in." property))
(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (property symbol)
                                    (vdirector null))
  "Checkout property in case of force clock-in."
  (ignore obj)
  (occ-error "Implement it for %s: Checkout property in case of force clock-in." property))


(cl-defmethod occ-obj-impl-inheritable-p ((property symbol))
  (occ-error "Define (cl-defmethod occ-obj-impl-inheritable-p ((prop (eql %s))) ... t or nil ...)")
  t)









;; NOTE: These two around methods not belongs to occ-prop-intf.el
;;       they belongs here only.
(cl-defmethod occ-obj-impl-get :around ((user occ-user-agent)
                                        (prop symbol)
                                        (obj  occ-obj-tsk))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (cl-call-next-method)
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-impl-get ((obj occ-obj-tsk) (prop (eql %s)))
  ...)

method provided." prop))))

(cl-defmethod occ-obj-impl-get :around ((user occ-user-agent)
                                        (prop symbol)
                                        (obj  occ-obj-tsk))
  "Read value of element of list for property PROP from user for
OCC-TSK OBJ."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (cl-call-next-method)
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-impl-get ((obj occ-obj-tsk) (prop (eql %s)))
   ...)

method provided." prop))))


(cl-defmethod occ-do-impl-operation :around ((obj  marker)
                                             (operation symbol)
                                             (prop symbol)
                                             value)
  "Around method do necessary setup before actual operation.
accept org compatible VALUE, NOTE only for writable OPERATION
forcing property block creation on org entry.

For write-p OPERATION it ensure to add property drawer if not already present."

  ;; (occ-message "I should be called first in case of MARKER")
  (occ-debug "I should be called first in case of MARKER")
  (lotus-with-marker obj
    ;; (unless (org-get-property-block)
    ;;   ;; create property drawer
    ;;   ;; TODO: NOTE: only create property block if 100% sure value is going to be set.
    ;;   (occ-debug "occ-do-impl-operation[ :around ]: property block not exist. will %s create"
    ;;              (if (occ-obj-op-write-p operation)
    ;;                  "do"
    ;;                "not")))
    (let ((range (org-get-property-block (point)
                                         (when (occ-obj-op-write-p operation)
                                           ;; create property drawer if write operation
                                           'force))))
        (if range
            (let ((start (when (consp range)
                           (1- (car range)))))
              (if (numberp start)
                  (progn
                    ;; Not sure if required?
                    ;; (when (occ-obj-op-write-p operation)
                    ;;   (goto-char start))
                    (occ-debug "occ-do-impl-operation[ :around ]: going to %s prop: %s value: %s using next method."
                               operatin
                               prop
                               (occ-obj-nonocc-format value))
                    (let ((retval (condition-case e ;; if (cl-next-method-p)
                                      (cl-call-next-method obj
                                                           operation
                                                           prop
                                                           (occ-obj-to-org prop
                                                                           operation
                                                                           value))
                                    ((cl-no-next-method) (occ-error (format "No \n%s\n\nmethod provided."
                                                                            (pp-to-string '(cl-defmethod occ-do-impl-operation ((pom marker)
                                                                                                                                (operation (eql %s))
                                                                                                                                (prop (eql %s)) value)
                                                                                             ...)))
                                                                    operation prop)))))
                      (occ-debug "occ-do-impl-operation: (occ-do-impl-operation obj) returned %s" retval)
                      retval))
                (occ-error "Error in range")))
          (when (occ-obj-op-write-p operation)
            (funcall (if (occ-obj-op-write-p operation)
                         #'occ-error
                       #'occ-debug)
                     "occ-do-impl-operation[ :around ]: not able to %s property block to %s property %s: %s"
                     (if (occ-obj-op-write-p operation)
                         "create"
                       "get")
                     operatin
                     prop
                     (occ-obj-nonocc-format value)))))))

;;
;;
;;
;; (org-read-date) (org--deadline-or-schedule arg 'scheduled tim)e

;;; occ-impl.el ends here
