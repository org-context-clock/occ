;;; occ-impl-builtin.el --- OCC implementation generics and general default methods  -*- lexical-binding: t; -*-

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

(provide 'occ-impl-builtin)


(require 'occ-prop-org)


;; (cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
;;                                  (operation (eql get))
;;                                  (prop      symbol)
;;                                  values)
;;   ;; Required by occ-obj-gen-edit-if-required in occ-prop-gen-edit-actions.el#L101
;;   ;; To generate add and delete increment actions
;;   (ignore obj)
;;   (occ-debug "occ-obj-impl-require-p11 prop %s operation %s values %s is called" prop operation values)
;;   nil)

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql add))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (not (occ-obj-has-p obj
                      prop
                      value)))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql put))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (ignore obj)
  (not (occ-obj-has-p obj
                      prop
                      value)))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql remove))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (occ-obj-has-p obj
                 prop
                 value))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-tsk)
                                      (operation (eql delete))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (occ-obj-has-p obj
                 prop
                 value))

(cl-defmethod occ-obj-impl-require-p ((obj       occ-obj-ctx)
                                      (operation (eql checkout))
                                      (prop      symbol)
                                      value)
  "Built in for LIST PROP"
  (and (occ-obj-checkout-p obj prop value)
       (not (occ-obj-has-p obj
                           prop
                           value))))


(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql add)))
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property ctx
                                     property)))
    ;; (occ-message "occ-obj-impl-values[add] value = %s" value)
    (if (occ-obj-op-list-p operation)
        (if (occ-obj-list-p tsk property)
            (if (occ-obj-list-p ctx property)
                value
              (list value)))
      (occ-error "error operation %s" operation))))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql remove)))
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property ctx
                                     property)))
    (if (occ-obj-op-list-p operation)
        (if (occ-obj-list-p tsk property)
            (if (occ-obj-list-p ctx property)
                value
              (list value)))
      (occ-error "error operation %s" operation))))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql put)))
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property ctx
                                     property)))
    (if (not (occ-obj-op-list-p operation))
        (if (not (or (occ-obj-list-p tsk property)
                     (occ-obj-list-p ctx property)))
            value)
      (occ-error "error operation %s" operation))))

(cl-defmethod occ-obj-impl-values ((tsk occ-obj-tsk)
                                   (ctx occ-obj-ctx)
                                   (property symbol)
                                   (operation (eql delete)))
  (let ((tsk   (occ-obj-tsk tsk))
        (ctx   (occ-obj-ctx ctx))
        (value (occ-obj-get-property ctx
                                     property)))
    (if (not (occ-obj-op-list-p operation))
        (if (not (or (occ-obj-list-p tsk property)
                     (occ-obj-list-p ctx property)))
            value)
      (occ-error "error operation %s" operation))))


(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql get))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop GET and VALUES"
  (ignore operation)
  (ignore value)
  (if (occ-obj-list-p pom prop)
      (occ-org-entry-get-multivalued-property pom
                                              prop)
    (list (occ-org-entry-get pom
                             prop))))

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql add))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER for
prop ADD and VALUES"
  (ignore operation)
  (if (occ-obj-list-p pom prop)
      (occ-org-entry-add-to-multivalued-property pom
                                                 prop
                                                 value)
    (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation)))))

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql put))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop PUT and VALUES"
  (ignore operation)
  (if (occ-obj-list-p pom prop)
      (occ-org-entry-put-multivalued-property pom
                                              prop-string
                                              value)
    (occ-org-entry-put pom
                       prop
                       value)))

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql remove))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop REMOVE and VALUES"
  (ignore operation)
  (if (occ-obj-list-p pom prop)
      (occ-org-entry-remove-from-multivalued-property pom
                                                      prop
                                                      value)
    (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation)))))

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql delete))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop REMOVE and VALUES"
  (ignore operation)
  (occ-org-entry-delete pom
                        prop
                        value))

(cl-defmethod occ-do-impl-operation ((pom  marker)
                                     (operation (eql member))
                                     (prop symbol)
                                     value)
  "Org operation implementation of OPERATION on POINT-OF-MARKER
for prop MEMBER and VALUES"
  (ignore operation)
  (if (occ-obj-list-p pom prop)
      (occ-org-entry-member-in-multivalued-property pom
                                                    prop
                                                    value)
    (string= value
             (occ-obj-to-org prop
                             operation
                             (occ-org-entry-get pom
                                                prop)))))


(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql add))
                                     (prop      symbol)
                                     value)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk add): operation %s prop %s" operation prop)
    (if (occ-obj-list-p obj prop)
        (occ-obj-set-property tsk prop
                              ;; (nconc (occ-obj-get-property tsk prop)
                              ;;        (list (cl-first values)))
                              (cons value
                                    (occ-obj-get-property tsk prop)))
      (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))
      ;; (occ-obj-set-property tsk prop
      ;;                       (cl-first values))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql put))
                                     (prop      symbol)
                                     value)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (occ-obj-set-property tsk prop
                          value)))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql remove))
                                     (prop      symbol)
                                     value)
  (let ((tsk (occ-obj-tsk obj)))
    (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
    (if (occ-obj-list-p obj prop)
        (occ-obj-set-property tsk prop
                              ;; (remove (cl-first values)
                              ;;         (occ-obj-get-property tsk prop))
                              (remove value
                                      (occ-obj-get-property tsk prop)))
      (occ-error "Property `%s' is type of LIST, %s operation not applied to it." prop (upcase (symbol-name operation))))))

(cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
                                     (operation (eql delete))
                                     (prop      symbol)
                                     value)
  (occ-obj-set-property tsk prop nil))

;; (cl-defmethod occ-do-impl-operation ((obj       occ-obj-tsk)
;;                                  (operation (eql member))
;;                                  (prop      symbol)
;;                                  values)
;;   (let ((tsk (occ-obj-tsk obj)))
;;     (occ-debug "(occ-do-impl-operation occ-obj-tsk): operation %s prop %s" operation prop)
;;     (occ-obj-has-p tsk prop
;;                    values)))


(define-skeleton occ-property-method-gen
  "Generate method for occ property"
  "Property name: ")


(defvar occ-property-method-skeleton
  '(
    (cl-defmethod occ-obj-impl-occ-prop-p ((prop (eql PROPERTY)))
      t)
    (cl-defmethod occ-obj-impl-prop= ((prop (eql PROPERTY))
                                      prop-value
                                      value)
      (occ-pu-string= prop-value
                      value))
    (cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                    (prop (eql PROPERTY))
                                    (arg null))
      "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
      (ignore prop)
      (occ-obj-get-property ctx
                            prop))
    (cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                    (prop (eql PROPERTY))
                                    (obj occ-obj-ctx-tsk))
      "Read value of list of elements if (occ-obj-list-p OBJ PROPERTY)
        else element for property PROPERTY from user for OCC-TSK OBJ,
        must return ORG compatible value."
      (read-from-minibuffer (format "%s: " prop)))

    (cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                        (prop (eql PROPERTY))
                                        (vdirector number))
      (require 'magit-git)
      (let* ((tsk        (occ-obj-tsk obj))
             (prop-value (occ-obj-pvalue tsk
                                         prop
                                         vdirector)))
        (when prop-value
          (find-file prop-value))))

    (cl-defmethod occ-obj-impl-rank ((tsk occ-obj-tsk)
                                     (ctx occ-obj-ctx)
                                     (prop (eql PROPERTY)))
      "Return the RANK (number) for OCC-TSK based on the property PROPERTY"
      (occ-aggregate-rank tsk-PROPERTY PROPERTY tsk #'max
        (if (occ-obj-prop= PROPERTY
                           tsk-PROPERTY
                           (occ-obj-get ctx nil PROPERTY nil))
            (occ-rank-percentage 100)
          (occ-rank-percentage 0))))
    (cl-defmethod occ-obj-impl-propfmt ((obj occ-obj-tsk)
                                        (prop (eql PROPERTY))
                                        value)
      "Return format printable value of property PROPERTY."
      (format "%s" value))
    (cl-defmethod occ-obj-impl-list-p ((mrk marker)
                                       (prop (eql PROPERTY)))
      "Is the property PROPERTY has VALUES in list, Method tell
         property represent list or not."
      t)
    (cl-defmethod occ-obj-impl-to-org ((prop (eql PROPERTY))
                                       value)
      "Return string representation for property PROPERTY, Method
      convert value VALUE of property PROPERTY from occ to org string
      representation."
      (format "%s" value))
    (cl-defmethod occ-obj-impl-from-org ((prop (eql PROPERTY)
                                               value))
      "Return the Actual Object representation for property
      PROPERTY, Method convert value VALUE of property PROPERTY from
      org string to occ representation."
      (unless (string= value "")
        value))
    (cl-defmethod occ-obj-impl-inheritable-p ((prop (eql PROPERTY)))
      t)
    (cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
                                          (operation (eql _operation_))
                                          (prop (eql PROPERTY))
                                          values)
      "Used by OCC-OBJ-GEN-EDIT-IF-REQUIRED to decide for this property
      PROPERTY if CALLABLE (helm method) should be generated."
      nil)
    ))


;; (pp-to-string (car occ-property-method-skeleton))

;; (s-trim-right (pp-to-string (car occ-property-method-skeleton)))

;; (helpful--syntax-highlight
;;  (helpful--pretty-print (nth 4 occ-property-method-skeleton)) 'emacs-lisp-mode)




;;; occ-impl-builtin.el ends here
