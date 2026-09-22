;;; occ-prop-op-edit.el --- property edit code          -*- lexical-binding: t; -*-

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

(provide 'occ-prop-op-edit)


(require 'occ-prop-base)
(require 'occ-property-editor)
(require 'occ-assert)
(eval-when-compile
 (require 'occ-macros))
(require 'occ-impl-builtin)


(cl-defgeneric occ-obj-select-operation (obj
                                         prop)
  "occ-obj-select-operation")

;; TODO: Add log not on property editing.
(cl-defmethod occ-obj-select-operation ((obj  occ-obj-tsk)
                                        (prop symbol)
                                        &optional
                                        value)
  (occ-assert prop)
  (if (occ-obj-list-p obj prop)
      ;; TODO: where are generated actions?? (occ-obj-operations-for-prop 'occ-obj-tsk 'root)
      (let* ((operations (occ-obj-operations-for-prop obj
                                                      prop))
             ;; (operations (cl-remove-if #'(lambda (op) (occ-obj-iXntf-require-p obj op prop value))
             ;;                           (occ-obj-operations-for-prop obj
             ;;                                                        prop)))

             ;; (actions '(("add" . add)
             ;;            ("del" . remove)
             ;;            ("put" . put)))
             (existing-value (occ-obj-get-property obj prop))
             (actions        (mapcar #'(lambda (op)
                                         (cons (format "%s [%s]" (symbol-name op) existing-value) op))
                                     operations)))

        (occ-assert actions)
        (let ((action  (completing-read (format "%s [%s] action: " prop existing-value) actions)))
          (occ-assert action)
          (cl-rest (assoc action
                          actions))))
    'put))


(cl-defgeneric occ-do-op-prop-edit (obj
                                    prop
                                    operation
                                    value)
  "Accept occ compatible VALUES")

(cl-defmethod occ-do-op-prop-edit ((obj  occ-obj-ctx-tsk) ;; occ-obj-tsk
                                   (prop symbol)
                                   (operation symbol)
                                   value)
  "Accept occ compatible VALUES"
  (occ-debug "occ-do-op-prop-edit: prop: %s, value: %s" prop value)
  (occ-assert prop)
  (occ-assert operation)
  (occ-debug "(occ-do-op-prop-edit occ-obj-tsk): operation %s prop %s" operation prop)
  (occ-do-operation obj
                    operation
                    prop
                    ;; FIXED: TODO - add, remove use VALUE of add, use PROP-VALUE for remove . -- (occ-intf-match operation value prop ) -- pass operation to return value or matched value
                    (occ-obj-operation-value (occ-obj-tsk obj)
                                             prop
                                             operation
                                             value)))

(cl-defmethod occ-do-op-prop-edit ((obj  occ-obj-ctx-tsk) ;; occ-obj-tsk
                                   (prop symbol)
                                   (operation null)
                                   value)
  (let* ((operation  (or operation
                         (occ-obj-select-operation obj prop value)))
         (prop-value (or value
                         (occ-obj-get (occ-get-user-agent)
                                      obj
                                      prop
                                      operation))))
    (cl-call-next-method obj
                         prop
                         operation
                         ;; FIXED: TODO - add, remove use VALUE of add, use PROP-VALUE for remove . -- (occ-intf-match operation value prop ) -- pass operation to return value or matched value
                         prop-value)))

;; Usage not implemented
;; (occ-do-op-props-edit obj '(timebeing add 10)) in occ-obj-try-fast-clock-in and occ-obj-try-until-associable-p

(cl-defgeneric occ-do-op-props-edit (obj)
  "Edit all property for forced clock-in.")

;; (cl-defmethod occ-do-op-props-edit ((obj occ-obj-tsk))
;;   "Misc all property for forced clock-in."
;;   (dolist (prop (occ-obj-properties-to-edit obj))
;;     (occ-debug "occ-do-op-props-edit: checkout prop %s" prop)
;;     (occ-do-op-prop-edit obj
;;                          prop)))
(cl-defmethod occ-do-op-props-edit ((obj occ-obj-tsk))
  "Edit all property for forced clock-in."
  ;; (debug)
  ;; (occ-properties-editor obj)
  ;; (occ-do-properties-editor-combined obj)
  (occ-do-properties-window-editor obj))

;;; occ-prop-op-edit.el ends here
