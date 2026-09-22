;;; occ-prop.el --- occ properties methods           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

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

;; https://www.gnu.org/software/emacs/manual/html_node/eieio/Quick-Start.html#Quick-Start
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html

;; The type specializer, (arg type), can specify one of the system types in the
;; following list. When a parent type is specified, an argument whose type is
;; any of its more specific child types, as well as grand-children,
;; grand-grand-children, etc. will also be compatible.
;;
;; integer (Parent type: number.)
;; number
;; null (Parent type: symbol.)
;; symbol
;; string (Parent type: array.)
;; array (Parent type: sequence.)
;; cons (Parent type: list.)
;; list (Parent type: sequence.)
;; marker
;; overlay
;; float (Parent type: number.)
;; window-configuration
;; process
;; window
;; subr
;; compiled-function
;; buffer
;; char-table (Parent type: array.)
;; bool-vector (Parent type: array.)
;; vector (Parent type: array.)
;; frame
;; hash-table
;; font-spec
;; font-entity
;; font-object


;;; Code:

(provide 'occ-prop-base)


(require 'dash)
;; (require 'subr)
(require 'org-misc-utils-lotus)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-cl-utils)
(require 'occ-obj-common)
(require 'occ-tree)
(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-obj-method)
(require 'occ-rank)
(require 'occ-prop-intf)
(require 'occ-intf)
(require 'occ-prop-org)
(require 'occ-assert)


(require 'ert)
(require 'ert-x)
(require 'el-mock)


;; TODO: multi-value property https://orgmode.org/manual/Using-the-property-API.html


(ert-deftest ert-occ-test-match-prop-method-args ()
  "Test"
  :expected-result :passed
  :tags '(occ)
  (should (equal (occ-obj-cl-method-sigs-matched-arg '(occ-obj-impl-get (`(occ-user-agent (eql ,val) occ-obj-ctx-tsk) val))
                                                     '(occ-obj-impl-get (`(occ-ctx (eql ,val) null) val))
                                                     (occ-obj-make-ctx-at-point))
                 '(timebeing)))
  ;; do this test in buffer of a temporary file.
  (should (equal (occ-obj-cl-method-sigs-matched-arg '(occ-obj-impl-get (`(occ-user-agent (eql ,val) occ-obj-ctx-tsk) val))
                                                     '(occ-obj-impl-get (`(occ-ctx (eql ,val) null) val))
                                                     `(,(occ-obj-make-ctx-at-point) val nil))
                 '(timebeing root currfile))))


;; (cl-defmethod occ-obj-cl-method-sigs-matched-arg ((method-sig1 cons)
;;                                                   (method-sig2 cons)
;;                                                   (args cons))
;;   (let ((slots (occ-cl-method-param-case-with-value-new method-sig2 args)))
;;     (cl-remove-if-not #'(lambda (arg) (memq arg slots))
;;                       (occ-cl-method-param-case method-sig1))))

;; (occ-cl-method-param-case-with-value-new  '(occ-obj-impl-get (`(occ-ctx (eql ,val) null) val)) '((occ-obj-make-ctx-at-point) val nil))


;; (occ-cl-method-param-signs 'occ-obj-impl-get)
;; ((occ-user-agent symbol occ-obj-tsk) (occ-ctx (eql git-branch) null) (occ-user-agent (eql timebeing) occ-tsk) (occ-user-agent (eql root) occ-obj-ctx-tsk) (occ-ctx (eql root) null) (occ-user-agent (eql currfile) occ-obj-ctx-tsk) (occ-ctx (eql currfile) null) (occ-user-agent symbol occ-obj-tsk))



(cl-defgeneric occ-do-add-ineq (property
                                ineq)
  "Add ineq for property PROPERTY.")
(cl-defmethod occ-do-add-ineq ((property symbol)
                               (ineq list))
  "Add ineq for property PROPERTY."
  (occ-do-add-ineq-internal property ineq))
(cl-defmethod occ-do-add-ineq ((property symbol)
                               (ineq string))
  "Add ineq for property PROPERTY."
  (occ-do-add-ineq-internal property
                            ineq))

(cl-defgeneric occ-obj-ineq (property)
  "Add ineq for property PROPERTY.")
(cl-defmethod occ-obj-ineq ((property symbol))
  "Add ineq for property PROPERTY."
  (occ-obj-ineq-internal property))

(cl-defgeneric occ-obj-priority (property)
  "Add ineq for property PROPERTY.")
(cl-defmethod occ-obj-priority ((property symbol))
  "Add ineq for property PROPERTY."
  (occ-obj-priority-internal property))


(cl-defgeneric occ-obj-priority-rank (tsk
                                      ctx
                                      prop)
  "Get prioritised rank.")

(cl-defmethod occ-obj-priority-rank ((obj number)
                                     (ctx null)
                                     (prop symbol))
  (let ((value    obj)
        (priority (occ-obj-priority prop)))
    (unless priority
      (occ-error "Priority is not present for property %s" prop))
    (* priority
       value)))

(cl-defmethod occ-obj-priority-rank ((obj number)
                                     (ctx null)
                                     (prop symbol))
  (let ((value obj)
        (priority (occ-obj-priority prop)))
    (if priority
        (* priority
           value)
      0)))

(cl-defmethod occ-obj-priority-rank ((tsk occ-obj-tsk)
                                     (ctx occ-obj-ctx)
                                     (prop symbol))
  "Get prioritised rank."
  (occ-obj-priority-rank (occ-obj-intf-rank tsk
                                            ctx
                                            prop)
                         nil
                         prop))
(cl-defmethod occ-obj-priority-rank ((tsk occ-obj-tsk)
                                     (ctx null)
                                     (prop symbol))
  "Get prioritised rank."
  (occ-obj-priority-rank (occ-obj-intf-rank tsk
                                            ctx
                                            prop)
                         nil
                         prop))


(cl-defgeneric occ-obj-properties-to-edit (obj)
  "return PROPERTIES list that can be edited.")
;; (cl-defgeneric occ-obj-properties-to-inherit (obj)
;;   "return PROPERTIES list that can be inherited.")
(cl-defgeneric occ-obj-properties-for-ranking (obj)
  "return PROPERTIES list that can be inherited.")
(cl-defgeneric occ-obj-properties-to-calculate-rank (obj1 obj2)
  "return PROPERTIES list that can be used in calculating rank.")
(cl-defgeneric occ-obj-properties-to-checkout (obj)
  "return PROPERTIES list that can be checked-out.")


;; TODO: occ-prop-base.el: Warning: ‘cl-next-method-p’ is an obsolete macro (as of 25.1); make sure there’s always a next method, or catch ‘cl-no-next-method’ instead [7 times]
(cl-defmethod occ-obj-properties-to-edit :around (obj)
  "return PROPERTIES list that can be edited."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-to-edit (obj)
  ...)

method provided."))))
;; (cl-defmethod occ-obj-properties-to-inherit :around (obj)
;;   "return PROPERTIES list that can be inherited."
;;   (ignore obj)
;;   (condition-case e ;; if (cl-next-method-p)
;;       (occ-internal-remove-template-symbol (cl-call-next-method))
;;     ((cl-no-next-method) (occ-error "No
;; (cl-defmethod occ-obj-properties-to-inherit (obj)
;;    ...)
;; 
;; method provided."))))

(cl-defmethod occ-obj-properties-for-ranking :around (obj)
  "return PROPERTIES list that can be inherited."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-for-ranking (obj)
   ...)

method provided."))))
(cl-defmethod occ-obj-properties-to-calculate-rank :around (obj1 obj2)
  "return PROPERTIES list that can be used in calculating rank."
  (ignore obj1 obj2)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-to-calculate-rank (obj1 obj2)
  ...)

method provided."))))
(cl-defmethod occ-obj-properties-to-checkout :around (obj)
  "return PROPERTIES list that can be checked-out."
  (ignore obj)
  (condition-case e ;; if (cl-next-method-p)
      (occ-internal-remove-template-symbol (cl-call-next-method))
    ((cl-no-next-method) (occ-error "No
(cl-defmethod occ-obj-properties-to-checkout (obj)
  ...)

method provided."))))


(cl-defmethod occ-obj-properties-to-edit ((class symbol))
  "return PROPERTIES list that can be edited."
  (occ-cl-method-param-values 'occ-obj-impl-get
                              (list '\` `(occ-user-agent (eql ,'(\, val)) ,class))
                              'val))

(cl-defmethod occ-obj-properties-to-edit ((obj occ-tsk))
  "return PROPERTIES list that can be edited."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-edit
                             obj))

;; TODO: improve
(cl-defmethod occ-obj-properties-to-edit ((obj occ-obj-ctx-tsk))
  "return PROPERTIES list that can be edited."
  (occ-obj-cl-method-sigs-matched-arg '(occ-obj-impl-get   (`(occ-user-agent (eql ,val) occ-obj-ctx-tsk) val))
                                      '(occ-obj-impl-get   (`(occ-ctx        (eql ,val) null)            val))
                                      `(,(occ-obj-ctx obj) val nil)))


;; (cl-defmethod occ-obj-properties-to-inherit ((class symbol))
;;   "return PROPERTIES list that can be inherited."
;;   (occ-cl-method-param-values 'occ-obj-impl-get
;;                               (list '\` `(occ-user-agent (eql ,'(\, val)) ,class))
;;                               'val))

;; (cl-defmethod occ-obj-properties-to-inherit ((obj occ-obj-tsk))
;;   "return PROPERTIES list that can be inherited."
;;   (occ-cl-collect-on-classes #'occ-obj-properties-to-inherit
;;                              obj))

;; (cl-defmethod occ-obj-properties-to-inherit ((obj null))
;;   "return PROPERTIES list that can be inherited."
;;   (occ-obj-properties-to-inherit 'occ-obj-ctx-tsk))


;; (defun occ-obj-properties-to-inherit-ctsk () ;;TODO: check about them
;;   "return PROPERTIES list that can be inherited."
;;   (occ-obj-properties-to-inherit 'occ-obj-ctx-tsk))


(cl-defmethod occ-obj-properties-for-ranking ((class symbol))
  "return PROPERTIES list that can be inherited."
  (occ-cl-method-param-values 'occ-obj-impl-get
                              (list '\` `(occ-user-agent (eql ,'(\, val)) ,class))
                              'val))

(cl-defmethod occ-obj-properties-for-ranking ((obj occ-obj-tsk))
  "return PROPERTIES list that can be inherited."
  (occ-cl-collect-on-classes #'occ-obj-properties-for-ranking
                             obj))

(cl-defmethod occ-obj-properties-for-ranking ((obj null))
  "return PROPERTIES list that can be inherited."
  (occ-obj-properties-for-ranking 'occ-obj-ctx-tsk))


(defun occ-obj-properties-for-ranking-ctsk () ;;TODO: check about them
  "return PROPERTIES list that can be inherited."
  (occ-obj-properties-for-ranking 'occ-obj-ctx-tsk))


(cl-defmethod occ-obj-properties-to-calculate-rank ((class1 symbol)
                                                    (class2 symbol))
  "return PROPERTIES list that can be used in calculating rank."
  (occ-cl-method-param-values 'occ-obj-impl-rank
                              (list '\` `(,class1 ,class2 (eql ,'(\, val))))
                              'val))

(cl-defmethod occ-obj-properties-to-calculate-rank ((tsk occ-obj-tsk)
                                                    (ctx occ-obj-ctx))
  "return PROPERTIES list that can be used in calculating rank."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-calculate-rank ;; occ-properties-to-calcuate-rank
                             tsk
                             ctx))

(cl-defmethod occ-obj-properties-to-calculate-rank ((tsk occ-obj-tsk)
                                                    (ctx null))
  "return PROPERTIES list that can be used in calculating rank."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-calculate-rank ;; occ-properties-to-calcuate-rank
                             tsk
                             ctx))


(cl-defmethod occ-obj-properties-to-checkout ((class symbol))
  "return PROPERTIES list that can be checked-out."
  (occ-cl-method-param-values 'occ-do-impl-checkout ;NOTE: user have to define them for each properties.
                              (list '\` `(,class (eql ,'(\, val)) ,'(\, _)))
                              'val))
;; (occ-cl-method-param-signs 'occ-do-impl-checkout)
;; (occ-obj-properties-to-checkout 'occ-obj-tsk)
;;
;; (pcase '(a b c)
;;   (`(a ,val ,z) val)
;;   (_ nil))

;; (let ((class 'CLASS))
;;  (list '\` `(,class (eql ,'(\, val)) ,'(\, _))))

;; (let ((class 'CLASS))
;;   `(,class (eql ,'(\, 'val)) _))

(cl-defmethod occ-obj-properties-to-checkout ((obj occ-obj-tsk))
  "return PROPERTIES list that can be checked-out."
  (occ-cl-collect-on-classes #'occ-obj-properties-to-checkout
                             obj))


(cl-defmethod occ-obj-operations-for-prop ((class symbol)
                                           (prop  symbol))
  ;; check about (occ-obj-list-p prop) also
  (let ((ops (append (occ-cl-method-param-values 'occ-do-impl-operation
                                                 (list '\` `(,class (eql ,'(\, val)) symbol t))
                                                 'val)
                     (occ-cl-method-param-values 'occ-do-impl-operation
                                                 (list '\` `(,class (eql ,'(\, val)) (eql ,prop) t))
                                                 'val))))
    ;; BUG: TODO: inelegant solution fix it,
    (cl-remove-if-not #'(lambda (op)
                          (or (occ-obj-list-p nil
                                              prop)
                              (not (memq op '(add remove)))))
                  (delete-dups ops))))

(cl-defmethod occ-obj-operations-for-prop ((obj  occ-obj-tsk)
                                           (prop symbol))
  ;; check about (occ-obj-list-p prop) also
  (let ((ops (occ-cl-collect-on-classes #'(lambda (class)
                                            (occ-obj-operations-for-prop class
                                                                         prop))
                                    obj)))
    (delete-dups ops)))

(cl-defmethod occ-obj-operations-for-prop ((obj  marker)
                                           (prop symbol))
  ;; check about (occ-obj-list-p prop) also
  (let ((ops (occ-cl-collect-on-classes #'(lambda (class)
                                            (occ-obj-operations-for-prop class
                                                                         prop))
                                        obj)))
    (delete-dups ops)))


;; (occ-testing
;;  (occ-obj-operations-for-prop 'occ-obj-tsk 'currfile)
;;  (occ-obj-operations-for-prop 'occ-obj-tsk 'root)
;;  (occ-obj-operations-for-prop 'occ-obj-tsk 'git-branch)
;;  (occ-obj-list-p nil 'git-branch)
;;  (occ-obj-operations-for-prop 'occ-obj-tsk 'current-clock)
;;  (occ-obj-operations-for-prop 'occ-obj-tsk 'key)
;;  (occ-obj-operations-for-prop 'occ-obj-tsk 'status)
;;  (occ-obj-operations-for-prop 'occ-obj-tsk 'subtree)

;;  (occ-cl-collect-on-classes #'occ-obj-operations-org-operation
;;                             (point-marker))

;;  (occ-obj-operations-for-prop (make-marker) 'git-branch)
;;  (occ-obj-operations-for-prop (make-marker) 'root)
;;  (occ-obj-operations-for-prop (occ-get-debug-obj) 'root))




(defun occ-internal-remove-template-symbol (prop-list)
  (cl-remove-if #'(lambda (prop)
                    (string-match "^_.+_$" (symbol-name prop)))
                prop-list))


;; (cl-defmethod occ-obj-rereadprop-value ((prop symbol)
;;                                         value)
;;   "Read org string property PROP to occ representation."
;;   (occ-assert (not (consp value)))
;;   (if (occ-obj-list-p nil
;;                       prop)
;;       (let* ((values (and value (split-string value))))
;;         (mapcar #'(lambda (v)
;;                     ;; from Org world to Occ world
;;                     (occ-obj-intf-from-org prop
;;                                            v))
;;                 (mapcar #'org-entry-restore-space
;;                         values)))
;;     (occ-obj-intf-from-org prop
;;                            value)))

;; (cl-defmethod occ-obj-reread-props ((obj occ-tsk))
;;   "Read all org string properties for task TSK to occ representation."
;;   (let ((props-by-is-list   (occ-cl-method-param-case '(occ-obj-impl-list-p (`(marker (eql ,val)) val))))
;;         (props-by-converter (occ-cl-method-param-case '(occ-obj-impl-from-org (`((eql ,val) t) val)))))
;;     ;; BUG: TODO: try to check on all properties and check if they are list-p or not.
;;     (let ((props (-union props-by-is-list
;;                          props-by-converter))) ;dash
;;       (dolist (p props)
;;         (let* ((value         (occ-obj-get-property obj p))
;;                (rearead-value (occ-obj-rereadprop-value p value)))
;;           (occ-obj-set-property obj p
;;                                 rearead-value))))))

;; (cl-defmethod occ-obj-reread-props :around (obj)
;;   "return PROPERTIES list that can be checked-out."
;;   (ignore obj)
;;   (condition-case e ;; if (cl-next-method-p)
;;       (occ-internal-remove-template-symbol (cl-call-next-method))
;;     ((cl-no-next-method) (occ-error "No
;; (cl-defmethod occ-obj-reread-props (obj)
;;   ...)

;; method provided."))))


;; (cl-defmethod occ-obj-operation-valid-p ((obj       occ-obj-tsk)
;;                                          (operation symbol)
;;                                          (prop      symbol))
;;   ;; (memq operation
;;   ;;       '(add remove get put member))
;;   (ignore prop)
;;   (memq operation
;;         (occ-obj-operations-for-prop obj
;;                                      prop)))

(cl-defmethod occ-obj-operation-valid-p ((tsk occ-obj-tsk)
                                         (ctx occ-obj-ctx)
                                         (property symbol)
                                         (operation symbol))
  (when (memq operation
              (occ-obj-operations-for-prop obj
                                           property))
   (or (and (not (occ-obj-list-p 'operation operation))
            (and (occ-obj-list-p (occ-obj-tsk tsk) property)
                 (occ-obj-list-p (occ-obj-ctx ctx) property))
            (not (or (occ-obj-list-p (occ-obj-tsk tsk) property)
                     (occ-obj-list-p (occ-obj-ctx ctx) property))))
       (and (occ-obj-list-p 'operation operation)
            (occ-obj-list-p (occ-obj-tsk tsk) property)
            (not (occ-obj-list-p (occ-obj-ctx ctx) property))))))

(cl-defmethod occ-obj-operation-valid-p ((tsk occ-obj-tsk)
                                         (ctx null)
                                         (property symbol)
                                         (operation symbol))
  (when (memq operation
              (occ-obj-operations-for-prop obj
                                           property))
    (or (and (not (occ-obj-list-p 'operation operation))
             (and (occ-obj-list-p (occ-obj-tsk tsk) property)
                  (occ-obj-list-p (occ-obj-ctx ctx) property))
             (not (or (occ-obj-list-p (occ-obj-tsk tsk) property)
                      (occ-obj-list-p (occ-obj-ctx ctx) property))))
        (and (occ-obj-list-p 'operation operation)
             (occ-obj-list-p (occ-obj-tsk tsk) property)
             (not (occ-obj-list-p (occ-obj-ctx ctx) property))))))


(occ-testing
 (occ-obj-operations-for-prop 'occ-obj-tsk 'currfile)
 (occ-obj-operations-for-prop 'occ-obj-tsk 'root)
 (occ-obj-operations-for-prop 'occ-obj-tsk 'git-branch)
 (occ-obj-operations-for-prop 'occ-obj-tsk 'current-clock)
 (occ-obj-operations-for-prop 'occ-obj-tsk 'key)
 (occ-obj-operations-for-prop 'occ-obj-tsk 'status)
 (occ-obj-operations-for-prop 'occ-obj-tsk 'subtree))
(cl-defmethod occ-obj-map ((tsk occ-obj-tsk)
                           (ctx occ-obj-ctx)
                           (property symbol)
                           (operation symbol)
                           func)
  (mapcar func
          (occ-obj-values tsk
                          ctx
                          property
                          operation)))
(cl-defmethod occ-obj-map ((tsk occ-obj-tsk)
                           (ctx null)
                           (property symbol)
                           (operation symbol)
                           func)
  (mapcar func
          (occ-obj-values tsk
                          ctx
                          property
                          operation)))


(cl-defmethod occ-obj-inheritable-p ((property symbol))
  (occ-obj-intf-inheritable-p property))

(cl-defmethod occ-obj-inheritable ((properties list))
  (cl-remove-if-not #'occ-obj-inheritable-p
                    properties))

(cl-defmethod occ-obj-nonheritable ((properties list))
  (cl-remove-if #'occ-obj-inheritable-p
                properties))


(cl-defmethod occ-obj-occ-prop-p ((prop symbol))
  (occ-obj-intf-occ-prop-p prop))


(cl-defmethod occ-do-operation ((obj       marker)
                                (operation symbol)
                                (prop      symbol)
                                value)
  (occ-do-intf-operation obj
                         operation
                         prop
                         value))
(cl-defmethod occ-do-operation ((obj       occ-obj-tsk)
                                (operation symbol)
                                (prop      symbol)
                                value)
  (prog1
    (occ-do-intf-operation obj
                           operation
                           prop
                           value)
    (when (occ-obj-op-write-p operation)
      (occ-obj-reset-prop-rank obj
                               prop))))
(cl-defmethod occ-do-operation ((obj       occ-obj-ctx)
                                (operation symbol)
                                (prop      symbol)
                                value)
  (occ-error "OCC-CTX is read only, can not be changed"))

(cl-defmethod occ-do-operation :around ((obj       occ-obj-tsk)
                                        (operation symbol)
                                        (prop      symbol)
                                        value)
  "Accept occ compatible VALUES"
  (occ-message "I should be called FIRST.")
  (occ-debug "(occ-do-intf-operation occ-obj-tsk): operation %s prop %s" operation prop)
  ;; BUG: TODO: inelegant solution fix it,
  ;; Valid operation require.
  ;; 1. occ-do-operation on mark
  ;; 2. operation to set and get value from ORG file.
  ;; 3. if both are present then it could be done.
  ;; -- I guess

  ;;  Not check just continue, correct later
  (if t ;; (occ-obj-operation-valid-p (occ-obj-tsk obj);; BUG: TODO: inelegant solution fix it,
        ;;                          (occ-obj-ctx obj)
        ;;                          operation
        ;;                          prop)
      ;; (if (cl-call-next-method (occ-obj-marker obj)
      ;;                          operation
      ;;                          prop
      ;;                          value)
      ;;     (cl-call-next-method)
      ;;   (occ-error "Failed to %s on marker %s of %s in org world"
      ;;              operation
      ;;              (occ-obj-marker obj)
      ;;              (occ-obj-Format obj)))


    (if (occ-do-operation (occ-obj-marker obj)
                          operation
                          prop
                          value)
        (cl-call-next-method)
      (occ-error "Failed to %s on marker %s of %s in org world"
                 operation
                 (occ-obj-marker obj)
                 (occ-obj-Format obj)))
    (occ-error "operation %s not allowed for prop %s"
               operation
               prop)))


(cl-defmethod occ-obj-propfmt (obj
                               property
                               value)
  "Return format printable value of property PROPERTY."
  (occ-obj-intf-propfmt obj
                        property
                        value))

;; (cl-defgeneric occ-obj-impl-list-p (property)
(cl-defgeneric occ-obj-list-p (mrk
                               property)
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not.")

(cl-defmethod occ-obj-list-p ((mrk marker)
                              (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  (occ-obj-intf-list-p (occ-obj-marker mrk)
                       property))

(cl-defmethod occ-obj-list-p ((mrk null)
                              (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  (occ-obj-list-p (make-marker)
                  property))

(cl-defmethod occ-obj-list-p ((tsk occ-obj-tsk)
                              (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  (occ-obj-list-p (occ-obj-marker (occ-obj-tsk tsk))
                  property))

(cl-defmethod occ-obj-list-p ((ctx occ-obj-ctx)
                              (property symbol))
  "Is the property PROPERTY has VALUES in list, Method tell
   property represent list or not."
  (occ-obj-intf-list-p (occ-obj-ctx ctx)
                       property))


(cl-defmethod occ-obj-op-list-p ((operation symbol))
  nil)
(cl-defmethod occ-obj-op-list-p ((operation (eql add)))
  t)
(cl-defmethod occ-obj-op-list-p ((operation (eql remove)))
  t)

(cl-defmethod occ-obj-op-delete-p ((operation symbol))
  nil)
(cl-defmethod occ-obj-op-delete-p ((operation symbol))
  nil)
(cl-defmethod occ-obj-op-delete-p ((operation (eql remove)))
  t)
(cl-defmethod occ-obj-op-delete-p ((operation (eql delete)))
  t)

(cl-defmethod occ-obj-op-write-p ((operation symbol))
  t)
(cl-defmethod occ-obj-op-write-p ((operation (eql get)))
  nil)
(cl-defmethod occ-obj-op-write-p ((operation (eql member)))
  nil)


(cl-defmethod occ-obj-get ((user occ-user-agent)
                           (ctsk occ-obj-ctx-tsk)
                           (property symbol)
                           (operation symbol))
  (if (occ-obj-list-p (occ-obj-tsk ctsk)
                      property)
      ;; (mapcar #'(lambda (v)
      ;;             (occ-obj-intf-get user
      ;;                               ctsk
      ;;                               prop))
      ;;         value)
      (occ-error "Implemention to read list of value for property from user not done. property: %s, operation %s" property (upcase (symbol-name operation)))
    (occ-obj-intf-get user
                      property
                      ctsk)))
(cl-defmethod occ-obj-get ((user occ-user-agent)
                           (ctsk occ-obj-ctx-tsk)
                           (property symbol)
                           (operation (eql add)))
  (if (occ-obj-list-p (occ-obj-tsk ctsk)
                      property)
      (occ-obj-intf-get user
                        property
                        ctsk)
    (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
               property
               (upcase (symbol-name operation)))))
(cl-defmethod occ-obj-get ((user occ-user-agent)
                           (ctsk occ-obj-ctx-tsk)
                           (property symbol)
                           (operation (eql remove)))
  (if (occ-obj-list-p (occ-obj-tsk ctsk)
                      property)
      (occ-obj-intf-get user
                        property
                        ctsk)
    (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
               property
               (upcase (symbol-name operation)))))
(cl-defmethod occ-obj-get ((user occ-user-agent)
                           (ctsk occ-obj-ctx-tsk)
                           (property symbol)
                           (operation null))
  (cl-call-next-method))


(cl-defmethod occ-obj-get ((ctx occ-ctx)
                           (dummy null)
                           (property symbol)
                           (operation null))
  "It done by OCC-OBJ-INTF-GET which has implementations ares in
  property-methods it handle simple full property extraction from
  context ctx for mult/single OCC-OBJ-VALUES calls
  OCC-OBJ-GET-PROPERTY in impl.el file OCC-OBJ-GET-PROPERTY
  finally rely on OCC-OBJ-GET"
  ;; (occ-error "Find where it gets called")
  (occ-obj-intf-get ctx
                    property
                    nil))


(defun occ-org-list-value-to-org (value)
  (string-join value))

(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation symbol)
                              value)
  (if (occ-obj-list-p nil
                      property)
      (occ-org-list-value-to-org (mapcar #'(lambda (v)
                                             (occ-obj-intf-to-org prop v))
                                         value))
    (occ-obj-intf-to-org property
                         value)))
(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation symbol)
                              value)
  (if (occ-obj-list-p nil
                      property)
      (if (occ-obj-op-list-p operation)
          (occ-obj-intf-to-org property
                               value)
          (occ-org-list-value-to-org (mapcar #'(lambda (v)
                                                 (occ-obj-intf-to-org prop v))
                                             value)))
    (if (occ-obj-op-list-p operation)
        (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
                   property
                   (upcase (symbol-name operation)))
      (occ-obj-intf-to-org property
                           value))))
(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation (eql add))
                              value)
  (if (occ-obj-list-p nil
                      property)
      (occ-obj-intf-to-org property
                           value)
    (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
               property
               (upcase (symbol-name operation)))))
(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation (eql remove))
                              value)
  (if (occ-obj-list-p nil
                      property)
      (occ-obj-intf-to-org property
                           value)
    (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
               property
               (upcase (symbol-name operation)))))
(cl-defmethod occ-obj-to-org ((property symbol)
                              (operation null)
                              value)
  (cl-call-next-method))


(defun occ-org-list-value-from-org (value)
  (split-string value))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation symbol)
                                value)
  (if (occ-obj-list-p nil
                      property)
      (mapcar #'(lambda (v)
                  (occ-obj-intf-from-org property
                                         v))
              (occ-org-list-value-from-org value))
    (occ-obj-intf-from-org property
                           value)))
(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation symbol)
                                value)
  (if (occ-obj-list-p nil
                      property)
      (if (occ-obj-op-list-p operation)
          (occ-obj-intf-to-org property
                               value)
        (mapcar #'(lambda (v)
                    (occ-obj-intf-from-org property
                                           v))
                (occ-org-list-value-from-org value)))
    (if (occ-obj-op-list-p operation)
        (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
                   property
                   (upcase (symbol-name operation)))
      (occ-obj-intf-to-org property
                           value))))
(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation (eql add))
                                value)
  (if (occ-obj-list-p nil
                      property)
      (occ-obj-intf-from-org property
                             value)
    (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
               property
               (upcase (symbol-name operation)))))
(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation (eql remove))
                                value)
  (if (occ-obj-list-p nil
                      property)
      (occ-obj-intf-from-org property
                             value)
    (occ-error "Property `%s' is not type of LIST, %s operation not applied to it."
               property
               (upcase (symbol-name operation)))))

(cl-defmethod occ-obj-from-org ((property symbol)
                                (operation null)
                                value)
  (cl-call-next-method))


(cl-defgeneric occ-obj-prop= (property
                              prop-value
                              value)
  "OBJ has property VALUE for PROPERTY")
(cl-defmethod occ-obj-prop= ((property symbol)
                             prop-value
                             value)
  "OBJ has property VALUE for PROPERTY"
  (occ-obj-intf-prop= property
                      prop-value
                      value))


(cl-defmethod occ-obj-match ((obj occ-obj-tsk)
                             (prop symbol)
                             value)
  "VALUE equal prop-value of OBJ for PROPERTY"
  (let ((matches (occ-obj-intf-matches obj
                                       prop
                                       value)))
    (if (occ-obj-list-p obj prop)
        (cl-first matches)
      matches)))

(cl-defmethod occ-obj-operation-value ((tsk occ-obj-tsk)
                                       (property symbol)
                                       (operation symbol)
                                       value)
  "Return value VALUE for for all except delete and remove
OPERATION, return value TSK property value for VALUE for delete
and remove OPERATION."
  value)
(cl-defmethod occ-obj-operation-value ((tsk occ-obj-tsk)
                                       (property symbol)
                                       (operation (eql delete))
                                       value)
  "ANYVAL")

(cl-defmethod occ-obj-operation-value ((tsk occ-obj-tsk)
                                       (property symbol)
                                       (operation (eql remove))
                                       value)
  (occ-obj-match (occ-obj-tsk tsk)
                 property
                 value))

(cl-defmethod occ-obj-operation-value ((tsk occ-obj-tsk)
                                       (property symbol)
                                       (operation symbol)
                                       value)
  "Return value VALUE for for all except delete and remove
OPERATION, return value TSK property value for VALUE for delete
and remove OPERATION."
  (if (occ-obj-op-delete-p operation)
      (if (occ-obj-op-list-p operation)
          (occ-obj-intf-match (occ-obj-tsk tsk)
                              property
                              value)
        "ANYVAL")
    value))


(cl-defmethod occ-obj-values ((tsk occ-obj-tsk)
                              (ctx occ-obj-ctx)
                              (property symbol)
                              (operation symbol))
  (occ-obj-intf-values tsk
                       ctx
                       property
                       operation))
(cl-defmethod occ-obj-values ((tsk occ-obj-tsk)
                              (ctx null)
                              (property symbol)
                              (operation symbol))
  (occ-obj-intf-values tsk
                       ctx
                       property
                       operation))


(cl-defmethod occ-obj-vdirectors ((tsk occ-obj-tsk)
                                  (property symbol))
  (if (occ-obj-list-p tsk
                      property)
      (cl-loop for i from 1 to (length (occ-obj-get-property tsk property))
            collect i)
    (list null)))
(cl-defmethod occ-obj-pvalue ((tsk occ-obj-tsk)
                              (property symbol)
                              (vdirector number))
  (nth (1- vdirector)
       (occ-obj-get-property tsk
                             property)))
(cl-defmethod occ-obj-pvalue ((tsk occ-obj-tsk)
                              (property symbol)
                              (vdirector null))
  (occ-obj-get-property tsk
                        property))


(cl-defgeneric occ-obj-has-p (obj
                              property
                              value)
  "OBJ has property VALUE for PROPERTY")
(cl-defmethod occ-obj-has-p ((obj occ-obj-tsk)
                             (prop symbol)
                             value)
  "OBJ has property VALUE for PROPERTY"
  (occ-obj-intf-has-p obj
                      prop
                      value))
(cl-defmethod occ-obj-has-p ((obj occ-obj-ctx)
                             (prop symbol)
                             value)
  "OBJ has property VALUE for PROPERTY"
  (occ-obj-intf-has-p obj
                      prop
                      value))


(cl-defmethod occ-obj-require-p ((obj       occ-obj-tsk)
                                 (operation symbol)
                                 (prop      symbol)
                                 value)
  "Built in for LIST PROP"
  (occ-obj-intf-require-p obj
                          operation
                          prop
                          value))
(cl-defmethod occ-obj-require-p ((obj       occ-obj-ctx)
                                 (operation symbol)
                                 (prop      symbol)
                                 value)
  "Built in for LIST PROP"
  (occ-obj-intf-require-p obj
                          operation
                          prop
                          value))


(cl-defgeneric occ-obj-checkout-p (obj
                                   prop
                                   value)
  "Return if OBJ support checking-out PROP with VALUE")

(cl-defmethod occ-obj-checkout-p ((obj occ-obj-ctx)
                                  (prop symbol)
                                  value)
  (occ-obj-intf-checkout-p obj
                           prop
                           value))


(cl-defmethod occ-do-checkout ((obj occ-obj-tsk)
                               (property symbol)
                               (vdirector number))
  "Checkout property in case of force clock-in."
  (occ-do-intf-checkout obj
                        property
                        vdirector))
(cl-defmethod occ-do-checkout ((obj occ-obj-tsk)
                               (property symbol)
                               (vdirector null))
  "Checkout property in case of force clock-in."
  (occ-do-intf-checkout obj
                        property
                        vdirector))

;; TODO: Implement Plist with title here (??)

;;; occ-prop.el ends here
