;;; occ-cl-utils.el --- occ cl utils                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'occ-cl-utils)


(eval-when-compile
  (require 'occ-macros))
(eval-when-compile
  (require 'pcase))
(require 'cl-macs)
(require 'cl-generic)


;; (occ-testing ;; required
;;  (let (tsk-test)

;;     (setq tsk-test (occ-obj-make-tsk-at-point (occ-default-collection)))

;;     (cl-structure-class cl-struct-occ-tree-tsk)

;;     (cl-structure-class- cl-struct-occ-tree-tsk)

;;     (cl-structure-class tsk-test)

;;     (cl--struct-get-class tsk-test)

;;     (cl--struct-class-slots cl-struct-occ-tree-tsk)

;;     (cl--struct-get-class cl-struct-occ-tree-tsk)

;;     (cl--struct-class-slots cl-struct-cl-structure-class)

;;     (cl-structure-class-parents cl-struct-cl-structure-class)

;;     (cl-struct-slot-value 'cl-structure-class 'parents cl-struct-cl-structure-class)

;;     (cl-struct-slot-value 'cl-structure-class 'parents cl-struct-occ-tree-tsk)

;;     (cl--struct-get-class tsk-test)

;;     (cl-struct-slot-value 'cl-structure-class 'name (symbol-value (aref tsk-test 0)))

;;     (cl--struct-class-slots (symbol-value (aref tsk-test 0)))


;;     (occ-cl-class-parent-names (occ-cl-class tsk-test))

;;     (occ-cl-inst-class-parent-names tsk-test)

;;     (occ-obj-operations-for-prop tsk-test 'root)))


(defun occ-flatten (L)
  ;; https://stackoverflow.com/a/19967639
  "Converts a list to single level."
  (if (null L)
      nil
    (if (atom (cl-first L))
        (cons (cl-first L) (occ-flatten (cl-rest L)))
      (append (occ-flatten (cl-first L)) (occ-flatten (cl-rest L))))))


;; (setq test-xyz (make-occ-ctx))

;; (cl--struct-class-slots
;;  (cl--struct-get-class 'occ-ctx))

;; (cl--struct-class-slots
;;  (cl--struct-get-class 'cl-structure-class))

;; (arrayp [(point-marker)])

(defun occ-cl-class (inst)
  ;; BUG: TODO: Improve it.
  (if (cl-struct-p inst);; t ;; (arrayp inst)
      (let* ((class-sym (aref inst 0))
             (class-sym (if (boundp class-sym)
                            class-sym
                          (intern (concat "cl-struct-" (symbol-name class-sym))))))
        (if (boundp class-sym)
            (symbol-value class-sym)
          (cl--struct-get-class (aref inst 0))))
    (if (eq inst nil)
        'null
        (type-of inst))))

(defun occ-cl-classname (class)
  (if (cl-struct-p class) ;; t ;; (eql 'cl-structure-class (occ-cl-class class))
      (cl-struct-slot-value 'cl-structure-class
                            'name
                            class)
    class))

(when nil
  (type-of (occ-cl-class (occ-get-debug-obj)))
  (type-of (occ-cl-class (occ-cl-class (occ-get-debug-obj))))
  (cl- (occ-get-debug-obj))
  (occ-cl-class (make-marker))
  (type-of (make-marker))

  (occ-cl-inst-classname (occ-get-debug-obj))
  (occ-cl-inst-classname (cl-struct-p (occ-get-debug-obj))) 
  (occ-cl-inst-classname (make-marker))

  (cl-struct-p (occ-get-debug-obj))
  (cl-struct-p (make-marker))

  (length (occ-cl-class-parent-names (occ-cl-class (occ-get-debug-obj))))
  (occ-cl-class-parent-names (occ-cl-class (make-marker))))



(defun occ-cl-class-parents (class)
  (when (cl-struct-p class) ;; (symbolp (occ-cl-class class))
    (cl-struct-slot-value 'cl-structure-class
                          'parents
                          class)))

(defun occ-cl-class-parent-names (class)
  (mapcar #'(lambda (parent)
              (cons (cl-struct-slot-value 'cl-structure-class
                                          'name
                                          parent)
                    (when parent (occ-cl-class-parent-names parent))))
          (occ-cl-class-parents class)))


(defun occ-cl-inst-classname (inst)
  (occ-cl-classname (occ-cl-class inst)))

(defun occ-cl-inst-class-parent-names (inst)
  (occ-flatten (occ-cl-class-parent-names (occ-cl-class inst))))

(defun occ-cl-inst-class-names (inst)
  (cons (occ-cl-inst-classname inst)
        (occ-flatten (occ-cl-class-parent-names (occ-cl-class inst)))))


(defun occ-cl-get-field (object field)
  (cl-struct-slot-value (occ-cl-inst-classname object) field object))
(defun occ-cl-set-field (object field value)
  (setf (cl-struct-slot-value (occ-cl-inst-classname object) field object) value))
(defun occ-cl-get-fields (object fields)
  (mapcar #'(lambda (field)
              (cons field
                    (occ-cl-get-field object field)))
          fields))
(defun occ-cl-class-slots (class)
  (mapcar #'(lambda (slot) (aref slot 1))
          (cl--struct-class-slots (cl--struct-get-class class))))
;; (defun cl-class-slot-value (obj slot)
;;   (when (member slot (occ-cl-class-slots (occ-cl-inst-classname obj)))
;;     (cl-struct-slot-value (occ-cl-inst-classname obj) slot obj)))
(defun occ-cl-class-obj-slot-value (class slot obj)
  (when (member slot (occ-cl-class-slots class))
    (cl-struct-slot-value class slot
                          obj)))
(defun occ-cl-obj-slot-value (obj slot)
  (occ-cl-class-obj-slot-value (occ-cl-inst-classname obj) slot obj))
(defun occ-cl-obj-plist-value (obj)
  (occ-cl-obj-slot-value obj 'plist))


(cl-defun occ-cl-method-param-signs (method)
  "Return all params signatures for all defined METHOD"
  (let ((method-instances (cl--generic method)))
    (mapcar #'(lambda (x) (aref x 1))
            (if method-instances
                (aref method-instances 3)))))
;; http://newartisans.com/2016/01/pattern-matching-with-pcase/

(cl-defun occ-cl-method-param-case (signature-val-spec)
  "Return all matched VAL for all matched METHOD with PARAM,
 where signature-val-spec = (METHOD `(PARAMS ,VAL)) "
  (cl-destructuring-bind (method (param-spec val)) signature-val-spec
    (remove nil
            (mapcar #'(lambda (fspec)
                        (funcall `(lambda ()
                                    (pcase ',fspec
                                      (,param-spec ,val)
                                      (_ nil)))))
                    (occ-cl-method-param-signs method)))))

(defun occ-cl-method-param-case-with-value-new (signature-vars-spec args)
  "Return all VARS for all matched METHOD with PARAM which return non nil value of METHOD call on PARAM with OBJ,
 where signature-vars-spec = (METHOD `(PARAMS ,VARS))"
  (cl-destructuring-bind (method (param-spec vars)) signature-vars-spec
    (remove nil
            (mapcar #'(lambda (fspec)
                        (let ((arg-names (funcall `(lambda ()
                                                     (pcase ',fspec
                                                       (,param-spec ,vars)
                                                       (_ nil))))))
                          (when (and arg-names
                                     (funcall `(lambda ()
                                                 (pcase-let ((,vars ',arg-names))
                                                   (,method ,@args)))))
                            arg-names)))
                    (occ-cl-method-param-signs method)))))

;; (ignore
;;  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Destructuring-with-pcase-Patterns.html
;;  (pcase '(occ-ctx (eql git-branch) null)
;;    (`(occ-ctx (eql ,val) null) val)
;;    (_ nil))
;;
;;  (pcase '(add a b)
;;    (`(add ,x ,y)  (message "Contains %S and %S" x y)))
;;
;;  (pcase-let ((`(add ,x ,y) '(add a b)))
;;    (message "Contains %S and %S" x y))
;;
;;  (pcase-let ((`(,x ,y) '(a b)))
;;    (message "Contains %S and %S" x y))
;;
;;  ;; (pcase-let (( `(,(occ-obj-make-ctx-at-point) val nil)  )))
;;
;;  ;; (occ-cl-method-param-signs 'occ-obj-impl-get)
;;
;;  (let ((z 'x))
;;    (funcall `(lambda ()
;;                (pcase-let ((`,x 1))
;;                  (list ,z))))))

(defun occ-cl-method-arg-get (method fn)
  (mapcar fn
          (occ-cl-method-param-signs method)))

(defun occ-cl-method-first-arg (method)
  (occ-cl-method-arg-get method #'cadar))

(defun occ-cl-method-first-arg-with-value (method obj)
  (mapcar #'(lambda (fspec)
              (let ((first-arg (cadar fspec)))
                (when (funcall method (cons first-arg obj))
                  first-arg)))
          (occ-cl-method-param-signs method)))


(defun occ-cl-method-param-values (method param-exp val)
  (funcall `(lambda ()
              (occ-cl-method-param-case '(,method (,param-exp ,val))))))

(defun occ-cl-collect-on-classes (fn &rest insts)
  (mapcan #'(lambda (class)
              (apply fn class))
          (apply #'occ-util-combine
                 (mapcar #'occ-cl-inst-class-names
                         insts))))


(cl-defun occ-cl-method-param-signs (method)
  "Return all params signatures for all defined METHOD"
  (let ((method-instances (cl--generic method)))
    (mapcar #'(lambda (x) (aref x 1))
            (if method-instances
                (aref method-instances 3)))))
(occ-cl-method-param-signs 'occ-obj-impl-get)


(cl-defun occ-cl-method-param-case (signature-vars-spec)
  "Return all matched VARS for all matched METHOD with PARAM,
 where signature-vars-spec = (METHOD `(PARAMS ,VARS)) "
  (cl-destructuring-bind (method (param-spec vars)) signature-vars-spec
    (remove nil
            (mapcar #'(lambda (fspec)
                        (funcall `(lambda ()
                                    (pcase ',fspec
                                      (,param-spec ,vars)
                                      (_ nil)))))
                    (occ-cl-method-param-signs method)))))


(cl-defun occ-cl-method-param-case-1 (method sign vars)
  "Return all matched VAL for all matched METHOD with PARAM,
 where signature-val-spec = (METHOD `(PARAMS ,VAL)) "
  (cl-destructuring-bind (method param-spec vars) (list method sign vars)
    (remove nil
            (mapcar #'(lambda (fspec)
                        (funcall `(lambda ()
                                    (pcase ',fspec
                                      (,param-spec ,vars)
                                      (_ nil)))))
                    (occ-cl-method-param-signs method)))))

(ignore (occ-cl-method-param-case '(occ-obj-impl-list-p (`((eql ,val)) `(,val))))
        (occ-cl-method-param-case '(occ-obj-impl-list-p (`((eql ,val)) `(,val)))))

(defun occ-cl-method-param-case-with-vars-value (signature-vars-spec args)
  "Return all VARS for all matched METHOD with PARAM which return non nil varsue of METHOD call on PARAM with OBJ,
 where signature-vars-spec = (METHOD `(PARAMS ,VARS))"
  (cl-destructuring-bind (method (param-spec vars)) signature-vars-spec
    (remove nil
            (mapcar #'(lambda (fspec)
                        (let ((arg-names (funcall `(lambda ()
                                                     (pcase ',fspec
                                                       (,param-spec ,vars)
                                                       (_ nil))))))
                          (when (and arg-names
                                     (funcall `(lambda ()
                                                 (pcase-let ((,vars ',arg-names))
                                                   (,method ,@args)))))
                            arg-names)))
                    (occ-cl-method-param-signs method)))))

;;; occ-cl-utils.el ends here
