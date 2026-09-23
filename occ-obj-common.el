;;; occ-obj-common.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

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

;; occ-obj-common.el defines the common property access API shared by all
;; OCC object model objects.
;;
;; It provides plist helpers (`occ-plist-get', the `occ-plist-set' macro,
;; `occ-list-get-evens', `occ-plist-get-keys') and the central property
;; reading generics `occ-obj-get-property' / `occ-obj-get-properties'
;; (with `-internal' variants): a value comes from a struct slot when the
;; property names one, otherwise from the object's org-property plist
;; (looked up both as-is and upcased).  Methods dispatch on `occ-obj',
;; `occ-obj-tsk', `occ-obj-ctx-tsk' and `occ-obj-ctx', and
;; `occ-obj-set-property' does the inverse, storing into slots or the
;; plist.  Reflection helpers (`occ-obj-class-slots',
;; `occ-obj-defined-slots', `occ-obj-cl-method-matched-arg',
;; `occ-obj-cl-method-sig-matched-arg') use occ-cl-utils to match
;; properties against cl-generic method specializers.  Object model
;; layer, base plumbing for the property protocol.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-obj-common)


(require 'occ-obj)
(require 'occ-prop-intf)
(require 'occ-intf)
(require 'occ-assert)
(require 'occ-cl-utils)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)

;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; "org tsks accss common api"

(defun occ-plist-get (plist prop)
  "Return the value of property PROP from PLIST.
Looks up PROP as a keyword with sym2key and signals occ-error
when no keyword can be made for PROP."
  (let ((key (sym2key prop)))
    (if key
        (plist-get plist
                   (sym2key prop))
      (occ-error "occ-plist-get: Can not make keyword for `'%s'" prop))))

(defmacro occ-plist-set (plist prop value)
  "Store VALUE as the value of property PROP in PLIST.
Asserts an even length PLIST and signals occ-error when no
keyword can be made for PROP."
  `(let ((key (sym2key ,prop)))
     (occ-assert (cl-evenp (length ,plist)))
     (if key
         (setf ,plist (plist-put ,plist ;TODO ??? (occ-cl-obj-plist-value obj)
                                 key ,value))
       (occ-error "occ-plist-set: Can not make keyword for `'%s'" ,prop))))

(defun occ-list-get-evens (lst)
  "Return the elements of LST at even indices starting at zero.
Collects every other element of LST beginning with the first."
  (cond
   ((null lst) nil)
   (t          (cons (cl-first lst)
                     (occ-list-get-evens (nthcdr 2 lst))))))

;; (defun list-get-odds (lst)
;;   (cond
;;    ((null lst) nil)
;;    ( t (cons  (nth 1 lst) (list-get-odds (cl-rest (cl-rest lst)))))))

(defun occ-plist-get-keys (plist)
  "Return the property keys of PLIST.
Collects the even indexed elements of PLIST via
occ-list-get-evens."
  (occ-list-get-evens plist))


(cl-defgeneric occ-obj-get-property-internal (obj
                                              prop)
  "get property of object")
(cl-defgeneric occ-obj-get-property (obj
                                     prop)
  "get property of object")
(cl-defgeneric occ-obj-get-properties-internal (obj
                                                props)
  "get property of object")
(cl-defgeneric occ-obj-get-properties (obj
                                       props)
  "get property of object")

(cl-defmethod occ-obj-get-property-internal ((obj occ-obj)
                                             (prop symbol))
  "Return the raw value of property PROP of OCC-OBJ OBJ.
Reads the class slot PROP when OBJ has one and otherwise looks
PROP up in the plist of OBJ as an org property key as is and
upcased."
  ;; mainly used by occ-tsk only.
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ)): calling for prop %s" prop)
  (if (memq prop
            (occ-cl-class-slots (occ-cl-inst-classname obj)))
      (occ-cl-get-field obj prop)
    (let ((org-prop (occ-obj-org-property-symb prop)))
      (or (occ-plist-get (occ-cl-obj-plist-value obj)
                         org-prop)
          (occ-plist-get (occ-cl-obj-plist-value obj)
                         (upcase-sym org-prop))))))

(cl-defmethod occ-obj-get-properties-internal ((obj   occ-obj)
                                               (props list))
  "Return an alist of PROP to value for PROPS of OCC-OBJ OBJ.
Pairs each prop of PROPS with its occ-obj-get-property value."
  ;; mainly used by occ-tsk only.
  (mapcar #'(lambda (prop)
              (cons prop (occ-obj-get-property obj prop)))
          props))


(cl-defmethod occ-obj-get-property ((obj  occ-obj-tsk)
                                    (prop symbol))
  "Return occ compatible value of prop PROP from OCC-CTX OBJ."
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ-TSK)): calling for prop %s" prop)
  (occ-obj-get-property-internal (occ-obj-tsk obj) prop))

(cl-defmethod occ-obj-get-property ((obj  occ-obj-ctx-tsk)
                                    (prop symbol))
  "Return occ compatible value of prop PROP from OCC-CTX OBJ."
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ-CTX-TSK)): calling for prop %s" prop)
  (occ-obj-get-property (occ-obj-tsk obj) prop))

(cl-defmethod occ-obj-get-property ((obj  occ-obj-ctx)
                                    (prop symbol))
  "Return occ compatible value of prop PROP from OCC-CTX OBJ."
  (occ-debug "(OCC-OBJ-GET-PROPERTY (OBJ OCC-OBJ-CTX)): calling for prop %s" prop)
  (occ-obj-get (occ-obj-ctx obj)
               nil
               prop
               nil))


(cl-defmethod occ-obj-get-properties ((obj   occ-obj-tsk)
                                      (props list))
  "Return PROPS as an alist of values from OCC-OBJ-TSK OBJ.
Delegates to occ-obj-get-properties-internal on the wrapped task."
  ;; mainly used by occ-tsk only.
  (occ-obj-get-properties-internal (occ-obj-tsk obj) props))

(cl-defmethod occ-obj-get-properties ((obj   occ-obj-ctx-tsk)
                                      (props list))
  "Return PROPS as an alist of values from OCC-OBJ-CTX-TSK OBJ.
Delegates to occ-obj-get-properties on the wrapped task."
  ;; mainly used by occ-tsk only.
  (occ-obj-get-properties (occ-obj-tsk obj) props))

(cl-defmethod occ-obj-get-properties ((obj   occ-obj-ctx)
                                      (props list))
  "Return PROPS as an alist of values from OCC-OBJ-CTX OBJ.
Delegates to occ-obj-get-properties on the context wrapped in
OBJ."
  ;; mainly used by occ-tsk only.
  (occ-obj-get-properties (occ-obj-ctx obj) props))


(cl-defmethod occ-obj-set-property ((obj occ-obj)
                                    prop
                                    value)
  "Set the value of property PROP of OCC-OBJ OBJ to VALUE.
Stores into the matching class slot when PROP names one and
otherwise into the plist slot of OBJ under the org property key
for PROP."
  ;; mainly used by occ-tsk only
  ;; (occ-debug "(occ-obj-set-property occ-obj): prop %s, value %s"
  ;;            (prin1-to-string prop)
  ;;            (prin1-to-string (occ-obj-nonocc-format value)))
  (if (memq prop
            (occ-cl-class-slots (occ-cl-inst-classname obj)))
      (progn
        ;; (occ-message "IF")
        (setf (cl-struct-slot-value (occ-cl-inst-classname obj) prop obj)
            value))
    (let* ((org-prop   (occ-obj-org-property-symb prop))
           (plist-prop (if (occ-plist-get (occ-cl-obj-plist-value obj)
                                         org-prop)
                          org-prop
                        (upcase-sym org-prop))))
      ;; (occ-message "ELSE")
      (occ-debug "(occ-obj-set-property occ-obj): plist got %s using %s"
                 prop plist-prop)
      (occ-plist-set
       ;; NOTE: as Property block keys return by (org-element-at-point) are in
       ;; UPCASE even in actual org file it is lower or camel case. so our obj
       ;; (tsk) also must have to be in line of it as it also got created with
       ;; same function (org-element-at-point).
       (cl-struct-slot-value (occ-cl-inst-classname obj)
                             'plist
                             obj)
       plist-prop value))))

(cl-defmethod occ-obj-set-property ((obj occ-tree-tsk)
                                    prop
                                    value)
  "Set the value of property PROP of OCC-TREE-TSK OBJ to VALUE.
Forwards to the next method which stores slots or plist values."
  ;; TODO: do it recursively.
  ;; mainly used by occ-tsk only
  ;; NOTE
  ;; (occ-debug "(occ-obj-set-property (obj occ-tree-tsk)) prop %s, value %s"
  ;;            (prin1-to-string prop)
  ;;            (prin1-to-string (occ-obj-nonocc-format value)))
  (cl-call-next-method))
  ;; (when not-recursive
  ;;   (dolist (subtsk (occ-tree-tsk-subtree (occ-obj-tsk obj)))
  ;;     (occ-obj-set-property subtsk
  ;;                           prop
  ;;                           value
  ;;                           :not-recursive not-recursive)))


(cl-defmethod occ-obj-set-property ((obj occ-obj-tsk)
                                    prop
                                    value)
  "Set the value of property PROP of OCC-OBJ-TSK OBJ to VALUE.
Ignores OBJ itself and forwards to the next method."
  (ignore obj)
  ;; (occ-debug "(occ-obj-set-property (obj occ-obj-tsk)) prop %s, value %s"
  ;;            (prin1-to-string prop)
  ;;            (prin1-to-string (occ-obj-nonocc-format value)))
  (cl-call-next-method))

(cl-defmethod occ-obj-set-property ((obj occ-obj-ctx-tsk)
                                    prop
                                    value)
  "Set the value of property PROP of OCC-OBJ-CTX-TSK OBJ to VALUE.
Delegates to occ-obj-set-property on the task wrapped in OBJ."
  ;; (occ-debug "(occ-obj-set-property (obj occ-obj-ctx-tsk)) prop %s, value %s"
  ;;            (prin1-to-string prop)
  ;;            (prin1-to-string (occ-obj-nonocc-format value)))
  (occ-obj-set-property (occ-obj-tsk obj) prop
                        value))

(cl-defmethod occ-obj-set-property ((obj occ-obj-ctx)
                                    prop
                                    value)
  "Set the value of property PROP of OCC-OBJ-CTX OBJ to VALUE.
Delegates to occ-obj-set-property on the context wrapped in OBJ."
  ;; (occ-debug "(occ-obj-set-property (obj occ-obj-ctx)) prop %s, value %s"
  ;;            (prin1-to-string prop)
  ;;            (prin1-to-string (occ-obj-nonocc-format value)))
  (occ-obj-set-property (occ-obj-ctx obj) prop
                        value))


(cl-defmethod occ-obj-class-slots ((obj occ-obj))
  "Return the slot symbols of OCC-OBJ OBJ as a list.
Appends the class slots of OBJ with the org property keys found
in the plist of OBJ."
  (let* ((plist      (occ-cl-obj-plist-value obj))
         (plist-keys (occ-plist-get-keys plist))
         (slots      (occ-cl-class-slots (occ-cl-inst-classname obj))))
    (append slots
            (mapcar #'key2sym plist-keys))))
(cl-defmethod occ-obj-defined-slots ((obj occ-obj))
  "Return every defined slot symbol of OCC-OBJ OBJ as a list.
Combines the class slots of OBJ with the org property keys in the
plist of OBJ."
  (let* ((plist      (occ-cl-obj-plist-value obj))
         (plist-keys (occ-plist-get-keys plist))
         (slots      (append (occ-cl-class-slots (occ-cl-inst-classname obj))
                             (mapcar #'key2sym
                                     plist-keys))))
    slots))
(cl-defmethod occ-obj-defined-slots-with-value ((obj occ-obj))
  "Return the defined slots of OCC-OBJ OBJ that read non nil.
Filters occ-obj-defined-slots keeping slots whose property has a
value."
  (let* ((slots (occ-obj-defined-slots obj)))
    (cl-remove-if-not #'(lambda (slot)
                          (occ-obj-get-property obj slot))
                      slots)))
(cl-defmethod occ-obj-cl-method-matched-arg ((method symbol)
                                             (ctx symbol))
  "Return the first args of METHOD ignoring the symbol CTX.
Applies no property filtering when CTX is a symbol."
  (ignore ctx)
  (occ-cl-method-first-arg method))
(cl-defmethod occ-obj-cl-method-matched-arg ((method symbol)
                                             (ctx occ-ctx))
  "Return the first args of METHOD matching valued slots of CTX.
Keeps args that are among the defined slots with values on OCC-CTX
CTX."
  (let ((slots (occ-obj-defined-slots-with-value ctx)))
    (cl-remove-if-not #'(lambda (arg) (memq arg slots))
                      (occ-cl-method-first-arg method))))
(cl-defmethod occ-obj-cl-method-matched-arg ((method1 symbol)
                                             (method2 symbol)
                                             (ctx occ-ctx))
  "Return first args of METHOD1 matching METHOD2 valued slots of CTX."
  (let ((slots (occ-cl-method-first-arg-with-value method2
                                               ctx)))
    (cl-remove-if-not #'(lambda (arg) (memq arg slots))
                      (occ-cl-method-first-arg method1))))


(cl-defgeneric occ-obj-cl-method-sig-matched-arg (method-sig
                                                  ctx)
  "test")
(cl-defmethod occ-obj-cl-method-sig-matched-arg ((method-sig cons)
                                                 (ctx symbol))
  "Return the param case of METHOD-SIG ignoring the symbol CTX.
Applies no property filtering when CTX is a symbol."
  (ignore ctx)
  (occ-cl-method-param-case method-sig))
(cl-defmethod occ-obj-cl-method-sig-matched-arg ((method-sig cons)
                                                 (ctx occ-ctx))
  "Return the param case args of METHOD-SIG valued on CTX.
Keeps args among the defined slots with values on OCC-CTX CTX."
  (let ((slots (occ-obj-defined-slots-with-value ctx))) ;; ((slots (occ-obj-defined-slots-with-value-new ctx)))
    (cl-remove-if-not #'(lambda (arg) (memq arg slots))
                      (occ-cl-method-param-case method-sig))))
;; (cl-defmethod occ-obj-cl-method-sigs-matched-arg ((method-sig1 cons)
;;                                                   (method-sig2 cons)
;;                                                   (ctx occ-ctx))
;;   "Find common properties of METHOD-SIG1 and properties of METHOD-SIG2, which return non nil on METHOD-SIG2 calls."
;;   (let ((slots (occ-cl-method-param-case-with-value-new method-sig2 ctx)))
;;     (cl-remove-if-not #'(lambda (arg) (memq arg slots))
;;                       (occ-cl-method-param-case method-sig1))))
(cl-defmethod occ-obj-cl-method-sigs-matched-arg ((method-sig1 cons)
                                                  (method-sig2 cons)
                                                  (args cons))
  "Return param case args of METHOD-SIG1 valued by METHOD-SIG2.
Keeps args of METHOD-SIG1 among the slots with values when
METHOD-SIG2 runs on ARGS."
  (let ((slots (occ-cl-method-param-case-with-value-new method-sig2 args)))
    (cl-remove-if-not #'(lambda (arg) (memq arg slots))
                      (occ-cl-method-param-case method-sig1))))
;; ;; (occ-cl-method-param-signs 'occ-obj-impl-get)

;;; occ-obj-common.el ends here
