;;; occ-prop-gen-checkout-actions.el --- generate checkout helm actions for property  -*- lexical-binding: t; -*-

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

(provide 'occ-prop-gen-checkout-actions)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-prop-base)
(require 'occ-prop-op-checkout)
(require 'occ-assert)


(cl-defmethod occ-obj-gen-checkout-prompt ((obj  occ-obj-tsk)
                                           (prop symbol)
                                           vdirector
                                           &key param-only)
  "Used by occ-obj-gen-checkout"
  (ignore param-only)
  (let ((list-p (occ-obj-list-p obj prop)))
    (ignore list-p)
    (format "Checkout property %s: %s of %s"
            prop
            (occ-obj-propfmt obj prop (occ-obj-pvalue obj
                                                      prop
                                                      vdirector))
            (occ-obj-Format obj))))

(cl-defmethod occ-obj-gen-checkout-fun ((obj  occ-obj-tsk)
                                        (prop symbol)
                                        vdirector
                                        &key param-only)
  "Generate helm function, purpose PARAM-ONLY for the case where
only argument required for some other further processing"
  (ignore obj)
  (if param-only
      (list prop
            vdirector)
    #'(lambda (obj)
        (occ-do-op-prop-checkout obj
                                 prop
                                 vdirector))))


(cl-defgeneric occ-obj-gen-checkout (obj
                                     prop
                                     vdirector
                                     &key param-only)
  "occ-obj-gen-checkout")

(cl-defmethod occ-obj-gen-checkout ((obj       occ-obj-tsk)
                                    (prop      symbol)
                                    vdirector
                                    &key param-only)
  (occ-debug "occ-obj-gen-checkout: checking prop %s" prop)
  (let ((prompt (occ-obj-gen-checkout-prompt obj
                                             prop
                                             vdirector
                                             :param-only param-only))
        (fun    (occ-obj-gen-checkout-fun obj
                                          prop
                                          vdirector
                                          :param-only param-only))
        (keyword (sym2key (gensym))))
    (occ-obj-make-callable-normal keyword
                                  prompt
                                  fun)))


(cl-defmethod occ-obj-gen-checkout-if-required ((obj       occ-obj-tsk)
                                                (prop      symbol)
                                                vdirector
                                                &key param-only)
  (occ-debug "occ-obj-gen-checkout: checking prop %s" prop)
  (let ((tsk       (occ-obj-tsk obj))
        (ctx       (occ-obj-ctx obj))
        (operation 'checkout))
   (if (occ-obj-require-p ctx
                          operation
                          prop
                          (occ-obj-pvalue tsk
                                          prop
                                          vdirector))
       (occ-obj-gen-checkout obj
                             prop
                             vdirector
                             :param-only param-only)
     (when nil
       (occ-message "No match")))))


(cl-defmethod occ-obj-gen-checkouts-if-required ((obj  occ-obj-tsk)
                                                 (prop symbol)
                                                 &key param-only)
  (if (occ-obj-get-property obj
                            prop)
      (mapcar #'(lambda (vdirector)
                  (occ-obj-gen-checkout-if-required obj
                                                    prop
                                                    vdirector
                                                    :param-only param-only))
              (occ-obj-vdirectors obj
                                  prop))
    (occ-debug "occ-obj-gen-checkouts-if-required: no value for prop %s present for %s"
                 prop
                 (occ-obj-Format obj))))

(cl-defmethod occ-obj-gen-checkouts-if-required ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                                 (prop null)
                                                 &key param-only)
  (let* ((props        (occ-obj-properties-to-checkout (occ-obj-tsk obj)))
         (checkout-ops (mapcan #'(lambda (prop)
                                   (occ-obj-gen-checkouts-if-required obj
                                                                      prop
                                                                      :param-only param-only))
                               props)))
    (occ-assert props)
    (remove nil
            checkout-ops)))


(cl-defmethod occ-obj-gen-each-prop-checkouts ((obj null)
                                               &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-each-prop-checkouts ((obj occ-tsk) ;cover OCC-TSK, OCC-TREE-TSK, OCC-LIST-TSK only
                                               &key param-only)
  nil)

(cl-defmethod occ-obj-gen-each-prop-checkouts ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                               &key param-only)
  (occ-obj-gen-checkouts-if-required obj
                                     nil
                                     :param-only param-only))

(cl-defmethod occ-obj-gen-each-prop-checkouts ((obj occ-obj-ctx)
                                               &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)


(cl-defun occ-obj-gen-each-prop-fast-checkouts (obj
                                                &key param-only)
  (append (occ-obj-gen-each-prop-checkouts obj
                                           :param-only param-only)
          (occ-obj-gen-each-prop-checkouts (occ-obj-tsk obj)
                                           :param-only param-only)))


(cl-defmethod occ-obj-gen-simple-checkouts ((obj null)
                                            &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-simple-checkouts ((obj occ-obj-tsk)
                                            &key param-only)
  (ignore param-only)
  (list (occ-obj-make-callable-normal :checkout
                                  (format "Checkout %s" (occ-obj-Format obj))
                                  #'(lambda (obj)
                                      (occ-do-op-props-checkout obj)))))

(cl-defmethod occ-obj-gen-simple-checkouts ((obj occ-obj-ctx)
                                            &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)

;;; occ-prop-gen-checkout-actions.el ends here
