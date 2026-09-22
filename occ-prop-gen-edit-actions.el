;;; occ-prop-gen-edit-actions.el --- generate edit helm actions for property  -*- lexical-binding: t; -*-

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

(provide 'occ-prop-gen-edit-actions)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-prop-base)
(require 'occ-prop-op-edit)


;; TODO: also accommodate increase decrease etc.
(cl-defmethod occ-obj-gen-edit-prompt ((obj       occ-obj-tsk)
                                       (prop      symbol)
                                       (operation symbol)
                                       value
                                       &key param-only)
  "Used by occ-obj-gen-edit"
  ;; TODO: Improve it.
  (ignore param-only)
  (let ((list-p (occ-obj-list-p obj prop)))
    (format "%s - property %s: %s %s %s"
              (capitalize (symbol-name operation))
              prop
              (occ-obj-propfmt obj prop value)
              (if list-p "in" "from")
              (occ-obj-Format obj))))


(cl-defmethod occ-obj-gen-edit-prompt ((obj       occ-obj-tsk)
                                       (prop      symbol)
                                       (operation (eql add))
                                       value
                                       &key param-only)
  "Used by occ-obj-gen-edit"
  ;; TODO: Improve it.
  (ignore param-only)
  (let ((list-p (occ-obj-list-p obj prop)))
    (format "%s - property %s: %s %s %s"
            (capitalize (symbol-name operation))
            prop
            (occ-obj-propfmt obj prop value)
            (if list-p "in" "from")
            (occ-obj-Format obj))))

(cl-defmethod occ-obj-gen-edit-prompt ((obj       occ-obj-tsk)
                                       (prop      symbol)
                                       (operation (eql remove))
                                       value
                                       &key param-only)
  "Used by occ-obj-gen-edit"
  ;; TODO: Improve it.
  (ignore param-only)
  (let ((list-p (occ-obj-list-p obj prop)))
    (format "%s - property %s: %s %s %s"
            (capitalize (symbol-name operation))
            prop
            (occ-obj-propfmt obj prop
                             ;; (occ-obj-iXntf-match obj prop value)
                             value)
            (if list-p "in" "from")
            (occ-obj-Format obj))))

(cl-defmethod occ-obj-gen-edit-fun ((obj       occ-obj-tsk)
                                    (prop      symbol)
                                    (operation symbol)
                                    value
                                    &key param-only)
  "Generate helm function, purpose PARAM-ONLY for the case where
only argument required for some other further processing"
  (if param-only
      (list prop
            operation
            value)
    #'(lambda (candidate)
        (ignore candidate)
        (occ-do-op-prop-edit obj
                             prop
                             operation
                             value))))


(cl-defgeneric occ-obj-gen-edit (obj
                                 prop
                                 operation
                                 value
                                 &key param-only)
  "occ-obj-gen-edit")

(cl-defmethod occ-obj-gen-edit ((obj       occ-obj-tsk)
                                (prop      symbol)
                                (operation symbol)
                                value
                                &key param-only)
  (occ-debug "occ-obj-gen-edit: checking prop %s operation %s" prop operation)
  (let ((prompt  (occ-obj-gen-edit-prompt obj
                                          prop
                                          operation
                                          value
                                          :param-only param-only))
        (fun     (occ-obj-gen-edit-fun obj
                                       prop
                                       operation
                                       value
                                       :param-only param-only))
        (keyword (sym2key (gensym))))
    (occ-obj-make-callable-normal keyword
                                  prompt
                                  fun)))


(cl-defmethod occ-obj-gen-edit-if-required ((obj       occ-obj-tsk)
                                            (prop      symbol)
                                            (operation symbol)
                                            value
                                            &key param-only)
  (if (occ-obj-require-p obj
                         operation
                         prop
                         value)
    (occ-obj-gen-edit obj
                      prop
                      operation
                      value
                      :param-only param-only)
    (when nil
      (occ-message "No match"))))


(cl-defmethod occ-obj-gen-edits-if-required ((obj       occ-obj-tsk)
                                             (prop      symbol)
                                             (operation null)
                                             &key param-only)
  (ignore operation)
  (let* ((ops      (occ-obj-operations-for-prop obj
                                                prop))
         ;; will use occ-obj-mapper onward
         (edit-ops (mapcan #'(lambda (operation)
                                     (mapcar #'(lambda (val)
                                                 ;; (occ-message "Val: %s" val)
                                                 (when val
                                                   (occ-obj-gen-edit-if-required obj
                                                                                 prop
                                                                                 operation
                                                                                 val
                                                                                 :param-only param-only)))
                                             (occ-obj-values (occ-obj-tsk obj)
                                                             (occ-obj-ctx obj)
                                                             prop
                                                             operation)))
                             ops)))
    ;; (occ-message "edit-ops: len %d" (length edit-ops))
    (remove nil
            edit-ops)))

(cl-defmethod occ-obj-gen-edits-if-required ((obj       occ-obj-tsk)
                                             (prop      null)
                                             (operation symbol)
                                             &key param-only)
  ;; NOTE: occ-obj-properties-to-edit will handle (obj occ-obj-ctx-tsk)
  (ignore prop)
  (let* ((props    (occ-obj-properties-to-edit obj))
         ;; will be call (OCC-OBJ-GEN-EDITS-IF-REQUIRED OBJ PROP OPERATION :PARAM_ONLY PARAM_ONLY)
         (edit-ops (mapcar #'(lambda (prop)
                               (occ-obj-gen-edits-if-required obj
                                                              prop
                                                              operation
                                                              :param-only param-only))
                           props)))
    (remove nil
            edit-ops)))

(cl-defmethod occ-obj-gen-edits-if-required ((obj       occ-obj-tsk)
                                             (prop      null)
                                             (operation null)
                                             &key param-only)
  (ignore prop)
  (let* ((props (occ-obj-properties-to-edit obj))
         ;; NOTE:
         ;; will be calling            (OCC-OBJ-GEN-EDITS-IF-REQUIRED OBJ PROP NIL :PARAM_ONLY PARAM_ONLY)
         ;; which in turn will be call (OCC-OBJ-GEN-EDITS-IF-REQUIRED OBJ PROP OPERATION :PARAM_ONLY PARAM_ONLY)
         (edit-ops (mapcar #'(lambda (prop)
                               (occ-obj-gen-edits-if-required obj
                                                              prop
                                                              operation ;nil
                                                              :param-only param-only))
                           props)))
    (apply #'append
           edit-ops)))


(cl-defmethod occ-obj-gen-each-prop-edits ((obj null)
                                           &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-each-prop-edits ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                           &key param-only)
  ;; NOTE:
  ;; will call (OCC-OBJ-GEN-EDITS-IF-REQUIRED ((OBJ OCC-OBJ-TSK) (PROP NULL) (OPERATION NULL) &KEY PARAM-ONLY)
  ;; function as number of arguments are different.
  (occ-obj-gen-edits-if-required obj
                                 nil
                                 nil
                                 :param-only param-only))

(cl-defmethod occ-obj-gen-each-prop-edits ((obj occ-obj-ctx)
                                           &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)


(cl-defun occ-obj-gen-each-prop-fast-edits (obj &key param-only)
  (append (occ-obj-gen-each-prop-edits obj
                                       :param-only param-only)
          (occ-obj-gen-each-prop-edits (occ-obj-tsk obj)
                                       :param-only param-only)))


(cl-defmethod occ-obj-gen-simple-edits ((obj null)
                                        &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-simple-edits ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                        &key param-only)
  (ignore param-only)
  (list (occ-obj-make-callable-normal :edit
                                      (format "Edit %s" (occ-obj-Format obj))
                                      #'(lambda (obj)
                                          (occ-do-op-props-edit obj)))))

(cl-defmethod occ-obj-gen-simple-edits ((obj occ-obj-ctx)
                                        &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)


(cl-defmethod occ-obj-gen-clock-operations ((obj null)
                                            &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-clock-operations ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                            &key param-only)
  (ignore param-only)
  (if (occ-obj-clocking-in-p obj)
      (list (occ-obj-make-callable-normal :clock-out
                                          (format "Clock out %s" (occ-obj-Format obj))
                                          #'(lambda (obj)
                                              (occ-do-clock-out (occ-obj-tsk obj)))))))

(cl-defmethod occ-obj-gen-clock-operations ((obj occ-obj-ctx)
                                            &key param-only)
  (ignore obj)
  (ignore param-only)
  nil)

;; (cl-defun occ-obj-gen-each-prop-fast-clock-operations (obj &key param-only)
;;   (append (occ-obj-gen-each-prop-edits obj
;;                                        :param-only param-only)
;;           (occ-obj-gen-each-prop-edits (occ-obj-tsk obj)
;;                                        :param-only param-only)))

;;; occ-prop-gen-edit-actions.el ends here
