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

;; occ-prop-gen-edit-actions.el generates the edit helm actions of the
;; OCC property protocol at runtime, from property and operation
;; information rather than by hand.
;;
;; occ-obj-gen-edit-prompt formats prompts like "Add - property root:
;; /dir in task", occ-obj-gen-edit-fun builds the callback (calling
;; occ-do-op-prop-edit, or returning the parameter list when PARAM-ONLY
;; is set), occ-obj-gen-edit assembles both into an
;; occ-callable-normal, and occ-obj-gen-edit-if-required filters through
;; occ-obj-require-p.
;;
;; The bulk generators occ-obj-gen-edits-if-required,
;; occ-obj-gen-each-prop-edits with occ-obj-gen-each-prop-fast-edits,
;; occ-obj-gen-simple-edits and occ-obj-gen-clock-operations expand this
;; over occ-obj-properties-to-edit, occ-obj-operations-for-prop and
;; occ-obj-values, yielding the "Fast Edits", "Simple Edit" and "Clock
;; Operations" helm action generators registered in
;; occ-helm-actions-config.el.
;;
;; Layer: generated action layer on top of occ-prop-op-edit.el and
;; occ-prop-base.el.
;;
;; See doc/occ-design.org for the full design.

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
  "Build an edit helm callable for the occ-obj-tsk OBJ.
Combines a prompt from occ-obj-gen-edit-prompt and a callback from
occ-obj-gen-edit-fun into an occ-callable-normal keyed by a fresh
keyword.  PARAM-ONLY makes the callback return the parameter list
instead of calling occ-do-op-prop-edit."
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
  "Build an edit callable for PROP on the occ-obj-tsk OBJ when required.
Delegates to occ-obj-gen-edit only when occ-obj-require-p approves
OPERATION for VALUE and returns nil otherwise.  PARAM-ONLY passes
through."
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
  "Generate edit callables for every operation and value of PROP on OBJ.
Expands occ-obj-operations-for-prop over PROP and occ-obj-values per
operation filtering each candidate through occ-obj-gen-edit-if-required.
OPERATION is nil.  PARAM-ONLY passes through and nil results are
removed."
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
  "Generate edit callables for OPERATION on every editable property.
Expands occ-obj-properties-to-edit over the occ-obj-tsk OBJ collecting
per-property results.  PROP is nil.  PARAM-ONLY passes through and nil
results are removed."
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
  "Generate edit callables for every operation of every editable property.
PROP and OPERATION are nil.  Recurses per property over
occ-obj-properties-to-edit and appends the per-property results.
PARAM-ONLY passes through."
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
  "Return no edit callables when OBJ is null.
PARAM-ONLY is ignored."
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-each-prop-edits ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                           &key param-only)
  "Generate edit callables for all editable properties of the OBJ task.
Delegates to occ-obj-gen-edits-if-required with PROP and OPERATION nil.
PARAM-ONLY passes through."
  ;; NOTE:
  ;; will call (OCC-OBJ-GEN-EDITS-IF-REQUIRED ((OBJ OCC-OBJ-TSK) (PROP NULL) (OPERATION NULL) &KEY PARAM-ONLY)
  ;; function as number of arguments are different.
  (occ-obj-gen-edits-if-required obj
                                 nil
                                 nil
                                 :param-only param-only))

(cl-defmethod occ-obj-gen-each-prop-edits ((obj occ-obj-ctx)
                                           &key param-only)
  "Return no edit callables for a plain occ-obj-ctx OBJ.
PARAM-ONLY is ignored."
  (ignore obj)
  (ignore param-only)
  nil)


(cl-defun occ-obj-gen-each-prop-fast-edits (obj &key param-only)
  "Generate fast edit callables for OBJ and its task part.
Appends the edit callables generated for OBJ itself with those
generated for its task from occ-obj-gen-each-prop-edits.  PARAM-ONLY
passes through."
  (append (occ-obj-gen-each-prop-edits obj
                                       :param-only param-only)
          (occ-obj-gen-each-prop-edits (occ-obj-tsk obj)
                                       :param-only param-only)))


(cl-defmethod occ-obj-gen-simple-edits ((obj null)
                                        &key param-only)
  "Return no simple edit callables when OBJ is null.
PARAM-ONLY is ignored."
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-simple-edits ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                        &key param-only)
  "Generate the single simple edit callable for the occ-obj-tsk OBJ.
Returns one occ-callable-normal with an Edit prompt whose callback runs
occ-do-op-props-edit to edit all properties in the timed window.
PARAM-ONLY is ignored."
  (ignore param-only)
  (list (occ-obj-make-callable-normal :edit
                                      (format "Edit %s" (occ-obj-Format obj))
                                      #'(lambda (obj)
                                          (occ-do-op-props-edit obj)))))

(cl-defmethod occ-obj-gen-simple-edits ((obj occ-obj-ctx)
                                        &key param-only)
  "Return no simple edit callables for a plain occ-obj-ctx OBJ.
PARAM-ONLY is ignored."
  (ignore obj)
  (ignore param-only)
  nil)


(cl-defmethod occ-obj-gen-clock-operations ((obj null)
                                            &key param-only)
  "Return no clock operation callables when OBJ is null.
PARAM-ONLY is ignored."
  (ignore obj)
  (ignore param-only)
  nil)

(cl-defmethod occ-obj-gen-clock-operations ((obj occ-obj-tsk) ;cover OCC-OBJ-CTX-TSK also
                                            &key param-only)
  "Generate the clock out callable for the clocking occ-obj-tsk OBJ.
Returns one occ-callable-normal with a Clock out prompt whose callback
runs occ-do-clock-out on the task when OBJ is currently clocking in
and nil otherwise.  PARAM-ONLY is ignored."
  (ignore param-only)
  (if (occ-obj-clocking-in-p obj)
      (list (occ-obj-make-callable-normal :clock-out
                                          (format "Clock out %s" (occ-obj-Format obj))
                                          #'(lambda (obj)
                                              (occ-do-clock-out (occ-obj-tsk obj)))))))

(cl-defmethod occ-obj-gen-clock-operations ((obj occ-obj-ctx)
                                            &key param-only)
  "Return no clock operation callables for a plain occ-obj-ctx OBJ.
PARAM-ONLY is ignored."
  (ignore obj)
  (ignore param-only)
  nil)

;; (cl-defun occ-obj-gen-each-prop-fast-clock-operations (obj &key param-only)
;;   (append (occ-obj-gen-each-prop-edits obj
;;                                        :param-only param-only)
;;           (occ-obj-gen-each-prop-edits (occ-obj-tsk obj)
;;                                        :param-only param-only)))

;;; occ-prop-gen-edit-actions.el ends here
