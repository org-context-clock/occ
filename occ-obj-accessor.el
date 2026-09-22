;;; occ-obj-accessor.el --- occ-api               -*- lexical-binding: t; -*-
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

;;

;;; Code:

(provide 'occ-obj-accessor)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-tsk)
(require 'occ-print)
(require 'occ-obj-ctor)
(require 'occ-helm-actions-config)
(require 'occ-prop)
(require 'occ-rank)
(require 'occ-assert)
(require 'occ-commands)
(require 'occ-util-common)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)


(cl-defmethod occ-obj-class-name (obj)
  (ignore obj)
  "unknown")

(cl-defmethod occ-obj-class-name ((obj symbol))
  (ignore obj)
  "symbol")

(cl-defmethod occ-obj-class-name ((obj null))
  (ignore obj)
  "null")

(cl-defmethod occ-obj-class-name ((obj marker))
  (ignore obj)
  "marker")

(cl-defmethod occ-obj-class-name ((obj occ-tsk))
  (ignore obj)
  "task")

(cl-defmethod occ-obj-class-name ((obj occ-ctsk))
  (ignore obj)
  "context task")

(cl-defmethod occ-obj-class-name ((obj occ-ctxual-tsk))
  (ignore obj)
  "contextual task")


(cl-defmethod occ-obj-obj ((obj occ-obj))
  (occ-obj-return-get-value obj))

(cl-defmethod occ-obj-obj ((obj occ-return))
  (occ-obj-return-get-value obj))


(cl-defmethod occ-obj-obj ((obj null))
  (ignore obj)
  nil)


(cl-defmethod occ-obj-tsk ((obj null))
  (ignore obj)
  nil)

(cl-defmethod occ-obj-tsk ((obj occ-tsk))
  obj)

;; will also cover occ-ctxual-tsk
(cl-defmethod occ-obj-tsk ((obj occ-ctsk))
  (occ-ctsk-tsk obj))

(cl-defmethod occ-obj-tsk ((obj occ-obj-ctx))
  (ignore obj)
  nil)


(cl-defmethod occ-obj-ctx ((obj null))
  (ignore obj)
  nil)

(cl-defmethod occ-obj-ctx ((obj occ-ctx))
  obj)

;; will also cover occ-ctxual-tsk
(cl-defmethod occ-obj-ctx ((obj occ-ctsk))
  (occ-ctsk-ctx obj))

(cl-defmethod occ-obj-ctx ((obj occ-obj-tsk))
  nil)


(cl-defmethod occ-obj-marker ((obj null))
  (ignore obj)
  nil)

(cl-defmethod occ-obj-marker ((obj marker))
  obj)

(cl-defmethod occ-obj-marker ((obj occ-obj-tsk))
  (occ-tsk-marker (occ-obj-tsk obj)))


(cl-defmethod occ-obj-org-marker ((obj null))
  (ignore obj)
  nil)

(cl-defmethod occ-obj-org-marker ((obj marker))
  (occ-mac-with-org-marker obj
    (point-marker)))

(cl-defmethod occ-obj-org-marker ((obj occ-obj-tsk))
  (occ-tsk-marker (occ-obj-tsk obj)))


(cl-defmethod occ-obj-heading-marker ((obj null))
  (ignore obj)
  (make-marker))

(cl-defmethod occ-obj-heading-marker ((obj marker))
  (occ-mac-with-org-marker obj
    (point-marker)))

(cl-defmethod occ-obj-heading-marker ((obj occ-obj-tsk))
  (occ-obj-heading-marker (occ-obj-marker obj)))


(cl-defmethod occ-obj-buffer ((obj null))
  (ignore obj)
  nil)

(cl-defmethod occ-obj-buffer ((obj marker))
  (marker-buffer obj))

(cl-defmethod occ-obj-buffer ((obj occ-obj-tsk))
  (occ-obj-buffer (occ-tsk-marker (occ-obj-tsk obj))))

(cl-defmethod occ-obj-buffer ((obj occ-ctx))
  (occ-ctx-buffer obj))

(cl-defmethod occ-obj-buffer ((obj occ-ctsk))
  (let ((ctx (occ-obj-ctx obj)))
    (occ-ctx-buffer ctx)))


(cl-defmethod occ-obj-file ((obj occ-ctx))
  (occ-ctx-file obj))

(cl-defmethod occ-obj-file ((obj occ-ctsk))
  (let ((ctx (occ-obj-ctx obj)))
    (occ-ctx-file ctx)))


(cl-defmethod occ-obj-callable ((callable occ-callable))
  callable)

(cl-defmethod occ-obj-callable-build ((callable list)
                                      (type (eql :normal)))
  (if (= (length callable) 3)
      (let ((keyword (nth 0 callable))
            (name    (nth 1 callable))
            (fun     (nth 2 callable)))
           (occ-obj-make-callable-normal keyword name fun))
    (occ-error "callable [%s] is not list of 3 elements" callable)))

(cl-defmethod occ-obj-callable-build ((callable list)
                                      (type (eql :generator)))
  (if (= (length callable) 3)
      (let ((keyword (nth 0 callable))
            (name    (nth 1 callable))
            (fun     (nth 2 callable)))
        (occ-obj-make-callable-generator keyword name fun))
    (occ-error "callable [%s] is not list of 3 elements" callable)))

(cl-defmethod occ-obj-callable-normal ((callable list))
  (occ-obj-callable-build callable
                          :normal))

(cl-defmethod occ-obj-callable-generator ((callable list))
  (occ-obj-callable-build callable
                          :generator))

(cl-defmethod occ-obj-callable ((callable list))
  (occ-obj-callable-normal callable))


(cl-defmethod occ-callable-desc     ((callable occ-callable))
  (occ-callable-name callable))

(cl-defmethod occ-obj-callable-desc ((callable occ-callable))
  (occ-callable-desc callable))

(cl-defmethod occ-obj-callable-name ((callable occ-callable))
  (occ-callable-name callable))

;; methods

(cl-defmethod occ-obj-callables ((callable list)
                                 (obj      null))
  "Return list of ((NAME . FUN) ...)"
  (ignore obj)
  (mapcar #'occ-obj-callable
          callable))

(cl-defmethod occ-obj-callables ((callable list)
                                 (obj      occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (ignore obj)
  (occ-obj-callables callable nil))

(cl-defmethod occ-obj-callables ((callable occ-callable-normal)
                                 (obj      occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (ignore obj)
  (list callable))

(cl-defmethod occ-obj-callables ((callable occ-callable-generator)
                                 (obj      occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (occ-debug "occ-obj-callables(callable occ-callable-generator): got %s" (occ-callable-desc callable))
  (let ((fun (occ-callable-fun callable)))
    (let ((callables (funcall fun obj
                              :param-only nil)))
      ;; (occ-assert callables)
      (dolist (x callables)
        (occ-debug "occ-obj-callables(callable occ-callable-generator): generated %s" (occ-callable-desc x)))
      (occ-assert (cl-every #'occ-callable-p
                            callables))
      (occ-assert (cl-notany #'occ-callable-generator-p
                             callables))
      callables)))

;; methods

(cl-defmethod occ-obj-callable-helm-action ((callable occ-callable))
  "Return pair or (NAME . FUN)"
  (cons (occ-callable-name callable)
        (occ-callable-fun  callable)))

;; methods

(cl-defmethod occ-obj-callable-helm-actions ((callables list)
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (let ((callables-list (occ-obj-callables callables obj)))
    (occ-assert (cl-every #'occ-callable-normal-p callables-list))
    (occ-assert (cl-notany #'occ-callable-generator-p callables-list))
    (mapcar #'occ-obj-callable-helm-action
            callables-list)))

(cl-defmethod occ-obj-callable-helm-actions ((callable occ-callable)
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (let ((callables (occ-obj-callables callable
                                      obj)))
    (occ-obj-callable-helm-actions callables)))

(cl-defmethod occ-obj-callable-helm-actions ((callables (head :callables))
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (ignore obj)
  (let ((callables (cl-rest callables)))
    (occ-obj-callable-helm-actions callables)))

(cl-defmethod occ-obj-callable-helm-actions ((callables (head :keywords))
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (ignore obj)
  (let* ((keywords  (cl-rest callables))
         (callables (occ-helm-callables-get keywords)))
    (occ-obj-callable-helm-actions callables)))


;; TODO: Consider preparing
(cl-defmethod occ-obj-ap (xyz)
  (ignore xyz)
  (occ-error "Implement it"))

(cl-defmethod occ-obj-ap-normal (xyz)
  (ignore xyz)
  (occ-error "Implement it"))

(cl-defmethod occ-obj-ap-transf (xyz)
  (ignore xyz)
  (occ-error "Implement it"))


(cl-defmethod occ-obj-ap-base ((ap-obj occ-ap))
  (occ-ap-tree-keybranch ap-obj))

(cl-defmethod occ-obj-ap-base ((ap-obj occ-ap-normal))
  (let ((base (cl-call-next-method)))
    (or base
        (let ((callables (occ-ap-normal-callables ap-obj)))
          (when callables
            (cons :callables callables))))))

(cl-defmethod occ-obj-ap-base ((ap-obj occ-ap-transf))
  (let ((base (cl-call-next-method)))
    (or base
        (let ((transform (occ-ap-transf-transform ap-obj)))
          (when transform
            (cons :transform transform))))))

(cl-defmethod occ-obj-ap-tree-keybranch ((ap-obj occ-ap)
                                         (obj    occ-obj))
  (ignore obj)
  (unless (occ-ap-tree-keybranch ap-obj)
    (occ-error "occ-ap obj %s missing tree-keybranch %s" ap-obj (occ-ap-tree-keybranch ap-obj)))
  (occ-ap-tree-keybranch ap-obj))

(cl-defmethod occ-obj-ap-callables ((ap-obj occ-ap-normal)
                                    (obj occ-obj))
  (occ-debug "occ-obj-ap-callables: ap-obj = %s" ap-obj)
  ;; NOTE:
  ;; If TREE-KEYBRANCH are present then callable must have to generated afresh every time
  ;; this method is called.
  (unless (and (not (occ-ap-tree-keybranch ap-obj))
               (occ-ap-normal-callables ap-obj))
    (occ-debug "Called")
    (let ((tree-keybranch (occ-obj-ap-tree-keybranch ap-obj obj)))
      (let* ((keywords-list (occ-get-keywords-list-from-tree tree-keybranch))
             (callables     (occ-obj-get-callables obj ;; ???
                                                   keywords-list)))
        (unless keywords-list
          (occ-error "for tree-keybranch %s empty keywords-list %s returned "
                     tree-keybranch
                     keywords-list))
        (when tree-keybranch
          (occ-assert callables)
          (occ-assert keywords-list))
        (setf (occ-ap-normal-callables ap-obj) callables))))
  (occ-ap-normal-callables ap-obj))

(cl-defmethod occ-obj-ap-transform ((ap-obj occ-ap-transf))
  "This return callables"
  (unless (occ-ap-transf-transform ap-obj)
    (let ((transform #'(lambda (action
                                candidate)
                         (ignore action)
                         (occ-debug "occ-obj-ap-transform: lambda: ap-obj = %s" ap-obj)
                         (let* ((candidate-obj (occ-obj-obj candidate))
                                (callables (occ-obj-ap-callables ap-obj
                                                                 (occ-obj-obj candidate-obj))))
                           (occ-assert callables)
                           (occ-debug "occ-obj-ap-transform: lambda: transform: callables = %s" callables)
                           (occ-obj-make-ap-normal (cons :callables callables))))))
      (occ-debug "occ-obj-ap-transform: setting transform tp %s" transform)
      (setf (occ-ap-transf-transform ap-obj) transform)))
  (occ-ap-transf-transform ap-obj))


(cl-defmethod occ-obj-ap-helm-actions ((ap-obj list)
                                       (obj occ-obj))
  (let* ((ap-obj    (occ-obj-build-ap-normal ap-obj obj))
         (callables (occ-obj-ap-callables ap-obj obj)))
    (occ-obj-callable-helm-actions callables
                                   obj)))

(cl-defmethod occ-obj-ap-helm-actions ((ap-obj occ-ap-normal)
                                       (obj occ-obj))
  (let ((callables (occ-obj-ap-callables ap-obj obj)))
    (occ-obj-callable-helm-actions callables
                                   obj)))

(cl-defmethod occ-obj-ap-helm-actions ((ap-obj occ-ap-transf)
                                       (obj occ-obj))
  (ignore obj)
  (ignore ap-obj)
  (occ-error "OCC-OBJ-AP-HELM-ACTIONS can not work for OCC-AP-TRANSF as it requires OCC-AP-NORMAL to run TRANSFORMATION function"))


(cl-defmethod occ-obj-ap-helm-transformation ((ap-obj occ-ap-transf))
  (let ((transform (occ-obj-ap-transform ap-obj)))
    (occ-assert transform)
    #'(lambda (action
               candidate)
        ;; (occ-debug "occ-obj-ap-helm-transformation: lambda: transform = %s" transform)
        (occ-assert transform)
        (let* ((candidate-obj (occ-obj-obj candidate))
               (ap-normal-obj (funcall transform
                                       action
                                       candidate-obj)))
          (occ-assert (occ-ap-normal-p ap-normal-obj))
          ;; (occ-debug "helm-transformation: got ap-normal-obj = %s" (occ-name ap-normal-obj))
          (let ((helm-actions (occ-obj-ap-helm-actions ap-normal-obj
                                                       candidate-obj)))
            (occ-assert helm-actions)
            ;; (dolist (a helm-actions)
            ;;   (occ-debug "occ-obj-ap-helm-transformation: helm-action: %s" (prin1-to-string (occ-name a))))
            ;; (when helm-actions
            ;;   (dolist (x helm-actions)
            ;;     (occ-debug "occ-obj-ap-helm-transformation: %s -> %s -> %s"
            ;;                  (car x)
            ;;                  (cdr x)
            ;;                  (functionp (cdr x))))
            ;;   (dolist (x helm-actions)
            ;;     (let ((prompt (car x))
            ;;           (action-callback (cdr x)))
            ;;       (occ-assert (and prompt
            ;;                        (stringp prompt))
            ;;                   t
            ;;                   "Prompt(%s) is nil of not string for Action-Callback(%s)"
            ;;                   prompt
            ;;                   action-callback)
            ;;       (occ-assert (and action-callback
            ;;                        (functionp action-callback))
            ;;                   t
            ;;                   "Action-Callback(%s) is nil of not function for Prompt(%s)"
            ;;                   action-callback
            ;;                   prompt))
            ;;     (occ-debug "occ-obj-ap-helm-transformation: %s -> %s -> %s"
            ;;                   (car x)
            ;;                   (cdr x)
            ;;                   (functionp (cdr x))))
            ;;   (occ-assert (cl-every #'(lambda (x)
            ;;                             (let ((f (cl-rest x))) (and f (functionp f))))
            ;;                         helm-actions))
            ;;   (occ-assert (cl-every #'(lambda (x)
            ;;                             (let ((p (cl-first x))) (and p (stringp p))))
            ;;                         helm-actions)))
            helm-actions)))))


(cl-defmethod occ-obj-ap-helm-transformed-actions ((apn occ-ap-normal)
                                                   (apt occ-ap-transf)
                                                   (obj occ-obj))
  (let ((callables (occ-obj-callable-helm-actions (occ-obj-ap-callables apn obj)
                                                  obj))
        (fun       (occ-obj-ap-helm-transformation apt)))
    (funcall fun callables obj)))


(cl-defmethod occ-obj-ap-helm-get-actions ((obj occ-obj)
                                           (apn occ-ap-normal)
                                           (apt occ-ap-transf))
  (occ-obj-ap-helm-transformed-actions apn apt obj))

(cl-defmethod occ-obj-ap-helm-get-actions ((obj occ-obj)
                                           (apn occ-ap-normal)
                                           (apt null))
  (ignore apt)
  (occ-obj-ap-helm-actions apn obj))

(cl-defmethod occ-obj-ap-helm-get-actions ((obj occ-obj)
                                           (apn null)
                                           (apt occ-ap-transf))
  (ignore obj)
  (ignore apn)
  (ignore apt)
  (occ-error "test"))


(cl-defmethod occ-obj-ap-helm-item ((ap-obj occ-ap-normal)
                                    (obj occ-obj))
  "Return actions"
  (occ-obj-ap-helm-actions ap-obj obj))

(cl-defmethod occ-obj-ap-helm-item ((ap-obj occ-ap-transf)
                                    (obj occ-obj))
  "Return lambda function which do transformation on actions and return actions"
  (ignore obj)
  (occ-obj-ap-helm-transformation ap-obj))


(cl-defmethod occ-obj-ranktbl-with ((tsk occ-obj-tsk)
                                    (ctx occ-obj-ctx))
  (unless (cdr (assoc tsk
                      (occ-ctx-tsk-ranktbl-list ctx)))
    (setf (occ-obj-ranktbl-with tsk
                                ctx)
          (occ-make-ranktbl)))
  (cdr (assoc tsk
              (occ-ctx-tsk-ranktbl-list ctx))))

(cl-defmethod occ-obj-ranktbl-with ((tsk occ-obj-tsk)
                                    (ctx null))
  (occ-obj-ranktbl tsk))

(cl-defmethod (setf occ-obj-ranktbl-with) ((rt  occ-ranktbl)
                                           (tsk occ-obj-tsk)
                                           (ctx occ-obj-ctx))
  (if (cdr (assoc tsk
                  (occ-ctx-tsk-ranktbl-list ctx)))
      (setf (cdr (assoc tsk (occ-ctx-tsk-ranktbl-list ctx))) rt)
    (cl-pushnew (cons tsk rt)
                (occ-ctx-tsk-ranktbl-list ctx))))


(cl-defmethod (setf occ-obj-ranktbl-with) ((rt  occ-ranktbl)
                                           (tsk occ-obj-tsk)
                                           (ctx null))
  (setf (occ-obj-ranktbl tsk) rt))


(cl-defmethod occ-obj-ranktbl ((obj occ-obj-tsk))
  (let ((tsk (occ-obj-tsk obj)))
    (unless (occ-tsk-ranktbl tsk)
      (setf (occ-tsk-ranktbl tsk) (occ-make-ranktbl)))

    (occ-assert (occ-tsk-ranktbl tsk))
    (occ-tsk-ranktbl tsk)))

(cl-defmethod occ-obj-ranktbl ((obj occ-obj-ctx-tsk))
  (occ-obj-ranktbl-with (occ-obj-tsk obj)
                        (occ-obj-ctx obj)))

(cl-defmethod (setf occ-obj-ranktbl) ((rt  occ-ranktbl)
                                      (obj occ-obj-tsk))
  (let ((tsk (occ-obj-tsk obj)))
    (setf (occ-tsk-ranktbl tsk) rt)))

(cl-defmethod (setf occ-obj-ranktbl) ((rt  occ-ranktbl)
                                      (obj occ-obj-ctx-tsk))
  (setf (occ-obj-ranktbl-with (occ-obj-tsk obj)
                              (occ-obj-ctx obj)) rt))


(cl-defmethod occ-obj-member-tsk-rank ((obj occ-ctxual-tsk))
  ;; (occ-debug "occ-obj-member-tsk-rank(occ-ctxual-tsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  ;; (occ-debug "occ-obj-member-tsk-rank(occ-ctxual-tsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-ctxual-tsk-tsk obj)))
    ;; (occ-obj-rank tsk) ;; - BUG
    (occ-obj-rank tsk)))


;; occ-tsk - accessors
(cl-defmethod occ-obj-format-string ((obj occ-tsk)
                                     &optional
                                     no-propterties)
  ;; (occ-debug "occ-tsk-format-string(occ-tsk=%s)" obj)
  (let ((format-string (occ-tsk-format-string obj)))
    (unless format-string
      (setf (occ-tsk-format-string obj) (occ-obj-build-format-string obj
                                                                     no-propterties)))
    (occ-tsk-format-string obj)))

(cl-defmethod (setf occ-obj-format-string) (value (obj occ-tsk))
  ;; (occ-debug "occ-tsk-format-string(occ-tsk=%s)" obj)
  (setf (occ-tsk-format-string obj) value))


;; occ-tsk - accessors
(cl-defmethod occ-obj-format-file ((obj occ-tsk))
  ;; (occ-debug "occ-tsk-format-file(occ-tsk=%s)" obj)
  (let ((format-file (occ-tsk-format-file obj)))
    (unless format-file
      (setf (occ-tsk-format-file obj) (occ-obj-build-format-file obj)))
    (occ-tsk-format-file obj)))

(cl-defmethod (setf occ-obj-format-file) (value (obj occ-tsk))
  ;; (occ-debug "occ-tsk-format-file(occ-tsk=%s)" obj)
  (setf (occ-tsk-format-file obj) value))


;; occ-ctx - accessors
(cl-defmethod occ-obj-avgrank ((obj occ-ctx))
  ;; (occ-debug "occ-obj-avgrank(occ-ctx=%s)" obj)
  (let ((avgrank (occ-ctx-avgrank obj)))
    (unless avgrank
      (setf (occ-ctx-avgrank obj) (occ-obj-calculate-avgrank obj)))
    (occ-ctx-avgrank obj)))

(cl-defmethod (setf occ-obj-avgrank) (value (obj occ-ctx))
  ;; (occ-debug "occ-obj-avgrank(occ-ctx=%s)" obj)
  (setf (occ-ctx-avgrank obj) value))


;; occ-ctx - accessors
(cl-defmethod occ-obj-varirank ((obj occ-ctx))
  ;; (occ-debug "occ-obj-varirank(occ-ctx=%s)" obj)
  (let ((varirank (occ-ctx-varirank obj)))
    (unless varirank
      (setf (occ-ctx-varirank obj) (occ-obj-calculate-varirank obj)))
    (occ-ctx-varirank obj)))

(cl-defmethod (setf occ-obj-varirank) (value (obj occ-ctx))
  ;; (occ-debug "occ-obj-varirank(occ-ctx=%s)" obj)
  (setf (occ-ctx-varirank obj) value))


;; occ-collection - accessors
(cl-defmethod occ-obj-avgrank ((obj occ-collection))
  ;; (occ-debug "occ-obj-avgrank(occ-collection=%s)" obj)
  (let ((avgrank (occ-collection-avgrank obj)))
    (unless avgrank
      (setf (occ-collection-avgrank obj) (occ-obj-calculate-avgrank obj)))
    (occ-collection-avgrank obj)))

(cl-defmethod (setf occ-obj-avgrank) (value (obj occ-collection))
  ;; (occ-debug "occ-obj-avgrank(occ-collection=%s)" obj)
  (setf (occ-collection-avgrank obj) value))


;; occ-ctxual-tsk - accessors
(cl-defmethod occ-obj-varirank ((obj occ-collection))
  ;; (occ-debug "occ-obj-varirank(occ-collection=%s)" obj)
  (let ((varirank (occ-collection-varirank obj)))
    (unless varirank
      (setf (occ-collection-varirank obj) (occ-obj-calculate-varirank obj)))
    (occ-collection-varirank obj)))

(cl-defmethod (setf occ-obj-varirank) (value (obj occ-collection))
  ;; (occ-debug "occ-obj-varirank(occ-collection=%s)" obj)
  (setf (occ-collection-varirank obj) value))


(cl-defgeneric occ-obj-candidate (obj)
  "occ-obj-candidate")

(cl-defmethod occ-obj-candidate ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (cons (occ-obj-format obj nil t)
        obj))

(cl-defmethod occ-obj-candidate ((obj occ-obj-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-obj-format obj nil t)
        obj))


;; find place to put these all function

(cl-defmethod occ-do-checkout ((obj occ-obj-tsk))
  (occ-do-op-props-checkout obj))
(cl-defmethod occ-do-checkout ((obj occ-ctxual-tsk))
  (occ-do-op-props-checkout (occ-ctxual-tsk-tsk obj)))


;; BUG  in Menu
;;           "Set debug obj"
;;           "Call with obj"
;;       occ-return getting passed

(defun occ-util-read-sexp-from-minibuffer (prompt)
 (cl-first (read-from-string (read-from-minibuffer prompt))))

(cl-defmethod occ-do-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj obj)
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (obj) ,exp-with-obj) obj)))))
    (funcall fun)))

(cl-defmethod occ-do-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj-name     (occ-util-read-sexp-from-minibuffer "obj name: ")) ;prefill with obj
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (,obj-name) ,exp-with-obj) obj)))))
    (funcall fun)))

(let ((occ-debug-object nil))
  (cl-defmethod occ-do-set-debug-obj ((obj occ-obj-tsk))
    (setq occ-debug-object obj)
    (occ-debug "Use (occ-get-debug-obj) to access object."))
  (defun occ-describe-debug-obj ()
    (interactive)
    (occ-do-describe-obj occ-debug-object))
  (defun occ-get-debug-obj ()
    (interactive)
    occ-debug-object))

(cl-defmethod occ-do-describe-obj ((obj occ-obj-tsk))
  (let ((buf (get-buffer-create (format "*helpful occ-object: %s*"
                                        (occ-obj-format obj)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; (setf (buffer-string) "")
        (erase-buffer)
        ;; (cl-prettyprint obj)
        (insert (format "Object: %s\n\n"
                        (occ-obj-Format obj)))
        (insert (pp-to-string obj)))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)))


(cl-defun occ-current-tsk (&key other-allowed)
  (let ((ctxual-tsk (cl-first *occ-clocked-ctxual-tsk-ctx-history*))
        (org-clock  (or (occ-valid-marker org-clock-marker)
                        (occ-valid-marker org-clock-hd-marker))))
    (if (and ctxual-tsk
             org-clock
             (occ-obj-marker= ctxual-tsk
                              org-clock))
        (occ-obj-tsk ctxual-tsk)
        (when org-clock
          (let ((msg (cond
                      ((and ctxual-tsk
                            org-clock)
                       (format "occ-current-ctxual-tsk: %s %d from head of *occ-clocked-ctxual-tsk-ctx-history* is not equal to current clocking clock %s %d"
                        (occ-obj-Format ctxual-tsk nil t nil)
                        (marker-position (occ-obj-marker ctxual-tsk))
                        (occ-obj-Format org-clock nil t nil)
                        (marker-position org-clock)))
                      ((and (null ctxual-tsk)
                            org-clock)
                       (format "occ-current-ctxual-tsk: %s is outside of occ"
                               (occ-obj-Format org-clock nil t nil)))
                      (t (format "Unknown error")))))
            (if (not other-allowed)
                (occ-error msg)
              (occ-debug :warning msg)
              (occ-obj-make-tsk org-clock)))))))

(cl-defun occ-current-ctxual-tsk (&key other-allowed)
  (let ((current-tsk (occ-current-tsk :other-allowed other-allowed)))
    (occ-obj-build-ctxual-tsk-with current-tsk
                                   (occ-obj-make-ctx-at-point))))


(defun occ-default-collection (&optional noerror)
  (let ((key (occ-collector-default-key)))
    (unless (occ-collector-roots key)
      (if (occ-collector-spec key)
          (progn
            ;; (occ-obj-make-collection (occ-collector-spec key))
            (occ-obj-tsks (occ-collector-get key) t))
        (progn
          (occ-uninsinuate)
          (occ-debug "(occ-collector-spec key) is nil, set it using M-x occ-obj-build-spec or set (occ-collector-spec key), disabled occ")
          (unless noerror
            (occ-error "(occ-collector-spec key) is nil, set it using M-x occ-obj-build-spec or set (occ-collector-spec key), disabled occ")))))
    (occ-collector-get key)))


(cl-defmethod occ-do-reset-tsks ((collection null))
  (ignore collection)
  nil)

(cl-defmethod occ-do-reset-tsks ((collection occ-list-collection))
  (setf (occ-list-collection-list collection) nil))

(cl-defmethod occ-do-reset-tsks ((collection occ-tree-collection))
  ;; (occ-message "tree %s" (occ-tree-collection-list collection))
  (setf (occ-list-collection-list collection) nil)
  (setf (occ-tree-collection-tree collection) nil))


(cl-defmethod occ-obj-tsk-collection ((tsk occ-obj-tsk))
  (occ-tsk-collection (occ-obj-tsk tsk)))

(cl-defmethod occ-obj-collection ((tsk occ-obj-tsk))
  (occ-tsk-collection (occ-obj-tsk tsk)))

(cl-defmethod occ-obj-collection ((obj symbol))
  (let ((key obj))
    (occ-collector-get key)))

(cl-defmethod occ-obj-collection ((obj occ-obj-collection))
  obj)

;; BUG (occ-obj-collect-tsks tree) method not called (occ-obj-collect-tsks list)
;; is called and it is settign (occ-list-collection-list coll), need to be fixed

(cl-defmethod occ-obj-collect-tsks (collection
                                    &optional
                                    force)
  (ignore collection)
  (ignore force)
  (occ-error "first argument should be of type (or occ-tree-collection occ-list-collection)"))

(cl-defmethod occ-obj-collect-tsks ((collection occ-list-collection)
                                    force)
  (unless (and (not force)
               (occ-list-collection-list collection))
    (setf (occ-list-collection-list collection) (occ-obj-build-tsks collection)))
  (occ-list-collection-list collection))

;; FIND: what it mean by tsks collection-tree are same ?
(cl-defmethod occ-obj-collect-tsks ((collection occ-tree-collection)
                                    &optional
                                    force)
  (unless (and (not force)
               (occ-tree-collection-tree collection))
    (setf (occ-tree-collection-tree collection) (occ-obj-build-tsks collection)))
  (occ-tree-collection-tree collection))
;; To deprecate

(cl-defmethod occ-obj-tsks (collection
                            &optional
                            force)
  (ignore collection)
  (ignore force)
  (occ-error "first argument should be of type (or occ-tree-collection occ-list-collection)"))

;; FIND: what it mean by tsks collection-tree are same ?
(cl-defmethod occ-obj-tsks ((collection occ-tree-collection)
                            &optional
                            force)
  (occ-obj-collect-tsks collection force))

(cl-defmethod occ-obj-tsks ((collection occ-list-collection)
                            force)
  (occ-obj-collect-tsks collection force))


(cl-defmethod occ-obj-collection-tsks ((collection occ-tree-collection))
  (unless (occ-tree-collection-tree collection)
    (occ-obj-tsks collection nil)
    (run-hooks '*occ-collection-change-hook*))
  (occ-tree-collection-tree collection))

(cl-defmethod occ-obj-collection-tsks ((collection occ-list-collection))
  (unless (occ-list-collection-list collection)
    (occ-obj-tsks collection nil)
    (run-hooks '*occ-collection-change-hook*))
  (occ-list-collection-list collection))


(cl-defmethod occ-obj-collect-files ((collection occ-tree-collection)
                                     &optional
                                     force)
  (ignore force)
  (unless (occ-tree-collection-files collection)
    ;; (occ-obj-collect-tsks collection nil)
    (let ((occ-files (let ((tsks  (occ-obj-collection-tsks collection))
                           (files '()))
                       (mapc #'(lambda (tsk)
                                 (occ-mapc-tree-tsks #'(lambda (tsk args)
                                                         (ignore args)
                                                         (cl-pushnew (occ-tsk-file tsk) files))
                                                     tsk
                                                     nil))
                             tsks)
                       files)))
       (setf (occ-tree-collection-files collection) (remove nil (delete-dups occ-files)))))
  (occ-tree-collection-files collection))

(cl-defmethod occ-obj-collect-files ((collection occ-list-collection)
                                     &optional
                                     force)
  (ignore force)
  (unless (and (null force)
               (occ-list-collection-files collection))
    (setf (occ-list-collection-files collection)
          (occ-list-collection-roots collection)))
  (occ-list-collection-files collection))

(cl-defmethod occ-obj-files (&optional
                             collection
                             force)
  (occ-obj-collect-files (or collection
                             (occ-default-collection))
                         force))


;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

(cl-defgeneric occ-obj-list (obj
                             &key
                             builder
                             obtrusive)
  "occ-obj-list")

(cl-defmethod occ-obj-list ((obj occ-ctx)
                            &key
                            builder
                            obtrusive)
  "return CTXUAL-TSKs container"
  (occ-obj-list-with obj
                     (occ-default-collection)
                     :builder   builder
                     :obtrusive obtrusive))

(cl-defmethod occ-obj-list ((obj null)
                            &key
                            builder
                            obtrusive)
  "return TSKs container"
  (ignore obj)
  (occ-obj-list (occ-obj-make-ctx-at-point)
                :builder   builder
                :obtrusive obtrusive))

(cl-defmethod occ-obj-list ((collection occ-tree-collection)
                            &key
                            builder
                            obtrusive)
  (ignore builder)
  (ignore obtrusive)
  (unless (occ-tree-collection-list collection)
    (let ((tsks     (occ-obj-collection-tsks collection))
          (tsk-list '()))
      (mapc #'(lambda (tsk)
                (occ-mapc-tree-tsks #'(lambda (subtsk args)
                                        (ignore args)
                                        (setf tsk-list (nconc tsk-list (list subtsk))))
                                    tsk
                                    nil))
            tsks)
      (setf (occ-tree-collection-list collection)
            tsk-list)))
  (occ-tree-collection-list collection))

(cl-defmethod occ-obj-list ((collection occ-list-collection)
                            &key
                            builder
                            obtrusive)
  (ignore builder)
  (ignore obtrusive)
  (let ((tsks (occ-obj-collection-tsks collection)))
    tsks))


(cl-defmethod occ-obj-list-with ((obj        occ-ctx)
                                 (collection occ-collection)
                                 &key
                                 builder
                                 obtrusive)
  "return CTSKs list"
  (ignore obtrusive)
  (let ((builder (or builder
                     #'occ-obj-build-ctsk-with)))
    (let ((ctsks (occ-run-unobtrusively obtrusive
                   (let ((tsks (occ-obj-list collection))) ;;????TODO
                     (when tsks
                       (mapcar #'(lambda (tsk) (funcall builder tsk obj))
                               tsks))))))
      (unless (eq t ctsks)
        ;; BUG: TODO: convey it tpo occ-select occ-do-clock-in
        (occ-debug "Busy user input `%s'"
                     (if (numberp last-input-event)
                         (single-key-description last-input-event)
                       last-input-event))
        ctsks))))

;; BUG: For now, facing some STRUCK issue with occ-run-unobtrusively
(cl-defmethod occ-obj-list-with ((obj        occ-ctx)
                                 (collection occ-collection)
                                 &key
                                 builder
                                 obtrusive)
  "return CTSKs list"
  (ignore obtrusive)
  (let ((builder (or builder
                     #'occ-obj-build-ctsk-with)))
    (let ((ctsks (let ((tsks (occ-obj-list collection))) ;;????TODO
                   (when tsks
                     (mapcar #'(lambda (tsk) (funcall builder tsk obj))
                             tsks)))))
      (unless (eq t ctsks)
        ;; BUG: TODO: convey it top occ-select occ-do-clock-in
        (occ-debug "Busy user input `%s'"
                     (if (numberp last-input-event)
                         (single-key-description last-input-event)
                       last-input-event))
        ctsks))))

(cl-defmethod occ-obj-list-with ((obj null)
                                 (collection occ-collection)
                                 &key
                                 builder
                                 obtrusive)
  "return TSKs container"
  (ignore obj)
  (occ-obj-list-with (occ-obj-make-ctx-at-point)
                     (occ-obj-collection collection)
                     :builder   builder
                     :obtrusive obtrusive))

(cl-defmethod occ-obj-list-with ((obj occ-ctx)
                                 (collection null)
                                 &key
                                 builder
                                 obtrusive)
  "return TSKs container"
  (ignore obj)
  (ignore collection)
  (ignore builder)
  (ignore obtrusive)
  nil)

(cl-defmethod occ-obj-list-with ((obj null)
                                 (collection null)
                                 &key
                                 builder
                                 obtrusive)
  "return TSKs container"
  (ignore obj)
  (ignore collection)
  (ignore builder)
  (ignore obtrusive)
  nil)

(cl-defmethod occ-obj-length ((collection symbol))
  (length (occ-obj-list (occ-obj-collection collection))))

(cl-defmethod occ-obj-length ((collection occ-collection))
  (length (occ-obj-list (occ-obj-collection collection))))


(cl-defmethod occ-obj-select-obj ((source null))
  (ignore source)
  nil)

(cl-defmethod occ-ob-select-obj ((source occ-hsrc))
  (occ-hsrc-obj source))

(cl-defmethod occ-obj-obj ((source occ-hsrc))
  (occ-hsrc-obj source))


(cl-defmethod occ-obj-rank ((obj null))
  (ignore obj)
  nil)

(cl-defmethod occ-ob-rank ((obj occ-obj-collection))
  (occ-obj-collection-rank obj))

(cl-defmethod occ-obj-rank ((obj occ-hsrc))
  (occ-hsrc-rank obj))


(cl-defmethod occ-obj-level ((obj null))
  (ignore obj)
  nil)

(cl-defmethod occ-ob-rank ((obj occ-obj-collection))
  (occ-obj-collection-level obj))

(cl-defmethod occ-obj-level ((obj occ-hsrc))
  (occ-hsrc-level obj))


(cl-defgeneric occ-name (obj)
  "Return name")

(cl-defmethod occ-name ((obj null))
  "return NIL"
  (ignore obj)
  "NIL")

(cl-defmethod occ-name ((obj occ-obj))
  "return name"
  (occ-obj-name obj))

;; (cl-defmethod occ-name ((obj list))
;;   "return name"
;;   (cond ((assoc 'name obj) (cdr (assoc 'name obj)))
;;         ((assoc :name obj) (cdr (assoc :name obj)))
;;         ((assoc "name" obj) (cdr (assoc "name" obj)))
;;         ((plist-get 'name obj) (plist-get 'name obj))
;;         ((plist-get :name obj) (plist-get :name obj))
;;         ((plist-get "name" obj) (plist-get "name" obj))))

(cl-defmethod occ-name ((obj list))
  "return name"
  (or (cdr (assoc 'name obj))
      (cdr (assoc :name obj))
      (cdr (assoc "name" obj))
      (plist-get 'name obj)
      (plist-get :name obj)
      (plist-get "name" obj)))

(cl-defmethod occ-name ((obj buffer))
  "return name"
  (buffer-name obj))

(cl-defmethod occ-name ((obj marker))
  "return name"
  (format "%s" obj))

;;; occ-obj-accessor.el ends here
