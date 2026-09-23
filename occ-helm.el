;;; occ-helm.el --- occ helm                         -*- lexical-binding: t; -*-

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

;; occ-helm.el holds the plumbing of the helm-based selection UI: the
;; global registry of helm callables, the helm action keyword tree, and
;; the org-capture+ template selector for new-task capture.
;;
;; It declares the `occ-helm-callables' registry (`occ-helm-callable-add',
;; `occ-helm-callables-get') and the keyword tree `occ-helm-actions-tree'
;; (`occ-add-helm-actions', `occ-get-keywords-list-from-tree'), both
;; populated by occ-helm-actions-config-initialize; the generic
;; `occ-obj-get-helm-actions' resolves a tree branch into callables via
;; the `occ-obj-get-callables' methods on `occ-obj' and `occ-obj-tsk'.
;; `occ-obj-capture+-helm-select-template' selects an org-capture+ heading
;; template; occ-capture.el uses it when creating new tasks.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-helm)


(require 'lotus-tree-manager)
(require 'org-capture+-helm)
(require 'org-capture+-helm-dynamic)
(require 'lotus-helm)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj)
(require 'occ-obj-ctor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-obj-method)
(require 'occ-helm-method)
(require 'occ-util-common)


(defun occ-obj-capture+-helm-select-template ()
  "Select an org-capture+ heading template through helm.
The template selector is generated for the keyword branch
t occ tsk clockable todo."
  (let ((selector (helm-template-gen-selector #'org-capture+-tree-predicate
                                              '(t occ tsk clockable todo)
                                              0)))
    (funcall selector)))


(defvar occ-helm-callables)
(defun occ-helm-callable-add (callable)
  "Add CALLABLE to the global occ-helm-callables registry."
  (cl-pushnew callable
              occ-helm-callables))
(defun occ-helm-callables-get (keylist)
  "Return the registered callables whose keyword is in KEYLIST."
  (mapcan #'(lambda (key)
              (cl-remove-if-not #'(lambda (callable)
                                    (eq key
                                        (occ-callable-keyword callable)))
                                occ-helm-callables))
          keylist))


(cl-defmethod occ-obj-get-callables ((obj occ-obj-tsk)
                                     keylist)
  "Return the callables applicable to the occ-obj-tsk OBJ.
Resolves every callable returned by occ-helm-callables-get for
KEYLIST through occ-obj-callables."
  ;; TODO: do we require (apply #'append ...)
  (occ-debug "(OCC-OBJ-GET-CALLABLES OCC-OBJ-TSK): called")
  (mapcan #'(lambda (callable)
              (occ-obj-callables callable
                                 obj))
          (occ-helm-callables-get keylist)))

(cl-defmethod occ-obj-get-callables ((obj occ-obj)
                                     keylist)
  "Return the callables applicable to the occ-obj OBJ specialization.
Resolves every callable returned by occ-helm-callables-get for
KEYLIST through occ-obj-callables."
  ;; TODO: do we require (apply #'append ...)
  (occ-debug "(OCC-OBJ-GET-CALLABLES OCC-OBJ): called")
  (mapcan #'(lambda (callable)
              (occ-obj-callables callable
                                 obj))
          (occ-helm-callables-get keylist)))


(defvar occ-helm-actions-tree '(t))

(defun occ-add-helm-actions (tree-keybranch class &rest actions)
  "Add ACTIONS of CLASS to occ-helm-actions-tree under TREE-KEYBRANCH."
  (apply #'tree-add-class-item
         occ-helm-actions-tree
         tree-keybranch
         class
         actions))


(defun occ-get-keywords-list-from-tree (tree-keybranch)
  "Collect the keywords of occ-helm-actions-tree under TREE-KEYBRANCH."
  (tree-collect-items occ-helm-actions-tree ;tree
                      nil                   ;predicate
                      tree-keybranch      ;arg
                      0))                   ;level


(cl-defgeneric occ-obj-get-helm-actions (obj tree-keybranch)
  "occ-obj-get-helm-actions")

(cl-defmethod occ-obj-get-helm-actions ((obj null) tree-keybranch)
  "Return the helm actions for the null OBJ specialization.
Flattens the callables resolved for the keywords of
TREE-KEYBRANCH."
  ;; (occ-debug "occ-obj-get-helm-actions: called with obj = %s, tree-keybranch = %s" obj tree-keybranch)
  (mapcan #'identity
         (occ-obj-get-callables obj
                            (occ-get-keywords-list-from-tree tree-keybranch))))

(cl-defmethod occ-obj-get-helm-actions ((obj occ-obj) tree-keybranch)
  "Return the helm actions for the occ-obj OBJ specialization.
Flattens the callables resolved for the keywords of
TREE-KEYBRANCH."
  ;; (occ-debug "occ-obj-get-helm-actions: called with obj = %s, tree-keybranch = %s" obj tree-keybranch)
  (mapcan #'identity
          (occ-obj-get-callables obj
                                 (occ-get-keywords-list-from-tree tree-keybranch))))

(cl-defmethod occ-obj-get-helm-actions-genertator ((obj null) tree-keybranch)
  "Return a helm action generator for the null OBJ specialization.
The returned function maps a candidate through
occ-obj-get-helm-actions for TREE-KEYBRANCH."
  (ignore obj)
  #'(lambda (action candidate)
      (ignore action)
      (occ-obj-get-helm-actions candidate
                                    tree-keybranch)))

(cl-defmethod occ-obj-get-helm-actions-genertator ((obj occ-obj) tree-keybranch)
  "Return a helm action generator for the occ-obj OBJ specialization.
The returned function maps a candidate through
occ-obj-get-helm-actions for TREE-KEYBRANCH."
  (ignore obj)
  #'(lambda (action candidate)
      (ignore action)
      (occ-obj-get-helm-actions candidate
                                      tree-keybranch)))


















(occ-testing
 (tree-collect-items occ-helm-actions-tree nil '(t actions general edit) 0)
 (tree-collect-items occ-helm-actions-tree (occ-obj-make-ctx-at-point) '(t actions general edit) 0)
 (occ-obj-get-helm-actions (occ-obj-make-ctx-at-point) '(t actions general edit)))

(occ-testing
  (collect-alist (tree-collect-items occ-helm-actions-tree nil '(t actions select) 0))
  (occ-helm-callables-get :edits-gen)
  (occ-helm-callables-get :identity)
  ;; (occ-helm-callables-get :identity :clock-in)
  (occ-helm-callables-get :identity)
  (occ-helm-callables-get :clock-in))

(occ-testing
 (occ-get-keywords-list-from-tree '(t actions select))
 (occ-get-keywords-list-from-tree '(t actions general))
 (occ-get-keywords-list-from-tree '(t actions general edit))
 (occ-get-keywords-list-from-tree '(t actions select general edit))
 (occ-helm-callables-get (occ-get-keywords-list-from-tree '(t actions select general edit)))
 (occ-obj-get-callables (occ-obj-make-ctx-at-point)
                        (occ-get-keywords-list-from-tree '(t actions select general edit)))

 (ignore (cl-first (occ-obj-get-callables (occ-obj-make-ctx-at-point (occ-get-keywords-list-from-tree '(t actions select general edit))))))

 (ignore (tree-collect-items occ-helm-actions-tree nil '(t actions general) 0)))

 ;; (occ-obj-get-helm-actions-plist nil
 ;;                             (cl-first (occ-get-keywords-list-from-tree '(t actions general))))

 ;; (ignore (mapcan #'(lambda (name-action-key)
 ;;                     (occ-obj-get-helm-actions-plist nil
 ;;                                                     name-action-key))
 ;;               (occ-get-keywords-list-from-tree '(t actions general))))
 

;;; occ-helm.el ends here
