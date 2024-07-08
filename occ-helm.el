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

;;

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
  (let ((selector (helm-template-gen-selector #'org-capture+-tree-predicate
                                              '(t occ tsk clockable todo)
                                              0)))
    (funcall selector)))


(defvar occ-helm-callables)
(defun occ-helm-callable-add (callable)
  (cl-pushnew callable
              occ-helm-callables))
(defun occ-helm-callables-get (keylist)
  (mapcan #'(lambda (key)
              (cl-remove-if-not #'(lambda (callable)
                                    (eq key
                                        (occ-callable-keyword callable)))
                                occ-helm-callables))
          keylist))


(cl-defmethod occ-obj-get-callables ((obj occ-obj-tsk)
                                     keylist)
  ;; TODO: do we require (apply #'append ...)
  (occ-debug "(OCC-OBJ-GET-CALLABLES OCC-OBJ-TSK): called")
  (mapcan #'(lambda (callable)
              (occ-obj-callables callable
                                 obj))
          (occ-helm-callables-get keylist)))

(cl-defmethod occ-obj-get-callables ((obj occ-obj)
                                     keylist)
  ;; TODO: do we require (apply #'append ...)
  (occ-debug "(OCC-OBJ-GET-CALLABLES OCC-OBJ): called")
  (mapcan #'(lambda (callable)
              (occ-obj-callables callable
                                 obj))
          (occ-helm-callables-get keylist)))


(defvar occ-helm-actions-tree '(t))

(defun occ-add-helm-actions (tree-keybranch class &rest actions)
  (apply #'tree-add-class-item
         occ-helm-actions-tree
         tree-keybranch
         class
         actions))


(defun occ-get-keywords-list-from-tree (tree-keybranch)
  (tree-collect-items occ-helm-actions-tree ;tree
                      nil                   ;predicate
                      tree-keybranch      ;arg
                      0))                   ;level


(cl-defgeneric occ-obj-get-helm-actions (obj tree-keybranch)
  "occ-obj-get-helm-actions")

(cl-defmethod occ-obj-get-helm-actions ((obj null) tree-keybranch)
  ;; (occ-debug "occ-obj-get-helm-actions: called with obj = %s, tree-keybranch = %s" obj tree-keybranch)
  (mapcan #'identity
         (occ-obj-get-callables obj
                            (occ-get-keywords-list-from-tree tree-keybranch))))

(cl-defmethod occ-obj-get-helm-actions ((obj occ-obj) tree-keybranch)
  ;; (occ-debug "occ-obj-get-helm-actions: called with obj = %s, tree-keybranch = %s" obj tree-keybranch)
  (mapcan #'identity
          (occ-obj-get-callables obj
                                 (occ-get-keywords-list-from-tree tree-keybranch))))

(cl-defmethod occ-obj-get-helm-actions-genertator ((obj null) tree-keybranch)
  (ignore obj)
  #'(lambda (action candidate)
      (ignore action)
      (occ-obj-get-helm-actions candidate
                                    tree-keybranch)))

(cl-defmethod occ-obj-get-helm-actions-genertator ((obj occ-obj) tree-keybranch)
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
