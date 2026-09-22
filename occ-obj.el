;;; occ-obj.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-obj)



;; * cl-macs
;; https://nullprogram.com/blog/2018/02/14/
;;
;; * TODO org-base-buffer
;;
;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode
;;
;; https://stackoverflow.com/questions/40884764/lisp-get-all-slot-names-from-an-class-instance
;;
;; https://www.gnu.org/software/emacs/manual/html_node/eieio/Quick-Start.html#Quick-Start
;;
;; Static method
;; https://www.gnu.org/software/emacs/manual/html_node/eieio/Static-Methods.html
;;
;; https://elpa.gnu.org/packages/cl-generic.html
;;
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


(defvar occ-verbose 0)

;;
;;                                                                                               obj
;;                                                                                                |
;;                                       +--------------------------------------------------------+--------------------------------------------------------+---------------------------------------+-------------------------+-----------------------------------------------+
;;                                       |                                                        |                                                        |                                       |                          |                                              |
;;                                    obj-tsk                                                 obj-prop                                                  obj-ctx                                   stat                      action                                         agent
;;                                       |                                                        |                                                        |                                                                  |                                              |
;;          +----------------------------+----------------------------+                         prop                               +-----------------------+-----------------------+                               +----------+-----------+                       +----------+-----------+
;;          |                                                         |                                                            |                                               |                               |                      |                       |                      |
;;     obj-ctx-tsk                                                   tsk                                                    obj-collection                                        ctx                             direct                transformer             user                    emacs
;;          |                                                         |                                                            |
;;        ctsk                                             +----------+----------+                                      +----------+----------+
;;          |                                              |                     |                                      |                     |
;;      ctxual-tsk                                      tree-tsk              list-tsk                           list-collection         tree-collection
;;


(cl-defstruct occ-obj
  (name "occ-obj"))
(cl-defstruct (occ-obj-tsk (:include occ-obj))
  "Object to hold tsk ctx obj-ctx-tsk"
  ;; prop-ranks-plist
  (selectable nil)
  filter-rank-plist)
(cl-defstruct (occ-obj-ctx-tsk (:include occ-obj-tsk))
  "Object to hold ctsk occ-ctxual-tsk")
;; (cl-defstruct (occ-obj-prop (:include occ-obj))
;;   "Will hold prop")
(cl-defstruct (occ-stat (:include occ-obj))
  "occ-stat"
  average
  stddev
  variance)


(cl-defstruct (occ-obj-ctx (:include occ-obj))
  "Will hold ctx"
  stat-plist)
(cl-defstruct (occ-obj-collection (:include occ-obj-ctx))
  "Will hold collection"
  (desc "occ-obj-collection")
  spec
  (roots nil)
  (files nil)
  (depth 0)
  (limit 0)
  ;; BUG: TODO: implement it for occ-obj-helm-build-collection-source issue
  (rank  0)
  (level :optional)
  ;; TODO: implement it
  (file-lcp nil))
(cl-defstruct (occ-collection (:include occ-obj-collection))
  "occ-collection")
(cl-defstruct (occ-list-collection (:include occ-collection))
  "occ-list-collection"
  (list nil))
(cl-defstruct (occ-tree-collection (:include occ-list-collection))
  "occ-tree-collection"
  (tree nil))


;; (cl-defmethod occ-collection-dyn ()
;;   collection)

;; (cl-defstruct (occ-prop (:include occ-obj-prop))
;;   value)


(cl-defstruct (occ-ranktbl (:include occ-obj))
  plist
  value
  inheritable
  nonheritable
  max-decendent)
  ;; acquired


;; NOTE: Remember when adding new attributes, nned to destroy existing object, else it will cause miss-match.
(cl-defstruct (occ-tsk (:include occ-obj-tsk))
  "occ-tsk"
  ;; [[file:~/.repos/git/main/resource/info/doc/orgs/private/doc/contents/org/tasks/personal/works/emacs/todo.org::*Each%20task%20should%20have%20different%20types%20of%20actions%20associated%20to%20it,%20default%20is%20to%20clock-in%20to%20it][Each task should have different types of actions associated to it, default is to clock-in to it]]
  dummy
  collection
  parent
  action
  ;; *** Each task should have different types of actions associated to it, default is to clock-in to it
  ;; - rest could be
  ;; to ignore this task continue where already clockin or not
  ;; which could be used to avoid non-interesting buffers
  ;; - etc many different could be thought
  heading
  heading-prop
  marker
  file
  point
  clock-sum
  level
  subtree-level
  file-level
  cat
  format-file
  format-string
  sibling-count
  children-count
  descendant-count
  descendant-weight
  plist
  (ranktbl (occ-make-ranktbl)))
  ;; rank-inheritable
  ;; rank-nonheritable
  ;; rank-acquired
  ;; plist


(cl-defstruct (occ-tree-tsk (:include occ-tsk))
  "occ-tree-tsk"
  subtree)
(cl-defstruct (occ-list-tsk (:include occ-tsk))
  "occ-list-tsk")


(cl-defstruct (occ-ctx (:include occ-obj-ctx))
  "occ-ctx"
  buffer
  file
  ;; tsk-aplist
  tsk-ranktbl-list)


(cl-defstruct (occ-ctsk (:include occ-obj-ctx-tsk))
  "occ-ctsk"
  ;; Reason to have one more occ-ctsk along with occ-ctxual-tsk to avoid calculating rank.
  ctx
  tsk)
(cl-defstruct (occ-ctxual-tsk (:include occ-ctsk)))

  ;; rank-inheritable
  ;; rank-nonheritable
  ;; rank-acquired


;; TODO: need to add ability to be proxy object
;;       to pass all method to underlying object in
;;       Menu
;;           "Set debug obj"
;;           "Call with obj"
;;       occ-return getting passed
(cl-defstruct occ-return
  "occ-return"
  label
  value)


(cl-defstruct (occ-callable (:include occ-obj))
  "occ-callable"
  keyword
  fun)
(cl-defstruct (occ-callable-normal (:include occ-callable))
  "occ-callable-normal")
(cl-defstruct (occ-callable-generator (:include occ-callable))
  "occ-callable-generator")


(cl-defstruct (occ-static-filter (:include occ-obj))
  "occ-static-filter"
  keyword
  ;; fun
  points-gen-fn
  compare-fn
  default-pivot-fn
  rank-display-fn
  rank-select-fn)

(cl-defstruct (occ-obj-dyn-filter (:include occ-obj))
  "occ-obj-dyn-filter"
  init-closure-fn
  seq-closure-fn
  display-filter-closure-fn
  selectable-filter-closure-fn
  ;; points-closure-fn
  ;; pivot
  increment-closure-fn
  decrement-closure-fn
  reset-closure-fn)

(cl-defstruct (occ-dyn-filter (:include occ-obj-dyn-filter))
  "occ-dyn-filter"
  prev)

(cl-defstruct (occ-combined-dyn-filter (:include occ-obj-dyn-filter))
  "occ-combined-dyn-filter"
  curr-closure-fn
  prev-closure-fn
  next-closure-fn)


(cl-defstruct (occ-ap (:include occ-obj))
  "AP stand for `actions pack'"
  tree-keybranch)
(cl-defstruct (occ-ap-normal (:include occ-ap))
  "AP stand for `actions pack'"
  callables)
(cl-defstruct (occ-ap-transf (:include occ-ap-normal))
  "AP stand for `actions pack'"
  transform)


;; BUG: TODO - use other name then occ-hsrc-... like occ-hselect-... as select is verb, while I need noun
(cl-defstruct (occ-hsrc (:include occ-obj))
  "helm select obj or source"
  obj
  (rank  0)
  (level :optional))

(cl-defstruct (occ-hsrc-null (:include occ-hsrc))
  "helm select object")

(cl-defstruct (occ-hsrc-candidate (:include occ-hsrc))
  "helm select object")

(cl-defstruct (occ-hsrc-source (:include occ-hsrc))
  "helm select source")


(cl-defstruct (occ-obj-agent (:include occ-obj))
  "Will hold dummy agent")

(cl-defstruct (occ-user-agent (:include occ-obj-agent))
  "Will hold dummy user agent")

(cl-defstruct (occ-org-agent (:include occ-obj-agent))
  "Will hold dummy org agent")

(cl-defstruct (occ-emacs-agent (:include occ-obj-agent))
  "Will hold dummy org agent")

;;; occ-obj.el ends here
