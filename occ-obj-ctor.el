;;; occ-obj-ctor.el --- occ-api               -*- lexical-binding: t; -*-
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

;; occ-obj-ctor.el contains the constructors of the OCC object model,
;; following the OCC convention that `occ-obj-make-*' allocates fresh
;; objects while `occ-obj-build-*' reuses or coerces existing ones.
;;
;; It builds task snapshots from org buffers (`occ-make-tsk-at-point',
;; `occ-obj-make-tsk-at-point', `occ-obj-make-tsk(-with)', the
;; `occ-obj-tsk-builder' and `occ-obj-tsk-builder-at-point' builders that
;; pick the list or tree builder from the collection spec, and
;; `occ-obj-org-entry-tsk-p' which rejects `[NONTSK]' entries), interned
;; contexts (`occ-obj-make-ctx-at-point', `occ-obj-make-ctx' with the
;; `occ-ctx-hash' cache), task-context pairs (`occ-obj-make-ctsk-with',
;; `occ-obj-make-ctxual-tsk-with' and their `build' forms), collections
;; (`occ-obj-make-collection', `occ-obj-build-collection'), callables,
;; filters and action packs (`occ-obj-make-callable-normal',
;; `occ-obj-make-callable-generator', `occ-obj-make-static-filter',
;; `occ-obj-make-dyn-filter', `occ-obj-make-ap-normal',
;; `occ-obj-make-ap-transf'), the `occ-return-*' label variables,
;; `occ-obj-build-return-lambda', `occ-build-hsrc-*' source builders and
;; the agent getters `occ-get-user-agent' / `occ-get-org-agent' /
;; `occ-get-emacs-agent'.  Object model layer.
;;
;; Reference notes on cl-generic type specializers:
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

;;; Code:

(provide 'occ-obj-ctor)


(require 'seq)
(require 'list-utils)
(require 'org-element)
(require 'org-clock)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj)
;; (require 'occ-prop)
(require 'occ-rank)
(require 'occ-property-methods)
(require 'occ-assert)
(require 'occ-helm)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-prop-base)


(defvar *occ-collection-change-hook* nil
  "run when *occ-collection-change-hook* get changed.")


;; org-todo-line-regexp

;; (defun org-get-todo-state ()
;;   "Return the TODO keyword of the current subtree."
;;   (save-excursion
;;     (occ-back-to-heading)
;;     (and (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
;;          (match-end 2)
;;          (match-string 2))))


(defun occ-heading-content-only ()
  "Return the content text of the current heading without drawers."
  (when (org-at-heading-p)
    (save-excursion
      (save-restriction
        (let ((start (progn
                       (goto-char (org-element-property :contents-begin
                                                        (org-element-at-point)))
                       (while (org-at-drawer-p)
                         (goto-char (org-element-property :end
                                                          (org-element-at-point))))
                       (point))))
          (unless (org-at-heading-p)
            (progn
              (outline-next-heading)
              ;; (outline-next-visible-heading 1)
              (backward-char)
              (buffer-substring start (point)))))))))


;; utils
(defun occ-util-keyword2sym (key)
  "Return the symbol named by KEY as a plain symbol from a keyword."
  (key2sym key))

(defun occ-util-plist-mapcar (fun plist)
  "return alist"
  (mapcar fun
          (seq-partition plist 2)))

(defun occ-util-plist-value-mapcar (fun plist)
  "Return PLIST with FUN applied to each of its values via mapcan."
  (mapcan #'identity
          (occ-util-plist-mapcar #'(lambda (c))
                                (list (cl-first c)
                                      (funcall fun (nth 1 c)))
                            plist)))

(defun occ-tsk-plist-from-org (plist)
  "Convert org PLIST of properties into an OCC task plist."
  (let ((ret-plist (mapcan #'identity
                           (occ-util-plist-mapcar #'(lambda (c)
                                                      (list (cl-first c)
                                                            (occ-obj-from-org (occ-util-keyword2sym (cl-first c))
                                                                              'get
                                                                              (nth 1 c))))
                                                  plist))))
    (occ-assert (cl-evenp (length plist)))
    (occ-debug "occ-tsk-plist-from-org: plist %s" plist)
    (occ-assert (cl-evenp (length ret-plist)))
    ret-plist))

(occ-testing
 (ignore (eq (aref (symbol-name :test) 0) ?:))
 (list-utils-flatten '((a  b) (x)))
 (seq-partition (list :a 1 :b 2 :c 3 :more (list 4 5 6) :x nil) 2)
 (mapcan #'identity (seq-partition (list :a 1 :b 2 :c 3 :more (list 4 5 6) :x nil) 2)))


;; utils
(defun occ-get-tsk-category (heading plist)
  "Return the category for HEADING from a tag or PLIST or TODO default."
  (if (stringp heading)
      (or (when (string-match "<\\([a-zA-Z][a-zA-Z0-9]+\\)>" heading)
            (match-string 1
                          heading))
          (plist-get plist :CATEGORY)
          "TODO")
    "TODO"))


(cl-defgeneric occ-obj-tsk-builder (obj)
  "Task constructor")

(cl-defmethod occ-obj-tsk-builder ((collection occ-tree-collection))
  "Return the tree task constructor for a tree COLLECTION."
  (ignore collection)
  #'make-occ-tree-tsk)

(cl-defmethod occ-obj-tsk-builder ((collection occ-list-collection))
  "Return the list task constructor for a list COLLECTION."
  (ignore collection)
  #'make-occ-list-tsk)

(cl-defmethod occ-obj-tsk-builder ((collection occ-obj-collection))
  "Return the base task constructor for an occ-obj COLLECTION."
  (ignore collection)
  #'make-occ-tsk)

(cl-defmethod occ-obj-tsk-builder ((tsk occ-tree-tsk))
  "Return the tree task constructor for a tree TSK object."
  (ignore tsk)
  #'make-occ-tree-tsk)

(cl-defmethod occ-obj-tsk-builder ((tsk occ-list-tsk))
  "Return the list task constructor for a list TSK object."
  (ignore tsk)
  #'make-occ-list-tsk)

(cl-defmethod occ-obj-tsk-builder ((tsk occ-obj-tsk))
  "Return the base task constructor for an occ-obj TSK."
  (ignore tsk)
  #'make-occ-tsk)

(cl-defmethod occ-obj-tsk-builder ((obj null))
  "Return the base task constructor for a null OBJ."
  (ignore obj)
  #'make-occ-tsk)

(cl-defmethod occ-obj-tsk-builder ((builder compiled-function))
  "Signal an error for a compiled-function BUILDER."
  (occ-error "Error %s" builder)
  builder)

(cl-defmethod occ-obj-tsk-builder ((builder symbol))
  "Signal an error for a symbol BUILDER."
  (occ-error "Error %s" builder)
  builder)


(defun occ-obj-org-entry-tsk-p (org-element)
  "Check if or entry qualify to be a Task"
  (let ((heading (org-element-property :raw-value org-element)))
    (not (if heading
             (string-match "\\[NONTSK\\]" heading)))))

;; (string-match "\\[NONTSK\\]" "Hello aaa")

(defun occ-make-tsk-at-point (collection
                              file)
  "Build a task snapshot from the org heading at point for COLLECTION.
FILE selects whether the task is treated as file-backed; [NONTSK]
entries are rejected."
  ;; (occ-debug "occ-make-tsk-at-point: Builder %s" builder)
  (let ((builder (occ-obj-tsk-builder collection))
        (heading-with-string-prop (if (org-before-first-heading-p)
                                      'noheading
                                    (org-get-heading 'notags))))
    (let ((heading      (when heading-with-string-prop
                          (if (eq heading-with-string-prop 'noheading)
                              heading-with-string-prop
                            (substring-no-properties heading-with-string-prop))))
          (heading-prop heading-with-string-prop)
          (marker       (move-marker (make-marker)
                                     (point)
                                     (org-base-buffer (current-buffer))))
          (file-name    (buffer-file-name))
          (point        (point))
          (clock-sum    (if (org-before-first-heading-p)
                            0
                          (org-clock-sum-current-item)))
          ;; BUG: TODO: SHOULD need to maintain plist of :PROPERTIES:
          ;; separately as keys for these are returned in UPCASE. while it
          ;; is not the case with other generic properties which are not
          ;; part of :PROPERTIES: block.

          ;; NOTE also these two are mixed in one list only
          (tsk-element  (org-element-at-point)))
      (let ((tsk-plist tsk-element))
        (occ-assert (cl-evenp (length tsk-plist)))
        (when (occ-obj-org-entry-tsk-p tsk-element)
          (let ((tsk (funcall builder       ; build = make-occ-tree-tsk, make-occ-list-tsk
                              ;; (occ-obj-iXntf-from-org) from Org world to Occ world.
                              :name         (occ-obj-from-org 'name 'get heading)
                              :dummy        (if file :file nil)
                              :collection   collection
                              :parent       nil
                              :action       nil
                              :heading      (occ-obj-from-org 'heading 'get heading)
                              :heading-prop (occ-obj-from-org 'heading-prop 'get heading-prop)
                              :marker       (occ-obj-from-org 'marker 'get marker)
                              :file         (occ-obj-from-org 'file 'get file-name)
                              :point        (occ-obj-from-org 'point 'get point)
                              :clock-sum    (occ-obj-from-org 'clock-sum 'get clock-sum)
                              :level        (occ-obj-from-org 'level 'get (org-element-property :level tsk-element))
                              :cat          (occ-obj-from-org 'cat 'get (occ-get-tsk-category heading tsk-plist))
                              :plist        (occ-tsk-plist-from-org tsk-plist))))
            (dolist (prop (occ-obj-properties-for-ranking nil))
              (when (occ-obj-list-p nil prop)
                ;; set :plist here
                ;;
                ;; Unconditionally Set property as - (tsk-plist    (nth 1 (org-element-at-point)))
                ;; which is using in occ-obj-get-property and occ-obj-set-property
                ;; put list also as atom
                ;; (occ-obj-set-property tsk prop (org-entry-get nil (occ-obj-org-property-name prop)))
                (let ((value (occ-do-operation (occ-obj-marker tsk)
                                               'get
                                               prop
                                               nil)))
                  (when value
                    (occ-obj-set-property tsk
                                          prop
                                          value)))))
            ;; (occ-obj-reread-props tsk)      ;reset list properties
            tsk))))))

(cl-defmethod occ-obj-make-tsk-at-point ((collection occ-obj-collection)
                                         file)
  "Make-tsk-at-point specialization on occ-obj COLLECTION and FILE."
  (occ-make-tsk-at-point (occ-obj-collection collection)
                         file))
(cl-defmethod occ-obj-make-tsk-at-point ((tsk occ-obj-tsk)
                                         file)
  "Make-tsk-at-point specialization on occ-obj TSK and FILE."
  (occ-obj-make-tsk-at-point (occ-obj-collection tsk)
                             file))


(cl-defmethod occ-obj-tsk-builder-at-point ((collection occ-obj-collection))
  "Return a builder lambda making a task at point for COLLECTION."
  #'(lambda (file)
      (occ-obj-make-tsk-at-point (occ-obj-collection collection)
                                 file)))

(cl-defmethod occ-obj-tsk-builder-at-point ((collection null))
  "Return a builder lambda making a task at point for a null COLLECTION."
  #'(lambda (file)
      (occ-obj-make-tsk-at-point (occ-default-collection)
                                 file)))


(cl-defmethod occ-obj-make-tsk ((obj number)
                                &optional
                                collection
                                subtree-level)
  "Make-tsk specialization on a number OBJ position within COLLECTION."
  ;; (occ-debug "point %s" obj)
  (let ((collection (if collection      ;for when collection were not passed, like in (defun occ-current-tsk (&optional occ-other-allowed) ...)
                        (occ-obj-collection collection)
                      (occ-default-collection))))
   (if (<= obj (point-max))
     (save-restriction
       (save-excursion
         (goto-char obj)
         (let ((builder (occ-obj-drived-tsk-builder collection
                                                    subtree-level)))
           (funcall builder nil)))))))

(cl-defmethod occ-obj-make-tsk ((obj marker)
                                &optional
                                collection
                                subtree-level)
  "Make-tsk specialization on a marker OBJ resolved in its buffer."
  ;; (occ-debug "point %s" obj)
  (when (and (marker-buffer obj)
             (numberp (marker-position obj)))
      (with-current-buffer (marker-buffer obj)
        (when (<= (marker-position obj)
                  (point-max))
          (occ-obj-make-tsk (marker-position obj)
                            (occ-obj-collection collection)
                            subtree-level)))))

(cl-defmethod occ-obj-make-tsk ((obj null)
                                &optional
                                collection
                                subtree-level)
  "Make-tsk specialization on null OBJ using point-marker."
  (ignore obj)
  (occ-debug "current pos %s" (point-marker))
  (occ-obj-make-tsk (point-marker)
                    (occ-obj-collection collection)
                    subtree-level))

(cl-defmethod occ-obj-make-tsk ((obj occ-tsk)
                                &optional
                                collection
                                subtree-level)
  "Make-tsk specialization on occ-tsk OBJ, returning it unchanged."
  obj)


(cl-defmethod occ-obj-make-tsk-with ((obj null)
                                     (collection occ-obj-collection)
                                     &optional
                                     subtree-level)
  "Make-tsk-with specialization on null OBJ and a COLLECTION."
  (occ-obj-make-tsk obj
                    collection
                    subtree-level))
(cl-defmethod occ-obj-make-tsk-with ((obj number)
                                     (collection occ-obj-collection)
                                     &optional
                                     subtree-level)
  "Make-tsk-with specialization on a number OBJ and a COLLECTION."
  (occ-obj-make-tsk obj
                    collection
                    subtree-level))
(cl-defmethod occ-obj-make-tsk-with ((obj marker)
                                     (collection occ-obj-collection)
                                     &optional
                                     subtree-level)
  "Make-tsk-with specialization on a marker OBJ and a COLLECTION."
  (occ-obj-make-tsk obj
                    collection
                    subtree-level))

(cl-defmethod occ-obj-make-tsk-with ((obj null)
                                     (tsk occ-obj-tsk)
                                     &optional
                                     subtree-level)
  "Make-tsk-with specialization on null OBJ and an occ-obj TSK."
  (occ-obj-make-tsk obj
                    (occ-obj-collection (occ-obj-tsk tsk))
                    (or subtree-level
                        (occ-obj-set-property tsk 'subtree-level))))
(cl-defmethod occ-obj-make-tsk-with ((obj number)
                                     (tsk occ-obj-tsk)
                                     &optional
                                     subtree-level)
  "Make-tsk-with specialization on a number OBJ and an occ-obj TSK."
  (occ-obj-make-tsk obj
                    (occ-obj-collection tsk)
                    subtree-level))
(cl-defmethod occ-obj-make-tsk-with ((obj marker)
                                     (tsk occ-obj-tsk)
                                     &optional
                                     subtree-level)
  "Make-tsk-with specialization on a marker OBJ and an occ-obj TSK."
  (occ-obj-make-tsk obj
                    (occ-obj-collection (occ-obj-tsk tsk))
                    subtree-level))

(cl-defmethod occ-obj-make-tsk-with ((obj occ-tsk)
                                     anything
                                     &optional
                                     subtree-level)
  "Make-tsk-with specialization on occ-tsk OBJ returning it unchanged."
  obj)


(defvar occ-ctx-hash (make-hash-table :test #'equal :size 100 :rehash-size 20))
(defun occ-ctx-puthash (plist ctx)
  "Store CTX under PLIST in occ-ctx-hash and return CTX."
  (puthash plist ctx occ-ctx-hash))
(defun occ-ctx-gethash (plist)
  "Return the interned context stored under PLIST or nil."
  (gethash plist occ-ctx-hash))
(defun occ-ctx-remhash (plist)
  "Remove the context stored under PLIST from occ-ctx-hash."
  (remhash plist occ-ctx-hash))
(defun occ-ctx-clrhash ()
  "Clear the occ-ctx-hash context cache."
  (clrhash occ-ctx-hash))
(defun occ-ctx-hashlen ()
 "Return the number of interned contexts in occ-ctx-hash."
 (hash-table-count occ-ctx-hash))


(cl-defmethod occ-obj-make-stat (&key average stddev variance)
  "Make a fresh occ-stat from the keyword AVERAGE STDDEV and VARIANCE."
  (make-occ-stat :average  average
                 :stddev   stddev
                 :variance variance))


(cl-defgeneric occ-obj-make-ctx (obj)
  "occ-obj-make-ctx")

(cl-defmethod occ-obj-make-ctx-at-point (&optional mrk)
  "Return the context for MRK and intern it in occ-ctx-hash.
The buffer is resolved via org-base-buffer with the window only a
fallback."
  (let* ((mrk   (or mrk
                    (point-marker)))
         (buff  (marker-buffer mrk))
         (buff  (if buff
                    (if (bufferp buff)
                        buff
                      (if (stringp buff)
                          (or (get-buffer buff)
                              (if (file-exists-p buff)
                                  (get-file-buffer buff)))))
                  (window-buffer)))
         (buff  (org-base-buffer buff))
         (file  (buffer-file-name buff))
         (plist (list :name (buffer-name buff)
                      :file file
                      :buffer buff)))
    (unless (occ-ctx-gethash plist)
      (occ-ctx-puthash plist (make-occ-ctx :name (buffer-name buff)
                                           :file file
                                           :buffer buff)))
    (occ-ctx-gethash plist)))

(cl-defmethod occ-obj-make-ctx ((obj buffer))
  "Make-ctx specialization on a buffer OBJ."
  (let ((mrk (make-marker)))
    (set-marker mrk 0 obj)
    (occ-obj-make-ctx-at-point mrk)))

(cl-defmethod occ-obj-make-ctx ((obj marker))
  "Make-ctx specialization on a marker OBJ."
  (occ-obj-make-ctx-at-point obj))

(cl-defmethod occ-obj-make-ctx ((obj null))
  "Make-ctx specialization on null OBJ using point-marker."
  (ignore obj)
  (occ-obj-make-ctx-at-point (point-marker)))

(cl-defmethod occ-obj-make-ctx ((obj occ-ctx))
  "Make-ctx specialization on occ-ctx OBJ returning it unchanged."
  obj)


(cl-defgeneric occ-obj-make-ctsk-with (tsk ctx)
  "occ-obj-make-ctsk-with")

(cl-defmethod occ-obj-make-ctsk-with ((tsk occ-tsk)
                                      (ctx occ-ctx))
  "Make a fresh occ-ctsk pairing occ-tsk TSK with occ-ctx CTX."
  ;; use occ-obj-build-ctsk-with
  (make-occ-ctsk :name nil
                 :tsk  tsk
                 :ctx  ctx))

(cl-defmethod occ-obj-make-ctsk ((obj occ-ctsk))
  "Make-ctsk specialization on occ-ctsk OBJ returning it unchanged."
  obj)

(cl-defmethod occ-obj-make-ctsk ((obj occ-ctxual-tsk))
  "Make a fresh occ-ctsk from the task and context of OBJ."
  ;; use occ-obj-build-ctsk-with
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (make-occ-ctsk :name nil
                   :tsk  tsk
                   :ctx  ctx)))

(cl-defmethod occ-obj-build-ctsk-with ((tsk occ-tsk) ;ctor
                                       (ctx occ-ctx))
  "Build or coerce an occ-ctsk pairing TSK with CTX."
  (occ-obj-make-ctsk-with tsk
                          ctx))

(cl-defmethod occ-obj-build-ctsk ((obj occ-ctxual-tsk))
  "Build-ctsk specialization on occ-ctxual-tsk OBJ."
  (occ-obj-make-ctsk obj))

(cl-defmethod occ-obj-build-ctsk ((obj occ-ctsk))
  "Build-ctsk specialization on occ-ctsk OBJ returning it unchanged."
  obj)


(cl-defgeneric occ-obj-make-ctxual-tsk-with (tsk
                                             ctx
                                             &optional
                                             rank)
  "occ-obj-make-ctxual-tsk-with")

(cl-defmethod occ-obj-make-ctxual-tsk-with ((tsk occ-tsk)
                                            (ctx occ-ctx)
                                            &optional
                                            rank)
  "Make a fresh occ-ctxual-tsk pairing TSK with CTX while ignoring RANK."
  ;; use occ-obj-build-ctxual-tsk-with
  (make-occ-ctxual-tsk :name nil
                       :tsk  tsk
                       :ctx  ctx))
                       ;; :rank rank

(cl-defmethod occ-obj-build-ctxual-tsk-with ((tsk occ-tsk) ;ctor
                                             (ctx occ-ctx))
  "Build or coerce an occ-ctxual-tsk pairing TSK with CTX."
  (occ-obj-make-ctxual-tsk-with tsk
                                ctx))

(cl-defmethod occ-obj-build-ctxual-tsk-with ((tsk occ-ctxual-tsk) ;ctor
                                             (ctx occ-ctx))
  "Build-ctxual-tsk-with specialization on occ-ctxual-tsk TSK and CTX."
  (ignore tsk)
  (ignore ctx)
  (debug))

(cl-defmethod occ-obj-build-ctxual-tsk-with ((tsk null) ;ctor
                                             (ctx occ-ctx))
  "Build-ctxual-tsk-with specialization on null TSK and CTX returning nil."
  (ignore tsk)
  (ignore ctx)
  nil)

(cl-defmethod occ-obj-make-ctxual-tsk ((obj occ-ctsk)
                                       &optional
                                       rank)
  "Make a fresh occ-ctxual-tsk from the task and context of occ-ctsk OBJ."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (make-occ-ctxual-tsk :name nil
                         :tsk  tsk
                         :ctx  ctx)))
                         ;; :rank rank

(cl-defmethod occ-obj-make-ctxual-tsk ((obj occ-ctxual-tsk)
                                       &optional
                                       rank)
  "Make-ctxual-tsk specialization on occ-ctxual-tsk OBJ returning OBJ."
  (ignore rank)
  obj)

(cl-defmethod occ-obj-build-ctxual-tsk ((obj occ-ctsk)
                                        &optional
                                        rank)
  "Build-ctxual-tsk specialization on occ-ctsk OBJ passing RANK."
  (occ-obj-make-ctxual-tsk obj
                           rank))

(cl-defmethod occ-obj-build-ctxual-tsk ((obj occ-ctxual-tsk)
                                        &optional
                                        rank)
  "Build-ctxual-tsk specialization on occ-ctxual-tsk OBJ returning OBJ."
  (ignore rank)
  obj)


(cl-defmethod occ-obj-build-obj-with ((obj occ-tsk)
                                      (ctx occ-ctx))
  "Build-obj-with specialization on occ-tsk OBJ and occ-ctx CTX."
  (occ-obj-build-ctxual-tsk-with obj
                                 ctx))

(cl-defmethod occ-obj-build-obj-with ((obj occ-tsk)
                                      (ctx null))
  "Build-obj-with specialization on occ-tsk OBJ and null CTX.
The context is taken from point."
  (ignore ctx)
  (occ-obj-build-obj-with obj
                            (occ-obj-make-ctx-at-point)))


(cl-defmethod occ-obj-make-collection ((desc string)
                                       (key symbol)
                                       (spec (eql :tree))
                                       (files list)
                                       (depth integer)
                                       (limit integer)
                                       (rank  integer)
                                       (level symbol))
  "Make a fresh occ-tree-collection named by KEY from FILES.
DESC SPEC DEPTH LIMIT RANK and LEVEL configure the collection."
  (make-occ-tree-collection :desc  desc
                            :name  (symbol-name key) ;; "tsk collection tree"
                            :spec  spec
                            :roots files
                            :depth depth
                            :limit limit
                            :rank  rank
                            :level level))

(cl-defmethod occ-obj-make-collection ((desc string)
                                       (key symbol)
                                       (spec (eql :list))
                                       (files list)
                                       (depth integer)
                                       (limit integer)
                                       (rank  integer)
                                       (level symbol))
  "Make a fresh occ-list-collection named by KEY from FILES.
DESC SPEC DEPTH LIMIT RANK and LEVEL configure the collection."
  (make-occ-list-collection :desc  desc
                            :name  (symbol-name key) ;; "tsk collection list"
                            :spec  spec
                            :roots files
                            :depth depth
                            :limit limit
                            :rank  rank
                            :level level))



(cl-defmethod occ-obj-build-collection ((desc string)
                                        (key symbol)
                                        (spec (eql :tree))
                                        (files list)
                                        (depth integer)
                                        (limit integer)
                                        (rank  integer)
                                        (level symbol))
  "Build or coerce a tree collection named by KEY from FILES."
  (occ-obj-make-collection desc
                           key
                           spec
                           files
                           depth
                           limit
                           rank
                           level))

(cl-defmethod occ-obj-build-collection ((desc string)
                                        (key symbol)
                                        (spec (eql :list))
                                        (files list)
                                        (depth integer)
                                        (limit integer)
                                        (rank  integer)
                                        (level symbol))
  "Build or coerce a list collection named by KEY from FILES."
  (occ-obj-make-collection desc
                           key
                           spec
                           files
                           depth
                           limit
                           rank
                           level))


(defun occ-obj-make-return (label
                            value)
  "Make a fresh occ-return tagged with LABEL carrying VALUE."
  (make-occ-return :label label
                   :value value))


(defun occ-obj-make-callable-normal (keyword
                                     name
                                     fun)
  "Dynamic object"
  (make-occ-callable-normal :keyword keyword
                            :name    name
                            :fun     fun))

(defun occ-obj-make-callable-generator (keyword
                                        name
                                        fun)
  "Dynamic object"
  (make-occ-callable-generator :keyword keyword
                               :name    name
                               :fun     fun))

(defun occ-obj-build-callable-normal (keyword
                                      name
                                      fun)
  "Callable creation and to be stored via (OCC-HELM-CALLABLE-ADD CALLABLE)"
  (let ((callable (occ-obj-make-callable-normal keyword
                                                name
                                                fun)))
    (occ-helm-callable-add callable)
    callable))

(defun occ-obj-build-callable-generator (keyword
                                         name
                                         fun)
  "Callable creation and to be stored via (OCC-HELM-CALLABLE-ADD CALLABLE)"
  (let ((callable (occ-obj-make-callable-generator keyword
                                                   name
                                                   fun)))
    (occ-helm-callable-add callable)
    callable))


(cl-defun occ-obj-make-static-filter (keyword
                                      name
                                      &key
                                      points-gen-fn
                                      compare-fn
                                      default-pivot-fn
                                      rank-select-fn
                                      rank-display-fn)
  "Dynamic object"
  (make-occ-static-filter :keyword keyword
                          :name    name
                          :points-gen-fn points-gen-fn
                          :compare-fn compare-fn
                          :default-pivot-fn default-pivot-fn
                          :rank-select-fn rank-select-fn
                          :rank-display-fn rank-display-fn))

(cl-defun occ-obj-build-static-filter (keyword
                                       name
                                       &key
                                       points-gen-fn
                                       compare-fn
                                       default-pivot-fn
                                       rank-select-fn
                                       rank-display-fn)

  "Filter creation and to be stored via (OCC-HELM-STATIC-FILTER-ADD FILTER)"
  (let ((filter (occ-obj-make-static-filter keyword
                                            name
                                            :points-gen-fn points-gen-fn
                                            :compare-fn compare-fn
                                            :default-pivot-fn default-pivot-fn
                                            :rank-select-fn rank-select-fn
                                            :rank-display-fn rank-display-fn)))
    (occ-obj-static-filter-add filter)
    filter))


(cl-defun occ-obj-make-dyn-filter (name
                                   &key
                                   init-closure-fn
                                   seq-closure-fn
                                   display-filter-closure-fn
                                   selectable-filter-closure-fn
                                   increment-closure-fn
                                   decrement-closure-fn
                                   reset-closure-fn
                                   prev)
  "Make a fresh dynamic filter named NAME from the closure keywords."
  (make-occ-dyn-filter :name name
                       :init-closure-fn init-closure-fn
                       :seq-closure-fn seq-closure-fn
                       :display-filter-closure-fn display-filter-closure-fn
                       :selectable-filter-closure-fn selectable-filter-closure-fn
                       :increment-closure-fn increment-closure-fn
                       :decrement-closure-fn decrement-closure-fn
                       :reset-closure-fn reset-closure-fn
                       :prev prev))

(cl-defun occ-obj-build-dyn-filter (name
                                    &key
                                    init-closure-fn
                                    seq-closure-fn
                                    display-filter-closure-fn
                                    selectable-filter-closure-fn
                                    increment-closure-fn
                                    decrement-closure-fn
                                    reset-closure-fn
                                    prev)
  "Build or coerce a dynamic filter named NAME from the closure keywords."
  (occ-obj-make-dyn-filter name
                           :init-closure-fn init-closure-fn
                           :seq-closure-fn seq-closure-fn
                           :display-filter-closure-fn display-filter-closure-fn
                           :selectable-filter-closure-fn selectable-filter-closure-fn
                           :increment-closure-fn increment-closure-fn
                           :decrement-closure-fn decrement-closure-fn
                           :reset-closure-fn reset-closure-fn
                           :prev prev))


(cl-defun occ-obj-make-combined-dyn-filter (name
                                            &key
                                            curr-closure-fn
                                            prev-closure-fn
                                            next-closure-fn
                                            init-closure-fn
                                            seq-closure-fn
                                            display-filter-closure-fn
                                            selectable-filter-closure-fn
                                            increment-closure-fn
                                            decrement-closure-fn
                                            reset-closure-fn)
  "Make a fresh combined dynamic filter named NAME from closures."
  (make-occ-combined-dyn-filter :name name
                                :init-closure-fn init-closure-fn
                                :seq-closure-fn seq-closure-fn
                                :display-filter-closure-fn display-filter-closure-fn
                                :selectable-filter-closure-fn selectable-filter-closure-fn
                                :increment-closure-fn increment-closure-fn
                                :decrement-closure-fn decrement-closure-fn
                                :reset-closure-fn reset-closure-fn
                                :curr-closure-fn curr-closure-fn
                                :prev-closure-fn prev-closure-fn
                                :next-closure-fn next-closure-fn))

(cl-defun occ-obj-build-combined-dyn-filter (name
                                             &key
                                             curr-closure-fn
                                             prev-closure-fn
                                             next-closure-fn
                                             init-closure-fn
                                             seq-closure-fn
                                             display-filter-closure-fn
                                             selectable-filter-closure-fn
                                             increment-closure-fn
                                             decrement-closure-fn
                                             reset-closure-fn)
  "Build or coerce a combined dynamic filter named NAME from closures."
  (occ-obj-make-combined-dyn-filter name
                                    :curr-closure-fn curr-closure-fn
                                    :prev-closure-fn prev-closure-fn
                                    :next-closure-fn next-closure-fn
                                    :init-closure-fn init-closure-fn
                                    :seq-closure-fn seq-closure-fn
                                    :display-filter-closure-fn display-filter-closure-fn
                                    :selectable-filter-closure-fn selectable-filter-closure-fn
                                    :increment-closure-fn increment-closure-fn
                                    :decrement-closure-fn decrement-closure-fn
                                    :reset-closure-fn reset-closure-fn))


;; ctors
(cl-defmethod occ-obj-make-ap-normal ((ap-obj list))
  "Make-ap-normal specialization on a list AP-OBJ as tree-keybranch."
  (make-occ-ap-normal :tree-keybranch ap-obj))

(cl-defmethod occ-obj-make-ap-normal ((ap-obj occ-ap-normal))
  "Make-ap-normal specialization on occ-ap-normal AP-OBJ returning it."
  ap-obj)

(cl-defmethod occ-obj-make-ap-normal ((ap-obj (head :tree-keybranch)))
  "Make-ap-normal specialization on a head tree-keybranch AP-OBJ."
  (let ((tree-keybranch (cl-rest ap-obj)))
    (make-occ-ap-normal :tree-keybranch tree-keybranch)))

(cl-defmethod occ-obj-make-ap-normal ((ap-obj (head :callables)))
  "Make-ap-normal specialization on a head callables AP-OBJ."
  (let ((callables (cl-rest ap-obj)))
    (make-occ-ap-normal :callables (occ-obj-callables callables nil))))

(cl-defmethod occ-obj-make-ap-normal ((ap-obj (head :keywords)))
  "Make-ap-normal specialization on a head keywords AP-OBJ."
  (let* ((keywords  (cl-rest ap-obj))
         (callables (occ-helm-callables-get keywords)))
   (make-occ-ap-normal :callables callables)))


(cl-defmethod occ-obj-make-ap-transf ((ap-obj list))
  "Make-ap-transf specialization on a list AP-OBJ as tree-keybranch."
  (make-occ-ap-transf :tree-keybranch ap-obj))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj occ-ap-normal))
  "Make-ap-transf specialization on occ-ap-normal AP-OBJ converting it."
  (let ((callables (occ-ap-normal-callables ap-obj)))
    (occ-assert callables t "ap-obj should have callable")
    (make-occ-ap-transf :callables (occ-obj-callables callables nil))))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj occ-ap-transf))
  "Make-ap-transf specialization on occ-ap-transf AP-OBJ returning it."
  ap-obj)

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :tree-keybranch)))
  "Make-ap-transf specialization on a head tree-keybranch AP-OBJ."
  (let ((tree-keybranch (cl-rest ap-obj)))
    (make-occ-ap-transf :tree-keybranch tree-keybranch)))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :callables)))
  "Make-ap-transf specialization on a head callables AP-OBJ."
  (let ((callables (cl-rest ap-obj)))
    (make-occ-ap-transf :callables callables)))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :keywords)))
  "Make-ap-transf specialization on a head keywords AP-OBJ."
  (let* ((keywords   (cl-rest ap-obj))
         (callables (occ-helm-callables-get keywords)))
    (make-occ-ap-transf :callables callables)))

(cl-defmethod occ-obj-make-ap-transf ((ap-obj (head :transform)))
  "Make-ap-transf specialization on a head transform AP-OBJ."
  (let ((transform (cl-rest ap-obj)))
    (make-occ-ap-transf :transform transform)))


;; ctors
(cl-defmethod occ-obj-build-ap-normal ((ap-obj list)
                                       &optional
                                       optional-obj)
  "Build-ap-normal specialization on a list AP-OBJ as tree-keybranch."
  (ignore optional-obj)
  (make-occ-ap-normal :tree-keybranch ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj (head :tree-keybranch))
                                       &optional
                                       optional-obj)
  "Build-ap-normal specialization on a head tree-keybranch AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-normal ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj (head :callables))
                                       &optional
                                       optional-obj)
  "Build-ap-normal specialization on a head callables AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-normal ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj (head :keywords))
                                       &optional
                                       optional-obj)
  "Build-ap-normal specialization on a head keywords AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-normal ap-obj))

(cl-defmethod occ-obj-build-ap-normal ((ap-obj occ-ap-normal)
                                       &optional
                                       optional-obj)
  "Build-ap-normal specialization on occ-ap-normal AP-OBJ returning it."
  (ignore optional-obj)
  ap-obj)

(cl-defmethod occ-obj-build-ap-normal ((ap-obj null)
                                       &optional
                                       optional-obj)
  "Build-ap-normal specialization on null AP-OBJ using OPTIONAL-OBJ."
  (ignore ap-obj)
  (occ-obj-make-ap-normal optional-obj))


(cl-defmethod occ-obj-build-ap-transf ((ap-obj list)
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on a list AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :tree-keybranch))
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on a head tree-keybranch AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :callables))
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on a head callables AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :keywords))
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on a head keywords AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj (head :transform))
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on a head transform AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj occ-ap-normal)
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on occ-ap-normal AP-OBJ."
  (ignore optional-obj)
  (occ-obj-make-ap-transf ap-obj))

(cl-defmethod occ-obj-build-ap-transf ((ap-obj occ-ap-transf)
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on occ-ap-transf AP-OBJ returning it."
  (ignore optional-obj)
  ap-obj)

(cl-defmethod occ-obj-build-ap-transf ((ap-obj null)
                                       &optional
                                       optional-obj)
  "Build-ap-transf specialization on null AP-OBJ using OPTIONAL-OBJ."
  (ignore ap-obj)
  (if optional-obj
        (occ-obj-build-ap-transf optional-obj)))

;; ctor

(defvar occ-return-select-label     :occ-selected    "should not be null")
(defvar occ-return-quit-label       :occ-nocandidate "should not be null")
(defvar occ-return-nocndidate-label :occ-quitted     "should not be null")
(defvar occ-return-timeout-label    :occ-timeout     "should not be null") ;TODO: need to implement.
(defvar occ-return-true-label       :occ-true        "should not be null")
(defvar occ-return-false-label      :occ-false       "should not be null")
(defvar occ-return-evaluate         :occ-eval        "should not be null")

(occ-assert occ-return-select-label)
(occ-assert occ-return-quit-label)
(occ-assert occ-return-nocndidate-label)
(occ-assert occ-return-true-label)
(occ-assert occ-return-false-label)

(defvar occ-return-select-function #'identity)
(defvar occ-return-select-name     "Select")
(occ-assert occ-return-select-function)
(occ-assert occ-return-select-name)

(fmakunbound 'occ-obj-build-return-lambda)
(cl-defmethod occ-obj-build-return-lambda ((callable occ-callable-normal)
                                           &optional
                                           label)
  "Build a return lambda wrapping occ-callable-normal CALLABLE with LABEL."
  (let ((newcallable #'(lambda (candidate)
                         (let ((fun (occ-callable-fun callable)))
                           (let* ((value (funcall fun candidate))
                                  (label (if (or (null label)
                                                 (eq label occ-return-evaluate))
                                             (if value
                                                 occ-return-true-label
                                               occ-return-false-label)
                                           label)))
                             (occ-obj-make-return label value)))))
        (name        (occ-callable-name callable))
        (keyword     (occ-callable-keyword callable)))
    (occ-obj-make-callable-normal keyword
                                  name
                                  newcallable)))

(cl-defmethod occ-obj-build-return-lambda ((callable occ-callable-generator)
                                           &optional
                                           label)
  "Signal an error; generators cannot build a return lambda."
  (ignore label)
  (occ-error "Can not use occ-callable-transf %s" callable))


(cl-defun occ-build-hsrc-null (candidate &key rank level)
  "Build an occ-hsrc-null from CANDIDATE with RANK and LEVEL.
CANDIDATE is ignored and stored as nil."
  (ignore candidate)
  (let ((rank  (or rank 0))
        (level (or level :optional)))
    (make-occ-hsrc-null :obj nil ;; candidate
                        :rank rank
                        :level level)))

(cl-defun occ-build-hsrc-candidate (candidate &key rank level)
  "Build an occ-hsrc-candidate wrapping CANDIDATE with RANK and LEVEL."
  (let ((rank  (or rank 0))
        (level (or level :optional)))
    (make-occ-hsrc-candidate :obj candidate
                             :rank rank
                             :level level)))

(cl-defun occ-build-hsrc-source (source &key rank level)
  "Build an occ-hsrc-source wrapping SOURCE with RANK and LEVEL."
  (let ((rank  (or rank 0))
        (level (or level :optional)))
    (make-occ-hsrc-source :obj source
                          :rank rank
                          :level level)))


(let ((instance))
  (defun occ-get-user-agent ()
    "Return the singleton user agent creating it on first use."
    (unless instance
      (setq instance (make-occ-user-agent)))
    instance))

(let ((instance))
  (defun occ-get-org-agent ()
    "Return the singleton org agent creating it on first use."
    (unless instance
      (setq instance (make-occ-org-agent)))
    instance))

(let ((instance))
  (defun occ-get-emacs-agent ()
    "Return the singleton emacs agent creating it on first use."
    (unless instance
      (setq instance (make-occ-emacs-agent)))
    instance))

(occ-testing
 (let* ((obj       (occ-obj-make-ctx-at-point))
        (ap-obj    (occ-obj-make-ap-transf '(t actions general)))
        (transform (occ-obj-ap-helm-item ap-obj obj)))
   transform)

 ())


(cl-defun occ-make-ranktbl (&key name)
  "Make a fresh occ-ranktbl named NAME defaulting to ranktbl."
  (make-occ-ranktbl :name (or name "ranktbl")))

;;; occ-obj-ctor.el ends here
