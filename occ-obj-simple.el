;;; occ-obj-simple.el --- occ simple methods         -*- lexical-binding: t; -*-

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

(provide 'occ-obj-simple)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'org-capture+-macros)
(eval-when-compile
  (require 'org-capture+-macros))
(require 'org-capture+)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-rank)
(require 'occ-prop)
(require 'occ-property-methods)
(require 'occ-filter-config)
(require 'occ-select)
(require 'occ-helm)
(require 'occ-helm-actions-config)
(require 'occ-clock)
(require 'occ-util-method)
(require 'occ-clock-report)


(defvar occ-idle-timeout 7)


(cl-defmethod occ-obj-find ((collection list)
                            (mrk marker))
  (ignore collection)
  (ignore mrk))

(cl-defgeneric occ-do-goto (obj)
  "occ-do-goto")

(cl-defmethod occ-do-goto ((obj marker))
  (switch-to-buffer (marker-buffer obj))
  ;; TODO find about "org-overview"
  ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
  ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
  ;; https://github.com/facetframer/orgnav
  ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
  ;; (outline-show-all)
  (org-content 10)
  (goto-char obj))

(cl-defmethod occ-do-goto ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and (markerp mrk)
             (marker-buffer mrk))
        (occ-do-goto mrk)
      (occ-error "marker %s invalid." mrk))))


(cl-defgeneric occ-do-set-to (obj)
  "occ-do-set-to")

(cl-defmethod occ-do-set-to ((obj marker))
  (set-buffer (marker-buffer obj))
  ;; TODO find about "org-overview"
  ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
  ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
  ;; https://github.com/facetframer/orgnav
  ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
  ;; (outline-show-all)
  ;; (org-content 10)
  (goto-char obj))

(cl-defmethod occ-do-set-to ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and (markerp mrk)
             (marker-buffer mrk))
        (occ-do-set-to mrk)
      (occ-error "marker %s invalid." mrk))))


(cl-defmethod occ-do-induct-child ((obj   occ-tree-tsk)
                                   (child occ-tree-tsk))
  ;; (occ-message "Inducting child %s in parent %s"
  ;;              (occ-obj-format child)
  ;;              (occ-obj-format obj))
  (occ-obj-set-property child 'parent obj)
  (occ-obj-set-property child 'subtree-level
                        (occ-obj-get-property obj 'subtree-level))
  (occ-insert-node-after-element child (if (occ-tree-tsk-subtree obj)
                                           (car (last (occ-tree-tsk-subtree obj)))
                                         obj) ;BUG: how to identify correction collection
                                 (occ-obj-list (occ-obj-tsk-collection obj)))
  ;; BUG: why child added to start when it is added in front, may need to fix in tree-tsk.el
  (setf (occ-tree-tsk-subtree obj) (nconc (occ-tree-tsk-subtree obj)
                                          (list  child))))

(cl-defmethod occ-do-induct-child ((obj   occ-list-tsk)
                                   (child occ-list-tsk))
  (occ-obj-set-property child 'parent obj)
  (occ-obj-set-property child 'subtree-level
                        (occ-obj-get-property obj 'subtree-level))
  (occ-insert-node-after-element child obj
                                 (occ-obj-list (occ-obj-tsk-collection obj))))


(cl-defmethod occ-do-abondon-child ((obj   occ-tree-tsk)
                                    (child occ-tree-tsk))
  ;; (occ-message "Inducting child %s in parent %s"
  ;;              (occ-obj-format child)
  ;;              (occ-obj-format obj))
  (occ-obj-set-property child 'subtree-level
                        (1+ (occ-obj-get-property obj
                                                  'subtree-level)))
  (occ-insert-node-after-element child (if (occ-tree-tsk-subtree obj)
                                           (car (last (occ-tree-tsk-subtree obj)))
                                         obj) ;BUG: how to identify correction collection
                                 (occ-obj-list (occ-obj-tsk-collection obj)))
  (setf (occ-tree-tsk-subtree obj) (nconc (occ-tree-tsk-subtree obj)
                                          (list  child))))

(cl-defmethod occ-do-abondon-child ((obj   occ-list-tsk)
                                    (child occ-list-tsk))
  (occ-obj-set-property child 'subtree-level
                        (1+ (occ-obj-get-property obj 'subtree-level)))
  (occ-insert-node-after-element child obj
                                 (occ-obj-list (occ-obj-tsk-collection obj))))


(cl-defgeneric occ-do-capture (obj
                               &key
                               template
                               clock-in
                               immediate-finish)
  "occ-do-capture")

(cl-defmethod occ-do-capture ((obj marker) &key
                              template
                              clock-in
                              immediate-finish)
  (ignore template)
  (ignore clock-in)
  (org-capture-run 'entry
                   `(marker ,obj)
                   'occ-obj-capture+-helm-select-template
                   :immediate-finish immediate-finish
                   :empty-lines 1))

(cl-defmethod occ-do-capture ((obj occ-tsk) &key
                              template
                              clock-in
                              immediate-finish)
  (let ((mrk (occ-tsk-marker obj)))
    (occ-do-capture mrk
                    :clock-in         clock-in
                    :template         template
                    :immediate-finish immediate-finish)))

(cl-defmethod occ-do-capture ((obj occ-obj-ctx-tsk) &key
                              template
                              clock-in
                              immediate-finish)
  (let ((mrk      (occ-obj-marker obj))
        (tsk      (occ-obj-tsk    obj))
        (ctx      (occ-obj-ctx    obj))
        (template (or template          ;FIX it.
                      (occ-obj-capture+-helm-select-template))))
    (when template
      (with-org-capture-run marker 'entry (list 'marker mrk) template (list :empty-lines 1
                                                                            :immediate-finish immediate-finish)
        (progn                          ;before
          (unless immediate-finish        ;*NOTE:
            (let* ((tmp-tsk  (occ-obj-make-tsk-with marker
                                                    tsk))
                   (tmp-ctsk (occ-obj-build-ctsk-with tmp-tsk
                                                      ctx)))
              (occ-do-op-props-edit tmp-ctsk)))
          t)
        (let ((child-tsk (occ-obj-make-tsk-with marker
                                                tsk))) ;after
          (when child-tsk
            (occ-do-induct-child tsk
                                 child-tsk)
            (when clock-in
              (let ((child-ctxual-tsk (occ-obj-build-ctxual-tsk-with child-tsk
                                                                     ctx)))
                (occ-do-try-clock-in child-ctxual-tsk)))))))))

(cl-defmethod occ-do-capture ((obj null) &key
                              template
                              clock-in
                              immediate-finish)
  (ignore obj)
  ;; BUG: occ-list-select is become an interactive function, here it is not returning desired object.
  ;; NOTE: ACTION-TRANSFORMER is superseding ACTION for OCC-LIST-SELECT
  (let ((ctx-tsk (occ-obj-list-select (occ-obj-make-ctx-at-point)
                                      (occ-collections-all)
                                      :ap-normal '(t actions select)
                                      :obtrusive t)))
    (if ctx-tsk
        (occ-do-capture ctx-tsk
                        :template template
                        :clock-in clock-in
                        :immediate-finish immediate-finish)
      (occ-error "Not able to get ctx-tsk(%s) at point" ctx-tsk))))


;; TODO: DEBUG
(occ-testing
  ;;; Group1

  ;; NOTE: ACTION-TRANSFORMER is superseding ACTION for OCC-LIST-SELECT
  ;; (occ-obj-get-helm-actions nil '(t actions select)) -> nil
 ;; (occ-obj-list-debug-select (occ-obj-make-ctx-at-point)
 ;;                            :ap-normal '(t actions select)
 ;;                            :obtrusive nil)
 (occ-obj-list-select (occ-obj-make-ctx-at-point)
                      :ap-normal '(t actions select)
                      :obtrusive nil)


 (occ-obj-list-select (occ-obj-make-ctx-at-point)
                      :ap-normal '(t actions select)
                      :obtrusive nil)

  ;;; Group2
 (let ((obj (occ-obj-make-ctx-at-point)))
   (occ-obj-select obj
                   (occ-collections-default)
                   :filters            (occ-list-filters)
                   :builder            #'occ-obj-build-ctsk-with
                   :action             (occ-obj-get-helm-actions obj
                                                                 occ-list-select-keys)
                   :return-transform   nil
                   :action-transformer #'(lambda (action candidate)
                                           (occ-obj-get-helm-actions obj
                                                                     occ-list-select-keys))
                   :timeout            occ-idle-timeout
                   :obtrusive         t))


  ;; (occ-obj-get-helm-actions nil '(t actions select)) -> nil
 (occ-obj-list-select (occ-obj-make-ctx-at-point)
                      :action (occ-obj-get-helm-actions nil '(t actions select))
                      :obtrusive nil)


  ;;; Group3
 (let ((obj                    (occ-obj-make-ctx-at-point))
       (occ-list-select-keys-1 '(t actions select))
       (occ-list-select-keys-2 '(t actions general)))
   (occ-obj-select obj
                   (occ-collections-default)
                   :filters            (occ-list-filters)
                   :builder            #'occ-obj-build-ctsk-with
                   :action             (occ-obj-get-helm-actions obj
                                                                 occ-list-select-keys-1)
                   :return-transform   nil
                   :action-transformer #'(lambda (action candidate)
                                           (ignore action)
                                           (ignore candidate)
                                           (occ-obj-get-helm-actions obj
                                                                     occ-list-select-keys-2))
                   :timeout            occ-idle-timeout
                   :obtrusive         t))
 ())


(cl-defgeneric occ-do-create-child (obj)
  "occ-child")

(cl-defmethod occ-do-create-child ((obj marker)
                                   &keys
                                   template
                                   clock-in
                                   immediate-finish)
  (if (not (occ-obj-unnamed-p obj))
      (occ-do-capture obj
                      :clock-in clock-in ;; helm-current-prefix-arg
                      :template template
                      :immediate-finish immediate-finish)
    (let ((title (occ-obj-title obj 'captilize)))
     (occ-error "%s is unnamed %s so can not create child "
           (occ-obj-format obj 'captilize)
           title
           title))))

(cl-defmethod occ-do-create-child ((obj occ-obj-tsk)
                                   &key
                                   template
                                   clock-in
                                   immediate-finish)
  (if (not (occ-obj-unnamed-p obj))
      (occ-do-capture obj
                      :clock-in clock-in ;; helm-current-prefix-arg
                      :template template
                      :immediate-finish immediate-finish)
    (let ((title (occ-obj-title obj 'captilize)))
      (occ-error "%s is unnamed %s so can not create child "
             (occ-obj-format obj 'captilize)
             title
             title))))


(cl-defmethod occ-obj-tsk-txt ((obj occ-obj-ctx)
                               (heading string))
  "Build a task name description from OBJ occ-ctx"
  (ignore obj)
  (concat "* " heading "\n"))


(cl-defmethod occ-do-fast-create-child ((heading string)
                                        &key
                                        template
                                        clock-in
                                        immediate-finish)
  "Create fast child task may use template"
  (ignore template)
  (let ((ctx (occ-obj-make-ctx-at-point)))
    (occ-do-capture nil
                    :clock-in         clock-in ;; helm-current-prefix-arg
                    :template         (occ-obj-tsk-txt ctx heading)
                    :immediate-finish t)))

(cl-defmethod occ-do-create-anonymous-child ((heading string)
                                             &key
                                             template
                                             clock-in
                                             immediate-finish)
  "Create Anonymous task"
  (ignore template)
  (let ((ctx (occ-obj-make-ctx-at-point)))
    (occ-do-capture nil
                    :clock-in clock-in ;; helm-current-prefix-arg
                    :template (occ-obj-tsk-txt ctx heading))))

(cl-defmethod occ-do-fast-create-anonymous-child (&key
                                                  template
                                                  clock-in
                                                  immediate-finish)
  "Create Anonymous (fast as unnamed)"
  (ignore template)
  (let ((ctx     (occ-obj-make-ctx-at-point))
        (heading "Unnamed task"))
    (occ-do-capture nil
                    :clock-in         clock-in ;; helm-current-prefix-arg
                    :template         (occ-obj-tsk-txt ctx heading)
                    :immediate-finish t)))

(cl-defmethod occ-do-fast-create-anonymous-child-in-collection (collection
                                                                &key
                                                                template
                                                                clock-in
                                                                immediate-finish)
  "Create Anonymous tsk in collection (fast as unnamed)"
  (ignore template)
  (let ((ctx     (occ-obj-make-ctx-at-point))
        (heading "Unnamed task"))
    (let* ((anonymous-heading-marker (cl-rest (org-without-org-clock-persist
                                                ;; TODO: Implement it.
                                                (lotus-org-create-anonymous-task))))
           (anonymous-tsk (when anonymous-heading-marker
                            (occ-obj-make-tsk anonymous-heading-marker))))
      (occ-do-capture anonymous-tsk
                      :clock-in         clock-in ;; helm-current-prefix-arg
                      :template         (occ-obj-tsk-txt ctx heading)
                      :immediate-finish t))))


(cl-defgeneric occ-do-create-child-clock-in (obj)
  "occ-child-clock-in")

(cl-defmethod occ-do-create-child-clock-in ((obj null))
  (occ-do-capture obj
               :clock-in t))

(cl-defmethod occ-do-create-child-clock-in ((obj marker))
  (occ-do-capture obj
               :clock-in t))

(cl-defmethod occ-do-create-child-clock-in ((obj occ-obj-tsk))
  (occ-do-capture obj
                  :clock-in t))


(cl-defgeneric occ-do-cut (obj)
  "Cut task")

(cl-defmethod occ-do-cut ((obj occ-obj-tsk))
  "Cut task"
  (occ-mac-with-org-marker (occ-obj-marker obj)
    (org-cut-subtree)))

(cl-defgeneric occ-do-paste (obj)
  "Paste task")

(cl-defmethod occ-do-paste ((obj occ-obj-tsk))
  "Paste task"
  (occ-mac-with-org-marker (occ-obj-marker obj)
    (org-paste-subtree)))


(cl-defgeneric occ-do-archive (obj)
  "Archive task")

(cl-defmethod occ-do-archive ((obj occ-obj-tsk))
  "Archive task"
  (occ-mac-with-org-marker (occ-obj-marker obj)
    (org-archive-subtree)))


(cl-defgeneric occ-do-delete (obj)
  "Delete task")

(cl-defmethod occ-do-delete ((obj occ-obj-tsk))
  "Delete task"
  (occ-do-cut obj))


(cl-defgeneric occ-do-report (obj)
  "Generate report")

(cl-defmethod occ-do-report ((obj occ-obj-tsk))
  "Generate report"
  (occ-clock-plain-report-tree (occ-obj-marker obj)))
  ;; (occ-clock-report-tree (occ-obj-marker obj))



;; (defun occ-confirm (fn new)
;;   (occ-y-or-n-timeout)
;;   (occ-error "Implement it."))


(defun sacha/org-capture-prefill-template (template &rest values)
  "Pre-fill TEMPLATE with VALUES."
  (let ((template (or template)))
   (with-temp-buffer
     (insert template)
     (goto-char (point-min))
     (while (re-search-forward
             (concat "%\\("
                     "\\[\\(.+\\)\\]\\|"
                     "<\\([^>\n]+\\)>\\|"
                     "\\([tTuUaliAcxkKInfF]\\)\\|"
                     "\\(:[-a-zA-Z]+\\)\\|"
                     "\\^\\({\\([^}]*\\)}\\)"
                     "?\\([gGtTuUCLp]\\)?\\|"
                     "%\\\\\\([1-9][0-9]*\\)"
                     "\\)") nil t)
       (if (cl-first values)
           (replace-match (cl-first values) nil t))
       (setq values (cl-rest values)))
     (buffer-string))))

(defun sacha/helm-org-create-task (candidate)
  (let ((entry (org-capture-select-template "T")))
    (org-capture-set-plist entry)
    (org-capture-get-template)
    (org-capture-set-target-location)
    (condition-case error
        (let* ((pre-fill-template (sacha/org-capture-prefill-template (org-capture-get :template)
                                                                      candidate))
               (cap-template (org-capture-fill-template pre-fill-template)))
          (org-capture-put :template cap-template)
          (org-capture-place-template (equal (cl-first (org-capture-get :target)) 'function)))
      ((occ-error quit)
       (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
       (occ-error "Capture abort: %s" error)))) t)

;;; occ-obj-simple.el ends here
