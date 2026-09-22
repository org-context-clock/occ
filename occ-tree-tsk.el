;;; occ-tree-tsk.el --- tree tsk                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

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

(provide 'occ-tree-tsk)


(require 'occ-util-common)
(require 'occ-tree)
(require 'occ-obj-ctor)
(require 'occ-assert)
(require 'occ-debug-method)


(defun occ-tree-tsk-node-p (tx)
  "Test org TX is org tsks tree non-leaf node"
  (occ-obj-get-property tx 'subtree))

(defun occ-tree-tsk-subtree (tx)
  "Test org TX is org tsks tree non-leaf node"
  (occ-obj-get-property tx 'subtree))

(defun occ-mapcar-tree-tsks (fn tree args)
  "Tree mapcar return result for FN for all TREE nodes with ARGS"
  (tree-mapcar-nodes
   'occ-tree-tsk-subtree fn tree args))

(defun occ-mapc-tree-tsks (fn tree args)
  "Tree mapc run FN for all TREE nodes with ARGS"
  (tree-mapc-nodes
   'occ-tree-tsk-subtree fn tree args))

(defun occ-remove-if-not-tree-tsks (fn tree args)
  "Tree remove if return TREE with all node and its subtree removed
if node return nil for PREDICATE"
  (tree-remove-if-not-nodes #'occ-tree-tsk-subtree
                            fn tree args))


(defun occ-tree-trim (limit subtree)
  (if (> limit 0)
      (let ((count (length subtree)))
        (occ-debug "occ-tree-trim: limit %s, count %d" limit count)
        (let ((limit (- limit count)))
          (when subtree
            (let ((sum (apply #'+ count
                              (mapcar #'occ-tsk-children-count
                                      subtree))))
              (dolist (entry subtree)
                (let* ((limit (/ (* limit
                                    (1+ (occ-tsk-children-count entry)))
                                 sum))
                       (subtree (occ-tree-trim limit
                                               (occ-obj-get-property entry 'subtree))))
                  (occ-obj-set-property entry 'subtree
                                        subtree)
                  (occ-obj-set-property entry 'children-count
                                        (if subtree
                                            (apply #'+
                                                   (length subtree)
                                                   (mapcar #'occ-tsk-children-count
                                                           subtree))
                                          0)))))))
        (if (> count limit)
            (nthcdr (- count limit) subtree)
          subtree))
    subtree))

;; (cl-defmethod occ-trim ((limit number) (tsk occ-tree-tsk))
;;   ())

;; (defun occ-tree-depth-trim (fn depth tree))


(defun occ-org-map-subheading (fun)
  "Call FUN for every heading underneath the current heading"
  ;; (occ-back-to-heading)
  (let ((level (funcall outline-level))
        (collection nil))
    (save-excursion
      (while (and (progn
                    (outline-next-heading)
                    (> (funcall outline-level)
                       level))
                  (not (eobp)))
        (if (= (funcall outline-level)
               (1+ level))
            (setf collection
                  (nconc collection (list (funcall fun)))))))
    collection))


;; TODO: ADD support for non existing file. and suppose it buffer is present but not file.
;; should work with any changes
(defun occ-tree-tsk-build (file
                           collection
                           &optional
                           subtree-level)
  "Build recursive org tsks from org FILE (or current buffer) using
TSK-BUILDER-AT-POINT function e.g. occ-collect-tsk"
  (progn ;; save-excursion
    ;; (occ-message "occ-tree-tsk-build: level = %s" subtree-level)
    (progn  ;; save-restriction
      (let ((depth     (occ-obj-collection-depth collection))
            (curr-buff (if file
                           (occ-find-file-noselect file)
                         (current-buffer))))
        (with-current-buffer curr-buff
          (let ((subtree-level        (or subtree-level
                                          1))
                (tsk-builder-at-point (occ-obj-tsk-builder-at-point collection)))
            (when (and (buffer-live-p (current-buffer))
                       (> (buffer-size (current-buffer))
                          30))
              (occ-do-setup-buffer)
              (when file
                (unless (string= file
                                 (buffer-file-name (current-buffer)))
                  (occ-error "file `%s' and current file %s:%d not same, current marker %s."
                             file
                             (buffer-file-name (current-buffer))
                             (point)
                             (point-marker))))
              (unless (eq major-mode 'org-mode)
                (occ-error "Not in `%s:%d' org buffer, current marker %s"
                           (current-buffer)
                           (point)
                           (point-marker)))
              (when file
                (goto-char (point-min)))
              ;; here many time if other call thread come then current buffer gets changed cause issue with tsk-builder-at-point
              (let ((entry         (funcall tsk-builder-at-point file))
                    (subtree-level (if subtree-level subtree-level 1)))
                (when entry
                  (occ-obj-set-property entry 'subtree-level
                                        subtree-level)
                  (occ-obj-set-property entry 'file-level
                                        subtree-level)
                  (occ-assert (numberp subtree-level))
                  (let* ((subtree (unless (and depth
                                               (not (zerop depth))
                                               (> subtree-level depth))
                                     (let ((buffer-local-list (occ-org-map-subheading #'(lambda ()
                                                                                          (occ-tree-tsk-build nil
                                                                                                              collection
                                                                                                              subtree-level))))
                                           (subtree-file-list (let ((subtree-file-prop (occ-obj-get-property entry :SUBTREEFILE)))
                                                                (when subtree-file-prop
                                                                  ;; (occ-message "subtree-file-prop: %s, file: %s" subtree-file-prop file)
                                                                  (let* ((file         (if file file (buffer-file-name)))
                                                                         (subtree-file (if subtree-file-prop
                                                                                           (expand-file-name subtree-file-prop
                                                                                                             (if file
                                                                                                                 (file-name-directory file)
                                                                                                               default-directory)))))
                                                                    ;; (occ-message "subtree-file: %s, default-directory %s" subtree-file default-directory)
                                                                    (if (and subtree-file
                                                                             (file-readable-p subtree-file))
                                                                        (list (occ-tree-tsk-build subtree-file
                                                                                                  collection
                                                                                                  (+ 1
                                                                                                     (or (occ-obj-get-property entry
                                                                                                                               'level)
                                                                                                         0)
                                                                                                     subtree-level)))))))))
                                       (remove nil (append buffer-local-list
                                                         subtree-file-list))))))
                    (when subtree
                      (occ-obj-set-property entry 'subtree
                                            subtree))
                    (occ-obj-set-property entry 'descendant-weight
                                          (if subtree
                                              (* 1.2
                                                 (cl-reduce #'+
                                                            (mapcar #'occ-tsk-descendant-weight
                                                                    subtree)))
                                            1))
                    (occ-assert (occ-tsk-descendant-weight entry))
                    (occ-obj-set-property entry 'children-count
                                          (if subtree
                                              ;; (apply #'+
                                              ;;        (length subtree)
                                              ;;        (mapcar #'occ-tsk-children-count
                                              ;;                subtree))
                                              (reduce #'+
                                                      subtree
                                                      :initial-value (length subtree)
                                                      :key #'occ-tsk-children-count)
                                            0))
                    (dolist (child subtree)
                      (occ-obj-set-property child 'parent entry))
                    entry))))))))))

;; (file-relative-name (expand-file-name "meru/report.org"
;;                                       "/home/s/hell/Documents/mirror/CreatedContent/contents/virtual/org/default/tasks/")
;;                     "/home/s/hell/Documents/mirror/CreatedContent/contents/virtual/org/default/tasks/")

(cl-defmethod occ-obj-drived-tsk-builder ((collection occ-tree-collection)
                                          &optional
                                          subtree-level)
  #'(lambda (file)
      (occ-tree-tsk-build file
                          collection
                          subtree-level)))

(cl-defmethod occ-obj-build-tsks ((collection occ-tree-collection))
  (let ((depth (occ-obj-collection-depth collection))
        (limit (occ-obj-collection-limit collection))
               ;; TODO: use collection-limit to limit childs it can be null pr 0
               ;; we have to use some beadth tree traversal to limit number of entries
        (builder (occ-obj-drived-tsk-builder collection)))
    (ignore depth)
    (let ((tree (remove nil (mapcar builder
                                    (occ-tree-collection-roots collection)))))
      (occ-tree-trim limit tree))))

;;; occ-tree-tsk.el ends here
