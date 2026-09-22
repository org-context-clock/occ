;;; occ-list-tsk.el --- list tsk                     -*- lexical-binding: t; -*-

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

(provide 'occ-list-tsk)


(require 'occ-obj-ctor)
(require 'occ-util-common)


(defun occ-list-tsk-build (file
                           collection
                           &optional
                           subtree-level)
  "Build recursive org tsks from org FILE (or current buffer) using
TSK-BUILDER-AT-POINT function e.g. occ-collect-tsk"
  (let ((tsk-builder-at-point   (occ-obj-tsk-builder-at-point collection)))
    (with-current-buffer (if file
                             (occ-find-file-noselect fil)
                           (current-buffer))
      (when (and (buffer-live-p (current-buffer))
                 (> (buffer-size (current-buffer))
                    30))
        (occ-do-setup-buffer)
        (when file
          (goto-char (point-min)))
        (let ((entry (funcall tsk-builder-at-point file)))
          (cons entry
                (org-map-entries #'(lambda ()
                                     (tsk-builder-at-point nil))
                                 t
                                 file)))))))

(cl-defmethod occ-obj-drived-tsk-builder ((collection occ-list-collection)
                                          &optional
                                          subtree-level)
  #'(lambda (file)
      (occ-list-tsk-build file
                          collection
                          subtree-level)))

(cl-defmethod occ-obj-build-tsks ((collection occ-list-collection))
  (let ((depth (occ-obj-collection-depth collection))
        (limit (occ-obj-collection-limit collection))
        ;; TODO: use collection-limit to limit childs it can be null pr 0
        ;; we have to limit on return value of this function
        (builder (occ-obj-drived-tsk-builder collection)))
    (ignore depth)
    (ignore limit)
    (remove nil (mapcar builder
                        (occ-list-collection-roots collection)))))

;;; occ-list-tsk.el ends here
