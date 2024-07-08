;;; occ-util-method.el --- Occ util method           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s


;; Author: s
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

(provide 'occ-util-method)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-filter-config)


(with-eval-after-load "occ-util-common"
  (defvar occ-list-select-keys   occ-list-select-ap-normal-keys))


(defun occ-obj-list-select-keys ()
  occ-list-select-keys)


;; NOTE: for naming check devel.org


(cl-defmethod occ-do-run-list-select ((obj occ-obj-ctx)
                                      &key
                                      ap-normal ;; TODO: -- newly added
                                      obtrusive
                                      prompt)
  "Will open helm selection for tsk, here return-transform must
be NIL, using (occ-list-filters) for FILTERS"

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (ignore ap-normal)
  (let ((filters          (occ-list-filters))
        (builder          #'occ-obj-build-ctsk-with)
        (ap-normal        (occ-obj-list-select-keys))
        (ap-transf        (occ-obj-list-select-keys))
        (return-transform nil)
        (timeout          occ-idle-timeout))
    (occ-debug "occ-do-run-list-select: ap-normal: %s" ap-normal)
    (occ-obj-select obj                   ;; TODO: passing ap-normal has no affect it show its own debug it ?
                    (occ-collections-default)
                    :filters          filters
                    :builder          builder
                    :ap-normal        ap-normal
                    :ap-transf        ap-transf
                    :return-transform return-transform
                    :timeout          timeout
                    :obtrusive        obtrusive
                    :prompt           prompt)))

(cl-defmethod occ-do-run-match-select ((obj occ-obj-ctx)
                                       &key
                                       obtrusive
                                       prompt)
  "Will open helm selection for tsk, here return-transform must
be NIL, using (occ-match-filters) for FILTERS"

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL

  (let ((filters          (occ-match-filters))
        (builder          #'occ-obj-build-ctxual-tsk-with)
        (ap-normal        (occ-obj-list-select-keys))
        (ap-transf        (occ-obj-list-select-ap-transf-keys))
        (return-transform nil)
        (timeout          occ-idle-timeout))
    (occ-obj-select obj
                    (occ-collections-default)
                    :filters          filters
                    :builder          builder
                    :ap-normal        ap-normal
                    :ap-transf        ap-transf
                    :return-transform return-transform
                    :timeout          timeout
                    :obtrusive        obtrusive
                    :prompt           prompt)))


(occ-testing
 (occ-obj-list-select (occ-obj-make-ctx-at-point)
                      :ap-normal (occ-obj-get-helm-actions nil '(t actions select))
                      :obtrusive nil))

(occ-testing
 (let ((obj (occ-obj-make-ctx-at-point)))
   (occ-obj-select obj
                   (occ-collections-default)
                   :filters           (occ-list-filters)
                   :builder           #'occ-obj-build-ctsk-with
                   :ap-normal         occ-list-select-keys
                   :ap-transf         occ-list-select-keys
                   :return-transform  nil
                   :timeout           occ-idle-timeout
                   :obtrusive         t)))

(cl-defmethod occ-do-run-list-debug-select ((obj occ-obj-ctx)
                                         &key
                                         ap-normal
                                         obtrusive
                                         prompt)
  "Will open helm selection for tsk, which then again run helm
selection for ap-normal to run on selected tsk. It is mainly meant
for testing given ap-normal on selected tsk."

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (ignore ap-normal)
  (let ((filters          (occ-list-filters))
        (builder          #'occ-obj-build-ctsk-with)
        (ap-normal        (occ-obj-list-select-keys))
        (ap-transf        (occ-obj-list-select-ap-transf-keys))
        (return-transform t)
        (timeout          occ-idle-timeout))
      (occ-debug "occ-do-run-list-debug-select: ap-normal: %s" ap-normal)
      (let ((retval-ctx-tsk (occ-obj-select obj
                                            (occ-collections-default)
                                            :filters          filters
                                            :builder          builder
                                            :return-transform return-transform
                                            :ap-normal        ap-normal
                                            :ap-transf        ap-transf
                                            :timeout          timeout
                                            :obtrusive        obtrusive
                                            :prompt           prompt)))
        (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                          retval-ctx-tsk
                          (occ-obj-format (occ-obj-obj retval-ctx-tsk)
                                      'capitalize)
                          (occ-obj-return-get-label retval-ctx-tsk))
        (if (and (occ-obj-return-in-labels-p retval-ctx-tsk
                                         occ-return-select-label)
                 (occ-obj-obj retval-ctx-tsk))
            (let* ((ctsk         (occ-obj-obj retval-ctx-tsk))
                   ;; (helm-actions (occ-obj-ap-helm-actions ap-normal obj)) -- TODO: VERIFY: here in place of OBJ CTSK should be present
                   (helm-actions (occ-obj-ap-helm-actions ap-normal ctsk))
                   ;; TODO: BUG: Correct it
                   (launcher (cl-rest (assoc (completing-read "Helm-Actions: " helm-actions)
                                          helm-actions))))
              (funcall launcher ctsk))
          (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection")))))

(occ-testing
 ;; (occ-obj-get-helm-actions nil '(t actions select)) -> nil
 (occ-obj-list-select (occ-obj-make-ctx-at-point)
                      :ap-normal (occ-obj-get-helm-actions nil '(t actions select))
                      :obtrusive nil))

(cl-defmethod occ-do-run-list-launch ((obj occ-obj-ctx)
                                   &key
                                   obtrusive
                                   prompt)
  "TODO?: Will open helm selection for tsk, here return-transform
must be NIL, using (occ-list-filters) for FILTERS"

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL

  (let ((filters          (occ-list-filters))
        (builder          #'occ-obj-build-ctsk-with)
        (return-transform t)
        (ap-normal        (occ-obj-list-select-keys))
        (ap-transf        (occ-obj-list-select-ap-transf-keys))
        (timeout          occ-idle-timeout))
    (occ-debug "occ-obj-list-launch: ap-normal: %s" ap-normal)
    (occ-debug "occ-obj-list-launch: ap-transf: %s" ap-transf)
    (let ((retval-ctx-tsk (occ-obj-select obj
                                          (occ-collections-default)
                                          :filters          filters
                                          :builder          builder
                                          :return-transform return-transform
                                          :ap-normal        ap-normal
                                          :ap-transf        ap-transf
                                          :timeout          timeout
                                          :obtrusive        obtrusive
                                          :prompt           prompt)))
       (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                         retval-ctx-tsk
                         (occ-obj-format (occ-obj-obj retval-ctx-tsk)
                                         'capitalize)
                         (occ-obj-return-get-label retval-ctx-tsk))
       (if (and (occ-obj-return-in-labels-p retval-ctx-tsk
                                            occ-return-select-label)
                (occ-obj-obj retval-ctx-tsk))
           (let* ((ctx-tsk      (occ-obj-obj retval-ctx-tsk))
                                  ;; (helm-actions (occ-obj-ap-helm-actions ap-normal obj)) -- TODO: VERIFY: here in place of OBJ CTX-TSK should be present
                  (helm-actions (occ-obj-ap-helm-actions ap-normal ctx-tsk))
                  (launcher     (cl-rest (assoc (completing-read "Action: " helm-actions)
                                             helm-actions))))
             (funcall launcher ctx-tsk)
             (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection"))))))


(cl-defmethod occ-do-run-list-select-interactive ())
(cl-defmethod occ-do-run-list-select-get-obj ())

;;; occ-util-method.el ends here
