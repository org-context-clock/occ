;;; occ-select.el --- selection                      -*- lexical-binding: t; -*-

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

;; Main machinery
;; TODO: Document it well

;;; Code:

(provide 'occ-select)


(require 'org-misc-utils-lotus)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-filter-config)
(require 'occ-helm)
(require 'occ-helm-actions-config)


;; TODO:
;; (helm-resume-select-buffer)
;; helm-buffers
;; helm-buffer

(defvar occ-helm-select-buffer-name " *helm: occ select*")
(defun occ-obj-helm-select-buffer ()
  occ-helm-select-buffer-name)

;; checkout spaceline--helm-buffer-ids variable
;; and spaceline-define-segment helm-buffer-id
;; or function definition of (spaceline-toggle-helm-buffer-id)

(when nil
  (let* ((name occ-helm-select-buffer-name)
         (name "*helm: ssss asfdsaf *"))
    (if (string-match "\\*helm:? \\(mode \\)?\\([^\\*]+\\)\\*" name)
        (match-string 2 name))))

(cl-defmethod occ-obj-list-select ((obj occ-ctx)
                                   (collections list)
                                   &key
                                   filters
                                   builder
                                   ap-normal
                                   ap-transf
                                   return-transform
                                   auto-select-if-only
                                   timeout
                                   obtrusive
                                   prompt)
  "Main Machinery, TODO: Document it, NOTE: ACTION-TRANSFORMER is
superseding ACTION, As in helm ACTION-TRANSFORMER are superseding
ACTION
TODO: Document it, Note: RETURN-TRANSFORM playing its game here."
  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (ignore obtrusive)
  (let* ((timeout (or timeout
                      occ-idle-timeout)))
    (helm-timed timeout (occ-obj-helm-select-buffer)
      (occ-debug "running occ-list-select")
      (progn
        (occ-debug "occ-list-select: ap-normal: %s" ap-normal)
        (occ-debug "occ-list-select: ap-transf: %s" ap-transf)
        ;; (occ-obj-list-select-internal obj
        (let ((selected (occ-obj-helm-act obj
                                          collections
                                          :filters             filters
                                          :builder             builder
                                          :ap-normal           ap-normal
                                          :ap-transf           ap-transf
                                          :return-transform    return-transform
                                          :auto-select-if-only auto-select-if-only
                                          :timeout             timeout
                                          ;; :obtrusive           obtrusive
                                          :prompt              prompt)))
          (occ-message "occ-list-select: selected = %s" (occ-obj-format selected))
          (occ-debug   "occ-list-select: selected = %s" (occ-obj-format selected))
          (if return-transform
              ;; TODO: add cl-defmethod magic here
              (or selected ;as return value is going to be used.
                  (occ-obj-make-return occ-return-quit-label
                                       selected))
            selected))))))


(cl-defgeneric occ-obj-select (obj
                               collections
                               &key
                               filters
                               builder
                               ap-normal
                               ap-transf
                               return-transform
                               auto-select-if-only
                               timeout
                               obtrusive
                               prompt)
  "return interactively selected TSK or NIL,   TODO: Document it.")

;; TODO: Not to run when frame is not open [visible.]
;; Getting targets...done
;; Error running timer: (occ-error "Window #<window 12> too small for splitting")
;; task-projbuffs-base-dir: changing supplied base-dir nil and task-projbuffs-base-dir to /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/
;; in occ-do-clock-in occ-ctx 1
;; Getting targets...done
;; Error running timer ‘occ-do-clock-in-curr-ctx-if-not’: (occ-error "Window #<window 12> too small for splitting")

;; (cl-defmethod occ-obj-select ((obj occ-ctx)
;;                               (collections list)
;;                               &key
;;                               filters
;;                               builder
;;                               ap-normal
;;                               ap-transf
;;                               return-transform
;;                               auto-select-if-only
;;                               timeout
;;                               obtrusive
;;                               prompt)
;;   "return interactively selected TSK or NIL,   TODO: Document it."
;;   ;; NOTE: AP-TRANSF is superseding AP-NORMAL
;;   (unless builder (occ-error "Builder can not be nil"))
;;   (occ-debug "OCC-SELECT((OBJ OCC-CTX)): begin")
;;   (let* ((timeout (or timeout occ-idle-timeout)))
;;     (let* ((unfiltered-count (occ-obj-length (cl-first collections))))
;;       (if (> unfiltered-count 0)
;;           (let ((retval (occ-obj-list-select obj
;;                                              collections
;;                                              :filters             filters
;;                                              :builder             builder
;;                                              :return-transform    return-transform
;;                                              :ap-normal           ap-normal
;;                                              :ap-transf           ap-transf
;;                                              :auto-select-if-only auto-select-if-only
;;                                              :timeout             timeout
;;                                              :obtrusive           obtrusive
;;                                              :prompt              prompt)))
;;             (occ-debug "OCC-OBJ-SELECT((OBJ OCC-CTX)): occ-list-select returned %s"
;;                        (occ-obj-Format retval))
;;             retval)
;;         (prog1
;;             (when return-transform
;;               (occ-obj-make-return occ-return-nocndidate-label nil))
;;           (occ-debug "OCC-SELECT((OBJ OCC-CTX)): no candidates available from %d."
;;                        unfiltered-count))))))

(cl-defmethod occ-obj-select ((obj occ-ctx)
                              (collections list)
                              &key
                              filters
                              builder
                              ap-normal
                              ap-transf
                              return-transform
                              auto-select-if-only
                              timeout
                              obtrusive
                              prompt)
  "return interactively selected TSK or NIL,   TODO: Document it."
  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (unless builder (occ-error "Builder can not be nil"))
  (occ-debug "OCC-SELECT((OBJ OCC-CTX)): begin")
  (let ((retval (occ-obj-list-select obj
                                     collections
                                     :filters             filters
                                     :builder             builder
                                     :return-transform    return-transform
                                     :ap-normal           ap-normal
                                     :ap-transf           ap-transf
                                     :auto-select-if-only auto-select-if-only
                                     :timeout             timeout
                                     :obtrusive           obtrusive
                                     :prompt              prompt)))
    (occ-debug "OCC-OBJ-SELECT((OBJ OCC-CTX)): occ-list-select returned %s"
                 (occ-obj-Format retval))
    retval))

(cl-defmethod occ-obj-select ((obj null)
                              (collections list)
                              &key
                              filters
                              builder
                              ap-normal
                              ap-transf
                              return-transform
                              auto-select-if-only
                              timeout
                              obtrusive
                              prompt)
  "TODO: Document it."
  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (ignore obj)
  (occ-debug "occ-select((obj null)): begin")
  (let ((retval (occ-obj-select (occ-obj-make-ctx-at-point)
                                collections
                                :filters             filters
                                :builder             builder
                                :ap-normal           ap-normal
                                :ap-transf           ap-transf
                                :return-transform    return-transform
                                :auto-select-if-only auto-select-if-only
                                :timeout             timeout
                                :obtrusive           obtrusive
                                :prompt              prompt)))
    (occ-debug "OCC-SELECT((OBJ NULL)): OCC-SELECT((OBJ OCC-CTX)) returned %s"
                      (occ-obj-Format retval))
    retval))


(occ-testing
 (let ((obj                    (occ-obj-make-ctx-at-point))
       (collections            nil)
       (occ-list-select-keys-1 '(t actions general))
       (occ-list-select-keys-2 '(t actions select)))
   (occ-obj-select obj
                   collections
                   :filters          (occ-list-filters)
                   :builder          #'occ-obj-build-ctsk-with
                   :ap-normal        occ-list-select-keys-1
                   :ap-transf        occ-list-select-keys-2
                   :return-transform nil
                   :timeout          occ-idle-timeout
                   :obtrusive        t)))

;;; occ-select.el ends here
