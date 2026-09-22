;;; occ-prop-gen-misc-actions.el --- dynamically generating edit checkout etc methods for prop on occ-tsk occ-ctx etc to be used in helm action  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  sharad

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'occ-prop-gen-misc-actions)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-prop-base)
;; (require 'occ-prop-op-edit)
;; (require 'occ-prop-op-checkout)
;; (require 'occ-prop-gen-edit-actions)
;; (require 'occ-prop-gen-checkout-actions)



;; Correct it ???
(cl-defmethod occ-obj-gen-misc ((obj null)
                                &param-only param-only)
  (ignore obj)
  (ignore param-only)
  nil)

;; (cl-defmethod occ-obj-gen-misc ((obj occ-obj-ctx-tsk)
;;                                 &param-only param-only)
;;   (ignore obj)
;;   (ignore param-only)
;;   (let ((continue (occ-obj-build-callable-normal :continue
;;                                                  "Continue"
;;                                                  #'(lambda (obj) (occ-message "ok continue"))))
;;         (checkout (occ-obj-build-callable-normal :checkout
;;                                                  "Checkout"
;;                                                  #'(lambda (obj)
;;                                                      (occ-do-checkout obj)))))
;;     (list continue
;;           checkout)))

(cl-defmethod occ-obj-gen-misc ((obj occ-obj-ctx-tsk)
                                &param-only param-only)
  (ignore obj)
  (ignore param-only)
  (let ((continue (occ-obj-build-callable-normal :continue
                                                 "Continue"
                                                 #'(lambda (obj) (occ-message "ok continue")))))
    (list continue)))

(cl-defmethod occ-obj-gen-misc ((obj occ-obj-ctx)
                                &param-only param-only)
  (ignore obj)
  (ignore param-only)
  nil)

;;; occ-prop-gen-misc-actions.el ends here
