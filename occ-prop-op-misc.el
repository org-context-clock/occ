;;; occ-prop-op-checkout.el --- property checkout code  -*- lexical-binding: t; -*-

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

;; occ-prop-op-misc.el implements the misc operation layer of the OCC
;; property protocol: a catch-all per-property operation for a forced
;; clock-in, currently a stub.
;;
;; occ-do-op-prop-misc handles one property and occ-do-op-props-misc
;; iterates over occ-obj-properties-to-misc, delegating each property to
;; occ-do-checkout (both bodies are marked with BUG comments in the
;; source, and occ-obj-properties-to-misc is not yet defined elsewhere).
;;
;; Layer: public operation layer above occ-prop-base.el, alongside
;; occ-prop-op-edit.el and occ-prop-op-checkout.el; consumed by
;; occ-prop-gen-misc-actions.el.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-prop-op-misc)


(require 'occ-prop-base)
(eval-when-compile
  (require 'occ-macros))


(cl-defgeneric occ-do-op-prop-misc (obj
                                    prop)
  "Misc property PROP for forced clock-in.")

(cl-defmethod occ-do-op-prop-misc ((obj  occ-obj-tsk)
                                   (prop symbol))
  "Misc property PROP for forced clock-in."
  ;; BUG: Fix
  (occ-do-checkout obj
                   prop))


(cl-defgeneric occ-do-op-props-misc (obj)
  "Misc all property for forced clock-in.")

(cl-defmethod occ-do-op-props-misc ((obj occ-obj-tsk))
  "Misc all property for forced clock-in."
  ;; BUG: Fix
  (dolist (prop (occ-obj-properties-to-misc obj))
    (occ-debug "occ-do-op-props-misc: checkout prop %s" prop)
    (occ-do-op-prop-misc obj
                         prop)))

;;; occ-prop-op-checkout.el ends here
