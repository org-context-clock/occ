;;; occ-prop-org.el --- occ prop org                 -*- lexical-binding: t; -*-

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


(provide 'occ-prop-org)


(require 'org)
(require 'org-misc-utils-lotus)

(require 'occ-prop-intf)
(require 'occ-intf)
(require 'occ-obj-common)
(require 'occ-property-methods)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)



(eval-when-compile
  (require 'lotus-misc-utils))


(cl-defmethod occ-obj-org-property-name ((prop symbol))
  (concat (and (occ-obj-occ-prop-p prop) "occ-")
          (symbol-name prop)))

(cl-defmethod occ-obj-org-property-symb ((prop symbol))
  (occ-symb (occ-obj-org-property-name prop)))


(defun occ-org-entry-get (pom
                          prop)
  (org-entry-get pom
                 (occ-obj-org-property-name prop)))

(defun occ-org-entry-put (pom
                          prop
                          value)
  (lotus-org-with-safe-modification
    (org-entry-put pom
                   (occ-obj-org-property-name prop)
                   value)))

(defun occ-org-entry-delete (pom
                             prop
                             value)
  (lotus-org-with-safe-modification
    (org-entry-delete pom
                      (occ-obj-org-property-name prop)
                      value)))

(defun occ-org-entry-get-multivalued-property (pom
                                               prop)
  (org-entry-get-multivalued-property pom
                                      (occ-obj-org-property-name prop)))

(defun occ-org-entry-put-multivalued-property (pom
                                               prop
                                               values)
  (lotus-org-with-safe-modification
    (org-entry-put-multivalued-property pom
                                        (occ-obj-org-property-name prop)
                                        values)))

(defun occ-org-entry-add-to-multivalued-property (pom
                                                  prop
                                                  value)
  (lotus-org-with-safe-modification
    (org-entry-add-to-multivalued-property pom
                                           (occ-obj-org-property-name prop)
                                           value)
    t))

(defun occ-org-entry-remove-from-multivalued-property (pom
                                                       prop
                                                       value)
  (lotus-org-with-safe-modification
    (org-entry-remove-from-multivalued-property pom
                                                (occ-obj-org-property-name prop)
                                                value)
    t))

(defun occ-org-entry-member-in-multivalued-property (pom
                                                     prop
                                                     values)
  (org-entry-member-in-multivalued-property pom
                                            (occ-obj-org-property-name prop)
                                            values))


(cl-defmethod occ-do-readprop-org ((obj  occ-obj-ctx-tsk)
                                   (prop symbol))
  "Read property PROP of OBJ-CTX-TSK OBJ from its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore ctx)
    (let* ((mrk    (or (occ-obj-marker tsk) (point)))
           (values (occ-do-operation mrk
                                     prop
                                     'get)))
      (mapcar #'(lambda (v)
                  (occ-obj-from-org prop
                                    nil
                                    v))
              values))))

(cl-defmethod occ-do-writeprop-org ((obj  occ-obj-ctx-tsk)
                                    (prop symbol))
  "Write property PROP of OBJ-CTX-TSK OBJ to its corresponding org file entry."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore ctx)
    (let* ((values (occ-obj-get-property tsk prop))
           (values (if (consp values) values (list values)))
           (values (mapcar #'(lambda (v)
                               (occ-obj-to-org prop 'put v))
                           values)))
      (occ-do-operation (occ-obj-marker tsk)
                        prop
                        'put
                        values))))

;;; occ-prop-org.el ends here
