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

;; occ-prop-org.el implements the org-world side of the OCC property
;; protocol: mapping property symbols to org drawer property names and
;; reading and writing them in org entries.
;;
;; occ-obj-org-property-name builds the `occ-'-prefixed org property name
;; for an OCC property symbol (guarded by occ-obj-occ-prop-p) and
;; occ-obj-org-property-symb interns it as a symbol.
;;
;; The occ-org-entry-get, occ-org-entry-put, occ-org-entry-delete and
;; occ-org-entry-*-multivalued-property wrappers adapt org-entry-get and
;; friends to that naming under safe modification, and
;; occ-do-readprop-org and occ-do-writeprop-org convert values between
;; the org and occ worlds with occ-obj-from-org and occ-obj-to-org.
;;
;; Layer: org conversion layer of the property protocol, between
;; occ-prop-base.el and the per-property converters in
;; occ-property-methods.el.
;;
;; See doc/occ-design.org for the full design.

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
  "Return the org drawer property name for the symbol PROP.
Prepends the occ- prefix only when occ-obj-occ-prop-p approves PROP as
in currfile becoming occ-currfile."
  (concat (and (occ-obj-occ-prop-p prop) "occ-")
          (symbol-name prop)))

(cl-defmethod occ-obj-org-property-symb ((prop symbol))
  "Return the interned org property symbol for the symbol PROP.
Interns the org property name from occ-obj-org-property-name with
occ-symb."
  (occ-symb (occ-obj-org-property-name prop)))


(defun occ-org-entry-get (pom
                          prop)
  "Read the org property of PROP at the org entry POM.
Thin wrapper around org-entry-get using the occ prefixed org property
name for PROP."
  (org-entry-get pom
                 (occ-obj-org-property-name prop)))

(defun occ-org-entry-put (pom
                          prop
                          value)
  "Write VALUE to the org property of PROP at the org entry POM.
Wraps org-entry-put in lotus-org-with-safe-modification and uses the
occ prefixed org property name for PROP."
  (lotus-org-with-safe-modification
    (org-entry-put pom
                   (occ-obj-org-property-name prop)
                   value)))

(defun occ-org-entry-delete (pom
                             prop
                             value)
  "Delete the org property of PROP at the org entry POM.
Wraps org-entry-delete in lotus-org-with-safe-modification with the
occ prefixed org property name for PROP; VALUE passes through."
  (lotus-org-with-safe-modification
    (org-entry-delete pom
                      (occ-obj-org-property-name prop)
                      value)))

(defun occ-org-entry-get-multivalued-property (pom
                                               prop)
  "Read the multivalued org property of PROP at the org entry POM.
Wraps org-entry-get-multivalued-property with the occ prefixed org
property name for PROP; list values are space split."
  (org-entry-get-multivalued-property pom
                                      (occ-obj-org-property-name prop)))

(defun occ-org-entry-put-multivalued-property (pom
                                               prop
                                               values)
  "Write VALUES to the multivalued org property of PROP at POM.
Wraps org-entry-put-multivalued-property in
lotus-org-with-safe-modification with the occ prefixed org property
name for PROP."
  (lotus-org-with-safe-modification
    (org-entry-put-multivalued-property pom
                                        (occ-obj-org-property-name prop)
                                        values)))

(defun occ-org-entry-add-to-multivalued-property (pom
                                                  prop
                                                  value)
  "Add VALUE to the multivalued org property of PROP at POM.
Wraps org-entry-add-to-multivalued-property in
lotus-org-with-safe-modification with the occ prefixed org property
name for PROP and returns t."
  (lotus-org-with-safe-modification
    (org-entry-add-to-multivalued-property pom
                                           (occ-obj-org-property-name prop)
                                           value)
    t))

(defun occ-org-entry-remove-from-multivalued-property (pom
                                                       prop
                                                       value)
  "Remove VALUE from the multivalued org property of PROP at POM.
Wraps org-entry-remove-from-multivalued-property in
lotus-org-with-safe-modification with the occ prefixed org property
name for PROP and returns t."
  (lotus-org-with-safe-modification
    (org-entry-remove-from-multivalued-property pom
                                                (occ-obj-org-property-name prop)
                                                value)
    t))

(defun occ-org-entry-member-in-multivalued-property (pom
                                                     prop
                                                     values)
  "Test membership of VALUES in the multivalued org property of PROP.
Wraps org-entry-member-in-multivalued-property with the occ prefixed
org property name for PROP at the org entry POM."
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
