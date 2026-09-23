;;; occ-prop-intf.el --- occ property interface      -*- lexical-binding: t; -*-

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

;; This file should only have the interface and default interface methods
;; which are implemented by occ-property-methods.org: it is the interface
;; layer of the OCC property protocol, meant to hold the stable, public
;; property generics that the rest of OCC calls, while the concrete
;; per-property methods live in the implementation layer
;; (occ-property-methods.el, tangled from occ-property-methods.org).
;;
;; The intended split, as listed in the outline at the bottom of this
;; file, is: read a property value from the user (occ-user-agent), read
;; it from the context (occ-ctx, live environment capture), read it from
;; a task, write a property value to a task, error on writing to the
;; read-only context, treat writing to the user as printing, and
;; checkout a task property value into the environment.
;;
;; As written, the file currently only provides itself and requires
;; occ-macros, occ-util-common, occ-obj, occ-prop-utils and
;; occ-normalize-ineqs; the property generics it refers to are declared
;; in occ-prop-base.el and occ-intf.el.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-prop-intf)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-util-common)
(require 'occ-obj)
(require 'occ-prop-utils)
(require 'occ-normalize-ineqs)



;;; occ-prop-intf.el ends here
;;
;;
;;
;; * read prop value from user
;; * read prop value from ctx
;; * read prop value from tsk
;; * write prop value to tsk
;; * write prop value to ctx error
;; * write to user means print
;; * checkout prop value from tsk
;; (org-read-date) (org--deadline-or-schedule arg 'scheduled tim)e
