;;; occ-assert.el --- occ assert                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

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

;; occ-assert.el is a one-definition support file of the support layer:
;; it aliases `occ-assert' to `cl-assert' so the rest of OCC can assert
;; invariants under an occ-prefixed name.  It is used throughout the
;; codebase (constructor, accessor, filter, rank and inequality code) to
;; validate structures and preconditions.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-assert)


(require 'cl-macs)


(defalias 'occ-assert #'cl-assert)

;;; occ-assert.el ends here
