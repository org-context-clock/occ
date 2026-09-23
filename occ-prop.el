;;; occ-prop.el --- property                         -*- lexical-binding: t; -*-

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

;; occ-prop.el is the top-level bundle of the OCC property protocol: it
;; has no code of its own beyond `provide', just the ordered `require's
;; that load the whole property stack.
;;
;; It pulls in the core protocol file occ-prop-base.el, the public
;; operation layer (occ-prop-op-edit.el, occ-prop-op-checkout.el,
;; occ-prop-op-misc.el) and the generated helm action layer
;; (occ-prop-gen-edit-actions.el, occ-prop-gen-checkout-actions.el,
;; occ-prop-gen-misc-actions.el), so that requiring `occ-prop' suffices
;; for editing properties, checking them out into the environment and
;; generating per-property helm actions.
;;
;; Layer: entry point of the property protocol files occ-prop-*.el.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-prop)


(require 'occ-prop-base)
(require 'occ-prop-op-edit)
(require 'occ-prop-op-checkout)
(require 'occ-prop-op-misc)

(require 'occ-prop-gen-edit-actions)
(require 'occ-prop-gen-checkout-actions)
(require 'occ-prop-gen-misc-actions)

;;; occ-prop.el ends here
