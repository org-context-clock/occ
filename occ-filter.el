;;; occ-filter.el --- occ filter                     -*- lexical-binding: t; -*-

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

;; occ-filter.el is the umbrella load file of the OCC filter subsystem:
;; besides providing the feature it only requires the three filter
;; modules, `occ-filter-base' (static and dynamic rank-threshold filter
;; machinery), `occ-filter-op' (additional rank-threshold set operations,
;; currently vestigial) and `occ-filter-config' (registered static filter
;; specs and the default filter lists).  Filters are interactive
;; rank-threshold mechanisms over ranked candidates, not boolean task
;; predicates.  Layer: filtering & ranking.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-filter)


(require 'occ-filter-base)
(require 'occ-filter-op)
(require 'occ-filter-config)

;;; occ-filter.el ends here
