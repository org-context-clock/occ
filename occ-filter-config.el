;;; occ-filter-config.el --- occ filter config       -*- lexical-binding: t; -*-

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

;; occ-filter-config.el registers the built-in static filter specs of the
;; filtering & ranking layer.  `occ-filter-config-initialize' resets
;; `occ-obj-static-filters' and registers the `occ-static-filter' entries
;; with `occ-obj-build-static-filter': `:incremental' keeps ranks >= a
;; pivot walking the distinct rank values (starting at the middle),
;; `:positive' keeps rank > 0, `:non-negative' keeps rank > -1 (i.e.
;; non-negative integer ranks), `:negative' keeps rank < 0 and
;; `:identity' keeps everything.  The file also defines the default
;; filter lists `occ-match-filters' (the default clock-in set),
;; `occ-list-filters' (defined three times; the last definition wins) and
;; `occ-never-filters' (mainly for non-tasks).  Filter specs are keywords
;; or conses of keyword and an optional rank function, otherwise
;; `occ-obj-rank' is used.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-filter-config)


(require 'occ-filter-op)
(require 'occ-filter-base)
(require 'occ-obj-ctor)


(defun occ-filter-config-initialize ()
  "Reset occ-obj-static-filters and register the built-in filter set.
Registers :incremental which keeps ranks >= a pivot walking the
distinct rank values from the middle.  Registers :positive keeping
ranks > 0 and :non-negative keeping ranks > -1.  Registers :negative
keeping ranks < 0 and :identity keeping every candidate."
  (setq occ-obj-static-filters nil)
  (occ-obj-build-static-filter :incremental
                               "Incremental"
                               :points-gen-fn #'(lambda (ctx sequence &key rank)
                                                  (delete-dups (mapcar rank
                                                                       sequence)))
                               :compare-fn #'>=
                               :default-pivot-fn #'(lambda (ctx points)
                                                     (/ (length points)
                                                        2))
                               :rank-select-fn  nil
                               :rank-display-fn nil)

  (occ-obj-build-static-filter :positive
                               "Positive"
                               :points-gen-fn #'(lambda (ctx sequence &key rank)
                                                  (list 0))
                               :compare-fn #'>
                               :default-pivot-fn #'(lambda (ctx points) 0)
                               :rank-select-fn  nil
                               :rank-display-fn nil)

  (occ-obj-build-static-filter :non-negative
                               "Non-Negative"
                               :points-gen-fn #'(lambda (ctx sequence &key rank)
                                                  (list -1))
                               :compare-fn #'>
                               :default-pivot-fn #'(lambda (ctx points)
                                                     (/ (length points)
                                                        2))
                               :rank-select-fn  nil
                               :rank-display-fn nil)

  (occ-obj-build-static-filter :negative
                               "Negative"
                               :points-gen-fn #'(lambda (ctx sequence &key rank)
                                                  (list 0))
                               :compare-fn #'<
                               :default-pivot-fn #'(lambda (ctx points)
                                                     (/ (length points)
                                                        2))
                               :rank-select-fn  nil
                               :rank-display-fn nil)

  (occ-obj-build-static-filter :identity
                               "Identity"
                               :points-gen-fn #'(lambda (ctx sequence &key rank)
                                                  (list 0))
                               :compare-fn #'(lambda (rank pivot) t)
                               :default-pivot-fn #'(lambda (ctx points)
                                                     0)
                               :rank-select-fn  nil
                               :rank-display-fn nil))


;; Filter should be list of keys or cons of key and customized rank function
;; else occ-obj-rank will be used.


(defun occ-list-filters ()
  "Return the occ-list-filters spec nil then :non-negative.
Superseded by the later definitions of occ-list-filters."
  '(nil
    :non-negative))

(defun occ-list-filters ()
  "Return the occ-list-filters spec nil then :identity.
Superseded by the final definition of occ-list-filters below."
  '(nil
    :identity))
(defun occ-list-filters ()
  ;; '(:non-negative)
  "Return the effective occ-list-filters spec for listing tasks.
The spec is nil then :incremental then :identity.  This final
definition overrides the two earlier ones in this file."
  (list nil
        :incremental
        ;; :negative
        :identity))

;; (defun occ-match-filters ()
;;   (list :positive
;;         :mutual-deviation
;;         (list :positive #'occ-obj-member-tsk-rank)))
(defun occ-match-filters ()
  "Return the default clock-in filter spec list.
The leading t sets the filter direction for the remaining specs:
:incremental then :positive with rank function occ-obj-rank then
:non-negative then :identity."
  (list t
        :incremental
        ;; :mutual-deviation
        (list :positive
              #'occ-obj-rank)
        :non-negative
        :identity))
;; (list :mutual-deviation #'occ-obj-member-tsk-rank)

(defun occ-never-filters ()
  "Used to filter mainly non-tsk"
  '(:non-negative))

;;; occ-filter-config.el ends here
