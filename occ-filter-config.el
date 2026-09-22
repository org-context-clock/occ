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

;;

;;; Code:

(provide 'occ-filter-config)


(require 'occ-filter-op)
(require 'occ-filter-base)
(require 'occ-obj-ctor)


(defun occ-filter-config-initialize ()
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
  '(nil
    :non-negative))

(defun occ-list-filters ()
  '(nil
    :identity))
(defun occ-list-filters ()
  ;; '(:non-negative)
  (list nil
        :incremental
        ;; :negative
        :identity))

;; (defun occ-match-filters ()
;;   (list :positive
;;         :mutual-deviation
;;         (list :positive #'occ-obj-member-tsk-rank)))
(defun occ-match-filters ()
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
