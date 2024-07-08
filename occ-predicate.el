;;; occ-predicate.el --- occ predicate               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords:

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

;;

;;; Code:

(provide 'occ-predicate)


(require 'occ-obj-clock-method)
(require 'occ-unnamed)
(require 'occ-obj-ctor)
(require 'occ-util-common)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)


(cl-defgeneric occ-obj-marker= ((obj marker)
                                (mrk marker))
  "compare markers")

(cl-defmethod occ-obj-marker= ((obj marker)
                               (mrk marker))
  (if (and (occ-valid-marker obj)
           (occ-valid-marker mrk))
   (let ((obj-marker (occ-obj-org-marker obj))
         (mrk-marker (occ-obj-org-marker mrk)))
     (if (and (occ-valid-marker obj-marker)
              (occ-valid-marker mrk-marker))
      (or (equal obj-marker
                 mrk-marker)
          (>= 1 (abs (- obj-marker
                        mrk-marker))))))))

(cl-defmethod occ-obj-marker= ((obj occ-obj-tsk)
                               (tsk occ-obj-tsk))
  (occ-obj-marker= (occ-obj-marker (occ-obj-tsk obj))
                   (occ-obj-marker (occ-obj-tsk tsk))))

(cl-defmethod occ-obj-marker= ((obj occ-obj-tsk)
                               (mrk marker))
  (occ-obj-marker= (occ-obj-marker (occ-obj-tsk obj))
                   (occ-obj-marker mrk)))

(cl-defmethod occ-obj-marker= ((obj marker)
                               (tsk occ-obj-tsk))
  (occ-obj-marker= (occ-obj-marker obj)
                   (occ-obj-marker (occ-obj-tsk tsk))))

(cl-defmethod occ-obj-marker= ((obj occ-obj-tsk)
                               (mrk null))
  (ignore obj)
  (ignore mrk)
  nil)

(cl-defmethod occ-obj-marker= ((obj null)
                               (tsk occ-obj-tsk))
  (ignore obj)
  (ignore tsk)
  nil)


(cl-defgeneric occ-obj-current-p (obj)
  "return if OBJ is currently clocking")

(cl-defmethod occ-obj-current-p ((obj occ-obj-tsk))
  "return if OBJ is currently clocking"
  (occ-obj-marker= (occ-current-tsk)
                   (occ-obj-tsk obj)))

(cl-defgeneric occ-obj-clocking-in-p (obj)
  "return if OBJ is currently clocking")

(cl-defmethod occ-obj-clocking-in-p ((obj occ-obj-tsk))
  "return if OBJ is currently clocking"
  (occ-obj-current-p obj))


(cl-defgeneric occ-obj-associable-p (obj)
  "Test if CTSK is associate")

(cl-defmethod occ-obj-associable-p ((obj null))
  "Test if CTSK is associate"     ;not required.
  (ignore obj)
  nil)

(cl-defmethod occ-obj-associable-p ((obj occ-obj-ctx-tsk))
  "Test if CTSK is associate"     ;not required.
  (let ((ctxual-tsk (occ-obj-build-ctxual-tsk obj)))
    (occ-obj-associable-p ctxual-tsk)))

(cl-defmethod occ-obj-associable-p ((obj occ-ctxual-tsk))
  (> (occ-obj-rank obj)
     0))


(cl-defgeneric occ-obj-associable-with-p (obj
                                          ctx)
  "Test if CTSK is associate")

(cl-defmethod occ-obj-associable-with-p ((obj occ-obj-tsk)
                                         (ctx occ-ctx))
  (let ((ctxual-tsk (occ-obj-build-ctxual-tsk-with (occ-obj-tsk obj)
                                                   ctx)))
    (occ-obj-associable-p ctxual-tsk)))


(cl-defgeneric occ-obj-unammed-p (obj)
  "occ-obj-unnamed-p")

(cl-defmethod occ-obj-unnamed-p ((obj marker))
  ;; (occ-debug "occ-obj-unnamed-p(marker=%s)" obj)
  (occ-clock-marker-unnamed-p obj))

(cl-defmethod occ-obj-unnamed-p ((obj occ-obj-tsk))
  ;; (occ-debug "occ-obj-unnamed-p(occ-tsk=%s)" (occ-name obj))
  (occ-obj-unnamed-p (occ-obj-marker obj)))


(cl-defmethod occ-obj-current-associable-p ((ctx occ-ctx))
  (let ((ctxual-current-tsk (occ-obj-ctxual-current-tsk ctx)))
    (occ-obj-associable-p ctxual-current-tsk)))

;;; occ-predicate.el ends here
