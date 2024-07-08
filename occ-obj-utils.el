;;; occ-obj-utils.el --- object utils                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

;;

;;; Code:

(provide 'occ-obj-utils)


(require 'occ-obj-ctor)
(require 'occ-obj)
(require 'occ-assert)
(require 'occ-cl-utils)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-util-common)


;; method
(fmakunbound 'occ-obj-return-tranform)

(cl-defmethod occ-obj-return-tranform ((ap-obj occ-ap-normal)
                                       (obj occ-obj))
  "Will make all action except first to return OCC-RETURN-SELECT-LABEL."
  ;; (occ-debug "(occ-obj-ap-callables ap-obj obj) = %s" (cl-first (occ-obj-ap-callables ap-obj obj)))
  (let* ((identity-sel-callable            (occ-obj-make-callable-normal :select
                                                                     occ-return-select-name
                                                                     occ-return-select-function))
         (identity-sel-ret-lambda-callable (occ-obj-build-return-lambda identity-sel-callable
                                                                    occ-return-select-label))
         (callables                        (occ-obj-ap-callables ap-obj obj))
         (new-callables                    (cons identity-sel-ret-lambda-callable
                                                 (mapcar #'occ-obj-build-return-lambda
                                                         ;; #'(lambda (c)
                                                         ;;     (occ-obj-build-return-lambda c occ-return-evaluate))
                                                         callables)))) ;;BUG to fix
    (occ-assert callables)
    (occ-obj-build-ap-normal (cons :callables
                                   new-callables))))

(cl-defmethod occ-obj-return-tranform ((ap-transf-obj occ-ap-transf)
                                       (obj occ-obj))
  "Will make transformer fun to change action except first to
return occ-return-label."
  (ignore obj)
  (let ((new-transform #'(lambda (action
                                    candidate)
                             (let ((transform (occ-obj-ap-transform ap-transf-obj)))
                               (occ-assert transform)
                               (let* ((candidate-obj (occ-obj-return-get-value candidate))
                                      (ap-normal-obj (funcall transform
                                                              action candidate-obj)))
                                 (occ-debug "occ-obj-return-tranform: lambda: ap-normal-obj = %s" ap-normal-obj)
                                 ;; (occ-obj-return-tranform ap-normal-obj obj)
                                 (occ-obj-return-tranform ap-normal-obj candidate-obj))))))
      (occ-obj-build-ap-transf (cons :transform
                                     new-transform))))


(cl-defmethod occ-obj-return-in-labels-p (retval &rest label)
  (ignore label)
  retval)

(cl-defmethod occ-obj-return-in-labels-p ((retval occ-return) &rest label)
  (memq (occ-return-label retval)
        label))


(cl-defmethod occ-obj-return-get-value (retval)
  retval)

(cl-defmethod occ-obj-return-get-value ((retval occ-return))
  (occ-return-value retval))


(cl-defmethod occ-obj-return-get-label (retval)
  retval)

(cl-defmethod occ-obj-return-get-label ((retval occ-return))
  (occ-return-label retval))


(defun occ-specs ()
  ;; (occ-cl-method-param-case '(occ-obj-make-collection (`((head ,val)) val)))
  (occ-cl-method-param-case '(occ-obj-make-collection (`(string symbol (eql ,val) list integer integer) val))))


(when nil                               ;testing
  (occ-cl-method-param-signs 'occ-obj-make-collection)
  (car (occ-cl-method-param-signs 'occ-obj-make-collection) ) (string symbol (eql :list) list integer integer)
  (occ-cl-method-param-case '(occ-obj-make-collection (`((string symbol (eql ,val))) val)))

  (pcase '((head :list))
    (`((head ,val)) val)
    (_ nil))

  (pcase '((string symbol (eql :list) list integer integer))
    (`((string symbol (eql ,val) list integer integer)) val)
    (_ nil))

  (occ-cl-method-param-case '(occ-obj-make-collection (`(string symbol (eql ,val) list integer integer) val))))
  

(defun occ-valid-spec-p (spec)
  (memq spec (list :tree :list)))

;;; occ-obj-utils.el ends here
