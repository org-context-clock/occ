;;; occ-normalize-ineqs.el --- normalize in-equalities  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: s <>
;; Keywords: convenience, abbrev

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

(provide 'occ-normalize-ineqs)


(require 'calc)
(require 'calc-ext)
(require 'calc-aent)


(require 'occ-obj)
(require 'occ-prop-intf)
(require 'occ-intf)


(defvar occ-property-priority-inequalities nil)
(defvar occ-property-priorities nil)


(defun occ-obj-properties-for-rank ()
  (delete-dups (append (occ-obj-properties-to-calculate-rank 'occ-obj-tsk 'null)
                       (occ-obj-properties-to-calculate-rank 'occ-obj-tsk 'occ-obj-ctx))))

(defun occ-do-assert-sexp-ineq (ineq)
  (occ-assert (= 3 (length ineq)))
  (occ-assert (memq (car ineq) '(> < =)))
  (occ-assert (not (cl-every #'consp (cdr ineq))))
  (occ-assert (cl-some #'symbolp (cdr ineq)))
  (dolist (el (cdr ineq))
    (cond ((symbolp el)
           (occ-assert (symbolp el)))
          ((consp el)
           (progn
             (occ-assert (= 3 (length el)))
             (occ-assert (memq (car el) '(* +)))
             (occ-assert (or (and (symbolp (car el))
                                  (numberp (cadr el)))
                             (and (numberp (car el))
                                  (symbolp (cadr el))))))))))
(defun occ-do-assert-math-ineq (ineq)
  "Assert variables of INEQ inequality are from properties."
  (cond ((and (listp ineq)
              (eql 'var (car ineq)))
         (let ((var (cadr ineq))
               (properties (occ-obj-properties-for-rank)))
           (occ-assert (memq var properties) nil "variable %s is not in properties %s" var properties)))
        ((atom ineq) ineq)
        ((listp ineq)
         (dolist (ine ineq)
           (occ-do-assert-math-ineq ine))))
  t)
(defun occ-do-assert-math-ineq-has-prop-p (ineq property)
  "Check if PROPERTY variable is present in INEQ inequality"
  (cond ((and (listp ineq)
              (eql 'var (car ineq)))
         (let ((var (cadr ineq)))
           (eql property var)))
        ((atom ineq) nil)
        ((listp ineq)
         (cl-some #'(lambda (e) (occ-do-assert-math-ineq-has-prop-p e property)) ineq))))
;; (occ-do-assert-math-ineq-has-prop (occ-obj-ineq-wash (occ-obj-math-read-expr "root + nil") 'key) 'key)

(defun occ-obj-math-var (sym)
  ;;(math-read-expr (symbol-name sym))
  `(var ,sym ,(intern (concat "var-" (symbol-name sym)))))
(defun occ-math-read-sexp-expr (sexp)
  "Converts a Lisp sexp expression SEXP into an equivalent expression."
  (cond ((atom sexp) (cond ((assoc (prin1-to-string sexp) math-standard-opers) (cadr (assoc (symbol-name sexp) math-standard-opers)))
                           ((symbolp sexp) (occ-obj-math-var sexp))
                           (t sexp)))
        ((listp sexp) (cons (occ-math-read-sexp-expr (car sexp))
                            (mapcar #'occ-math-read-sexp-expr (cdr sexp))))
        (t sexp)))
;; (occ-math-read-sexp-expr '(> current-clock timebeing))
(defun occ-math-read-str-expr (ineq-str)
  (let ((replace-op-alist '(("-" . "XXXMINUSXXX")
                            ("_" . "XXXEXPOXXX"))))
    (cl-labels ((replace-op-char (str op-char op-replacement)
                                 (let* ((op-regex (concat "[[:alpha:]]\+[[:alnum:]]\*\\(" op-char "\\)[[:alpha:]]\+[[:alnum:]]\*"))
                                        (rstr     (replace-regexp-in-string op-regex op-replacement str nil nil 1)))
                                   (if (string= rstr str)
                                       str
                                     (replace-op-char rstr op-char op-replacement))))
                (reinstate-op-char (ineq op-char op-replacement)
                              (cond ((and (listp ineq)
                                          (eql 'var (car ineq))
                                          (string-match op-replacement (symbol-name (cadr ineq))))
                                     (occ-obj-math-var (intern (string-replace op-replacement op-char (symbol-name (cadr ineq))))))
                                    ((atom ineq) ineq)
                                    ((listp ineq) (cons (reinstate-op-char (car ineq) op-char op-replacement)
                                                        (mapcar #'(lambda (ineq) (reinstate-op-char ineq op-char op-replacement))
                                                                (cdr ineq))))
                                    (t ineq))))
      (let ((ineq (cl-reduce #'(lambda (initial a)
                                 (funcall #'replace-op-char initial (car a) (cdr a)))
                             replace-op-alist
                             :initial-value ineq-str)))
        (cl-reduce #'(lambda (initial a)
                       (funcall #'reinstate-op-char initial (car a) (cdr a)))
                     replace-op-alist
                     :initial-value (math-read-expr ineq))))))
;; (occ-math-read-str-expr "current-clock > time_being")
(defun occ-obj-math-read-expr (ineq)
  "Return normalized calc math expression."
  (calc-normalize (cond ((stringp ineq) (occ-math-read-str-expr ineq)) ;; (math-read-expr ineq)
                        ((consp ineq)   (occ-math-read-sexp-expr ineq))
                        (t (occ-error "ineq %s not string or list" ineq)))))
(defun occ-obj-ineq-wash (ineq property)
  (cond ((and (listp ineq)
              (eql 'var (car ineq))
              (memq (cadr ineq) '(nil this)))
         (occ-obj-math-var property))
        ((atom ineq) ineq)
        ((listp ineq) (cons (occ-obj-ineq-wash (car ineq) property)
                            (mapcar #'(lambda (ineq) (occ-obj-ineq-wash ineq property)) (cdr ineq))))
        (t ineq)))
;; (occ-obj-ineq-wash (occ-obj-math-read-expr "a + nil") 'xx)
;; (occ-do-assert-ineq (occ-obj-ineq-wash (occ-obj-math-read-expr "root + nil") 'key))
(defun occ-obj-topo-sort-ineqs-expr (inequalities)
  (let ((graph     '())
        (in-degree '()))
    (dolist (ineq inequalities)
      (cl-destructuring-bind (op var1 var2) ineq
        (unless (cl-member var1 graph :key #'car)
          (push (list var1) graph)
          (push (cons var1 0) in-degree))
        (unless (cl-member var2 graph :key #'car)
          (push (list var2) graph)
          (push (cons var2 0) in-degree))
        (cond ((eq op 'calcFunc-gt)
               (progn
                 (cl-pushnew (cons var2 'gt) (cdr (assoc var1 graph)))
                 (cl-incf (cdr (assoc var2 in-degree)))))
              ((eq op 'calcFunc-lt)
               (progn
                 (cl-pushnew (cons var1 'lt) (cdr (assoc var2 graph)))
                 (cl-incf (cdr (assoc var1 in-degree)))))
              ((eq op 'calcFunc-eq)
               (progn
                 (cl-pushnew (cons var1 'eq) (cdr (assoc var2 graph)))
                 (cl-incf (cdr (assoc var1 in-degree))))))))
    (let ((in-degree-old (mapcar #'copy-list in-degree))
          (sorted-vars   '())
          (queue (mapcar #'(lambda (x) (cons x 'gt))
                         (cl-remove-if-not #'(lambda (x)
                                               (= 0 (cdr (assoc x in-degree))))
                                           (mapcar #'car graph))))
          (var nil))
      (while (setf var (pop queue))
        (setq sorted-vars (append sorted-vars (list var)))
        (dolist (vertex (cdr (assoc (car var) graph)))
          (cl-decf (cdr (assoc (car vertex) in-degree)))
          (if (= 0 (cdr (assoc (car vertex) in-degree)))
              (setf queue (append queue (list vertex))))))
      (list (list :inequalities inequalities)
            (list :graph graph)
            (list :in-degree-old in-degree-old)
            (list :in-degree in-degree)
            (list :sorted-vars sorted-vars)))))
(defun occ-obj-gen-constant (prefix)
  (gensym (concat prefix "")))
(defun occ-obj-gen-math-constant (prefix)
  (let ((const-var (occ-obj-gen-constant prefix)))
    `,(occ-obj-math-var const-var)))
(defun occ-obj-gen-ineq2eq-constant (op)
  (let* ((calc-const-geq-var (occ-obj-gen-math-constant "cxgeq"))
         (calc-const-gth-var (occ-obj-gen-math-constant "cxgth")))
    (if (memq op '(calcFunc-gt calcFunc-lt))
        `(+ ,calc-const-gth-var ,calc-const-geq-var)
      calc-const-geq-var)))
(defun occ-obj-ineq2eq (ineq)
  (cond ((and (consp ineq)
              (symbolp (car ineq))
              (memq (car ineq) '(calcFunc-gt calcFunc-geq calcFunc-lt calcFunc-leq)))
         (occ-assert (= 3 (length ineq)))
         (let ((arg1 (cadr ineq))
               (arg2 (caddr ineq)))
           (let ((const-expr (occ-obj-gen-ineq2eq-constant (car ineq))))
             (cond ((memq (car ineq) '(calcFunc-gt calcFunc-geq))
                    `(calcFunc-eq ,arg1 (+ ,arg2 ,const-expr)))
                   ((memq (car ineq) '(calcFunc-lt calcFunc-leq))
                    `(calcFunc-eq ,arg2 (+ ,arg1 ,const-expr)))))))
        ((listp ineq) (cons (occ-obj-ineq2eq (car ineq))
                            (mapcar #'occ-obj-ineq2eq (cdr ineq))))
        (t ineq)))
(defun occ-obj-ineqs2eqs (ineqs)
  (mapcar #'occ-obj-ineq2eq
          ineqs))
(defun occ-obj-ineqs-from-map (ineqs-map)
  (apply #'append
         (mapcar #'cdr ineqs-map)))
(defun occ-obj-vars-from-syms (syms)
  (mapcar #'(lambda (var) (occ-obj-math-var var))
          syms))
(defun occ-obj-vars-from-map (ineqs-map)
  (occ-obj-vars-from-syms (mapcar #'car
                                  ineqs-map)))
(defun occ-obj-build-math-solve-expr (eqs vars)
  `(calcFunc-solve (vec ,@(occ-obj-ineqs2eqs eqs))
                   (vec ,@vars)))
(defun occ-obj-normalize-eqs (eqs vars)
  (calc-normalize (occ-obj-build-math-solve-expr eqs
                                                 vars)))
(defun occ-obj-eqs-normalized-p (eqs vars)
  (let ((sol (occ-obj-normalize-eqs eqs
                                    vars)))
    (if (> (length (cdadr sol))
           1)
        (not (eql (car sol)
                  'calcFunc-solve))
      t)))
(defun occ-obj-normalize-ineqs-map (ineqs-map)
  (occ-obj-normalize-eqs (occ-obj-ineqs2eqs (occ-obj-ineqs-from-map ineqs-map))
                         (occ-obj-vars-from-map ineqs-map)))
(defun occ-obj-eqs-exprs (eqs)
  (mapcar #'caddr
          (cdr eqs)))
(defun occ-obj-find-expr-vars (expr)
  (cond ((and (listp expr)
              (eql 'var (car expr)))
         (list (cadr expr)))
        ((atom expr) nil)
        ((listp expr)
         (let ((first-el (occ-obj-find-expr-vars (car expr)))
               (rest-els (mapcar #'occ-obj-find-expr-vars (cdr expr))))
           (append first-el
                   (apply #'append  rest-els))))
        (t nil)))
(defun occ-obj-find-exprs-vars (exprs)
  (delete nil
          (delete-dups (apply #'append
                              (mapcar #'occ-obj-find-expr-vars exprs)))))
(defun occ-obj-find-eqs-vars (eqs)
  (occ-obj-find-exprs-vars (occ-obj-eqs-exprs eqs)))


(defun occ-do-add-ineq-internal (property ineq)
  (let ((ineq (occ-obj-ineq-wash (occ-obj-math-read-expr ineq)
                                 property)))
    (when (and (occ-do-assert-math-ineq-has-prop-p ineq property)
               (occ-do-assert-math-ineq ineq)
               (not (member ineq (cdr (assoc property
                                             occ-property-priority-inequalities)))))
      (let ((eqs (occ-obj-ineqs2eqs (append (occ-obj-ineqs-from-map (assoc-delete-all property
                                                                                      occ-property-priority-inequalities))
                                            (cons ineq
                                                  (cdr (assoc property
                                                              occ-property-priority-inequalities))))))
            (vars (occ-obj-vars-from-syms (delete-dups (cons property
                                                             (mapcar #'car
                                                                     occ-property-priority-inequalities))))))
        (if (occ-obj-eqs-normalized-p eqs
                                      vars)
            (if (assoc property
                       occ-property-priority-inequalities)
                (cl-pushnew ineq (cdr (assoc property
                                             occ-property-priority-inequalities)))
              (cl-pushnew (list property ineq)
                          occ-property-priority-inequalities))
          (occ-error "Failed to add inequality %s for property %s may be due to circular dependency"
                     (math-format-flat-expr ineq 1)
                     property))))))

(defun occ-obj-ineq-internal (property)
  (cdr (assoc property
              occ-property-priority-inequalities)))

(defun occ-obj-priority-internal (property)
  (cdr (assoc property
              occ-property-priorities)))

(defun occ-obj-const-value (const)
  (if occ-config-ineq-const-value
      (if (fboundp occ-config-ineq-const-value)
          (funcall occ-config-ineq-const-value)
        occ-config-ineq-const-value)
      (random 99)))

(defun occ-obj-equal-consts-exprs (consts)
  (cons 'vec
        (mapcar #'(lambda (c)
                    `(calcFunc-eq ,(occ-obj-math-var c)
                                  ,(occ-obj-const-value c)))
                consts)))

(defun occ-obj-eqality2pair (eq)
  (cons (cadadr eq)
        (caddr eq)))

(defun ooc (ineqs-map)
  (let* ((sols   (occ-obj-normalize-ineqs-map ineqs-map))
         (consts (occ-obj-find-eqs-vars sols)))
    (while consts
      (let ((const (pop consts)))
        (setq sols (calc-normalize (math-expr-subst sols
                                                    (occ-obj-math-var const)
                                                    (occ-obj-const-value const))))))
    (mapcar #'occ-obj-eqality2pair
            (cdr sols))))

(defun occ-obj-ineq-map-solution (ineqs-map)
  (let* ((sols   (occ-obj-normalize-ineqs-map ineqs-map))
         (consts (occ-obj-find-eqs-vars sols)))
    (while consts
      (let ((const (pop consts)))
        (setq sols (calc-normalize (math-expr-subst sols
                                                    (occ-obj-math-var const)
                                                    (occ-obj-const-value const))))))
    (mapcar #'occ-obj-eqality2pair
            (cdr sols))))

(defun occ-do-set-prop-priorities ()
  (setq occ-property-priorities (occ-obj-ineq-map-solution occ-property-priority-inequalities)))




;; (occ-do-add-ineq-1 "nil > (key + 20)" 'root)
;; (occ-do-add-ineq-1 "nil > (key + status + 2)" 'root)
;; (occ-do-add-ineq-1 "nil > (key * status + 2)" 'root)
;; (occ-do-add-ineq-1 "nil > 10" 'key)
;; (occ-do-add-ineq-1 "nil > 2 * root" 'status)


;; ;; (occ-do-add-ineq-1 "nil > status" 'key)
;; ;; (occ-do-add-ineq-1 "nil < key" 'status)

;; (occ-obj-ineq-wash (occ-obj-math-read-expr "root > (key + 2)") 'root)

;; ;; (math-format-flat-expr (occ-obj-normalize-ineqs-map occ-property-priority-inequalities) 1)

;; "[status = cx411135 - (2 * (cx411137 + (cx411135 + 1) * (cx411136 + 11)) + 6) / (2 * cx411136 + 21) + 1, key = cx411136 + 11, root = (cx411137 + (cx411135 + 1) * (cx411136 + 11) + 3) / (-2 * cx411136 - 21)]"

;; "[status = cx-1627737 + 2 * (cx-1627739 + cx-1627738) + 29, key = cx-1627738 + 11, root = cx-1627739 + cx-1627738 + 14]"

;; (occ-obj-find-expr-vars (cadr (occ-obj-eqs-exprs (occ-normalize-ineqs occ-property-priority-inequalities))))
;; (occ-obj-find-exprs-vars (occ-obj-eqs-exprs (occ-normalize-ineqs occ-property-priority-inequalities)))
;; (occ-obj-find-eqs-vars (occ-obj-normalize-ineqs-map occ-property-priority-inequalities))

;; (occ-obj-properties-for-rank)

;; (occ-obj-ineq-wash (occ-obj-math-read-expr "2 * root + 1  > root +  (key + 2)") 'root)
;; (occ-obj-ineq2eq (calc-normalize (math-read-expr "x > b +2")))

;; (equal (math-read-expr "x = (a + 2)")
;;        (occ-math-read-sexp-expr '(= x (+ a 2))))

;; (calc-normalize (list 'vec (car (math-read-exprs " a + b > (a + c)"))))


;; (math-expr-subst '(vec (calcFunc-eq (var x var-x) (var a var-a)))  '(var a var-a) 1)



;; (calc-normalize '(calcFunc-solve (vec (calcFunc-eq (* 2 (var a var-a)) (var d var-d)) (calcFunc-eq (+ (var b var-b) (var c1 var-c1)) (var d var-d)) (calcFunc-eq (+ (+ (var a var-a) (var c1 var-c1)) (var c2 var-c2)) (var d var-d)))
;;                                  (vec (var a var-a) (var b var-b))))

;; (math-simplify '(vec (calcFunc-eq (* 2 (var a var-a)) (var d var-d)) (calcFunc-eq (+ (var b var-b) (var c1 var-c1)) (var d var-d)) (calcFunc-eq (+ (+ (var a var-a) (var c1 var-c1)) (var c2 var-c2)) (var d var-d))))

;; (calc-normalize '(calcFunc-solve (vec (calcFunc-eq (+ (var a var-a) (var b var-b)) (var d var-d)) (calcFunc-eq (+ (var b var-b) (var c var-c)) (var z var-z)) (calcFunc-eq (+ (+ (var a var-a) (var c var-c)) 10) (var z var-z)))
;;                                  (vec (var a var-a) (var b var-b) (var c var-c))))

;; (vec (calcFunc-eq (var a var-a) (- (+ (- (var d var-d) (var z var-z)) (/ (- (* 2 (var z var-z)) (var d var-d)) 2)) 5))
;;      (calcFunc-eq (var b var-b) (+ (+ (var z var-z) (/ (- (var d var-d) (* 2 (var z var-z))) 2)) 5))
;;      (calcFunc-eq (var c var-c) (- (/ (- (* 2 (var z var-z)) (var d var-d)) 2) 5)))


;; (math-read-exprs "[ a + b = 1]")
;; (math-read-exprs "solve([x+y=3, x-y=1], [x,y])")
;; (math-format-flat-expr '(vec (calcFunc-eq (var a var-a) (float 55 -1)) (calcFunc-eq (var b var-b) (float 45 -1))) 0)
;; (math-format-flat-expr (car (math-read-exprs "[ a + b = 1]")) 0)

;; (calc-normalize "[a + b = c1 , a - b = c2]")
;; (calc-normalize "a - b > 0")
;; (calc-eval "solve([a - b > 0], [a,b])")
;; (calc-eval "solve([a + b = c1 , a - b = c2], [a,b])")



;; (setq xxx '((a)))

;; (pushnew 'c (cdr (cl-assoc 'a xxx)))


;; ;; (length '(a () x))

;; ;; (occ-obj-properties-to-calculate-rank 'occ-tsk)
;; ;; (occ-obj-properties-to-calculate-rank 'occ-obj-ctx-tsk)

;; (defvar occ-tsk-normalized-ineq '(none 1 key 10 status 20 current-clock 40 timebeing 50 root 10 currfile 20))
;; (defvar occ-obj-ctx-tsk-normalized-ineq '(none 1 root 10 currfile 20))

;;; occ-normalize-ineqs.el ends here
