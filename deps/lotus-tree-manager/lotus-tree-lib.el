;;; lotus-tree-lib.el --- Lotus Tree Library         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'lotus-tree-lib)


;; old
(defun collect-carlist-old (alist)
  (let ((ulist nil))
    (dolist (pair (copy-tree alist))
      (if (assoc (cl-first pair) ulist)
          (nconc (assoc (cl-first pair) ulist) (cl-rest pair))
        (setf ulist (append ulist (list pair)))))
    ulist))

;; new
(defun collect-carlist (alist)
  (let ((ulist nil))
    (dolist (pair alist)
      (unless (assoc (cl-first pair) ulist)
        (when (cl-first pair)
          (push (list (cl-first pair)) ulist)))
      (let ((ef (cl-first pair)))
        (ignore ef)
        (let ((xulist (assoc (cl-first pair) ulist))
              (tlist  nil)
              (ilist  (cl-rest pair)))
          (dolist (i ilist)
            (unless (member i (cl-rest xulist))
              (push i tlist)))
          (setq tlist (nreverse tlist))
          (nconc (assoc (cl-first pair) ulist) tlist))))
    (setq ulist (nreverse ulist))))


(defun collect-alist (alist)
  (let ((ulist nil))
    (dolist (pair alist)
      (unless (assoc (cl-first pair) ulist)
        (when (cl-first pair)
          (push (list (cl-first pair)) ulist)))
      (nconc (assoc (cl-first pair) ulist) (list (cl-rest pair))))
    (setq ulist (nreverse ulist))))


(defun delete-dups-alist (alist)
  (dolist (pair alist)
    (setcdr pair (delete-dups (cl-rest pair))))
  alist)


(defun max-depth (tree &optional nodep)
  (let ((nodep (or nodep #'atom)))
    (if (funcall nodep tree)
        0
      (1+ (cl-reduce #'max
                     (mapcar #'(lambda (subtree)
                                 (if (consp subtree)
                                     (max-depth subtree nodep)
                                   0))
                             tree))))))

(defun collect-elem-cond (tree nodep predicate)
  (if (funcall nodep tree)
      tree
    (when (funcall predicate tree)
      (remove nil
              (mapcar #'(lambda (subtree)
                          (collect-elem-cond subtree nodep predicate))
                      (cl-rest tree))))))

(defun collect-elem-cond-depth (tree nodep predicate depth)
  (collect-elem-cond tree
                     nodep
                     #'(lambda (subtree)
                         (or
                          (<= (max-depth subtree nodep) depth)
                          (funcall predicate subtree)))))

(defun collect-elem-simple-depth (tree depth)
  (collect-elem-cond-depth tree
                           #'(lambda (x)
                               (not (listp x)))
                           #'(lambda (subtree)
                               (memq (cl-first subtree) '(t)))
                           depth))


(defun tree-add-old (keys item list)
  (let ((key (cl-first keys)))
    (if (cl-rest list)
        (if key
            (progn
              (unless (assoc key (cl-rest list))
                (nconc (cl-rest list) (list (list key))))
              (tree-add (cl-rest keys) item (assoc key (cl-rest list))))
          (unless (memq item (cl-rest list))
            (nconc (cl-rest list) (list item))))
      (progn
        (when key
          (nconc list (list (list key))))
        (if (cl-rest keys)
            (tree-add (cl-rest keys) item (assoc key (cl-rest list)))
          (if key
              (nconc (assoc key (cl-rest list)) (list item))
            (nconc list (list item))))))))

;;;###autoload
(defun tree-add (keys item list)
  (let ((key (cl-first keys)))
    (if (cl-rest list)
        (if key
            (progn
              (unless (assoc key (cl-rest list))
                (nconc (cl-rest list) (list (list key))))
              (tree-add (cl-rest keys) item (assoc key (cl-rest list))))
          (unless (member item (cl-rest list))
            (nconc (cl-rest list) (list item))))
      (progn
        (when key
          (nconc list (list (list key))))
        (if (cl-rest keys)
            (tree-add (cl-rest keys) item (assoc key (cl-rest list)))
          (if key
              (nconc (assoc key (cl-rest list)) (list item))
            (nconc list (list item))))))))


;;;###autoload
(defun tree-flatten (predicate L)
  ;; https://stackoverflow.com/a/19967639
  "Converts a list to single level."
  (let ((predicate (or predicate #'atom)))
   (if (null L)
       nil
     (if (funcall predicate (cl-first L))
         (cons (cl-first L)
               (tree-flatten predicate (cl-rest L)))
       (append (tree-flatten predicate (cl-first L))
               (tree-flatten predicate (cl-rest L)))))))


(when nil
 (setq org-capture+-helm-templates-tree '(t))
 (tree-add '(x z) 'y org-capture+-helm-templates-tree)
 (tree-add '(x z) 'k org-capture+-helm-templates-tree)
 (tree-add '(a z) 'k org-capture+-helm-templates-tree)
 (tree-add '(x n) 'i org-capture+-helm-templates-tree)
 (tree-add '(x n b) 'i org-capture+-helm-templates-tree)
 (tree-add '(x #'(lambda () t) x) 'c org-capture+-helm-templates-tree))

;;; lotus-tree-lib.el ends here
