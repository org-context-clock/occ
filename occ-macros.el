;;; occ-macros.el --- occ macros                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad Pratap <>
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

(provide 'occ-macros)


(require 'org-refile)
(require 'org-capture+-lib)


(defvar occ-testing-status nil)
(defun occ-enable-testing ()
  (setq occ-testing-status t))
(defun occ-disable-testing ()
  (setq occ-testing-status nil))
(defun occ-toggle-testing ()
  (setq occ-testing-status (not occ-testing-status)))
(defmacro occ-testing (&rest body)
  `(when occ-testing-status ,@body))


(defmacro occ-mac-with-marker (marker &rest body)
  `(let ((marker ,marker))
     (progn
       ,@body)))
(put 'occ-mac-with-marker 'lisp-indent-function 1)


(defmacro occ-mac-with-org-marker (mrk &rest body)
  `(with-current-buffer (marker-buffer ,mrk)
     (save-excursion
       (goto-char ,mrk)
       (forward-char 1)                  ;required
       (end-of-line)
       (occ-back-to-heading)
       (progn
         ,@body))))
(put 'occ-mac-with-org-marker 'lisp-indent-function 1)


;; (defmacro occ-debug-return (label &rest body)
;;   `(let ((retval
;;           (progn ,@body)))
;;      (occ-debug "%s: returns %s\n" ,label retval)))
;; (put 'occ-debug-return 'lisp-indent-function 1)

(defmacro occ-debug-return (label &rest body)
  (ignore label)
  `(progn ,@body))
(put 'occ-debug-return 'lisp-indent-function 1)


(defmacro occ-try-until (tries test &rest body)
  `(let* ((total-tries ,tries)
          (try         total-tries))
     (while (and (> try 0)
                 ,test)
       (setq try (1- try))
       ,@body)))
(put 'occ-try-until 'lisp-indent-function 2)


(defmacro occ-aggregate-rank (var property tsk aggregator &rest body)
  `(let ((values    (if (occ-obj-list-p tsk ,property)
                        (occ-obj-get-property ,tsk ,property)
                      (list (occ-obj-get-property ,tsk ,property))))
         (total-rank 0))
     (dolist (,var values)
       (let ((rank (progn
                     ,@body)))
         (setq total-rank
               (funcall ,aggregator total-rank rank))))
     total-rank))
(put 'occ-aggregate-rank 'lisp-indent-function 4)


;;;###autoload
(defmacro occ-defcommand (name args &rest body)
  `(progn
     (defun ,name ,args
       ,@body)))
(put 'occ-defcommand 'lisp-indent-function 2)


(defmacro occ-generate-plist-functions (prefix item)
  (let* ((plist  (intern (concat (symbol-name prefix) "-" (symbol-name item) "s-plist")))
         (clear  (intern (concat (symbol-name prefix) "-" (symbol-name item) "-clear")))
         (add    (intern (concat (symbol-name prefix) "-" (symbol-name item) "-add")))
         (set    (intern (concat (symbol-name prefix) "-" (symbol-name item) "-set")))
         (get    (intern (concat (symbol-name prefix) "-" (symbol-name item) "-get")))
         (allget (intern (concat (symbol-name prefix) "-" (symbol-name item) "s-get"))))
    `(progn

       (defvar ,plist nil)

       (defun ,clear ()
         (setq ,plist nil))

       (defun ,add (key name ,item)
         (setq ,plist
               (plist-put
                ,plist
                key (cons name ,item))))

       (defun ,set (key name ,item)
         (setq ,plist
               (plist-put
                ,plist
                key (cons name ,item))))

       (defun ,get (key)
         (plist-get ,plist key))

       (defun ,allget (&rest keys)
         (let ((items nil))
           (dolist (key keys)
             (let ((name-item (,get key)))
               (when name-item
                 (setf items (nconc items (list name-item))))))
           items)))))


;; configs
(defun occ-mkstr (&rest args)
  (with-output-to-string
    (dolist (a args) (princ a))))

(defun occ-symb (&rest args)
  (intern (apply #'occ-mkstr args)))


(defmacro occ-gen-binary-option-commands (prefix
                                          name
                                          suffix
                                          default
                                          &rest body)
  (let* ((option      (occ-symb prefix name      suffix))
         (enable-fun  (occ-symb prefix 'enable-  name '-function))
         (disable-fun (occ-symb prefix 'disable- name '-function))
         (enable      (occ-symb prefix 'enable-  name))
         (disable     (occ-symb prefix 'disable- name))
         (toggle      (occ-symb prefix 'toggle-  name))
         (value       (occ-symb prefix 'value-   name)))
    `(let* ((,option ,default)
            (impl
              #'(lambda ()
                  (if ,option
                      (if (fboundp ',enable-fun)  (funcall #',enable-fun))
                      (if (fboundp ',disable-fun) (funcall #',disable-fun))))))
       (defun ,enable ()
         (interactive)
         (setf ,option t)
         (funcall impl))

       (defun ,disable ()
         (interactive)
         (setf ,option nil)
         (funcall impl))

       (defun ,toggle ()
         (interactive)
         (setf ,option (not ,option))
         (funcall impl))

       (defun ,value ()
         (interactive)
         ,option)

       (progn
         ,@body)

       (progn
         (occ-debug "Generated functions\n- %s\n- %s\n- %s\n- %s\n" ',enable ',disable ',toggle ',value)
         (unless (fboundp ',enable-fun)
           (occ-debug "Define %s function" ',enable-fun))
         (unless (fboundp ',disable-fun)
           (occ-debug "Define %s function" ',disable-fun))))))


(defmacro occ-gen-numeric-commands (prefix
                                    name
                                    suffix
                                    default
                                    &rest body)
  (let* ((option      (occ-symb prefix name      suffix))
         (enable-fun  (occ-symb prefix 'enable-  name '-function))
         (disable-fun (occ-symb prefix 'disable- name '-function))
         (enable      (occ-symb prefix 'enable-  name))
         (disable     (occ-symb prefix 'disable- name))
         (toggle      (occ-symb prefix 'toggle-  name))
         (value       (occ-symb prefix 'value-   name)))
    `(let* ((,option ,default)
            (impl    #'(lambda ()
                         (if ,option
                             (if (fboundp ',enable-fun)  (funcall #',enable-fun))
                           (if (fboundp ',disable-fun) (funcall #',disable-fun))))))
       (defun ,enable ()
         (interactive)
         (setf ,option t)
         (funcall impl))

       (defun ,disable ()
         (interactive)
         (setf ,option nil)
         (funcall impl))

       (defun ,toggle ()
         (interactive)
         (setf ,option (not ,option))
         (funcall impl))

       (defun ,value ()
         (interactive)
         ,option)

       (progn
         ,@body)

       (progn
         (occ-debug "Generated functions\n- %s\n- %s\n- %s\n- %s\n" ',enable ',disable ',toggle ',value)
         (unless (fboundp ',enable-fun)
           (occ-debug "Define %s function" ',enable-fun))
         (unless (fboundp ',disable-fun)
           (occ-debug "Define %s function" ',disable-fun))))))


(defun occ-get-location ())


;;;###autoload
(defmacro occ-load-noerror-mustsuffix (file)
  "Load FILE with optional arguments NOERROR and MUSTSUFFIX."
  `(load ,file 'noerror nil nil 'mustsuffix))


(defvar occ-condition-case-control-debug nil)

;;;###autoload
(defun occ-enable-condition-case-control-debug ()
  (interactive)
  (setq occ-condition-case-control-debug t))

;;;###autoload
(defun occ-disable-condition-case-control-debug ()
  (interactive)
  (setq occ-condition-case-control-debug nil))

(defmacro condition-case-control (var bodyform &rest handlers)
  (if occ-condition-case-control-debug
      `(condition-case ,var
           ,bodyform
         ,@handlers)
    bodyform))
(put 'condition-case-control 'lisp-indent-function 1)
(defmacro occ-mac-condition-case-control (var bodyform &rest handlers)
  (if occ-condition-case-control-debug
      `(condition-case ,var
           ,bodyform
         ,@handlers)
    bodyform))
(put 'occ-mac-condition-case-control 'lisp-indent-function 1)


;; (defmacro occ-run-unobtrusively (obtrusive &rest body)
;;   `(if (or obtrusive
;;            (called-interactively-p 'any))
;;        (progn
;;          ,@body)
;;      (while-no-input
;;        (redisplay)
;;        ,@body)))

(defmacro occ-run-unobtrusively (obtrusive &rest
                                           body)
  `(progn
     (ignore ,obtrusive)
     (if (or obtrusive
             (called-interactively-p 'any))
         (progn
           ,@body)
       (let ((retval (while-no-input
                       (redisplay)
                       ,@body)))
         (when (eq retval t)
           (occ-debug "user input %s retval %s" last-input-event retval))
         retval))))
(put 'occ-run-unobtrusively 'lisp-indent-function 1)


(occ-testing
  (let ((collection
         '("* TODO %? %^g\n %i\n [%a]\n"
           "* TODO %? %^g\n %i\n Test [%a]\n")))
    (helm :sources
          `(((name . "Templates: ")
             (multiline)
             (candidates ,@collection)
             (action . identity))))))

(occ-testing
  ;; https://code.orgmode.org/bzg/org-mode/commit/e2bdc488ee071ea9743b00424db28fce3505fe5d
  ;; Refiling: Allow to create new nodes.

  ;; When refiling, you can now create new parent nodes on the fly.  To do
  ;; this, set the variable `org-refile-allow-creating-parent-nodes' to
  ;; `confirm'.  Then, at a refiling prompt, proceed with completion until
  ;; you have an existing heading, and then add "/new heading", i.e. a
  ;; slash followed by the new heading.  That heading will be created as a
  ;; child of the existing heading, and the entry to be refiled will end up
  ;; under that new heading.

  ;;; occ-macros.el ends here

  (org-refile-get-location)


  (org-capture-run
   'entry
   '(marker org-clock-marker)
   "* Hello %^{PROMPT}"
   ;; :immediate-finish t
   :empty-lines 1))


;; (let (k f k0 x)
;;   (ignore k)
;;   (ignore f)
;;   (ignore x)
;;   (ignore k0)
;;   ;; https://stackoverflow.com/questions/3811448/can-call-with-current-continuation-be-implemented-only-with-lambdas-and-closures
;;   (lambda (f k)
;;     (f (lambda (v k0) (k v)) k))

;;   ;; https://stackoverflow.com/questions/612761/what-is-call-cc
;;   (defvar x 0)

;;   (+ 2 (call/cc (lambda (cc)
;;                   (setq x cc) 3)))
;;   (x 4))
