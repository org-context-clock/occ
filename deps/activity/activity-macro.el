;;; activity-macro.el --- avtivity macros            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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

(provide 'activity-macro)


;; (require '@)


(progn
  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(\\<\\(def@\\) +\\([^ ()]+\\)"
                             (1 'font-lock-keyword-face)
                             (2 'font-lock-function-name-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("\\(@\\^?:[^ ()]+\\)\\>"
                             (1 'font-lock-builtin-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("\\(@drive-object\\)\\>"
                             (1 'font-lock-builtin-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("\\(drive-extended@\\)\\>"
                             (1 'font-lock-builtin-face))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(\\<\\(defobjgen@\\) +\\([^ ()]+\\)"
                             (1 'font-lock-keyword-face)
                             (2 'font-lock-function-name-face)))))

  ;; (font-lock-add-keywords 'emacs-lisp-mode
  ;;                         '(("\\(@extend-object\\)\\>"
  ;;                            (1 'font-lock-builtin-face))))


;; (defmacro @extend-object (object &rest body)
;;   `(with-@@ object
;;      ,@(if (stringp (cl-first body))
;;            `((setf @:doc ,(cl-first body))))
;;      ,@(if (stringp (cl-first body))
;;            (cl-rest body) body)))
;; (put '@extend-object 'lisp-indent-function 1)

(defmacro @drive-object (baseobjs &rest body)
  "Drive or extend a new object from OBJECT, with BODY will belong
to new object, if first element of BODY is string then assigning
it to :doc property of new object."
  `(let ((drived-obj (@extend ,@baseobjs)))
     (with-@@ drived-obj
       ;; Documentation
       ,@(if (stringp (cl-first body))
             `((setf @:doc ,(cl-first body))))
       ;; BODY
       ,@(if (stringp (cl-first body)) (cl-rest body) body))

     drived-obj))
(put '@drive-object 'lisp-indent-function 1)

(defmacro drive-extended@ (obj baseobjs &rest body)
  "Directly assign using @drive-object"
  `(progn
     (setf ,obj (@drive-object ,baseobjs
                               ,@body))
     (when (and (memq :initialize (@! ,obj :keys))
                (functionp (@ ,obj :initialize)))
       (@! ,obj :initialize))))
(put 'drive-extended@ 'lisp-indent-function 2)

(defmacro defobjgen@ (baseobj gen-method params &rest body)
  "Add a method GEN-METHOD to OBJECT, to generate new extended
object from OBJECT itself. For this GEN-METHOD first parameter is
NAME and remaining remaining parameter as PARAMS, and if BODY
first parameter is string then making it documentation string for
this method, and rest of BODY will be part of generated object
from this method, "
  `(progn
     (def@ ,baseobj ,gen-method (name ,@params)
       ;; Documentation
       ,@(if (stringp (cl-first body))
             (list (cl-first body)) ())
       ;; BODY
       (@drive-object (,baseobj)
         ,@(if (stringp (cl-first body)) (cl-rest body) body)))))
(put 'defobjgen@ 'lisp-indent-function 3)


(defmacro @mapcar (f l)
  `(mapcar #'(lambda (e) (with-@@ e (funcall ,f @@)))
           ,l))

;;; activity-macro.el ends here
