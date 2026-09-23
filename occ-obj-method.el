;;; occ-obj-method.el --- tsk ctsk cxtual-tsk callable method action from helm  -*- lexical-binding: t; -*-

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

;; occ-obj-method.el defines the methods callable from the helm selection
;; menu for `occ-obj-tsk' and friends.
;;
;; It implements the menu actions themselves: `occ-do-checkout',
;; `occ-do-clock-out' (with `occ-run-do-clock-out'), `occ-do-close',
;; `occ-do-force-clockin' and `occ-do-print-rank', plus debug helpers
;; (`occ-do-call-with-obj', `occ-do-set-debug-obj', `occ-get-debug-obj',
;; `occ-describe-debug-obj') and the human readable description machinery
;; (`occ-obj-describe-string', `occ-obj-rank-desc-str', `occ-dformat',
;; `occ-obj-fmt-tree-p', `occ-do-describe-obj', `occ-do-display-obj').
;; `occ-do-properties-editor-combined' offers combined edit / checkout /
;; clock operations for a contextual task through a timed helm session.
;; Selection UI layer, at the object model level.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-obj-method)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-obj-clock-method)
(require 'occ-tsk)
(require 'occ-print)
(require 'occ-obj-ctor)
(require 'occ-helm-actions-config)
(require 'occ-prop)
(require 'occ-rank)


(cl-defmethod occ-do-checkout ((obj occ-obj-tsk))
  "Checkout the properties of OCC-OBJ-TSK OBJ.
Delegates to occ-do-op-props-checkout on OBJ."
  (occ-do-op-props-checkout obj))


(defun occ-util-read-sexp-from-minibuffer (prompt)
  "Read a Lisp expression from the minibuffer with PROMPT.
Returns the expression form without evaluating it."
  ;; (cl-first (read-from-string (read-from-minibuffer prompt)))
  (read--expression prompt))



(cl-defmethod occ-do-call-with-obj ((obj occ-obj-tsk))
  "Evaluate an expression read from the minibuffer with OBJ.
Prompts for an expression and funcalls it with OBJ bound as obj."
  (let ((fun (let ((obj obj)
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (obj) ,exp-with-obj) obj)))))
    (funcall fun)))

(cl-defmethod occ-do-call-with-obj ((obj occ-obj-tsk))
  "Evaluate an expression with OBJ bound under a chosen name.
Prompts for a variable name for OBJ and an expression to run with
that binding."
  (let ((fun (let ((obj-name     (occ-util-read-sexp-from-minibuffer "obj name: ")) ;prefill with obj
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (,obj-name) ,exp-with-obj) obj)))))
    (funcall fun)))

(let ((occ-debug-object nil))
  (cl-defmethod occ-do-set-debug-obj ((obj occ-obj-tsk))
    "Set occ-debug-object to OCC-OBJ-TSK OBJ for debug access.
Messages how to reach the object with occ-get-debug-obj."
    (setq occ-debug-object obj)
    (occ-message "Use (occ-get-debug-obj) to access object."))
  (defun occ-describe-debug-obj ()
    "Describe the current debug object in a description buffer."
    (interactive)
    (occ-do-describe-obj occ-debug-object))
  (defun occ-get-debug-obj ()
    "Return the object stored in occ-debug-object."
    (interactive)
    occ-debug-object))


(cl-defmethod occ-obj-fmt-tree-p (obj)
  "Return nil so that OBJ is described on a single line."
  nil)

(cl-defmethod occ-obj-fmt-tree-p ((obj occ-obj-tsk))
  "Return t so that OCC-OBJ-TSK OBJ is described as a tree."
  t)

(cl-defmethod occ-obj-fmt-tree-p ((obj occ-ranktbl))
  "Return t so that OCC-RANKTBL OBJ is described as a tree."
  t)

(defun occ-dformat (level &rest args)
  "Format ARGS and prefix the result with LEVEL indentation.
Uses two spaces per positive LEVEL and returns the string."
  (let ((levelstr (if (> level 0) (make-string (* 2 level) ?\s) "")))
    (concat levelstr (apply #'format args))))

(defun occ-dformat-kv (level obj slot)
  "Return a key value line for SLOT of OBJ indented at LEVEL.
Formats the slot value inline or as an indented tree per
occ-obj-fmt-tree-p."
  (occ-dformat (1+ level) "%s:%s%s\n"
               slot
               (if (occ-obj-fmt-tree-p (occ-cl-obj-slot-value obj slot)) "\n" " ")
               (occ-obj-describe-string (occ-cl-obj-slot-value obj slot)
                                        (if (occ-obj-fmt-tree-p (occ-cl-obj-slot-value obj slot))
                                            (+ 2 level)
                                          0))))

(cl-defmethod occ-obj-describe-string (obj
                                       &optional
                                       level)
  "something")

(cl-defmethod occ-obj-describe-string ((obj occ-obj-tsk)
                                       &optional
                                       level)
  "Return a human readable description of OCC-OBJ-TSK OBJ.
Adds the rank summary and every class slot of OBJ indented by
LEVEL which defaults to zero."
  (let ((level (or level 0)))
    (concat (occ-dformat level "Object: %s\n\n" (occ-obj-Format obj))
            (occ-obj-rank-desc-str obj level)
            (occ-dformat (1+ level) "Rank: %s"
                         (occ-obj-describe-string (occ-obj-ranktbl-with (occ-obj-tsk obj)
                                                                        (occ-obj-ctx obj))
                                                  (+ 2 level)))
            (occ-dformat-kv level obj 'descendant-count)
            (occ-dformat-kv level obj 'descendant-weight)
            (apply #'concat
                   (cl-loop for p in (occ-cl-class-slots (occ-cl-inst-classname obj))
                            collect (occ-dformat-kv level obj p))))))

(cl-defmethod occ-obj-describe-string ((obj occ-ranktbl)
                                       &optional
                                       level)
  "Return a description of OCC-RANKTBL OBJ.
Lists the name and value and plist and inheritance fields of OBJ
indented by LEVEL."
  (concat (occ-dformat level "Ranktbl %s\n" (occ-ranktbl-name obj))
          (occ-dformat level " Rank: %s\n" (occ-ranktbl-value obj))
          (occ-dformat level " Plist: %s\n" (occ-ranktbl-plist obj))
          (occ-dformat level " Inheritable: %s\n" (occ-ranktbl-inheritable obj))
          (occ-dformat level " Nonheritable: %s\n" (occ-ranktbl-nonheritable obj))
          (occ-dformat level " Max-Decendent: %s\n" (occ-ranktbl-max-decendent obj))))

(cl-defmethod occ-obj-rank-desc-str ((obj occ-obj-tsk)
                                     &optional
                                     level)
  "Return a rank breakdown line for OCC-OBJ-TSK OBJ.
Shows the acquired and ancestor ranks over the descendant weight
of the task and its final rank indented by LEVEL."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((acqrank (occ-obj-rank-acquired-with tsk
                                               ctx))
          (dweight (occ-tsk-descendant-weight tsk))
          (ancrank (occ-obj-ancestor-rank-with (occ-tsk-parent tsk)
                                               ctx
                                               0))
          (rank    (occ-obj-rank-with tsk
                                      ctx)))
      (occ-dformat (1+ level) "acquired(%f) + ancestor-rank(%s) / dweight(%f) = rank(%f)\n"
                   acqrank
                   ancrank
                   dweight
                   rank))))

(cl-defmethod occ-obj-describe-string ((obj string)
                                       &optional
                                       level)
  "Return the string OBJ itself formatted at indentation LEVEL."
  (occ-dformat level obj))

(cl-defmethod occ-obj-describe-string ((obj number)
                                       &optional
                                       level)
  "Return the number OBJ formatted as a float at LEVEL."
  (occ-dformat level "%f" obj))

(cl-defmethod occ-obj-describe-string ((obj null)
                                       &optional
                                       level)
  "Return the literal Nil formatted at indentation LEVEL."
  (occ-dformat level "Nil"))

(cl-defmethod occ-obj-describe-string ((obj symbol)
                                       &optional
                                       level)
  "Return the name of symbol OBJ formatted at LEVEL."
  (occ-dformat level "%s" obj))


(cl-defmethod occ-do-describe-obj ((obj occ-obj-tsk))
  "Show a description of OCC-OBJ-TSK OBJ in a read only buffer.
Fills a *helpful occ-object* buffer with occ-obj-describe-string
and switches to it in another window returning nil."
  (let ((buf (get-buffer-create (format "*helpful occ-object: %s*"
                                        (occ-obj-format obj)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; (setf (buffer-string) "")
        (erase-buffer)
        ;; (cl-prettyprint obj)
        (insert (occ-obj-describe-string obj)))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)
    nil))


(cl-defmethod occ-do-display-obj ((obj occ-obj-tsk))
  "Display OCC-OBJ-TSK OBJ by delegating to occ-do-describe-obj."
  (occ-do-describe-obj obj))


;; do both fast and interactive editing.
;; (occ-do-properties-editor obj)
(cl-defmethod occ-do-properties-editor-combined ((obj occ-obj-ctx-tsk))
  "Run a timed helm session of operations on OCC-OBJ-CTX-TSK OBJ.
Sources offer checkout and clock operations and fast and full
property edits for the contextual task OBJ."
  (cl-flet ((promptfn (prompt-txt) (format "%s: %s" prompt-txt (occ-obj-Format (occ-obj-tsk obj)))))
    (let ((helm-checkout-source (helm-build-sync-source (promptfn "Operations")
                                  :candidates (list (cons (format "Continue with same clock task %s" (occ-obj-Format obj))
                                                          (occ-clouser-call-obj-on-cand 'skip))
                                                    (cons "Try another clocking"
                                                          (occ-clouser-call-obj-on-cand 'no-action)) ;to bypass three repeat cycle of (occ-try-until ) function
                                                    (cons "Checkout"
                                                          (occ-clouser-call-obj-on-cand #'occ-do-checkout)))
                                  :action     (list (cons "Run"
                                                          (occ-clouser-call-cand-on-obj obj)))))
          (helm-clock-ops-source (helm-build-sync-source (promptfn "Clock Operations")
                                   :candidates (occ-obj-callable-helm-actions (occ-obj-gen-clock-operations obj)
                                                                              obj)
                                   :action     (list (cons "Run"
                                                           (occ-clouser-call-cand-on-obj obj)))))
          (helm-fast-edit-source (helm-build-sync-source (promptfn "Fast Edit")
                                   :candidates (occ-obj-callable-helm-actions (occ-obj-gen-each-prop-fast-edits obj)
                                                                              obj)
                                   :action     (list (cons "Run"
                                                           (occ-clouser-call-cand-on-obj obj)))))
          (helm-edit-source     (helm-build-sync-source (promptfn "Edit")
                                  :candidates (list (cons "Edit"
                                                          (occ-clouser-call-obj-on-cand #'occ-do-properties-editor)))
                                  :action     (list (cons "Run"
                                                          (occ-clouser-call-cand-on-obj obj)))))
          (helm-fast-checkout-source (helm-build-sync-source (promptfn "Fast Checkout")
                                       :candidates (occ-obj-callable-helm-actions (occ-obj-gen-each-prop-fast-checkouts obj)
                                                                                  obj)
                                       :action     (list (cons "Run"
                                                               (occ-clouser-call-cand-on-obj obj)))))
          (helm-simple-checkout-source (helm-build-sync-source (promptfn "Checkout")
                                         :candidates (occ-obj-callable-helm-actions (occ-obj-gen-simple-checkouts obj)
                                                                                    obj)
                                         :action     (list (cons "Run"
                                                                 (occ-clouser-call-cand-on-obj obj))))))
      (let ((sources (list helm-checkout-source
                           helm-clock-ops-source
                           helm-fast-edit-source
                           helm-edit-source
                           helm-fast-checkout-source
                           helm-simple-checkout-source)))
        (helm-timed occ-idle-timeout nil
          (helm :sources sources))))))


(cl-defmethod occ-do-print-rank ((obj occ-obj-tsk))
  "Log the rank of OCC-OBJ-TSK OBJ with occ-debug."
  (occ-debug "Rank for %s is %d"
               (occ-obj-Format obj)
               (occ-obj-rank obj)))


(cl-defmethod occ-do-close ((obj occ-obj-tsk))
  "Stub: only logs that closing OCC-OBJ-TSK OBJ is not implemented."
  (occ-debug "ImplementIt: mark this %s %d tsk CLOSE "
               (occ-obj-Format obj)
               (occ-obj-rank obj)))


(cl-defmethod occ-do-force-clockin ((obj occ-obj-tsk))
  "Stub: only logs that forced clock-in of OBJ is not implemented."
  (occ-debug "ImplementIt: Force this %s %d tsk clock-in for a PERIOD with period property"
               (occ-obj-Format obj)
               (occ-obj-rank obj)))


(defun occ-run-do-clock-out (&optional switch-to-state
                                       fail-quietly
                                       at-time)
  "Run org-clock-out on the current org entry.

Passes SWITCH-TO-STATE and FAIL-QUIETLY and AT-TIME through
unchanged."
  (interactive)
  (org-clock-out switch-to-state
                 fail-quietly
                 at-time))
(cl-defmethod occ-do-clock-out ((obj occ-obj-tsk))
  "Clock out the running clock of OCC-OBJ-TSK OBJ.
Delegates to occ-run-do-clock-out with the wrapped task of OBJ."
  (occ-run-do-clock-out (occ-obj-tsk obj)))

;;; occ-obj-method.el ends here
