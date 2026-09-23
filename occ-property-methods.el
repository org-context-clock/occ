;;; occ-property-methods.el --- org capture plus         -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author:
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

;; occ-property-methods.el implements the property protocol for each
;; concrete property: it is the implementation layer of the OCC property
;; protocol, tangled from occ-property-methods.org.
;;
;; For each property symbol (currfile, root, git-branch, timebeing,
;; status, key, current-clock, subtree, deadline, scheduled) it supplies
;; cl-defmethod specializations on (eql PROPS) of the impl generics:
;; occ-obj-impl-occ-prop-p, occ-obj-impl-prop=, occ-obj-impl-rank,
;; occ-obj-impl-get (capturing from occ-ctx or prompting the
;; occ-user-agent), occ-obj-impl-list-p, occ-obj-impl-to-org,
;; occ-obj-impl-from-org, occ-obj-impl-propfmt, occ-obj-impl-require-p,
;; occ-obj-impl-default, occ-obj-impl-inheritable-p,
;; occ-do-impl-operation and occ-do-impl-checkout.
;;
;; Highlights: currfile and root rank by file match and directory
;; containment and check out with find-file; git-branch stores
;; "vc-root::branch" values and checks out via magit-checkout; timebeing
;; scores the remaining time against clock-sum; status ranks done and
;; HOLD tasks; key carries an arbitrary rank capped at 100;
;; current-clock ranks the task being clocked; subtree is an ido subtree
;; file picker; deadline and scheduled mirror status.  The _template_
;; blocks in occ-testing at the end show how to define a new property.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

;; * Provide this file

;; [[file:occ-property-methods.org::*Provide this file][Provide this file:1]]
(provide 'occ-property-methods)

;; Provide this file:1 ends here

;; Required libraries

;; [[file:occ-property-methods.org::*Required libraries][Required libraries:1]]
(require 'org)
(require 'magit-git)
(require 'magit-process)


(require 'occ-impl-utils)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-prop-intf)
(require 'occ-rank)
(require 'occ-intf)

;; * Required libraries:1 ends here
;;
;;
;;
;; Current File property of task
;;
;; [[file:occ-property-methods.org::*Current File property of task][Current File property of task:1]]
;;{{ currfile

(cl-defmethod occ-obj-impl-occ-prop-p ((prop (eql currfile)))
  "Impl-occ-prop-p specialization for the currfile property:
marks currfile as an OCC introduced property."
  t)

(cl-defmethod occ-obj-impl-prop= ((prop (eql currfile))
                                  prop-value
                                  value)
  "Impl-prop= specialization for the currfile property: VALUE
matches PROP-VALUE when both name the same file."
  (occ-pu-file= prop-value
                value))
(cl-defmethod occ-obj-impl-rank ((tsk occ-obj-tsk)
                                 (ctx occ-obj-ctx)
                                 (prop (eql currfile))) ;; do not use (prop (eql file)) that is another property which represent file in which task defined.
  ;; file in which tsk aka org entry exists.
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (occ-aggregate-rank tsk-currfile prop tsk #'max
    (if (occ-obj-prop= prop
                       tsk-currfile
                       (occ-ctx-file ctx))
        (occ-rank-percentage 100)     ;Obsolete: as exact match to files giving double matching points.
      (occ-rank-percentage 0))))
(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql currfile))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (occ-debug "calling occ-obj-impl-get(ctx occ-ctx)")
  (let ((currfile (occ-ctx-file ctx)))
    ;; (occ-message "currfile %s" currfile)
    currfile))
(cl-defmethod occ-obj-impl-list-p ((mrk marker)
                                   (prop (eql currfile)))
  "Impl-list-p specialization for the currfile property: returns
t so currfile counts as a list valued property on MARKER."
  (ignore prop)
  t)

(cl-defmethod occ-obj-impl-to-org ((prop (eql currfile))
                                   value)
  "Impl-to-org specialization for the currfile property: VALUE is
already an org compatible string."
  (ignore prop)
  value)
(cl-defmethod occ-obj-impl-from-org ((prop (eql currfile))
                                     value)
  "Impl-from-org specialization for the currfile property: VALUE
is kept unchanged as the org drawer string."
  (ignore prop)
  value)
(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql currfile))
                                (obj occ-obj-ctx-tsk))
  "currfile property for tsk aka org entry"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (let* ((ctx-currfile (if ctx (occ-ctx-file ctx)))
           (ctx-dir      (when (stringp ctx-currfile)
                           (file-name-directory ctx-currfile)))
           (prompt       (concat (symbol-name prop) ": ")))
      (ido-read-file-name prompt ctx-dir ctx-currfile))))
(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (prop (eql currfile))
                                    (vdirector number))
  "Do-impl-checkout specialization for the currfile property:
find-file the currfile value of TSK selected by VDIRECTOR."
  (let* ((tsk  (occ-obj-tsk obj))
         (file (occ-obj-pvalue tsk
                               prop
                               vdirector)))
       (if file
           (find-file file)
         (occ-debug "occ-do-impl-checkout: %s value ruturned for prop %s" file prop))))
(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql currfile)))
  "Impl-inheritable-p specialization for the currfile property:
the rank propagates up to ancestor tasks."
  t)
      ;;}}

;; Current File property of task:1 ends here

;; Root dir property of task

;; [[file:occ-property-methods.org::*Root dir property of task][Root dir property of task:1]]
;;{{ root

(cl-defmethod occ-obj-impl-occ-prop-p ((prop (eql root)))
  "Impl-occ-prop-p specialization for the root property: marks
root as an OCC introduced property."
  t)

(cl-defmethod occ-obj-impl-prop= ((prop (eql root))
                                  prop-value
                                  value)
  "Impl-prop= specialization for the root property: VALUE matches
PROP-VALUE by directory containment."
  (occ-pu-file-in-dir-p prop-value
                        value))
(cl-defmethod occ-obj-impl-rank ((tsk occ-obj-tsk)
                                 (ctx occ-obj-ctx)
                                 (prop (eql root)))
  "RANK Predicate funtion to check if ctx matches to tsk's ROOT attribute."
  (occ-aggregate-rank tsk-root prop tsk #'max
    (if (occ-obj-prop= prop
                       tsk-root
                       (occ-ctx-file ctx))
        (occ-rank-percentage 100)
      (occ-rank-percentage 0))))

(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql root))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (let ((file (occ-ctx-file ctx)))
      (when file
        (directory-file-name (dirname-of-file file)))))
(cl-defmethod occ-obj-impl-list-p ((mrk marker)
                                   (prop (eql root)))
  "Impl-list-p specialization for the root property: returns t so
root counts as a list valued property on MARKER."
  (ignore prop)
  t)
(cl-defmethod occ-obj-impl-to-org ((prop (eql root))
                                   value)
  "Impl-to-org specialization for the root property: VALUE is
already an org compatible string."
  (ignore prop)
  value)
(cl-defmethod occ-obj-impl-from-org ((prop (eql root))
                                     value)
  "Impl-from-org specialization for the root property: VALUE is
kept unchanged as the org drawer string."
  (ignore prop)
  value)
(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql root))
                                (obj occ-obj-ctx-tsk))
  "READ"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (let* ((ctx-file   (when ctx (occ-ctx-file ctx)))
           (ctx-dir    (when (stringp ctx-file) (file-name-directory ctx-file)))
           (prompt     (concat (symbol-name prop) ": ")))
      (ido-read-directory-name prompt ctx-dir ctx-dir))))
(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (prop (eql root))
                                    (vdirector number))
  "Do-impl-checkout specialization for the root property:
find-file the root directory value of TSK selected by VDIRECTOR."
  (let* ((tsk (occ-obj-tsk obj))
         (dir (occ-obj-pvalue tsk
                              prop
                              vdirector)))
    (if dir
        (find-file dir)
      (occ-debug "occ-do-impl-checkout: %s value ruturned for prop %s" dir prop))))

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql root)))
  "Impl-inheritable-p specialization for the root property: the
rank propagates up to ancestor tasks."
  t)
      ;;}}

;; Root dir property of task:1 ends here

;; Git branch property of task

;; [[file:occ-property-methods.org::*Git branch property of task][Git branch property of task:1]]
;;{{ git-branch


(cl-defmethod occ-obj-impl-occ-prop-p ((prop (eql git-branch)))
  "Impl-occ-prop-p specialization for the git-branch property:
marks git-branch as an OCC introduced property."
  t)
(cl-defmethod occ-obj-impl-prop= ((prop (eql git-branch))
                                  prop-value
                                  value)
  "Impl-prop= specialization for the git-branch property: VALUE
matches PROP-VALUE when vc root and branch name both match."
  (let ((prop-val-list (when prop-value
                         (split-string prop-value "::")))
        (val-list      (when value
                         (split-string value "::"))))
    (when (= 2
             (length prop-val-list)
             (length val-list))
      (let ((pvroot (nth 0 prop-val-list))
            (vroot  (nth 0 val-list)))
        (when (occ-pu-file= pvroot
                            vroot)
          (let ((pvbranch (nth 1 prop-val-list))
                (vbranch  (nth 1 val-list)))
            (occ-pu-string= pvbranch
                            vbranch)))))))
(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql git-branch))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (require 'magit-git)
  (require 'magit-process)
  (let ((buff (occ-ctx-buffer ctx)))
    ;; (occ-message "occ-obj-impl-get: git-branch: buff = %s" buff)
    (when buff
      (with-current-buffer buff
        (let ((vc-root (vc-root-dir))
              (branch (magit-get-current-branch)))
          ;; (occ-message "occ-obj-impl-get: branch = %s" branch)
          (when (and vc-root branch)
              (concat vc-root "::" branch)))))))
(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql git-branch))
                                (obj occ-obj-ctx-tsk))
  "Read value of list of elements if (occ-obj-list-p OBJ PROPERTY)
        else element for property PROPERTY from user for OCC-TSK OBJ,
        must return ORG compatible value."
  (require 'magit-git)
  (require 'magit-process)
  (let ((buff (occ-obj-buffer obj)))
    ;; (occ-message "occ-obj-impl-get: git-branch: buff = %s" buff)
    (when buff
      (with-current-buffer buff
        (let ((vc-root (vc-root-dir))
              (branch  (magit-read-branch "Git branch")))
          ;; (occ-message "occ-obj-impl-get: branch = %s" branch)
          (concat vc-root "::" branch))))))

(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (prop (eql git-branch))
                                    (vdirector number))
  "Do-impl-checkout specialization for the git-branch property:
run magit-checkout for the branch VALUE of TSK selected by
VDIRECTOR."
  (require 'magit-git)
  (require 'magit-process)
  (let* ((tsk        (occ-obj-tsk obj))
         (git-branch (occ-obj-pvalue tsk
                                     prop
                                     vdirector))
         (git-branch-list (when git-branch
                            (split-string git-branch "::"))))
    (when git-branch-list
      (let ((root   (nth 0 git-branch-list))
            (branch (nth 1 git-branch-list)))
        (when root
          (when branch
            (when (occ-pu-file-in-dir-p root
                                        default-directory)
              (magit-checkout git-branch))))))))

(cl-defmethod occ-obj-impl-rank ((tsk occ-obj-tsk)
                                 (ctx occ-obj-ctx)
                                 (prop (eql git-branch)))
  "Return the RANK (number) for OCC-TSK based on the property GIT-BRANCH"
  (occ-aggregate-rank tsk-git-branch prop tsk #'max
    (if (occ-obj-prop= prop
                       tsk-git-branch
                       ;; (occ-obj-impl-get ctx prop nil)
                       (occ-obj-get ctx nil prop nil))
        (occ-rank-percentage 100)
      (occ-rank-percentage 0))))

(cl-defmethod occ-obj-impl-propfmt ((obj occ-obj-tsk)
                                    (prop (eql git-branch))
                                    value)
  "Return format printable value of property PROPERTY."
  (nth 1 (split-string value "::")))
(cl-defmethod occ-obj-impl-list-p ((mrk marker)
                                   (prop (eql git-branch)))
  "Is the property GIT-BRANCH has VALUES in list, Method tell
         property represent list or not."
  t)
(cl-defmethod occ-obj-impl-to-org ((prop (eql git-branch))
                                   value)
  "Return string representation for property GIT-BRANCH, Method
      convert value VALUE of property PROPERTY from occ to org string
      representation."
  (format "%s" value))
(cl-defmethod occ-obj-impl-from-org ((prop (eql git-branch)
                                     value))
  "Return the Actual Object representation for property
      GIT-BRANCH, Method convert value VALUE of property PROPERTY from
      org string to occ representation."
  (unless (string= value "")
    value))
(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql git-branch)))
  "Impl-inheritable-p specialization for the git-branch property:
the rank propagates up to ancestor tasks."
  t)

;; (cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
;;                                       (operation (eql _operation_))
;;                                       (prop (eql git-branch))
;;                                       values)
;;   "Used by OCC-OBJ-GEN-EDIT-IF-REQUIRED to decide for this property
;;       GIT-BRANCH if CALLABLE (helm method) should be generated."
;;   nil)


;; (cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
;;                                     (prop (eql git-branch))
;;                                     (operation (eql _operation_)))
;;   "Return a default VALUE of property GIT-BRANCH."
;;   nil)
;; (cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
;;                                     (prop (eql git-branch))
;;                                     (operation (eql _operation_)))
;;   "Return a default VALUE of property GIT-BRANCH."
;;   nil)
;; (cl-defmethod occ-do-impl-operation ((obj occ-obj-tsk)
;;                                      (operation (eql _operation_))
;;                                      (prop (eql git-branch))
;;                                      values)
;;   "Do the actual _OPERATION_."
;;   nil)

;; Git branch property of task:1 ends here










































































;; Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time

;; [[file:occ-property-methods.org::*Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time][Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time:1]]
(cl-defmethod occ-obj-impl-rank ((tsk occ-obj-tsk)
                                 (ctx null)
                                 (prop (eql timebeing)))
  "Impl-rank specialization for the timebeing property with a null
CTX: score the remaining timebeing minutes of TSK against its
clock sum on a 0 to 100 scale."
  (ignore prop)
  (let ((timebeing (occ-obj-get-property tsk
                                         'timebeing)))
    (let ((timebeing-time (if timebeing
                              (org-duration-to-minutes timebeing)
                            (occ-rank-percentage 0)))
          (clocked-time   (occ-obj-get-property tsk
                                                'clock-sum)))
      (if (and (numberp clocked-time)
               (numberp timebeing-time)
               (> timebeing-time clocked-time))
          (/ (* (occ-rank-percentage 100)
                (- timebeing-time
                   clocked-time))
             timebeing-time)
        (occ-rank-percentage 0)))))

(cl-defmethod occ-obj-impl-list-p ((mrk marker)
                                   (prop (eql timebeing)))
  "Impl-list-p specialization for the timebeing property: returns
nil so timebeing is treated as single valued on MARKER."
  (ignore prop)
  nil)

(cl-defmethod occ-obj-impl-to-org ((prop (eql timebeing))
                                   value)
  "Impl-to-org specialization for the timebeing property: convert
a numeric VALUE to its string form or the empty string."
  (ignore prop)
  (if (numberp value)
      (number-to-string value)
    ""))

(cl-defmethod occ-obj-impl-from-org ((prop (eql timebeing))
                                     value)
  "Impl-from-org specialization for the timebeing property: parse
VALUE as a number with 0 as fallback."
  (ignore prop)
  (if (stringp value)
      (or (string-to-number value)
          0)
    0))

(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql timebeing))
                                (obj occ-tsk))
  "READ"
  (let ((tsk (occ-obj-tsk obj)))
    (ignore tsk)
    (let* ((prompt     (concat (symbol-name prop)
                               ": ")))
      (ignore prompt)
      (read-number "Timebeing mins: "))))

(cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
                                      (operation (eql increment))
                                      (prop (eql timebeing))
                                      values)
  "Impl-require-p specialization for the timebeing property and
increment OPERATION: required only when TSK is currently clocked."
  (ignore operation)
  (ignore prop)
  (ignore values)
  (occ-obj-current-p obj))

(cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
                                    (prop (eql timebeing))
                                    (operation (eql increment)))
  "Impl-default specialization for the timebeing property and
increment OPERATION: a default VALUE of 10 minutes when TSK is
currently clocked."
  (ignore prop)
  (ignore operation)
  (when (occ-obj-current-p obj)
    10))

(cl-defmethod occ-do-impl-operation ((obj occ-obj-tsk)
                                     (operation (eql increment))
                                     (prop (eql timebeing))
                                     values)
  "Do-impl-operation specialization for the timebeing property
and increment OPERATION on TSK: Stub: not yet implemented (signals
occ-error)."
  (ignore operation)
  (ignore values)
  (let ((tsk    (occ-obj-tsk obj)))
    (ignore tsk)
    (if (occ-obj-list-p obj prop)
        (occ-error "Implement it.")
      (occ-error "Implement it."))))

(cl-defmethod occ-do-impl-operation ((obj marker)
                                     (operation (eql increment))
                                     (prop (eql timebeing))
                                     values)
  "Do-impl-operation specialization for the timebeing property and
increment OPERATION on MARKER: Stub: not yet implemented (signals
occ-error)."
  (ignore obj)
  (ignore operation)
  (ignore values)
  (let ((prop-string (symbol-name prop)))
    (ignore prop-string)
    (if (occ-obj-list-p obj prop
            (occ-error "Implement it.")
          (occ-error "Implement it.")))))


(cl-defmethod occ-obj-valid-p ((operation (eql increment))
                               (prop      (eql timebeing)))
  "Occ-obj-valid-p specialization for the timebeing property and
increment OPERATION: increment is a valid operation for timebeing."
  (ignore prop)
  (ignore operation)
  t)

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql timebeing)))
  "Impl-inheritable-p specialization for the timebeing property:
the rank does not propagate to ancestor tasks."
  nil)

;; Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time:1 ends here


;; STATUS property of task

;; [[file:occ-property-methods.org::*STATUS property of task][STATUS property of task:1]]
(cl-defmethod occ-obj-impl-rank ((tsk  occ-obj-tsk)
                                 (ctx  null)
                                 (prop (eql status)))
  "Predicate funtion to check if ctx matches to tsk's status attribute."
  (ignore prop)
  (let ((todo-type (occ-obj-get-property tsk 'todo-type))
        (closed    (occ-obj-get-property tsk 'closed))
        (status    (occ-obj-get-property tsk 'todo-keyword)))
    (if (or closed
            (eql todo-type 'done)
            (string= status "HOLD"))
        (occ-rank-percentage 100)
      (occ-rank-percentage 0))))

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql status)))
  "Impl-inheritable-p specialization for the status property: the
rank propagates up to ancestor tasks."
  t)

;; STATUS property of task:1 ends here

;; Key property of task for setting arbitrary rank

;; [[file:occ-property-methods.org::*Key property of task for setting arbitrary rank][Key property of task for setting arbitrary rank:1]]
(cl-defmethod occ-obj-impl-rank ((tsk  occ-obj-tsk)
                                 (ctx  null)
                                 (prop (eql key)))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (ignore prop)
  (let* ((key (occ-obj-get-property tsk 'KEY)))
      (if key
          (let ((nkey (string-to-number key)))
            (if (> nkey (occ-rank-percentage 100))
                (occ-rank-percentage 100)
              nkey))
        (occ-rank-percentage 0))))

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql key)))
  "Impl-inheritable-p specialization for the key property: the
rank does not propagate to ancestor tasks."
  nil)

;; Key property of task for setting arbitrary rank:1 ends here

;; Current clock status property of task (will rank based on task is currently clocking-in or not)

;; [[file:occ-property-methods.org::*Current clock status property of task (will rank based on task is currently clocking-in or not)][Current clock status property of task (will rank based on task is currently clocking-in or not):1]]
(cl-defmethod occ-obj-impl-rank ((tsk  occ-obj-tsk)
                                 (ctx  null)
                                 (prop (eql current-clock)))
  "Impl-rank specialization for the current-clock property with a
null CTX: rank 100 when TSK is the task currently clocked by
org-clock else 0."
  (ignore prop)
  (let* ((tsk-marker (occ-obj-get-property tsk 'marker)))
    (ignore tsk-marker)
    (if (and org-clock-marker
             (occ-obj-marker= tsk org-clock-marker))
        (occ-rank-percentage 100)
      (occ-rank-percentage 0))))

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql current-clock)))
  "Impl-inheritable-p specialization for the current-clock
property: the rank does not propagate to ancestor tasks."
  nil)

;; Current clock status property of task (will rank based on task is currently clocking-in or not):1 ends here

;; SubtreeFile property of task

;; [[file:occ-property-methods.org::*SubtreeFile property of task][SubtreeFile property of task:1]]
;;{{ sub-tree
(cl-defmethod occ-obj-readprop ((obj occ-obj-ctx-tsk)
                                (prop (eql subtree)))
  "Occ-obj-readprop specialization for the subtree property: pick
an org subtree file with ido and return its name relative to the
default directory."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (ignore ctx)
    (let* ((prompt (concat (symbol-name prop) ": "))
           ;; org-iread-file-name
           (filename (ido-read-file-name prompt
                                         default-directory ;DIR
                                         nil               ;DEFAULT-FILENAME
                                         t                 ;MUSTMATCH
                                         nil               ;PREDICATE
                                         #'(lambda (f)     ;INITIAL
                                             (string-match "\.\+.org" f)))))
      (file-relative-name filename
                          default-directory))))

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql subtree)))
  "Impl-inheritable-p specialization for the subtree property: the
rank does not propagate to ancestor tasks."
  nil)
;;}}

;; SubtreeFile property of task:1 ends here



;; DEADLINE property of task

;; [[file:occ-property-methods.org::*DEADLINE property of task][DEADLINE property of task:1]]
(cl-defmethod occ-obj-impl-rank ((tsk  occ-obj-tsk)
                                 (ctx  null)
                                 (prop (eql deadline)))
  "Predicate funtion to check if ctx matches to tsk's deadline attribute."
  (ignore prop)
  (let ((todo-type (occ-obj-get-property tsk 'todo-type))
        (closed    (occ-obj-get-property tsk 'closed))
        (deadline    (occ-obj-get-property tsk 'todo-keyword)))
    (if (or closed
            (eql todo-type 'done)
            (string= deadline "HOLD"))
        (occ-rank-percentage 100)
        (occ-rank-percentage 0))))

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql deadline)))
  "Impl-inheritable-p specialization for the deadline property:
the rank propagates up to ancestor tasks."
  t)

;; DEADLINE property of task:1 ends here


;; SCHEDULED property of task

;; [[file:occ-property-methods.org::*SCHEDULED property of task][SCHEDULED property of task:1]]
(cl-defmethod occ-obj-impl-rank ((tsk  occ-obj-tsk)
                                 (ctx  null)
                                 (prop (eql scheduled)))
  "Predicate funtion to check if ctx matches to tsk's scheduled attribute."
  (ignore prop)
  (let ((todo-type (occ-obj-get-property tsk 'todo-type))
        (closed    (occ-obj-get-property tsk 'closed))
        (scheduled    (occ-obj-get-property tsk 'todo-keyword)))
    (if (or closed
            (eql todo-type 'done)
            (string= scheduled "HOLD"))
        (occ-rank-percentage 100)
        (occ-rank-percentage 0))))

(cl-defmethod occ-obj-impl-inheritable-p ((prop (eql scheduled)))
  "Impl-inheritable-p specialization for the scheduled property:
the rank propagates up to ancestor tasks."
  t)

;; SCHEDULED property of task:1 ends here


;; _template1_ property of task


;; _template1_ property of task:1 ends here















(occ-testing
 (cl-defmethod occ-obj-impl-rank ((obj occ-tsk)
                                  (prop (eql _template_)))
   "Return the RANK (number) for OCC-TSK based on the property _TEMPLATE_"
   (ignore obj)
   (ignore prop))
 (cl-defmethod occ-obj-impl-has-p ((obj occ-obj-tsk)
                                   (property symbol)
                                   value)
   "OBJ-has-property PROPERTY"
   (ignore obj)
   (ignore property)
   (ignore value))
 (cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                 (property symbol)
                                 arg)
   "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
   (ignore obj)
   (ignore property)
   (occ-error "must return occ compatible value."))
 (cl-defmethod occ-obj-impl-format ((obj occ-obj-tsk)
                                    (property symbol)
                                    value)
   "Return format printable value of property PROPERTY."
   (ignore obj)
   (ignore property)
   value)
 (cl-defmethod occ-obj-impl-list-p ((prop (eql _template_)))
   "Is the property _TEMPLATE_ has VALUES in list, Method tell
   property represent list or not."
   (ignore prop))
 (cl-defmethod  occ-obj-impl-get ((user occ-user-agent)
                                  (prop (eql _template_))
                                  (ctsk occ-obj-tsk))
   "Read value of list of elements if (occ-obj-list-p CTSK PROPERTY) else
element for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
   (ignore obj)
   (ignore prop))
 (cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
                                       (operation (eql _operation_))
                                       (prop (eql _template_))
                                       values)
   "Used by OCC-OBJ-IMPL-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE_ if CALLABLE (helm method) should be generated."
   (ignore obj)
   (ignore operation)
   (ignore prop)
   (ignore values)
   (occ-debug "occ-obj-impl-require-p3 is called"))
 (cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
                                     (prop (eql _template_))
                                     (operation (eql _operation_)))
   "Return a default VALUE of property _TEMPLATE_."
   (ignore obj)
   (ignore prop)
   (ignore operation))
 (cl-defmethod occ-obj-impl-operation ((obj occ-obj-tsk)
                                       (operation (eql _operation_))
                                       (prop (eql _template_))
                                       values)
   "Do the actual _OPERATION_."
   (ignore obj)
   (ignore operation)
   (ignore prop)
   (ignore values))
 (cl-defmethod occ-do-impl-operation ((obj occ-obj-tsk)
                                      (operation (eql _operation_))
                                      (prop (eql _template_))
                                      values)
   "Do the actual _OPERATION_."
   (ignore obj)
   (ignore operation)
   (ignore prop)
   (ignore values))
 (cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                     (prop (eql _template_)))
   "Checkout property _TEMPLATE_ in case of force clock-in."
   (ignore obj)
   (ignore prop)))





;; File Ends Here

;; [[file:occ-property-methods.org::*File Ends Here][File Ends Here:1]]
;;; occ-property-methods.el ends here
;; File Ends Here:1 ends here

