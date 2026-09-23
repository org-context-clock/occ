;;; occ-commands.el --- occ commands                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

;; occ-commands.el defines the interactive user commands of OCC.
;;
;; Implemented commands cover helm selection (`occ-run',
;; `occ-helm-match-select', `occ-helm-list-select',
;; `occ-helm-list-debug-select', `occ-helm-list-launch'), the property
;; editor `occ-property-edit', `occ-clock-out', quiet mode
;; (`occ-keep-quiet'/`occ-keep-quiet-for'), spec management
;; (`occ-obj-make-spec', `occ-add-to-spec') and `occ-insinuate'/
;; `occ-uninsinuate' setup.  Day-workflow commands such as `occ-start-day'
;; are stubs that only signal `occ-error' ("Implement it.").
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-commands)


(require 'occ)
(require 'occ-macros)
(require 'occ-util-common)
(require 'occ-main)
(require 'occ-cl-utils)
(require 'occ-obj-utils)
(require 'occ-resolve-clock)
(require 'occ-mode)


;; example of clos in cl-struct-js2-export-binding-node is a variable defined in ‘js2-mode.el’.

;;;###autoload
(defun occ-helm-match-select (ctx)
  "Select a matching task for context CTX using helm."
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-do-run-match-select ctx))
;;;###autoload
(defun occ-helm-list-select (ctx)
  "Select a task for context CTX from a helm list."
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-do-run-list-select ctx))
;;;###autoload
(defun occ-helm-list-debug-select (ctx)
  "Select a task for context CTX from a debug helm list."
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-do-run-list-debug-select ctx))
;;;###autoload
(defun occ-helm-list-launch (ctx)
  "Launch a task for context CTX from a helm list."
  (interactive
   (list (occ-obj-make-ctx-at-point)))
  (occ-do-run-list-launch ctx))

;;;###autoload
(defun occ-property-edit ()
  "Open the property editor for the context at point."
  (interactive)
  (let ((ctx (occ-obj-make-ctx-at-point)))
    (occ-do-properties-window-editor ctx
                                     :ap-normal '(t actions edit)
                                     :ap-transf '(t actions edit))))


;;;###autoload
(defun occ-curr-create-child ()
  "Create a child task under the current clocked task."
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-do-create-child ctxual-tsk)
      (occ-debug "No current task clocking-in"))))

;;;###autoload
(defun occ-curr-create-child-clock-in ()
  "Create and clock in a child task under the current task."
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-do-create-child-clock-in ctxual-tsk)
      (occ-debug "No current task clocking-in"))))
;;;###autoload
(defun occ-curr-tsk-continue-for (mins)
  "Stub: not yet implemented (signals occ-error)."
  (ignore mins)
  (occ-error "Implement it."))


;;;###autoload
(defun occ-start-day ()
  "Stub: not yet implemented (signals occ-error)."
  (interactive)
  ;; also detect if day is started.
  (occ-error "Implement it."))
;;;###autoload
(defun occ-show-up (mins)
  "Stub: not yet implemented (signals occ-error)."
  (interactive)
  (ignore mins)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (occ-error "Implement it."))
;;;###autoload
(defun occ-stop-day ()
  "Stub: not yet implemented (signals occ-error)."
  (interactive)
  (occ-error "Implement it."))
;;;###autoload
(defun occ-pack-up (mins)
  "Stub: not yet implemented (signals occ-error)."
  (interactive)
  (ignore mins)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (occ-error "Implement it."))


;; action
(cl-defmethod occ-do-log-note ()
  "Stub: not yet implemented (signals occ-error)."
  (occ-error "Implement it."))

(cl-defmethod occ-do-curr-tsk-log-note ()
  "Stub: not yet implemented (signals occ-error)."
  (occ-error "Implement it."))


;;;###autoload
(defun occ-do-clock-in-force ()
  "Stub: not yet implemented (signals occ-error)."
  (occ-error "Implement it, open context ctx if not present, then occ-do-clock-in-if-associable else show error."))
;;;###autoload
(defun occ-interrupt-clock-in (mins)
  "Stub: not yet implemented (signals occ-error)."
  (ignore mins)
  (occ-error "Implement it."))
;;;###autoload
(defun occ-clock-out (&optional switch-to-state
                                fail-quietly
                                at-time)
  "Clock out of the current task.
Honor optional SWITCH-TO-STATE, FAIL-QUIETLY and AT-TIME."
  (interactive)
  (occ-run-do-clock-out switch-to-state
                        fail-quietly
                        at-time))
;;;###autoload
(defun occ-continue-prev ()
  "Stub: not yet implemented (signals occ-error)."
  (occ-error "Implement it."))
;;;###autoload
(defun occ-obj-make-anonymous ()
  "Stub: not yet implemented (signals occ-error)."
  (occ-error "Implement it."))

;; TODO: direct prop edit/add/replace/remove etc from helm menu


;; implement console.

;; TODO: direct prop edit/add/replace/remove etc from helm menu


(defvar occ-keep-quiet-timer nil)

;;;###autoload
(defun occ-keep-quiet ()
  "Keep OCC quiet for the default number of minutes."
  (interactive)
  (occ-keep-quiet-for 7))

;;;###autoload
(defun occ-keep-quiet-for (mins)
  "Keep OCC quiet for MINS minutes."
  (interactive "Nmins: ")
  (when occ-keep-quiet-timer
    (cancel-timer occ-keep-quiet-timer)
    (setq occ-keep-quiet-timer nil))
  (setq occ-keep-quiet-timer
        (run-with-timer (* mins 60) nil
                        #'(lambda ()
                            (setq occ-keep-quiet-timer nil)
                            (occ-config-disable-quiet)
                            (occ-debug "OCC noise ahead %s." (occ-config-value-quiet)))))
  (occ-config-enable-quiet)
  (occ-debug "OCC Keeping quiet for %d mins" mins))


;;;###autoload
(defun occ-register-resolve-clock ()
  "Register the resolve-clock advice integration."
  (interactive)
  (occ-rl-register-resolve-clock))

;;;###autoload
(defun occ-unregister-resolve-clock ()
  "Unregister the resolve-clock advice integration."
  (interactive)
  (occ-rl-unregister-resolve-clock))

;;;###autoload
(defun occ-switch-default-key (key)
  "Set the default collector key to KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (occ-collector-default-key key))

;;;###autoload
(defun occ-reset-spec (key)
  "Reset the collection spec and object associated with KEY."
  (interactive  (list (occ-collector-read-key "key for spec: ")))
  (occ-reset-collection-object key)
  (occ-collector-remove key))

;;;###autoload
(defun occ-obj-make-spec (key desc)
  "Create and store a collection spec named KEY with description DESC."
  (interactive (list (occ-collector-read-key "key for spec: ")
                     (read-from-minibuffer "Desc: ")))
  (if (occ-collector-spec key)
      (occ-debug "spec: %s already present, first reset it with occ-reset-spec"
                 (occ-collector-spec key))
    (let ((spec (completing-read "Spec: " (occ-specs))))
      (when spec
        (occ-collector-get-create key desc (intern spec) nil)
        ;; (push (intern spec)
        ;;       (occ-collector-spec key))
        (occ-reset-collection-object key)))))

;;;###autoload
(defun occ-add-to-spec (key file)
  "Add FILE to the collection spec identified by KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")
                     (read-file-name "org file:")))
  ;; TODO: Improve to create direct tree from here rather than resetting whole occ-global-tsk-collection
  (unless (occ-collector-spec key)
    (occ-obj-make-spec key (symbol-name key)))
  (if (occ-collector-spec key)
      (unless (memq file
                    (cl-rest (occ-collector-spec key)))
        (let ((spec       (cl-first (occ-collector-spec key)))
              (spec-files (cl-rest (occ-collector-spec key))))
          (setq spec-files
                (if current-prefix-arg
                    (nconc (list file)
                           spec-files)
                  (nconc spec-files
                         (list file))))
          (setf (occ-collection-spec (occ-collector-get key)) (cons spec spec-files)))
        (prog1
            (occ-collector-spec key)
          (occ-reset-collection-object key)))))

;;;###autoload
(defun occ-add-org-file (key buffer)
  "Add org BUFFER contents to the collection spec identified by KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")
                     (current-buffer)))
  (occ-do-add-org-buffer key
                         buffer))

;;;###autoload
(defun occ-obj-build-spec (key)
  "Interactively build and store the collection spec for KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (occ-obj-make-spec key (symbol-name key))
  (when (cl-first (occ-collector-spec key))
    (occ-add-to-spec key (read-file-name "Spec file: ")))
  (prog1
      (occ-collector-spec key)
    (occ-reset-collection-object key)))


;; testing verification
;;;###autoload
(defun occ-files-with-null-regex ()
  "Report collection files lacking a valid org heading regexp."
  (interactive)
  (let ((files (cl-remove-if #'(lambda (f)
                                 (with-current-buffer (occ-find-file-noselect f)
                                   org-complex-heading-regexp))
                             (occ-obj-files))))
    (occ-debug "files with null regex %s" files)))

;; testing verification
;;;###autoload
(defun occ-files-not-in-org-mode ()
  "Report collection files that are not in org-mode."
  (interactive)
  (let ((files (cl-remove-if #'(lambda (f)
                                 (with-current-buffer (occ-find-file-noselect f)
                                   (eq major-mode 'org-mode)))
                             (occ-obj-files))))
    (occ-debug "files not in org-mode %s" files)))


;;;###autoload
(defun occ-merge-unamed-task ()
  "Stub: not yet implemented (signals occ-error)."
  (interactive)
  (occ-error "Implement it."))


;;;###autoload
(defun occ-show-priority-ineql (prop)
  "Display the priority inequalities declared for PROP."
  (interactive (list (occ-util-select-from-sym-list "Select property: "
                                                    (cons nil (occ-obj-properties-for-rank)))))
  (require 'calc)
  (occ-message "prop %s: %s"
               (or prop "ALL")
               (math-format-flat-expr `(vec ,@(if prop
                                                  (occ-obj-ineq-internal prop)
                                                (mapcar #'cadr
                                                        occ-property-priority-inequalities)))
                                      1)))

;;;###autoload
(defun occ-add-priority-ineql (prop)
  "Prompt for and add a priority inequality for PROP."
  (interactive (list (occ-util-select-from-sym-list "Select property: "
                                                    (occ-obj-properties-for-rank))))
  (require 'calc)
  (let ((prompt (format "prop %s: %s: "
                        prop
                        (math-format-flat-expr `(vec ,@(occ-obj-ineq-internal prop))
                                               1))))
    (when (occ-do-add-ineq prop
                           (read-from-minibuffer prompt))
      (occ-do-set-prop-priorities)
      (occ-show-priorities))))
;;;###autoload
(defun occ-show-priorities ()
  "Display the computed property priorities."
  (interactive)
  (message "%s" occ-property-priorities))


;;;###autoload
(defun occ-insinuate (&optional key)
  "Set up OCC and wire its hooks for the spec under KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (occ-debug "occ-insinuate: begin")
  (prog1
      (occ-initialize key)
    (occ-set-bindings)
    (occ-debug "occ-insinuate: finish")))

;;;###autoload
(defun occ-uninsinuate ()
  "Tear down OCC and remove its hooks."
  (interactive)
  (occ-debug "occ-uninsinuate: begin")
  (prog1
      (occ-uninitialize)
    (occ-debug "occ-uninsinuate: finish")))


;;;###autoload
(defun occ-run-timer ()
  "Start the current context timer."
  (interactive)
  (occ-run-curr-ctx-timer))

(defvar occ-reload t)
;;;###autoload
(defun occ-reload (&optional uncompiled)
  "Reload all OCC libraries, optionally as UNCOMPILED sources."
  (interactive "P")
  (when occ-reload
    (let ((occ-reload nil))
      (occ-reload-lib uncompiled))))
;;;###autoload
(defun occ-version (&optional here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg
                     t
                     (not current-prefix-arg)))
  (occ-debug (occ-get-version here
                              full
                              message)))

;;;###autoload
(defun occ-run ()
  "Run the OCC helm action panel."
  (interactive)
  (helm :prompt "Run Actions"
        :sources (list (helm-build-sync-source "Actions"
                         :candidates (list (cons "occ clock-in current context (force)"
                                                 #'(lambda () (occ-do-clock-in-curr-ctx t)))
                                           (cons "occ clock-in current context"
                                                 #'(lambda () (occ-do-clock-in-curr-ctx nil))))
                         :action     (list (cons "run"
                                                 #'(lambda (candidate-fun)
                                                     (funcall candidate-fun)))))
                       (occ-obj-obj (occ-helm-build-extra-actions-ctx-buffer-source)))))

;;; occ-commands.el ends here
