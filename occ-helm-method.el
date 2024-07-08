;;; occ-helm-method.el --- occ helm method           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

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

(provide 'occ-helm-method)


(eval-when-compile
  (require 'helm-source))
(require 'helm-files)
(require 'helm-lib)
(require 'helm-source)
(require 'occ-assert)
(require 'occ-obj-simple)
(require 'occ-obj-clock-method)
(require 'occ-obj-utils)
(require 'occ-obj-accessor)
(require 'occ-obj-ctor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-select)
(require 'occ-obj)
(require 'occ-obj-simple)
(require 'occ-cl-utils)
(require 'occ-filter-base)
(require 'occ-util-common)


;; (progn
;;   (with-helm-buffer
;;     (let* ((posn (elt event 1)) 
;;            (cursor (line-number-at-pos (point)))
;;            (pointer (line-number-at-pos (posn-point posn))))
;;       (helm--next-or-previous-line (if (> pointer cursor)
;;                                        'next
;;                                      'previous)
;;                                    (abs (- pointer cursor)))))
;;   (when t
;;     (helm-maybe-exit-minibuffer)))

(defun occ-helm-next-line (&optional ARG)
  (interactive "p")
  (with-helm-alive-p
    (helm--next-or-previous-line 'next arg)))
(defun occ-helm-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (with-helm-alive-p
    (let (helm-scroll-amount)
      (helm-move-selection-common :where 'page :direction 'next))))
(defun occ-helm-next-visible-mark (&optional prev)
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (ignore-errors
        (goto-char (helm-next-point-in-list
                    (point)
                    (sort (mapcar 'overlay-start helm-visible-mark-overlays) '<)
                    prev)))
      (helm-mark-current-line))))

(defun occ-helm-previous-line (&optional ARG))
(defun occ-helm-previous-page ())
(defun occ-helm-previous-visible-mark (&optional prev))


(defun occ-helm-maybe-exit-minibuffer ()
  ;; https://www.reddit.com/r/emacs/comments/376won/select_helm_candidate_by_using_mouse_click/
  ;; helm-execute-persistent-action
  (interactive)
  (with-helm-alive-p
    (let* ((source     (helm-get-current-source))
           (obj        (and source (helm-get-selection nil nil source)))
           (selectable (and obj    (occ-obj-tsk-selectable obj))))
      ;; (occ-message "Selection(%s): %s" (occ-obj-tsk-selectable obj) (occ-obj-format obj))
      (if selectable
          (helm-maybe-exit-minibuffer)
        (occ-message "ReadOnly %s" (occ-obj-format obj))))))

(defvar occ-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; (define-key map (kbd "RET")           'helm-ff-RET)
    (define-key map (kbd "C-]")           'helm-ff-run-toggle-basename)
    (define-key map (kbd "S-RET")         'occ-helm-run-child-clock-in)
    ;; (helm-define-key-with-subkeys map
    ;;   '((kbd "DEL") ?\d 'helm-ff-delete-char-backward
    ;;     (C-backspace . helm-ff-run-toggle-auto-update)
    ;;     ([C-c DEL] . helm-ff-run-toggle-auto-update)
    ;;     nil 'helm-ff-delete-char-backward--exit-fn))
    (when t ;; helm-ff-lynx-style-map
      (define-key map (kbd "RET")         'occ-helm-maybe-exit-minibuffer)
      (define-key map (kbd "<left>")      'helm-find-files-up-one-level)
      (define-key map (kbd "<right>")     'helm-execute-persistent-action))
    (delq nil map))
  "Keymap for `helm-find-files'.")

(defvar occ-helm-doc-header " (\\<occ-helm-map>\\[helm-find-files-up-one-level]: Go up one level)"
  "*The doc that is inserted in the Name header of a find-files or dired source.")

(defun occ-helm-run-child-clock-in ()
  "Run mail attach files command action from `helm-source-find-files'."
  ;; (interactive)                         ;TODO: to move to occ-commands.el
  (with-helm-alive-p
    (helm-exit-and-execute-action 'occ-child-clock-in)))
(put 'occ-helm-run-child-clock-in 'helm-only t)
;; add occ-child-clock-in in action


(defvar occ-helm-before-init-hook nil)
(defvar occ-helm-after-init-hook nil)
(defun occ-helm-cleanup ())
;; checkout (defclass helm-source-ffiles (helm-source-sync) - in helm-files.el
;; C-h C-f helm-source-ffiles
;; C-h C-f helm-source
(defclass occ-helm-source-sync (helm-source-sync)
  ((header-name
    :initform #'(lambda (name)
                  (concat name (substitute-command-keys
                                occ-helm-doc-header))))
   (init
    :initform #'(lambda ()
                  (setq helm-ff-auto-update-flag
                        helm-ff-auto-update-initial-value)
                  (setq helm-ff--auto-update-state
                        helm-ff-auto-update-flag)
                  (helm-set-local-variable 'bookmark-make-record-function
                                           #'helm-ff-make-bookmark-record)
                 (require 'helm-external)))
   (candidates :initform 'helm-find-files-get-candidates)
   (update :initform #'(lambda ()))
   (match-on-real :initform nil)
   (filtered-candidate-transformer
    :initform '(helm-ff-fct
                helm-ff-maybe-show-thumbnails
                ;; These next two have to be called after
                ;; `helm-ff-fct' as they use only cons cell candidates.
                helm-ff-directories-only
                helm-ff-files-only
                helm-ff-sort-candidates))
   (persistent-action-if :initform 'helm-find-files-persistent-action-if)
   (persistent-help :initform "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
   (help-message :initform 'helm-ff-help-message)
   (mode-line :initform (list "Occ" helm-mode-line-string))
   (volatile :initform t)
   (cleanup :initform 'occ-helm-cleanup)
   (migemo :initform t)
   (nohighlight :initform t)
   (keymap :initform 'occ-helm-map)
   ;; (candidate-number-limit :initform 'helm-ff-candidate-number-limit)
   ;; (action-transformer
   ;;  :initform 'helm-find-files-action-transformer)
   ;; (action :initform 'helm-find-files-actions)
   (before-init-hook :initform 'occ-helm-before-init-hook)
   (after-init-hook :initform 'occ-helm-after-init-hook)
   (group :initform 'occ-helm)))

;; (fmakunbound 'occ-helm-null-candidate)

(cl-defmethod occ-helm-null-candidate ((obj occ-ctx))
  (ignore obj)
  nil)

;;
;; https://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

;; function 'sacha/helm-org-create-tsk

;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))

(defun occ-helm-dummy-source (prompt
                              action)
  (helm-build-dummy-source prompt
    :action (helm-make-actions prompt
                               action)))

(cl-defmethod occ-obj-helm-build-obj-source ((obj occ-obj-ctx)
                                             (collections list)
                                             &optional
                                             actions)
  ;; BUG: occ-helm-build-candidates not defined anywhere
  (occ-helm-build-candidates :source (occ-obj-list-with obj
                                                        collections)
                             actions))


(cl-defmethod occ-obj-helm-fun-action-function-call-source ((prompt     string)
                                                            (candidates list))
  (helm-build-sync-source prompt
    :candidates candidates
    :action     (list (cons "Run action"
                            (occ-lambda-call-cand)))))

(cl-defmethod occ-obj-helm-build-dummy-source ((prompt string)
                                               (fun    compiled-function))
  (occ-helm-dummy-source prompt
                         fun))

(cl-defmethod occ-obj-helm-build-dummy-source ((prompt string)
                                               (fun    symbol))
  (let ((source (occ-helm-dummy-source prompt
                                       fun)))
    (occ-build-hsrc-source source
                           :rank 0
                           :level :optional)))


(fmakunbound 'occ-helm-build-collection-source-prompt)

(cl-defmethod occ-helm-build-collection-source-prompt ((obj        occ-ctx)
                                                       (collection occ-obj-collection)
                                                       obj-class-name
                                                       unfiltered-count
                                                       filtered-count
                                                       &key
                                                       prompt)
  (ignore obj)
  (let ((override        (and prompt
                              (consp prompt)
                              (eq :overrride
                                  (cl-first prompt))))
        (prompt          (when (consp prompt)
                           (if (consp (cl-rest prompt))
                               (nth 1 prompt)
                             (cl-rest prompt))))
        (collection-name (occ-obj-collection-name collection))
        (collection-desc (occ-obj-collection-desc collection)))
    (if (and override
             prompt)
        prompt
      (format "Select matching %s(%d/%d) [%s]%s"
              obj-class-name
              unfiltered-count
              filtered-count
              (or collection-desc
                  collection-name)
              (format " %s" (or prompt ""))))))


(cl-defmethod occ-obj-build-helm-map ((combined-dyn-filter occ-combined-dyn-filter))
  (let ((filter-manage-fn  #'(lambda ()
                              (interactive)
                              (with-helm-buffer
                                (progn ;; code to manager filters
                                  ;; TODO: check https://github.com/emacsmirror/edit-list/blob/master/edit-list.el
                                  ;; TODO: implement list editor
                                  ;; TODO: search emacs elisp interactively modify list
                                  (occ-debug "Manage filters here.")
                                  ;; (setf filters default-filters)
                                  (read-from-minibuffer "Test: ")))
                              ;; (funcall gen-candidates)
                              (helm-refresh))))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map occ-helm-map)
      ;; (define-key map (kbd "M-<up>")     filter-manage-fn)
      (define-key map (kbd "M-<up>")     (occ-obj-dyn-filter-increment-closure-fn combined-dyn-filter))
      (define-key map (kbd "M-<down>")   (occ-obj-dyn-filter-decrement-closure-fn combined-dyn-filter))
      (define-key map (kbd "M-<return>") (occ-obj-dyn-filter-reset-closure-fn     combined-dyn-filter))
      (define-key map (kbd "s-<up>")     (occ-obj-dyn-filter-next-closure-fn      combined-dyn-filter))
      (define-key map (kbd "s-<down>")   (occ-obj-dyn-filter-prev-closure-fn      combined-dyn-filter))
      (define-key map (kbd "s-z")        filter-manage-fn)
      map)))

(cl-defmethod occ-obj-helm-build-real-collection-source ((obj        occ-ctx)
                                                         (collection occ-obj-collection) ;; (nth 0 (occ-collections-default))
                                                         (combined-dyn-filter occ-combined-dyn-filter)
                                                         &key
                                                         filters
                                                         builder
                                                         ap-normal
                                                         ap-transf
                                                         timeout
                                                         prompt)
  (let* ((timeout               (or timeout occ-idle-timeout)))
    ;; (candidates-unfiltered (occ-obj-list-with obj collection :builder builder)) ;; (occ-collections-default) -- occ-obj-list-with is in occ-obj-accessor.el
    ;; TODO: make a separate function for it.
    (ignore timeout)
    (let* ((default-filters filters)
           (filters         filters)
           (candidates-unfiltered (occ-obj-dyn-filter-seq    combined-dyn-filter)) ;; (occ-collections-default) -- occ-obj-list-with is in occ-obj-accessor.el
           (candidates-filtered   (occ-obj-dyn-filter-display-filter combined-dyn-filter))
           (unfiltered-count      (length candidates-unfiltered))
           (filtered-count        (length candidates-filtered)))
      (occ-debug "occ-obj-helm-build-real-collection-source: unfiltered-count = %d, filtered-count = %d" unfiltered-count filtered-count)
      (when (> filtered-count 0) ;; (> unfiltered-count 0)
        (let ((source-name          (occ-helm-build-collection-source-prompt obj
                                                                             collection
                                                                             (symbol-name (occ-cl-inst-classname (cl-first candidates-unfiltered)))
                                                                             unfiltered-count
                                                                             filtered-count
                                                                             :prompt prompt))
              (gen-candidate-lambda (occ-obj-dyn-filter-display-filter-closure-fn combined-dyn-filter))
              (h-map            (occ-obj-build-helm-map combined-dyn-filter)))
              ;; (helm-get-current-source)
          (let ((helm-actions (occ-obj-ap-helm-item ap-normal obj))
                (helm-transfm (occ-obj-ap-helm-item ap-transf obj)))
            ;; * Dynamic Match based templates
            ;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/
            (helm-make-source source-name 'occ-helm-source-sync ;; 'helm-source-sync
              :candidates                     gen-candidate-lambda
              ;; :header-name
              :keymap                         h-map
              :action                         helm-actions
              :persistent-action              helm-actions
              :persistent-help                "I don't want this line here"
              :action-transformer             helm-transfm
              :filtered-candidate-transformer nil ;; (lambda (candidates source) candidates)
              :history                        'org-refile-history)))))))

(cl-defmethod occ-obj-helmify ((combined-dyn-filter occ-combined-dyn-filter))
  (cl-flet ((occ-obj-build-helm-command-closure-fn (closure-fn)
                                                   #'(lambda ()
                                                       (interactive)
                                                       (funcall closure-fn)
                                                       (helm-refresh)))
            (occ-obj-build-helm-candidate-closure-fn (closure-fn)
                                                     #'(lambda ()
                                                         (mapcar #'occ-obj-candidate
                                                                 (funcall closure-fn)))))
    (occ-obj-build-combined-dyn-filter (occ-obj-name combined-dyn-filter)
                                       :curr-closure-fn      (occ-combined-dyn-filter-curr-closure-fn combined-dyn-filter)
                                       :prev-closure-fn      (occ-obj-build-helm-command-closure-fn (occ-combined-dyn-filter-prev-closure-fn combined-dyn-filter))
                                       :next-closure-fn      (occ-obj-build-helm-command-closure-fn (occ-combined-dyn-filter-next-closure-fn combined-dyn-filter))
                                       :seq-closure-fn       (occ-combined-dyn-filter-seq-closure-fn  combined-dyn-filter)
                                       :display-filter-closure-fn (occ-obj-build-helm-candidate-closure-fn (occ-combined-dyn-filter-display-filter-closure-fn combined-dyn-filter))
                                       :selectable-filter-closure-fn (occ-obj-build-helm-candidate-closure-fn (occ-combined-dyn-filter-selectable-filter-closure-fn combined-dyn-filter))
                                       :increment-closure-fn (occ-obj-build-helm-command-closure-fn (occ-combined-dyn-filter-increment-closure-fn combined-dyn-filter))
                                       :decrement-closure-fn (occ-obj-build-helm-command-closure-fn (occ-combined-dyn-filter-decrement-closure-fn combined-dyn-filter))
                                       :reset-closure-fn     (occ-obj-build-helm-command-closure-fn (occ-combined-dyn-filter-reset-closure-fn combined-dyn-filter)))))

;; * Dynamic Match based templates
;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

(cl-defmethod occ-obj-helm-build-collection-source ((obj        occ-ctx)
                                                    (collection occ-obj-collection) ;; (nth 0 (occ-collections-default))
                                                    &key
                                                    filters
                                                    builder
                                                    ap-normal
                                                    ap-transf
                                                    auto-select-if-only
                                                    timeout
                                                    prompt)
  "Generate helm-source for COLLECTION.
It first find unfiltered and filtered candidates for passed
COLLECTION, if only one filtered candidate present in passed
COLLECTION then it return that candidate via occ-hsrc-candidate,
if here is more than one filtered candidates then it make a
helm-source and returned as occ-hsrc-source which will be used to
select candidate from it."

  ;; (occ-assert candidates)
  (let* ((rank  (occ-obj-collection-rank collection))
         (level (occ-obj-collection-level collection))
         (timeout               (or timeout occ-idle-timeout))
         (combined-dyn-filter   (occ-obj-combined-dyn-filter obj
                                                             filters
                                                             (occ-obj-list-with obj collection :builder builder)))
         (candidates-unfiltered (occ-obj-dyn-filter-seq    combined-dyn-filter)) ;; (occ-collections-default) -- occ-obj-list-with is in occ-obj-accessor.el
         (candidates-filtered   (occ-obj-dyn-filter-selectable-filter combined-dyn-filter))
         (unfiltered-count      (length candidates-unfiltered))
         (filtered-count        (length candidates-filtered)))

    (occ-debug "occ-obj-helm-build-collection-source: combined-dyn-filter: done")
    (occ-debug "len candidates-unfiltered %d" unfiltered-count)
    (occ-debug "len candidates-filtered %d" filtered-count)

    (if (= filtered-count 0)
        (occ-build-hsrc-null nil ;; (cl-first candidates-filtered)
                             :rank rank
                             :level level)
      (if (and auto-select-if-only
               (= filtered-count 1))
          (occ-build-hsrc-candidate (cl-first candidates-filtered)
                                    :rank rank
                                    :level level)
        (let ((source (occ-obj-helm-build-real-collection-source obj
                                                                 collection ;; (nth 0 (occ-collections-default))
                                                                 (occ-obj-helmify combined-dyn-filter)
                                                                 :filters filters
                                                                 :builder builder
                                                                 :ap-normal ap-normal
                                                                 :ap-transf ap-transf
                                                                 :timeout timeout
                                                                 :prompt prompt)))
          (occ-build-hsrc-source source
                                 :rank rank
                                 :level level))))))


(defun occ-helm-build-dummy-sources ()
  (list (occ-obj-helm-build-dummy-source "Create fast child task may use template" #'occ-do-fast-create-child)
        (occ-obj-helm-build-dummy-source "Create Anonymous task"                   #'occ-do-create-anonymous-child)))
        ;; (occ-obj-helm-build-dummy-source "Create Anonymous (fast as unnamed)"      #'occ-do-fast-create-anonymous-child)

(defun occ-helm-build-extra-actions-tsk-source ()
  (unless (member (buffer-name (current-buffer))
                  occ-ignore-buffer-names)
    (let ((source (occ-obj-helm-fun-action-function-call-source "Other Task Actions"
                                                                (list (cons (format "Create Anonymous Unnamed task fast in collection.")
                                                                            #'occ-do-fast-create-anonymous-child-in-collection)
                                                                      (cons (format "Create Anonymous Unnamed task fast.")
                                                                            #'occ-do-fast-create-anonymous-child)))))
      (list (occ-build-hsrc-source source
                                   :rank 0
                                   :level :optional)))))

(defun occ-helm-build-extra-actions-ctx-buffer-source ()
  (unless (member (buffer-name (current-buffer))
                  occ-ignore-buffer-names)
    (let ((source (occ-obj-helm-fun-action-function-call-source "Other Actions"
                                                                (list (cons (format "Add current buffer %s to ignore list" (current-buffer))
                                                                            #'(lambda ()
                                                                                (let ((buff (current-buffer)))
                                                                                  (cl-pushnew (buffer-name buff)
                                                                                              occ-ignore-buffer-names)
                                                                                  (occ-helm-null-candidate (occ-obj-make-ctx buff)))))))))
      (list (occ-build-hsrc-source source
                                   :rank 0
                                   :level :optional)))))


(cl-defmethod occ-obj-helm-build-collections-sources ((obj         occ-ctx)
                                                      (collections list) ;; (occ-collections-default)
                                                      &key
                                                      filters
                                                      builder
                                                      ap-normal
                                                      ap-transf
                                                      auto-select-if-only
                                                      timeout
                                                      prompt)
  ;; (occ-debug "occ-obj-helm-build-collections-sources: ap-normal: %s" ap-normal)

  ;; BUG: return list of occ-hsrc-candidates/occ-hsrc-sources/NIL also
  ;; TODO need to handle NIL

  (mapcar (lambda (collection)
            (occ-obj-helm-build-collection-source obj
                                                  collection ;; (nth 0 (occ-collections-default))
                                                  :filters          filters
                                                  :builder          builder
                                                  :ap-normal        ap-normal
                                                  :ap-transf        ap-transf
                                                  :auto-select-if-only auto-select-if-only
                                                  :timeout          timeout
                                                  :prompt           prompt))
          collections))

(cl-defmethod occ-obj-helm-build-sources ((obj         occ-ctx)
                                          (collections list) ;; (occ-collections-default)
                                          &key
                                          filters
                                          builder
                                          ap-normal
                                          ap-transf
                                          auto-select-if-only
                                          timeout
                                          prompt)
  (ignore timeout)
  (ignore filters)
  (ignore builder)
  (ignore auto-select-if-only)
  (ignore prompt)
  ;; (occ-debug "occ-obj-helm-build-collections-sources: ap-normal: %s" ap-normal)
  (let ((collection-sources (occ-obj-helm-build-collections-sources obj
                                                                    collections ;; (occ-collections-default)
                                                                    :filters          filters
                                                                    :builder          builder
                                                                    :ap-normal        ap-normal
                                                                    :ap-transf        ap-transf
                                                                    :auto-select-if-only auto-select-if-only
                                                                    :prompt           prompt)))
    (append collection-sources
            (occ-helm-build-dummy-sources)
            (occ-helm-build-extra-actions-tsk-source)
            (occ-helm-build-extra-actions-ctx-buffer-source))))



(cl-defmethod occ-obj-get-first-helm-actions-for-obj ((obj occ-obj)
                                                      (apn occ-ap-normal)
                                                      (apt null))
  (let ((act (cl-first (occ-obj-ap-helm-get-actions obj
                                                    apn
                                                    apt))))
    (cl-rest act)))

(cl-defmethod occ-obj-get-first-helm-actions-for-obj ((obj occ-obj)
                                                      (apn occ-ap-normal)
                                                      (apt occ-ap-transf))
  (let ((act (cl-first (occ-obj-ap-helm-get-actions obj
                                                 apn
                                                 apt))))
    (cl-rest act)))


(cl-defmethod occ-obj-helm-act-on-candidate ((obj    occ-ctx)
                                             (source occ-hsrc-candidate)
                                             &key
                                             filters
                                             builder
                                             ap-normal
                                             ap-transf
                                             auto-select-if-only
                                             timeout
                                             prompt)
  "OBJ ignored"
  (ignore filters)
  (ignore builder)
  (ignore auto-select-if-only)
  (ignore timeout)
  (ignore prompt)
  (let* ((helm-action (occ-obj-get-first-helm-actions-for-obj obj
                                                              ap-normal
                                                              ap-transf)))
    (if (occ-obj-obj source)
        (funcall helm-action (occ-obj-obj source))
      (occ-warn "occ-obj-helm-act-on-candidate: wrong source"))))

(cl-defmethod occ-candidate-main-p ((source occ-hsrc))
  (and (> (occ-obj-rank source) 10)
       (not (eq (occ-obj-level source) :optional))
       ;; (not (occ-hsrc-null-p source))
       (occ-hsrc-candidate-p source)))

(cl-defmethod occ-source-main-p ((source occ-hsrc))
  (and (> (occ-obj-rank source) 10)
       (not (eq (occ-obj-level source) :optional))
       ;; (not (occ-hsrc-null-p source))
       (occ-hsrc-source-p source)))

(cl-defmethod occ-candidate-compare ((s1 occ-hsrc-candidate)
                                     (s2 occ-hsrc-candidate))
  ;; prefer non-optional level
  (let ((s1-level (if (eq (occ-obj-level s1) :optional) 0 1))
        (s2-level (if (eq (occ-obj-level s2) :optional) 0 1)))
      (and (> (occ-obj-rank s1) (occ-obj-rank s2))
           (> s1-level s2-level))))

(cl-defmethod occ-obj-helm-act-on-multiple ((obj         occ-ctx)
                                            (collections list) ;; (occ-collections-default)
                                            &key
                                            filters
                                            builder
                                            ap-normal
                                            ap-transf
                                            auto-select-if-only
                                            timeout
                                            prompt)
  (ignore timeout)
  (let ((cand-sources (occ-obj-helm-build-sources obj
                                                  collections ;; (occ-collections-default)
                                                  :filters          filters
                                                  :builder          builder
                                                  :ap-normal        ap-normal
                                                  :ap-transf        ap-transf
                                                  :auto-select-if-only auto-select-if-only
                                                  :prompt           prompt)))
    ;; TODO: here decide what to do with cand-sources all has rank and level
    (let* ((candidates          (cl-remove-if-not #'occ-candidate-main-p
                                                  cand-sources))
           (preferred-candidate (cl-first (sort candidates
                                                #'occ-candidate-compare))))
      (occ-debug "candidates: %s" candidates)
      (occ-debug "preferred-candidate: %s" preferred-candidate)
      (if preferred-candidate
          ;; Mean if first cand-sources has only one element then it will pack
          ;; that element using `occ-build-hsrc-source' to be acted by default
          ;; action.
          (occ-obj-helm-act-on-candidate obj
                                         preferred-candidate
                                         :filters          filters
                                         :builder          builder
                                         :ap-normal        ap-normal
                                         :ap-transf        ap-transf
                                         :auto-select-if-only auto-select-if-only
                                         :prompt           prompt)
        ;; (occ-assert (cl-first cand-sources)) -- can happen when no contxtual match found
        ;; Else all source will be passed to helm to be shown.
        (let ((helm-sources (cl-remove-if-not #'occ-hsrc-source-p
                                              cand-sources)))
          (occ-debug "len helm-sources %d" (length helm-sources))
          (occ-debug "some helm-sources %s" (cl-some #'occ-source-main-p
                                                     helm-sources))
          (when (cl-some #'occ-source-main-p
                         helm-sources)
            (occ-debug "len helm-sources %d" (length helm-sources))
            (let* ((in-occ-helm t)
                   (timer (run-with-timer 0.08 nil #'(lambda ()
                                                       (if in-occ-helm
                                                           (helm-refresh)
                                                         (occ-debug "Running occ-list-select-internal helm is gone"))))))
              (unwind-protect ; (error "No buffer named *helm: occ select*") ; (error "Selecting deleted buffer")
                  (when (occ-obj-obj (cl-first helm-sources))
                    (occ-mac-condition-case-control err
                        (helm :sources (mapcar #'occ-obj-obj helm-sources)
                              :buffer  (occ-obj-helm-select-buffer)
                              :resume  'noresume)
                      ((quit error)
                       (when (string= (format "No buffer named %s" (occ-obj-helm-select-buffer))
                                      (cadr err))
                         (occ-message "aborting OCC-OBJ-HELM-ACT-ON-MULTIPLE")
                         ;; (abort-recursive-edit)
                         (keyboard-quit))
                       (occ-message "Enable Disable occ with occ-mode %S." err)
                       nil)))
                (progn
                  (setq in-occ-helm nil)
                  (cancel-timer timer))))))))))

(cl-defmethod occ-obj-helm-act ((obj         occ-ctx)
                                (collections list) ;; (occ-collections-default)
                                &key
                                filters
                                builder
                                ap-normal
                                ap-transf
                                return-transform
                                auto-select-if-only
                                timeout
                                prompt)
  (when collections
    ;; (occ-debug "occ-obj-helm-act1: ap-normal: %s" ap-normal)
    ;; (occ-debug "occ-obj-helm-act1: ap-transf: %s" ap-transf)
    (let* ((ap-normal (occ-obj-build-ap-normal ap-normal))
           (ap-transf (occ-obj-build-ap-transf ap-transf
                                               (occ-obj-ap-base ap-normal))))
      ;; (occ-debug "occ-obj-helm-act2: ap-normal: %s" ap-normal)
      ;; (occ-debug "occ-obj-helm-act2: ap-transf: %s" ap-transf)
      (let* ((ap-normal (if return-transform (occ-obj-return-tranform ap-normal obj) ap-normal)) ;as return value is going to be used.
             (ap-transf (if return-transform (occ-obj-return-tranform ap-transf obj) ap-transf)))
        (occ-obj-helm-act-on-multiple obj
                                      collections
                                      :filters             filters
                                      :builder             builder
                                      :ap-normal           ap-normal
                                      :ap-transf           ap-transf
                                      :auto-select-if-only auto-select-if-only
                                      :timeout             timeout
                                      :prompt              prompt)))))

;;; occ-helm-method.el ends here
