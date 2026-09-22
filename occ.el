;;; occ.el --- occ               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; https://economictimes.indiatimes.com/small-biz/hr-leadership/leadership/getting-more-done-why-task-management-is-the-key-to-managing-time/articleshow/70814450.cms

;;; Code:

(provide 'occ)


(require 'switch-buffer-functions)
(require 'package)


(require 'occ-obj-ctor)
(require 'occ-main)
(require 'occ-util-common)
(require 'occ-filter-config)
(require 'occ-resolve-clock)
(require 'occ-test)
(require 'occ-config)
(require 'occ-mode)
(require 'occ-unnamed)


(defvar *occ-collector* nil)
(defvar *occ-collector-default-key* 'default)

(defun occ-collector-default-key (&optional key)
  (if key
      (setq *occ-collector-default-key* key)
    *occ-collector-default-key*))
(defun occ-collector-read-key (&optional prompt keys)
  (occ-util-select-from-sym-list (or prompt "key for spec: ")
                                 (or keys (occ-collector-keys))))
(defun occ-collector-get (key)
  (alist-get key *occ-collector*))
(cl-defun occ-collector-get-create (key desc spec files &key depth limit rank level)
  (let ((depth (or depth 0))
        (limit (or limit 0))
        (rank  (or rank 0))
        (level (or level :optional)))
   (unless (alist-get key *occ-collector*)
     (setf (alist-get key *occ-collector*) (occ-obj-build-collection desc
                                                                     key
                                                                     spec
                                                                     files
                                                                     depth
                                                                     limit
                                                                     rank
                                                                     level))))
  (alist-get key *occ-collector*))
(defun occ-collector-remove (key)
  (setq *occ-collector* (assoc-delete-all key *occ-collector*)))
(defun occ-collector-set (key value)
  (setcdr (alist-get key *occ-collector*) value))
(defun occ-collector-spec (key)
  (let ((collection (occ-collector-get key)))
    (when collection
      (occ-obj-collection-spec collection))))
(defun occ-collector-roots (key)
  (let ((collection (occ-collector-get key)))
    (when collection
      (occ-obj-collection-roots collection))))
(defun occ-collector-files (key)
  (let ((collection (occ-collector-get key)))
    (when collection
      (occ-obj-collection-files collection))))
(defun occ-collector-keys ()
  (let* ((all-list                 (mapcar #'cl-first
                                           *occ-collector*))
         (default-list             (list *occ-collector-default-key*))
         (all-without-unnamed-list (remove *occ-collector-unnamed-key*
                                           all-list))
         (unnamed-list             (when (memq *occ-collector-unnamed-key*
                                               all-list)
                                     (list *occ-collector-unnamed-key*))))
    (cl-remove-duplicates (append default-list
                                  all-without-unnamed-list
                                  unnamed-list)
                          :from-end t)))



;; moved to occ-obj-accessor.el

;; (cl-defmethod occ-obj-collection ((obj symbol))
;;   (let ((key obj))
;;     (occ-collector-get key)))

;; (cl-defmethod occ-obj-collection ((obj occ-obj-collection))
;;   obj)

(defun occ-collections (&rest keys)
  (remove nil
          (mapcar #'occ-obj-collection keys)))

(defun occ-collections-default ()
  (occ-collections (occ-collector-default-key)
                   *occ-collector-unnamed-key*))

(defun occ-collections-all ()
  (apply #'occ-collections
         (occ-collector-keys)))

;; (defun occ-switch-buffer-run-curr-ctx-timer-function (prev next)
;;   (occ-run-curr-ctx-timer))

;;;###autoload
(defun occ-add-after-save-hook-fun-in-org-mode ()
  (add-hook 'after-save-hook
            'occ-after-save-hook-fun t t))


(defun occ-reset-collection-spec (&optional key)
  "Reset spec for collection associated with KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (occ-debug "resetting deafult-tsk-collection")
  (let ((key (or key
                 (occ-collector-default-key))))
    (occ-reset-collection-object key)))

(defun occ-reset-collection-roots (key)
  "Reset roots for collection associated with KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (setf (occ-collection-roots (occ-collector-get key)) nil))

(defun occ-reset-collection-tsks (key)
  "Reset tasks for collection associated with KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (occ-do-reset-tsks (occ-collector-get key)))


(defun occ-collect-tsks (key)
  "Populate tasks for collection associated with KEY."
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (occ-obj-collect-tsks (occ-collector-get key)))


;;;###autoload
(defun occ-reset-collection-object (key)
  (interactive (list (occ-collector-read-key "key for spec: ")))
  (occ-reset-collection-tsks key))


;;;###autoload
(cl-defun occ-set-collection-spec (key desc spec files &key depth limit rank level)
  (let ((depth (or depth 0))
        (limit (or limit 0))
        (rank  (or rank 0))
        (level (or level :optional)))
    (occ-collector-get-create key
                              desc
                              spec
                              files
                              :depth depth
                              :limit limit
                              :rank  rank
                              :level level)))

;;;###autoload
(cl-defun occ-set-deafult-collection-spec (spec files &key depth limit rank level)
  (let ((depth (or depth 0))
        (limit (or limit 0))
        (rank  (or rank 0))
        (level (or level :optional)))
    (occ-set-collection-spec (occ-collector-default-key)
                             "Default"
                             spec
                             files
                             :depth depth
                             :limit limit
                             :rank  rank
                             :level level)))
;;;###autoload
(cl-defun occ-set-primary-deafult-collection-spec (spec files &key depth limit rank level)
  (let ((depth (or depth 0))
        (limit (or limit 0))
        (rank  (or rank 20))
        (level (or level :primary)))
    (occ-set-collection-spec (occ-collector-default-key)
                             "Default"
                             spec
                             files
                             :depth depth
                             :limit limit
                             :rank  rank
                             :level level)))

(defun occ-reset-deafult-collection-object ()
  (occ-debug "resetting deafult-tsk-collection")
  (occ-reset-collection-object (occ-collector-default-key)))


(defun occ-do-priority-initialize ()
  (interactive)
  ;; (occ-obj-properties-for-rank)
  ;; (current-clock key status timebeing root currfile)
  (occ-do-add-ineq 'root "nil > (key + 20)")
  (occ-do-add-ineq 'key "nil > 10")
  (occ-do-add-ineq 'status  "nil > 2 * root")
  (occ-do-add-ineq 'currfile  "nil > status")
  (occ-do-add-ineq 'timebeing  "nil > currfile")
  (occ-do-add-ineq 'current-clock  "nil > timebeing")
  (occ-do-add-ineq 'git-branch  "this > root")
  (occ-do-set-prop-priorities))


(defun occ-initialize-hooks (key)
  (let ((spec (occ-collector-spec key)))
    (when (and spec
               (occ-valid-spec-p spec))
      ;; (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer t)
      ;; (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer t)
      ;; (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer t)
      ;; (debug)
      (unless (memq 'switch-buffer-functions-run post-command-hook)
        (add-hook 'post-command-hook
                  'switch-buffer-functions-run))
      (add-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
      (add-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode)
      (add-hook 'org-mode-hook           #'occ-add-org-file-timer))))

(defun occ-uninitialize-hooks ()
  (remove-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
  (remove-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode)
  (remove-hook 'org-mode-hook           #'occ-add-org-file-timer))

;;;###autoload
(defun occ-initialize (key)
  "occ-initialize"
  (setq *occ-tsk-previous-ctx* (occ-obj-make-ctx-at-point))
  (progn
    (occ-filter-config-initialize)
    (occ-helm-actions-config-initialize)
    (occ-unnamed-initialize)
    (occ-enable-mode-map)
    (occ-register-resolve-clock)
    (occ-cancel-timer)
    (occ-reset-collection-object key)
    (occ-ctx-clrhash)
    (occ-initialize-hooks key))
  (when nil
    (dolist (prop (occ-obj-properties-for-ranking nil))
      (let ((propstr (upcase (occ-obj-org-property-name prop))))
        ;; CHECK: is it required, as inheritance inow maintained by occ-rank.el by (occ-obj-rank-inheritable) functions
        (unless (member propstr org-use-property-inheritance)
          (cl-pushnew propstr org-use-property-inheritance)))))
  (let ((spec (occ-collector-spec key)))
    (occ-debug "init Test %s" key)
    (unless spec
      (if (occ-valid-spec-p spec)
          (progn
            (occ-collector-get-create key "Test" spec (list (read-file-name "org file for occ: "
                                                                            "~/Documents"
                                                                            "~/Documents/tasks.org"
                                                                            t
                                                                            nil
                                                                            #'(lambda (f)
                                                                                (string-match "*.org/" f)))))
            (setq occ-mode t)
            (occ-initialize-hooks key))
        (if (called-interactively-p 'interactive) ;; (called-interactively-p 'interactive)
            (progn
              (occ-debug "init Test2")
              (occ-obj-build-spec key)
              (setq occ-mode t))
          (occ-error "Not able to start occ")
          nil))))

  (occ-do-priority-initialize)
  ;; newly added
  ;; (org-clock-load) ;; is getting struck
  (run-with-idle-timer 3
                       nil
                       #'org-clock-load))

;;;###autoload
(defun occ-uninitialize ()
  "occ-uninitialize"
  (progn
    (occ-disable-mode-map)
    (occ-unregister-resolve-clock)
    (occ-cancel-timer)
    (occ-reset-collection-object (occ-collector-default-key))
    (occ-ctx-clrhash)
    (occ-uninitialize-hooks))
  (when nil
    (dolist (prop (occ-obj-properties-for-ranking nil))
      (let ((propstr (upcase (occ-obj-org-property-name prop))))
        ;; CHECK: is it required, as inheritance inow maintained by occ-rank.el by (occ-obj-rank-inheritable) functions
        (unless (member propstr org-use-property-inheritance)
          (delete propstr org-use-property-inheritance)))))
  (setq occ-mode nil))

(defun occ-status ()
  (interactive)
  (occ-message "Occ mode is %s and switch-buffer-functions is %s"
               (if occ-mode "on" "off")
               (if (memq 'switch-buffer-functions-run
                         (default-value 'post-command-hook))
                   "working"
                 "not working")))


(defun occ-find-library-dir (library)
  (unless occ-dev-dir
    (occ-set-dev-dir))
  (unless occ-dev-dir
    (occ-error "occ-dev-dir is NIL"))
  (progn
     (cl-delete (expand-file-name occ-dev-dir library) load-path)
     (let ((libpath (expand-file-name (concat library ".el")
                                      occ-dev-dir)))
       (if (file-exists-p libpath)
           (prog1
               occ-dev-dir
             (cl-pushnew occ-dev-dir load-path))
         (file-name-directory (or (locate-library library)
                                  ""))))))

(defun occ-get-version (here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (ignore message)
  (let ((occ-dir            (ignore-errors (occ-find-library-dir "occ")))
        (save-load-suffixes (when (boundp 'load-suffixes) load-suffixes))
        (load-suffixes      (list ".el"))
        (occ-install-dir    (ignore-errors (occ-find-library-dir "occ-loaddefs"))))
    (unless (and (fboundp 'occ-release)
                 (fboundp 'occ-git-version))
      (org-load-noerror-mustsuffix (concat occ-dir "occ-version")))
    (let* ((load-suffixes save-load-suffixes)
           (release       (occ-release))
           (git-version   (occ-git-version))
           (version (format "Occ mode version %s (%s @ %s)"
                            release
                            git-version
                            (if occ-install-dir
                                (if (string= occ-dir occ-install-dir)
                                    occ-install-dir
                                  (concat "mixed installation! "
                                          occ-install-dir
                                          " and "
                                          occ-dir))
                              "occ-loaddefs.el can not be found!")))
           (version1 (if full version release)))
      (when here (insert version1))
      ;; (when message (occ-debug "%s" version1))
      version1)))

;;;###autoload
(defun occ-set-dev-dir (&optional dirpath)
  (interactive
   (list (read-directory-name "occ src dir: " nil nil t)))
  (let ((dirpath (or dirpath
                     (read-directory-name "occ src dir: " nil nil t))))
   (setq occ-dev-dir dirpath)))

;;;###autoload
(defun occ-add-deps-libs (pkg)
  (let ((deps (cons (symbol-name pkg)
                    (mapcar #'(lambda (x)
                                (symbol-name (cl-first x)))
                            (package-desc-reqs (nth 1
                                                    (assoc 'occ
                                                           package-alist)))))))
    (if occ-dev-dir
        (dolist (lib deps)
          (cl-delete (concat occ-dev-dir lib) load-path)
          (cl-pushnew (concat occ-dev-dir   lib) load-path))
      (occ-error "occ-dev-dir not defined"))))

;;;###autoload
(defun occ-load-pkg (pkg-str)
  ;; TODO: load all files in lib dir
  (let ((pkg-dir (occ-find-library-dir pkg-str)))
    (dolist (ef (directory-files pkg-dir nil ".el$"))
      (let ((efile (expand-file-name ef pkg-dir)))
        (unless (string-match "pkg.el$" efile)
          (occ-debug "trying to load %s %s %s"
                       pkg-str
                       ef
                       efile)
          (or (and (occ-load-noerror-mustsuffix efile)
                   't)
              pkg-str))))))

;;;###autoload
(defun occ-reload-lib (uncompiled)
  "Reload all Occ Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (require 'loadhist)
  (let* ((pkg            'occ)
         (occ-dir        (occ-find-library-dir (symbol-name pkg)))
         ;; (contrib-dir (or (occ-find-library-dir "org-contribdir") occ-dir))
         ;; (feature-re "^\\(org\\|ob\\|ox\\)\\(-.*\\)?")
         (occ-deps       (cons (symbol-name pkg)
                               (mapcar #'(lambda (x) (symbol-name (cl-first x)))
                                       (package-desc-reqs (nth 1 (assoc 'occ
                                                                        package-alist))))))
         (occ-pkg-regexp (regexp-opt occ-deps))
         (feature-re     (concat "^" occ-pkg-regexp "$"))
         (remove-re      (format "\\`%s\\'"
                                 (regexp-opt '("dash" "org" "org-loaddefs" "occ-version" "helm"))))
         (feats          (delete-dups (mapcar #'file-name-sans-extension
                                              (mapcar 'file-name-nondirectory
                                                      (delq nil (mapcar 'feature-file features))))))
         (lfeat          (append (sort (setq feats (delq nil (mapcar #'(lambda (f)
                                                                         (if (and (string-match feature-re f)
                                                                                  (not (string-match remove-re f)))
                                                                             (progn
                                                                               (occ-debug "%s matched." f)
                                                                               f)
                                                                           nil))
                                                                     feats)))
                                       'string-lessp)
                          (list "occ-version" "occ")))
         (load-suffixes  (when (boundp 'load-suffixes) load-suffixes))
         (load-suffixes  (if uncompiled (reverse load-suffixes) load-suffixes))
         (load-uncore    nil)
         (load-misses    nil))
    (ignore occ-dir)
    (unless occ-dev-dir
      (occ-set-dev-dir))
    (unless occ-dev-dir
      (occ-error "occ-dev-dir is NIL"))
    (occ-add-deps-libs pkg)
    (occ-debug "working on %s" lfeat)
    (let ((load-missed-1 (mapcar #'occ-load-pkg
                                 lfeat)))
     (setq load-misses (delq 't load-missed-1)))
    (occ-debug "starting")
    (when load-uncore
      (occ-debug "The following feature%s found in load-path, please check if that's correct:\n%s"
                 (if (> (length load-uncore) 1) "s were" " was")
                 load-uncore))
    (if load-misses
        (occ-debug "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
                     (if (> (length load-misses) 1) "s" "")
                     load-misses (occ-version nil 'full))
      (occ-debug "Successfully reloaded Org\n%s" (occ-version nil 'full)))))

(when nil
  (let* ((occ-pkg-regexp (regexp-opt (cons "occ" (mapcar #'(lambda (x) (symbol-name (cl-first x))) (package-desc-reqs (nth 1 (assoc 'occ package-alist)))))))
         (feature-re     (concat "^" occ-pkg-regexp "$")))
    (string-match feature-re "xocc")))

;;; occ.el ends here
