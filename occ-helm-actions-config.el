;;; occ-helm-actions-config.el --- occ helm configurations   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  sharad

;; Author: s
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

(provide 'occ-helm-actions-config)


(require 'org-capture+-helm-dynamic)


(require 'occ-helm)

(defvar occ-helm-callables nil)

;;;###autoload
(defun occ-helm-actions-config-initialize ()
  (interactive)
  (progn
    (progn
      ;; TODO
      (org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                         "* TODO %? %^g\n %i\n [%a]\n")
      (org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                         "* MILESTONE %? %^g\n %i\n [%a]\n")
      (org-capture+-add-heading-template '(occ tsk clockable meeting) "MEETING"
                                         "* MEETING %? %^g\n %i\n [%a]\n")
      ;; NOTE
      (org-capture+-add-heading-template '(occ tsk unclockable note) "NOTE"
                                         "* <NOTE> %? %^g\n %i\n [%a]\n")
      ;; INFO
      (org-capture+-add-heading-template '(occ tsk unclockable info) "INFO"
                                         "* <INFO> %? %^g\n %i\n [%a]\n")
      ;; EVENT
      (org-capture+-add-heading-template '(occ tsk unclockable event) "EVENT"
                                         "* <EVENT> %? %^g\n %i\n [%a]\n"))
    (progn
      (setq occ-helm-callables nil)
      (occ-obj-build-callable-normal :ignore                 "Ignore"                   #'ignore)
      (occ-obj-build-callable-normal :identity               "Select"                   #'identity)
      (occ-obj-build-callable-normal :clock-in               "Clock-in"                 #'occ-do-clock-in)
      (occ-obj-build-callable-normal :try-fast-clock-in      "Try Fast Clock-in"        #'occ-do-try-fast-clock-in)
      (occ-obj-build-callable-normal :try-clock-in           "Try Clock-in"             #'occ-do-try-clock-in)
      (occ-obj-build-callable-normal :create-child           "Create Child"             #'occ-do-create-child)
      (occ-obj-build-callable-normal :create-child-clock-in  "Create Child Clock-in"    #'occ-do-create-child-clock-in)
      (occ-obj-build-callable-normal :cut                    "Cut"                      #'occ-do-cut)
      (occ-obj-build-callable-normal :paste                  "Paste"                    #'occ-do-paste)
      (occ-obj-build-callable-normal :archive                "Archive"                  #'occ-do-archive)
      (occ-obj-build-callable-normal :delete                 "Delete"                   #'occ-do-delete)
      (occ-obj-build-callable-normal :report                 "Report"                   #'occ-do-report)
      (occ-obj-build-callable-normal :goto                   "Goto"                     #'occ-do-goto)
      (occ-obj-build-callable-normal :set-to                 "Set To"                   #'occ-do-set-to)
      (occ-obj-build-callable-normal :property-window-edit   "Properties Window Edit"   #'occ-do-properties-window-editor) ;TODO: implement it.
      (occ-obj-build-callable-normal :property-edit-combined "Properties Edit Combined" #'occ-do-properties-editor-combined) ;TODO: implement it.
      (occ-obj-build-callable-normal :call-with-obj          "Call with object"         #'occ-do-call-with-obj)
      (occ-obj-build-callable-normal :set-debug-obj          "Set debug obj"            #'occ-do-set-debug-obj)
      (occ-obj-build-callable-normal :rank                   "Get Rank"                 #'occ-do-print-rank)
      (occ-obj-build-callable-normal :tsk                    "Get Task"                 #'occ-do-print-tsk)
      (occ-obj-build-callable-normal :describe               "Describe object"          #'occ-do-describe-obj)
      (occ-obj-build-callable-normal :checkout               "Checkout"                 #'occ-do-checkout)
      (occ-obj-build-callable-normal :close                  "Close"                    #'occ-do-close)
      (occ-obj-build-callable-normal :force-clock-in         "Force Clock in"           #'occ-do-force-clockin))
    (progn
      (occ-obj-build-callable-generator :fast-edits-gen     "Fast Edits"       #'occ-obj-gen-each-prop-fast-edits)
      (occ-obj-build-callable-generator :clock-ops-gen      "Clock Operations" #'occ-obj-gen-clock-operations)
      (occ-obj-build-callable-generator :edits-gen          "Simple Edit"      #'occ-obj-gen-simple-edits)
      (occ-obj-build-callable-generator :checkouts-gen      "Simple Checkout"  #'occ-obj-gen-simple-checkouts)
      (occ-obj-build-callable-generator :fast-checkouts-gen "Fast Checkouts"   #'occ-obj-gen-each-prop-fast-checkouts)
      (occ-obj-build-callable-generator :misc-gen           "Misc"             #'occ-obj-gen-misc))
    (progn
     (setq occ-helm-actions-tree '(t))
     (progn
       (occ-add-helm-actions '(actions select)
                             "Select"
                             :identity)

       (occ-add-helm-actions '(actions general)
                             "Simple"
                             :create-child
                             :create-child-clock-in
                             :call-with-obj
                             :set-debug-obj
                             :try-clock-in
                             :close
                             :force-clock-in
                             :cut
                             :paste
                             :archive
                             :delete
                             :report
                             :goto
                             :rank
                             :tsk
                             :describe)

       (occ-add-helm-actions '(actions edit)
                             "Editing"
                             :property-window-edit
                             :property-edit-combined))
     (progn
       (occ-add-helm-actions '(actions general)
                             "General"
                             :misc-gen)

       (occ-add-helm-actions '(actions edit)
                             "Fast Editing"
                             :fast-edits-gen
                             :edits-gen)

       (occ-add-helm-actions '(actions checkout)
                             "Checkout"
                             :fast-checkouts-gen
                             :checkouts-gen)))))

;;; occ-helm-actions-config.el ends here
