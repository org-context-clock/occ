;;; activity.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(provide 'activity)


(require 'cl-seq)
(require '@)


(eval-when-compile
  (require 'activity-macro))
(require 'activity-base)


(defgroup activity nil
  "Customizations for Activity"
  :group 'convenience
  :prefix "activity-")


(drive-extended@ @activity-interface (@activity-base) "activity"
  (def@ @@ :key ()
    (@:error "Implement :key interface"))
  (def@ @@ :activate ()
    (@:error "Implement :activate interface"))
  (def@ @@ :deactivate ()
    (@:error "Implement :deactivate interface")))

(drive-extended@ @activity (@activity-base) "activity"
  "Activity class"
  (def@ @@ :init ()
    (@^:init)
    (@:message "@activity-class :init")
    (setf @:started-acts   nil
          @:activities     nil))

  (def@ @@ :reset ()
    (@:deactivate-all)
    (setf @:started-acts   nil
          @:activities   nil))

  (def@ @@ :activate (act)
    (if (memq act @:started-acts)
        (@:error "Activity %s: already active" (@! act :key))
      (when (@! act :activate)
        (remove act
                @:activities)
        (push act
              @:started-acts))))

  (def@ @@ :deactivate (act)
    (if (memq act @:started-acts)
        (when (@! act :deactivate)
          (remove act
                  @:started-acts)
          (push act
                @:activities))
      (@:error "Activity %s: not active" (@! act :key))))

  (def@ @@ :activate-all ()
    (dolist (act @:activities)
      (@:activate act)))

  (def@ @@ :deactivate-all ()
    (dolist (act @:started-acts)
      (@:deactivate act)))

  (def@ @@ :add (activity)
    (let ((key (@! activity :key)))
      (if (member key
                  (append (@mapcar @:key @:activities)
                          (@mapcar @:key @:started-acts)))
          (@:error "activity with %s already present." key)
        (push activity
              @:activities))))

  (def@ @@ :register (activity)
    (let ((key (@! activity :key)))
      (unless (member key
                      (append (@mapcar @:key @:activities)
                              (@mapcar @:key @:started-acts)))
        (push activity
              @:activities))))

  (def@ @@ :find (key)
    (cl-find-if #'(lambda (e) (equal (@! e :key) key))
                @:activities))

  (def@ @@ :inspect ()
    (@:message "activties: [%s], started-acts: [%s]"
               (@mapcar @:key @:activities)
               (@mapcar @:key @:started-acts)))

  (@:init))


(defun activity-inspect ()
  (interactive)
  (@! @activity :inspect))

;;;###autoload
(defun activity-reset ()
  (interactive)
  (@! @activity :reset))

;;;###autoload
(defun activity-activate (key)
  (interactive
   (list (completing-read "activity: "
                          (@mapcar @:key
                                   (@ @activity :activities)))))
  ;; (activity-add-subdirs-load-path)
  (let ((act (@! @activity :find key)))
    (when act
      (@! @activity :activate))))

;;;###autoload
(defun activity-deactivate (key)
  (interactive
   (list (completing-read "activity: "
                          (@mapcar @:key
                                   (@ @activity :started-acts)))))
  (let ((act (@! @activity :find key)))
    (when act
      (@! @activity :deactivate))))

;;;###autoload
(defun activity-activate-all ()
  (interactive)
  ;; (activity-add-subdirs-load-path)
  (@! @activity :activate-all))

;;;###autoload
(defun activity-deactivate-all ()
  (interactive)
  (@! @activity :deactivate-all))

;;;###autoload
(defun activity-add (activity)
  (interactive)
  (@! @activity :add activity))

;;;###autoload
(defun activity-register (activity)
  (interactive)
  (@! @activity :register activity))

;;; activity.el ends here
