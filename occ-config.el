;;; occ-config.el --- occ config                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

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

(provide 'occ-config)


(require 'occ-util-common)
(require 'occ-macros)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)


;; check
;; https://emacs.stackexchange.com/questions/12111/why-is-defgroup-useful

(defgroup occ nil
  "Org Context clock."
  :prefix "occ-"
  :group 'extensions
  :group 'convenience
  :version "0.1"
  :link '(emacs-commentary-link :tag "Commentary" "occ.el")
  :link '(emacs-library-link :tag "Lisp File" "occ.el")
  :link '(custom-manual "(occ) Top")
  :link '(info-link "(occ) Customization"))


(defcustom occ-prefix-key "M-n"
  "occ prefix key"
  :type '(string)
  :group 'occ)

(defcustom occ-unnamed t
  "occ-unnamed"
  :type '(boolean)
  :group 'occ)

(defcustom occ-clockout-unassociable-to-unnamed 'ask
  "occ-clockout-unassociable-to-unnamed"
  :type '(symbol)
  :group 'occ) ;; TODO: or could ask to continue for TIME(m/h) with current task.

(defcustom occ-completing-read-function 'completing-read "Occ Completing read function"
  :type '(choice (function-item 'completing-read)
                 (function-item 'ido-completing-read))
  :group 'occ)

(defcustom occ-clock-in-ctx-auto-select-if-only nil
  "occ-clock-in-ctx-auto-select-if-only"
  :type '(boolean)
  :group 'occ)

(defcustom occ-config-ineq-const-value nil
  "occ-config-ineq-const"
  :type '(choice (function-item #'(lambda () (random 99)))
                 (integer :tag "Constant integer value"))
  :group 'occ)


;; mozilla config
;; name type default-value custom-value possible types

;; emacs defcustom
;; https://emacs.stackexchange.com/questions/15064/how-to-properly-use-defcustom
;; https://stackoverflow.com/questions/15309548/how-to-set-defcustom-variable
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/defcustom.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/defcustom.html


;; (defun occ-confirm (fn new)
;;   (occ-y-or-n-timeout)
;;   (occ-error "Implement it."))

(defun occ-confirm (config
                    msg
                    timeout)
  (ignore timeout)
  (cond
     ((null config) nil)
     ((functionp config) (funcall config))
     ((eq config 'ask)  (y-or-n-p msg))
     ((eq config t) t)
     (t nil)))


;; org-agenda-category-icon-alist
;; https://orgmode.org/manual/Categories.html
;; org-todo-keywords
;; ((sequence "TODO(t)" "STARTED" "NEXT(n)" "|" "DONE(d@/!)" "|" "CLOSED(c@/!)")
;;  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(C@/!)" "PHONE" "MEETING"))


(cl-defstruct occ-entry-types
  sequence
  catogery)


(defvar occ-config-clock-in t)

;;;###autoload
(defun occ-config-enable-clock-in ()
  (interactive)
  (setq occ-config-clock-in t))

;;;###autoload
(defun occ-config-disable-clock-in ()
  (interactive)
  (setq occ-config-clock-in nil))

;;;###autoload
(defun occ-config-clock-in ()
  occ-config-clock-in)


;; option for keeping quiet
(occ-gen-binary-option-commands occ-config- quiet -p nil)



(defvar occ-dev-dir nil)

;;; occ-config.el ends here
