;;; occ-debug-method.el --- occ debug method         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords: convenience, abbrev

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

(provide 'occ-debug-method)


(require 'occ-util-common)
(require 'occ-obj)
(require 'occ-obj-accessor)


(defvar occ-log-levels '(:emergency :error :warning :debug :info :dmessage :nodisplay t))
(defvar occ-log-level t "Debug occ")
(defvar occ-log-current-levels (memq occ-log-level occ-log-levels))
(defvar occ-debug-uncond nil "occ-debug-uncond")


;; https://emacs.stackexchange.com/questions/2310/can-functions-access-their-name
(defun occ-get-current-func-name (&optional index)
  "Get the symbol of the function this function is called from."
  ;; 5 is the magic number that makes us look
  ;; above this function
  (let* ((index (+ 4 (or index 0)))
         (frame (backtrace-frame index))
         (max   (+ index 10)))
    ;; from what I can tell, top level function call frames
    ;; start with t and the second value is the symbol of the function
    (while (and (> max 0)
                (not (equal t (cl-first frame))))
      (cl-decf max)
      (setq frame (backtrace-frame (cl-incf index))))
    ;; (message "index %d" index)
    (if (equal t (cl-first frame))
        (let* ((fun     (cl-second frame))
               (funname (if (symbolp fun)
                            (symbol-name fun)
                          (format "%s" fun)))
               (flen    (length funname)))
          (substring funname 0 (if (> flen 10)
                                   10
                                 flen)))
      "unknown")))


;;;###autoload
(defun occ-set-log-level (level)
  (interactive (list (intern (completing-read (format "Log level [%s]: " occ-log-level)
                                              (reverse occ-log-levels)
                                              nil
                                              t))))
  (when (memq level occ-log-levels)
    (setq occ-log-level level)
    ;; occ-log-reaccess-current-levels
    (setq occ-log-current-levels (memq occ-log-level occ-log-levels))))
;;;###autoload
(defun occ-clear-log-level ()
  (occ-set-log-level t))


;;;###autoload
(defun occ-enable-debug ()
  (interactive)
  (occ-set-log-level :debug))

;;;###autoload
(defun occ-disable-debug ()
  (interactive)
  (occ-set-log-level t))


;;;###autoload
(defun occ-enable-debug-uncond ()
  (interactive)
  (setq occ-debug-uncond t))
;;;###autoload
(defun occ-disable-debug-uncond ()
  (interactive)
  (setq occ-debug-uncond nil))


;;;###autoload
(defun occ-lwarn (level &rest args)
  (when (or (null level)
            (memq level occ-log-current-levels))
    (apply #'lwarn 'occ level args)
    (apply #'message args))
  nil)

(defun occ-debug-index (index fmt &rest args)
  (let* ((strfmtp (stringp fmt))
         (level   (if strfmtp :debug fmt)))
    (when (memq level occ-log-current-levels)
      (let ((fmt (if strfmtp fmt (car args)))
            (args (if strfmtp args (cdr args)))
            (index   (or index 0))
            (funname (occ-get-current-func-name index)))
        (ignore index)
        (apply #'occ-lwarn level
                       (concat funname ": " fmt)
                       args)))))


(defun occ-critical (fmt &rest args)
  (apply #'occ-lwarn :critical fmt args)
  (apply #'error fmt args))

(defun occ-emergency (fmt &rest args)
  (apply #'occ-lwarn :emergency fmt args)
  (apply #'error fmt args))

(defun occ-error (fmt &rest args)
  (apply #'occ-lwarn :error fmt args)
  (apply #'error fmt args))

(defun occ-warn (fmt &rest args)
  (apply #'occ-lwarn :warning fmt args))

(defun occ-info (fmt &rest args)
  (apply #'occ-lwarn :info fmt args))

;;;### autoload
(defun occ-message (fmt &rest args)
  (apply #'message fmt args))

(when nil

  (defun occ-debug (fmt &rest args)
    (apply #'occ-debug-index 3 fmt args))

  ;;;### autoload
  (defun occ-dmessage (fmt &rest args)
    (apply #'occ-debug-index 3 :dmessage fmt args))

 (defun occ-nodisplay (fmt &rest args)
   (apply #'occ-lwarn :nodisplay fmt args))

 ;;;### autoload
 (defun occ-debug-uncond (&rest args)
   (when occ-debug-uncond
     (apply #'occ-lwarn nil args))))


(fmakunbound 'occ-debug)
(fmakunbound 'occ-dmessage)
(fmakunbound 'occ-nodisplay)
(fmakunbound 'occ-debug-uncond)

(defmacro occ-debug (fmt &rest args)
  nil)

(defmacro occ-dmessage (fmt &rest args)
  nil)

(defmacro occ-nodisplay (fmt &rest args)
  nil)

(defmacro occ-debug-uncond (&rest args)
  nil)


(cl-defmethod occ-do-print-tsk ((obj occ-obj-tsk))
  "Dump tsk"
  t)
  ;; (occ-debug "occ-do-print-tsk: %s" obj)

(cl-defmethod occ-do-print-tsk ((obj occ-obj-ctx-tsk))
  "Dump ctx-tsk"
  (let ((tsk (occ-obj-tsk obj)))
    ;; (occ-debug "occ-do-print-tsk: %s" tsk)
    t))

;;; occ-debug-method.el ends here
