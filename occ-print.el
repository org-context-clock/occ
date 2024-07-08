;;; occ-print.el --- occ print                       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'occ-print)


(require 'lotus-utils)
(require 's)
(eval-when-compile
  (require 'org-macs))
(require 'org-macs)
(require 'org)

(require 'occ-obj)
(require 'occ-predicate)
(require 'occ-obj-accessor)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)


(cl-defmethod occ-obj-uniquify-file ((tsk occ-tsk))
  (let* ((filename (occ-obj-get-property tsk 'file))
         (basename (file-name-nondirectory filename))
         (files (occ-obj-files)))
    (ignore basename)
    (ignore files)
    t))
        ;; (uniquify-buffer-file-name)

;; (file-name-nondirectory "/aaa/aaa/aaa")


(defvar occ-fontify-like-org-file-bullet ?\▆ "occ-fontify-like-org-file-bullet")
;; (defvar occ-fontify-like-org-file-bullet ?\▆ "occ-fontify-like-org-file-bullet")

(cl-defgeneric occ-obj-fontify-like-in-org-mode (obj &optional no-propterties)
  "occ-obj-fontify-like-in-org-mode")

(cl-defmethod occ-obj-fontify-like-in-org-mode ((obj marker) &optional no-propterties)
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (when (occ-obj-buffer obj)
    (with-current-buffer (org-base-buffer (occ-obj-buffer obj))
      (org-with-wide-buffer
       (progn
         (goto-char obj)
         (let* ((cat         (org-get-category))
                (heading-from-org (org-get-heading 'notags))
                (heading (if no-propterties
                             (substring-no-properties heading-from-org)
                           heading-from-org))
                (prefix    (save-excursion
                             (if (org-before-first-heading-p)
                                 ""
                               (progn
                                 (occ-back-to-heading)
                                 (looking-at org-outline-regexp)
                                 (match-string 0)))))
                (org-heading (substring (if no-propterties
                                            (concat prefix heading)
                                          ;; TODO  - disabled
                                          (org-fontify-like-in-org-mode (concat prefix heading)
                                                                        org-odd-levels-only))
                                        (length prefix))))
           (ignore cat)
           org-heading))))))

(cl-defmethod occ-obj-fontify-like-in-org-mode ((obj occ-tsk) &optional no-propterties)
  ;; (occ-message "occ-obj-fontify-like-in-org-mode: heading = %s, level = %s, subtree-level = %s"
  ;;              (occ-obj-get-property obj 'heading-prop)
  ;;              (occ-obj-get-property obj 'level)
  ;;              (occ-obj-get-property obj 'subtree-level))
  (let* ((level               (or (occ-obj-get-property obj 'level) 0))
         (subtree-level       (or (occ-obj-get-property obj 'subtree-level) 1))
         (filename            (occ-obj-format-file obj))
         ;; (filename-prefix     (concat " " (make-string (+ level subtree-level) occ-fontify-like-org-file-bullet) " "))
         (heading-prop        (occ-obj-get-property obj 'heading-prop))
         (heading             (if (eq heading-prop 'noheading)
                                  (concat (make-string 1 occ-fontify-like-org-file-bullet) (when filename
                                                                                                 (concat " " filename)))
                                heading-prop))
         (heading-prefix      " ")
         (prefix              (concat (make-string (+ subtree-level level) ?\*) " "))
         (org-heading-noprop  (concat prefix heading))
         (org-heading         (if no-propterties
                                  org-heading-noprop
                                  (org-fontify-like-in-org-mode org-heading-noprop org-odd-levels-only)))
         (display-org-heading (concat heading-prefix org-heading)))
    ;; (occ-debug "fontify: %s subtree-level=%s" heading subtree-level)
    display-org-heading))

(cl-defmethod occ-obj-build-format-string ((obj occ-tsk) &optional no-propterties)
  (occ-obj-fontify-like-in-org-mode obj no-propterties))


(cl-defmethod occ-obj-build-format-file ((obj occ-tsk))
  (let ((filename (occ-obj-get-property obj 'file))
        (lcp      (apply #'s-lcp (occ-obj-collect-files (occ-tsk-collection obj)))))
    (s-chop-prefix lcp filename)))

;; (cl-defmethod occ-obj-build-format-file ((obj occ-tsk))
;;   (debug)
;;   (let ((filename (occ-obj-get-property obj 'file)))
;;     filename))


(defun occ-case (case title)
  (if (fboundp case)
      (funcall case title)
    title))

(cl-defgeneric occ-obj-title (obj
                              case)
  "occ-obj-title")

(cl-defmethod occ-obj-title (obj
                             case)
  (occ-case case
            (occ-obj-class-name obj)))

(cl-defmethod occ-obj-title ((obj null)
                             case)
  (occ-case case
            (occ-obj-class-name obj)))

(cl-defmethod occ-obj-title ((obj  marker)
                             (case symbol))
  (occ-case case
            (occ-obj-class-name obj)))

(cl-defmethod occ-obj-title ((obj  occ-obj)
                             (case symbol))
  (occ-case case
            (occ-obj-class-name obj)))

(defun occ-obj-Title (obj)
  (occ-case 'capitalize
            (occ-obj-class-name obj)))


(defun occ-obj-TITLE (obj)
  (occ-case 'upcase
            (occ-obj-class-name obj)))


(fmakunbound 'occ-obj-format)

(cl-defgeneric occ-obj-format (obj
                               &optional
                               case
                               rank
                               no-curr-clock
                               no-propterties)
  "occ-obj-format")

(cl-defmethod occ-obj-format ((obj string)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (ignore rank
          no-curr-clock
          no-propterties)
  (concat (when case
            (concat (occ-obj-title obj
                                   case) ": "))
          (format "%s" obj)))

(cl-defmethod occ-obj-format ((obj null)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (ignore case
          rank
          no-curr-clock
          no-propterties)
  (concat (when case
            (concat (occ-obj-title obj
                                   case) ": "))
          (format "%s" obj)))

(cl-defmethod occ-obj-format ((obj marker)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (ignore rank)
  (ignore no-curr-clock)
  ;; TODO: BUG - only in org-mode buffer
  (concat (when case (concat (occ-obj-title obj case) ": "))
          (occ-obj-fontify-like-in-org-mode obj no-propterties)))

(defvar occ-obj-format-tsk-tag-alignment 100 "occ-obj-format-tsk-tag-alignment")

(cl-defmethod occ-obj-format ((obj occ-tsk)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (ignore no-curr-clock)
  (let* ((align      occ-obj-format-tsk-tag-alignment)
         (heading    (occ-obj-format-string obj no-propterties))
         (headinglen (length heading))
         (tags       (occ-obj-get-property obj 'tags))
         (tagstr     (if tags
                         (concat ":" (mapconcat #'identity tags ":") ":")
                       "")))
    (concat (when case (concat (occ-obj-title obj case) ": "))
            (when rank (format "[t%5d] " (or (occ-obj-rank obj) -128)))
            (format (format (if tags "%%-%ds         %%s" "%%s")
                            align
                            (if (< headinglen align) (- align headinglen) 0))
                    heading tagstr))))

(cl-defmethod occ-obj-format ((obj occ-ctx)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (ignore case)
  (ignore rank)
  (ignore no-curr-clock)
  (ignore no-propterties)
  (format "%s" (occ-obj-name obj)))

(cl-defmethod occ-obj-format ((obj occ-obj-ctx-tsk)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (let ((tsk (occ-ctsk-tsk obj)))
    (concat (when case (concat (occ-obj-title obj case) ": "))
            (when rank (format "[%5d] " (or (occ-obj-rank obj) -128)))
            (occ-obj-format tsk case nil nil no-propterties))))
            ;; (unless no-curr-clock
            ;;   (when (occ-obj-current-p obj) "          🕑"))

(cl-defmethod occ-obj-format ((obj occ-ctxual-tsk)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (let ((tsk        (occ-ctxual-tsk-tsk obj))
        (selectable (occ-obj-tsk-selectable obj)))
    (let ((tsk-str (format "%s" (occ-obj-format tsk case rank no-curr-clock no-propterties))))
      (concat (when case (concat (occ-obj-title obj case) ": "))
              ;; (if selectable "S " "U ")
              (when rank (format "[c%5d] " (or (occ-obj-rank obj) -128)))
              (if selectable
                  tsk-str
                (let* ((start (string-match org-outline-regexp tsk-str))
                       (end   (1+ (string-match " " (substring tsk-str start))))
                       (prefix  (substring tsk-str 0 (+ start end)))
                       (heading (substring tsk-str (+ start end))))
                       ;; (strike-heading (occ-obj-add-face-properties heading
                       ;;                                              ?w
                       ;;                                              :strike-through t))
                  ;; (occ-message "prefix: |%s|" prefix)
                  ;; (occ-message "heading: |%s|" heading)
                  (occ-obj-add-face-properties (concat prefix heading)
                                               nil ;;?w
                                               ;;:strike-through t
                                               :weight "bold"
                                               ;; :background "sienna1"
                                               ;; :background "DarkGrey"
                                               ;; :background "Steelblue4"
                                               ;; :background "Gray10"
                                               :background "Gray50")))))))

(cl-defmethod occ-obj-format ((obj occ-return)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (ignore case rank no-curr-clock no-propterties)
  (let ((label     (and obj (occ-return-label obj)))
        (value-obj (occ-obj-obj obj)))
    (format "%s: %s"
            (symbol-name label)
            (occ-obj-format (occ-name value-obj)))))

(cl-defmethod occ-obj-format ((obj buffer)
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (format "%s" obj))

(cl-defmethod occ-obj-format (obj
                              &optional
                              case
                              rank
                              no-curr-clock
                              no-propterties)
  (format "%s" obj))


(cl-defmethod occ-obj-nonocc-format (obj)
  obj)

(cl-defmethod occ-obj-nonocc-format ((obj occ-obj-tsk))
  (type-of obj))


(defun occ-obj-Format (obj
                       &optional
                       rank
                       no-curr-clock
                       no-propterties)
  (occ-obj-format obj 'capitalize
                  rank
                  no-curr-clock
                  no-propterties))

(defun occ-obj-FORMAT (obj
                       &optional
                       rank
                       no-curr-clock
                       no-propterties)
  (occ-obj-format obj 'upcase
                  rank
                  no-curr-clock
                  no-propterties))


(cl-defmethod cl-print-object ((obj occ-ctx)
                               stream)
  (princ (format "<CTX %s>" (occ-name obj)) stream))

(cl-defmethod cl-print-object ((obj occ-ctsk)
                               stream)
  (princ (format "<CTSK %s>" (occ-name obj)) stream))

(cl-defmethod cl-print-object ((obj occ-ranktbl)
                               stream)
  (princ (format "<RANKTBL %s>" (occ-name obj)) stream))

(cl-defmethod cl-print-object ((obj occ-tree-tsk)
                               stream)
  (princ (format "<TSK-TREE %s>" (occ-name obj)) stream))

;; (type-of (occ-get-debug-obj))
;; (message "%S" (occ-get-debug-obj))

;; (message "%s" (occ-obj-make-ctx-at-point))

;; (cl-print-object (occ-make-ranktbl) standard-output)
;; (message "%s" (occ-make-ranktbl) standard-output)


(cl-defgeneric occ-obj-display (obj)
  "Neatly Output OBJ with its properties")

(cl-defmethod occ-obj-display ((obj occ-obj-tsk))
  (ignore obj)
  (occ-error "Implement it: neatly output OBJ with its properties"))


;; (type-of (alist-get 'default *occ-collector*))

;; (dolist (x (occ-tree-collection-list (car (occ-collections-default))))
;;   (message "- %s" (occ-obj-format x)))


;; (setf (occ-tree-collection-list (car (occ-collections-default)))
;;       nil)

;; (cl-struct-slot-info (occ-cl-class (car (occ-collections-default))))

;;; occ-print.el ends here
