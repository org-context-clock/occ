;;; occ-mode.el --- occ mode                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords: convenience, tools

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

(provide 'occ-mode)


(require 'which-key)
(require 'core-keybindings)
(require 'occ)
(require 'occ-config)
(require 'occ-commands)
(require 'occ-util-common)


(defvar occ-mode-main-keymap (make-keymap) "occ-mode keymap.")
(defvar spacemacs-occ-mode-map (make-keymap) "occ-mode keymap.")
;; https://www.emacswiki.org/emacs/PrefixKey
;; https://stackoverflow.com/questions/25524710/define-key-in-prefix-keymap-for-a-particular-mode

;; TODO: spacemacs/set-leader-keys-for-minor-mode
;; (spacemacs/set-leader-keys "O" 'occ-mode-keymap)
;; check C-h C-f ipython-notebook/init-ein

(define-prefix-command 'occ-mode-keymap)
;;;###autoload
(defun occ-enable-mode-map ()
  (define-key occ-mode-main-keymap (kbd occ-prefix-key) 'occ-mode-keymap))
;;;###autoload
(defun occ-disable-mode-map ()
  (define-key occ-mode-main-keymap (kbd occ-prefix-key) nil))


(defun occ-set-bindings ()
  (let ((bindings '(("cr" occ-run)
                    ("ms" occ-helm-match-select)
                    ("ls" occ-helm-list-select)
                    ("ld" occ-helm-list-debug-select)
                    ("ll" occ-helm-list-launch)
                    ("le" occ-property-edit)


                    ("Cc" occ-curr-create-child)
                    ("CC" occ-curr-create-child-clock-in)
                    ("Ct" occ-curr-tsk-continue-for)

                    ("ds" occ-start-day)
                    ("du" occ-show-up)
                    ("do" occ-stop-day)
                    ("dp" occ-pack-up)

                    ("tif" occ-do-clock-in-force)
                    ("tii" occ-interrupt-clock-in)
                    ("to"  occ-clock-out)
                    ("tP"  occ-continue-prev)
                    ("ta"  occ-obj-make-anonymous)

                    ("q" occ-keep-quiet)
                    ("Q" occ-keep-quiet-for)

                    ("rr" occ-register-resolve-clock)
                    ("ru" occ-unregister-resolve-clock)

                    ("sR" occ-reset-spec)
                    ("sM" occ-obj-make-spec)
                    ("sA" occ-add-to-spec)
                    ("sO" occ-add-org-file)
                    ("sB" occ-obj-build-spec)

                    ("Tr" occ-files-with-null-regex)
                    ("To" occ-files-not-in-org-mode)

                    ("Um" occ-merge-unamed-task)

                    ("OR" occ-reset-collection-object)
                    ("Oi" occ-insinuate)
                    ("Ou" occ-uninsinuate)

                    ("oT" occ-run-timer)
                    ("oR" occ-reload)
                    ("v"  occ-version))))
    (progn
      (apply #'spacemacs/set-leader-keys-for-minor-mode 'occ-mode
             (cl-mapcan
              (lambda (bind)
                (if (fboundp (cl-second bind))
                    bind
                  (prog1 nil
                    (display-warning
                     'warn (format "occ-mode: undefined %s"
                                   (cl-second bind))))))
              (copy-tree bindings)))
      ;; (eval (append '(spacemacs|define-transient-state
      ;;                    ipython-notebook
      ;;                  :title "EIN Transient State"
      ;;                  :evil-leader-for-mode (ein:notebook-mode . ".")
      ;;                  :bindings
      ;;                  ("q" nil :exit t))
      ;;               bindings
      ;;               `(:doc ,(ipython-notebook/transient-doc bindings))))

      (dolist (it (copy-tree bindings))
        (define-key occ-mode-keymap (kbd (car it)) (cadr it)))))

  (dolist (it '(("l" . "format")
                ("C" . "folder")
                ("d" . "toggle")
                ("t" . "goto")
                ("r" . "help")
                ("s" . "refactor")
                ("T" . "workspace")
                ("U" . "actions")
                ("O" . "peek")
                ("O" . "peek")))
    (which-key-add-keymap-based-replacements spacemacs-occ-mode-map (car it) (cdr it))))



(defvar occ-mode-global-allowed nil)
;;;###autoload
(defun occ-mode-global-allow ()
  (interactive)
  (setq occ-mode-global-allowed t))
;;;###autoload
(defun occ-mode-global-disallow ()
  (interactive)
  (setq occ-mode-global-allowed nil))

(defface occ-mode-lighter
  '((t (:inherit mode-line :foreground "Green")))
  "Face used for lighter tasks in mode line."
  :group 'occ)

;;;###autoload
(define-minor-mode occ-mode
  "Toggle Occ mode.
      ...rest of documentation as before..."
  ;; The initial value.
  :init-value nil
  :global     t
  ;; The indicator for the mode line.
  :lighter (:eval (propertize " Occ" 'face 'occ-mode-lighter))
  ;; The minor mode bindings.
  :keymap occ-mode-main-keymap
  :group 'occ
  (if occ-mode-global-allowed
    (if occ-mode
        (occ-insinuate (occ-collector-default-key))
      (occ-uninsinuate))
    (occ-warn "occ-mode-global-allowed is nil, not enabling occ-mode")))

;;; occ-mode.el ends here
