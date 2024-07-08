;;; org-interactive-note.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

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

(provide 'org-interactive-note)


(eval-when-compile
  (require 'activity-macro))
(require 'activity-base)
(require 'activity)
(require 'org-insert-utils)


(drive-extended@ @org-interactive-note-dest (@dest-class)
  (def@ @@ :send (win-timeout
                  &key
                  fail-quietly
                  buff
                  chgcount
                  success
                  fail
                  run-before)
    (org-clock-lotus-log-note-current-clock-with-timed-new-win win-timeout
                                                               :fail-quietly fail-quietly
                                                               :buff         buff
                                                               :chgcount     chgcount
                                                               :success      success
                                                               :fail         fail
                                                               :run-before   run-before)))

;;; org-interactive-note.el ends here
