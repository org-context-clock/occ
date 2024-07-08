;;; avtivity-mode.el --- activity mode               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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

(provide 'avtivity-mode)


(require 'activity)
(require 'buff-trans)
(require 'mail-event)


(defvar activity-register-hook nil "activity-register-hook")

(defun activity-turn-on ()
  "Turn on Activity."
  (run-hooks 'activity-register-hook)
  (activity-activate-all))

(defun activity-turn-off ()
  "Turn off Activity."
  (activity-deactivate-all))


;;;###autoload
(define-minor-mode activity-mode
  "Toggle Activity (Activity mode)."
  :lighter    " Act"
  :init-value nil
  :global     t
  :group      'activity
  (if activity-mode
      (activity-turn-on)
    (activity-turn-off)))

;;; avtivity-mode.el ends here
