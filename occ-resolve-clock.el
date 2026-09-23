;;; occ-resolve-clock.el --- Occ resolve clock       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
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

;; occ-resolve-clock.el is currently an interface-registration stub
;; tying OCC into the vendored org-clock-resolve-advanced package
;; through its org-rl-intf interface.  occ-rl-register-resolve-clock
;; and occ-rl-unregister-resolve-clock manage the 'occ backend in
;; org-rl-intf-register / org-rl-intf-unregister, supplying the
;; placeholder functions occ-rl-clock-p, occ-rl-clock-clock-in,
;; occ-rl-clock-out, occ-rl-clock-clock-out,
;; occ-rl-select-other-clock and
;; occ-rl-capture+-helm-templates-alist, whose bodies just ignore
;; their arguments; no actual clock-resolve behavior is implemented
;; yet.  The commands occ-register-resolve-clock and
;; occ-unregister-resolve-clock in occ-commands.el call these.
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-resolve-clock)


(require 'org-rl-intf nil nil)


(defun occ-rl-clock-p (clock-marker)
  "Placeholder org-rl-intf clock predicate for CLOCK-MARKER.
Stub: ignores its argument and returns nil."
  (ignore clock-marker))
(defun occ-rl-clock-clock-in (clock-marker &optional resume start-time)
  "Placeholder org-rl-intf clock-in for CLOCK-MARKER.
Stub: ignores CLOCK-MARKER, RESUME and START-TIME and does nothing."
  (ignore clock-marker)
  (ignore resume)
  (ignore start-time))

(defun occ-rl-clock-out (&optional switch-to-state fail-quietly at-time)
  "Placeholder org-rl-intf clock-out hook.
Stub: ignores SWITCH-TO-STATE, FAIL-QUIETLY and AT-TIME and does
nothing."
  (ignore switch-to-state)
  (ignore fail-quietly)
  (ignore at-time))
(defun occ-rl-clock-clock-out (clock-marker &optional fail-quietly at-time)
  "Placeholder org-rl-intf clock-out for CLOCK-MARKER.
Stub: ignores CLOCK-MARKER, FAIL-QUIETLY and AT-TIME and does
nothing."
  (ignore clock-marker)
  (ignore fail-quietly)
  (ignore at-time))
(defun occ-rl-select-other-clock (clock-marker &optional target)
  "Placeholder org-rl-intf other clock selection for CLOCK-MARKER.
Stub: ignores CLOCK-MARKER and TARGET and does nothing."
  (ignore clock-marker)
  (ignore target))
(defun occ-rl-capture+-helm-templates-alist (clock-marker)
  "Placeholder org-capture+ helm template list for CLOCK-MARKER.
Stub: ignores its argument and returns nil."
  (ignore clock-marker))


;;;###autoload
(defun occ-rl-register-resolve-clock ()
  "Register the occ clock backend in the org-rl-intf registry.
Registration-only stub: stores the placeholder occ-rl-* functions
under the occ backend name when the org-rl-intf feature is loaded.
No clock-resolve behavior is implemented yet."
  (when (featurep 'org-rl-intf)
    (org-rl-intf-register 'occ (list
                                :org-rl-clock-p                       #'occ-rl-clock-p
                                :org-rl-clock-clock-in                #'occ-rl-clock-clock-in
                                :org-rl-clock-out                     #'occ-rl-clock-out
                                :org-rl-select-other-clock            #'occ-rl-select-other-clock
                                :org-rl-capture+-helm-templates-alist #'occ-rl-capture+-helm-templates-alist))))

;;;###autoload
(defun occ-rl-unregister-resolve-clock ()
  "Unregister the occ clock backend from the org-rl-intf registry.
Does nothing when the org-rl-intf feature is not loaded."
  (when (featurep 'org-rl-intf)
    (org-rl-intf-unregister 'occ)))

;;; occ-resolve-clock.el ends here
