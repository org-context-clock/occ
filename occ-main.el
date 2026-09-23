;;; occ-main.el --- occ-api               -*- lexical-binding: t; -*-
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

;; occ-main.el is the OCC entry-layer aggregator.  It currently only
;; provides the `occ-main' feature and requires the main API modules
;; (occ-obj-method, occ-util-common, occ-unnamed, occ-prop, occ-version)
;; plus the lotus utility libraries, fixing their load order; no functions
;; are defined here.  The TODO block records open questions about helm
;; source arguments for the selection UI (support layer).
;;
;; See doc/occ-design.org for the full design.

;;; Code:

(provide 'occ-main)


(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))

(require 'occ-obj-method)
(require 'occ-util-common)
(require 'occ-unnamed)
(require 'occ-prop)
(require 'occ-version)

;; TODO:
;; find obout
;; (helm-find-files-up-one-level)
;; (helm-find-files-switch-to-bookmark)
;; (with-helm-alive-p)
;; helm-find-files-initial-input
;; helm-browse-project-source
;;
;; find about helm source arguments
;; :initarg :root-dir
;; :initform nil
;; :custom 'file


;;; occ-main.el ends here
