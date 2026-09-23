;;; Commentary:

;; occ-pkg.el is package metadata for OCC: a single `define-package' form
;; declaring the version string ("20190310.2156") and the dependency list
;; (org, dash, lotus-utils, lotus-helm, the org-clock-* packages,
;; org-capture+, activity, switch-buffer-functions).
;;
;; See doc/occ-design.org for the full design.
(define-package "occ" "20190310.2156" "occ"
  '((org "9.6.9")
    (dash "1")
    (lotus-utils "1")
    (lotus-helm "1")
    (org-clock-unnamed-task "1")
    (org-clock-resolve-advanced "1")
    (org-clock-hooks "1")
    (org-clock-wrapper "1")
    (org-capture+ "1")
    (activity "1")
    (org-clock-table-misc-lotus "1")
    (switch-buffer-functions "1")))
