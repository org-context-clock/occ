;;; occ-scratch-space.el --- occ scratch space       -*- lexical-binding: t; -*-

;; copyright (c) 2019  sharad

;; author: sharad <sh4r4d@gmail.com>
;; keywords: convenience

;; this program is free software; you can redistribute it and/or modify
;; it under the terms of the gnu general public license as published by
;; the free software foundation, either version 3 of the license, or
;; (at your option) any later version.

;; this program is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the
;; gnu general public license for more details.

;; you should have received a copy of the gnu general public license
;; along with this program.  if not, see <http://www.gnu.org/licenses/>.

;;; commentary:

;;

;;; code:

(provide 'occ-scratch-space)


;; check about function (org-refile-new-child)

;; org-capture-templates
(eval-when-compile
  (require 'occ-macros))


(when nil
  (progn
    ;; http://pages.sachachua.com/.emacs.d/sacha.html#orgd593b27
    ;; self-tracking, statistics, and other data transformations
    ;; quantified awesome

    (defmacro my/org-with-current-task (&rest body)
      "execute body with the point at the subtree of the current task."
      `(if (derived-mode-p 'org-agenda-mode)
           (save-window-excursion
             (org-agenda-switch-to)
             ,@body)
         ,@body))

    (defun my/org-clock-in-and-track ()
      "start the clock running. clock into quantified awesome."
      (interactive)
      (my/org-with-current-task
       (org-clock-in)
       (call-interactively 'my/org-quantified-track)
       (when (org-entry-get (point) "auto")
         (org-open-link-from-string (org-entry-get (point) "auto")))))
    (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

    (defmacro my/with-org-task (&rest body)
      "run body within the current agenda task, clocked task, or cursor task."
      `(cond
        ((derived-mode-p 'org-agenda-mode)
         (let* ((marker (org-get-at-bol 'org-marker))
                (buffer (marker-buffer marker))
                (pos (marker-position marker)))
           (with-current-buffer buffer
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char pos)
                 ,@body)))))
        ((and (derived-mode-p 'org-mode) (org-at-heading-p)) (save-excursion ,@body))
        ((org-clocking-p) (save-excursion (org-clock-goto) ,@body))
        ((derived-mode-p 'org-mode) ,@body)))

    (defun my/org-quantified-track (&optional category note)
      "create a tracking record using category and note.
default to the current task in the agenda, the currently-clocked
entry, or the current subtree in org."
      (interactive (list nil nil))
      (unless (and category note)
        (my/with-org-task
         (setq category (or category
                            (org-entry-get-with-inheritance "quantified")))
         (cond
          ((null category)
           (setq category (read-string "category: "))
           (org-set-property "quantified" category))
          ((string= category "ask")
           (setq category (read-string "category: "))))
         (setq note
               (concat
                (if (string= (or (org-entry-get-with-inheritance "quantifiedquiet") "") "t")
                    "!private "
                  "")
                (or note (elt (org-heading-components) 4) (read-string "note: "))))))
      (quantified-track (concat category " | " note)))

    (defun my/org-quick-clock-in-task (location jump)
      "track and clock in on the specified task.
if jump is non-nil or the function is called with the prefix argument, jump to that location afterwards."
      (interactive (list (save-excursion (my/org-refile-get-location "location")) current-prefix-arg))
      (when location
        (if jump
            (progn (org-refile 4 nil location) (my/org-clock-in-and-track))
          (save-window-excursion
            (org-refile 4 nil location)
            (my/org-clock-in-and-track)))))
    (bind-key "c-c q" 'my/org-quick-clock-in-task)

    (require 'quantified nil t))






  (progn

    ;;http://pages.sachachua.com/.emacs.d/sacha.html
    ;;http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

    (defun my/org-contacts-template-email (&optional return-value)
      "try to return the contact email for a template.
  if not found return return-value or something that would ask the user."
      (or (nth 1 (if (gnus-alive-p)
                     (gnus-with-article-headers
                      (mail-extract-address-components
                       (or (mail-fetch-field "reply-to") (mail-fetch-field "from") "")))))
          return-value
          (concat "%^{" org-contacts-email-property "}p")))


    (defvar my/org-basic-task-template "* todo %^{task}
:properties:
:effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:end:
captured %<%y-%m-%d %h:%m>
%?

%i
" "basic task data")

    (setq org-capture-templates
          `(("t" "tasks" entry
             (file+headline "~/personal/organizer.org" "inbox")
             ,my/org-basic-task-template)
            ("t" "quick task" entry
             (file+headline "~/personal/organizer.org" "inbox")
             "* todo %^{task}\nscheduled: %t\n"
             :immediate-finish t)
            ("i" "interrupting task" entry
             (file+headline "~/personal/organizer.org" "inbox")
             "* started %^{task}"
             :clock-in :clock-resume)
            ("e" "emacs idea" entry
             (file+headline "~/code/emacs-notes/tasks.org" "emacs")
             "* todo %^{task}"
             :immediate-finish t)
            ("e" "energy" table-line
             (file+headline "~/personal/organizer.org" "track energy")
             "| %u | %^{energy 5-awesome 3-fuzzy 1-zzz} | %^{note} |"
             :immediate-finish t)

            ("b" "business task" entry
             (file+headline "~/personal/business.org" "tasks")
             ,my/org-basic-task-template)
            ("p" "people task" entry
             (file+headline "~/personal/people.org" "tasks")
             ,my/org-basic-task-template)
            ("j" "journal entry" plain
             (file+datetree "~/personal/journal.org")
             "%k - %a\n%i\n%?\n"
             :unnarrowed t)
            ("j" "journal entry with date" plain
             (file+datetree+prompt "~/personal/journal.org")
             "%k - %a\n%i\n%?\n"
             :unnarrowed t)
            ("s" "journal entry with date, scheduled" entry
             (file+datetree+prompt "~/personal/journal.org")
             "* \n%k - %a\n%t\t%i\n%?\n"
             :unnarrowed t)
            ("c" "protocol link" entry (file+headline ,org-default-notes-file "inbox")
             "* [[%:link][%:description]] \n\n#+begin_quote\n%i\n#+end_quote\n\n%?\n\ncaptured: %u")
            ("db" "done - business" entry
             (file+headline "~/personal/business.org" "tasks")
             "* done %^{task}\nscheduled: %^t\n%?")
            ("dp" "done - people" entry
             (file+headline "~/personal/people.org" "tasks")
             "* done %^{task}\nscheduled: %^t\n%?")
            ("dt" "done - task" entry
             (file+headline "~/personal/organizer.org" "inbox")
             "* done %^{task}\nscheduled: %^t\n%?")
            ("q" "quick note" item
             (file+headline "~/personal/organizer.org" "quick notes"))
            ("l" "ledger entries")
            ("lm" "mbna" plain
             (file "~/personal/ledger")
             "%(org-read-date) %^{payee}
    liabilities:mbna
    expenses:%^{account}  $%^{amount}
  " :immediate-finish t)
            ("ln" "no frills" plain
             (file "~/personal/ledger")
             "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * no frills
    liabilities:mbna
    assets:wayne:groceries  $%^{amount}
  " :immediate-finish t)
            ("lc" "cash" plain
             (file "~/personal/ledger")
             "%(org-read-date) * %^{payee}
    expenses:cash
    expenses:%^{account}  %^{amount}
  ")
            ("b" "book" entry
             (file+datetree "~/personal/books.org" "inbox")
             "* %^{title}  %^g
  %i
  *author(s):* %^{author} \\\\
  *isbn:* %^{isbn}

  %?

  *review on:* %^t \\
  %a
  %u"
             :clock-in :clock-resume)
            ("c" "contact" entry (file "~/personal/contacts.org")
             "* %(org-contacts-template-name)
  :properties:
  :email: %(my/org-contacts-template-email)
  :end:")
            ("n" "daily note" table-line (file+olp "~/personal/organizer.org" "inbox")
             "| %u | %^{note} |"
             :immediate-finish t)
            ("r" "notes" entry
             (file+datetree "~/personal/organizer.org")
             "* %?\n\n%i\n%u\n")))

    (bind-key "c-m-r" 'org-capture)

    ;; allow refiling in the middle(ish) of a capture

    ;; this lets me use c-c c-r to refile a capture and then jump to the new
    ;; location. i wanted to be able to file tasks under projects so that they
    ;; could inherit the quantified property that i use to track time (and any
    ;; beeminder-related properties too), but i also wanted to be able to clock in
    ;; on them.

    (defun my/org-refile-and-jump ()
      (interactive)
      (if (derived-mode-p 'org-capture-mode)
          (org-capture-refile)
        (call-interactively 'org-refile))
      (org-refile-goto-last-stored))
    (eval-after-load 'org-capture
      '(bind-key "c-c c-r" 'my/org-refile-and-jump org-capture-mode-map)))




  (progn
    ;;http://pages.sachachua.com/.emacs.d/sacha.html
    ;;http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/
    (progn



      (ert-deftest my/org-capture-prefill-template ()
        (should
         ;; it should fill things in one field at ia time
         (string=
          (my/org-capture-prefill-template
           "* todo %^{task}\nscheduled: %^t\n:properties:\n:effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:end:\n%?\n"
           "hello world")
          "* todo hello world\nscheduled: %^t\n:properties:\n:effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:end:\n%?\n"))

        (should
         (string=
          (my/org-capture-prefill-template
           "* todo %^{task}\nscheduled: %^t\n:properties:\n:effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:end:\n%?\n"
           "hello world" "<2015-01-01>")
          "* todo hello world\nscheduled: <2015-01-01>\n:properties:\n:effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:end:\n%?\n"))
        (should
         (string=
          (my/org-capture-prefill-template
           "* todo %^{task}\nscheduled: %^t\n:properties:\n:effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:end:\n%?\n"
           "hello world" "<2015-01-01>" "0:05")
          "* todo hello world\nscheduled: <2015-01-01>\n:properties:\n:effort: 0:05\n:end:\n%?\n")))

      (defun my/org-capture-prefill-template (template &rest values)
        "pre-fill template with values."
        (setq template (or template (org-capture-get :template)))
        (with-temp-buffer
          (insert template)
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "%\\("
                          "\\[\\(.+\\)\\]\\|"
                          "<\\([^>\n]+\\)>\\|"
                          "\\([ttuualiacxkkinff]\\)\\|"
                          "\\(:[-a-za-z]+\\)\\|"
                          "\\^\\({\\([^}]*\\)}\\)"
                          "?\\([ggttuuclp]\\)?\\|"
                          "%\\\\\\([1-9][0-9]*\\)"
                          "\\)") nil t)
            (if (cl-first values)
                (replace-match (cl-first values) nil t))
            (setq values (cl-rest values)))
          (buffer-string)))

      (defun my/org-get-current-refile-location ()
        "return the current entry as a location understood by org-refile."
        (interactive)
        (list (elt (org-heading-components) 4)
              (or buffer-file-name
                  (with-current-buffer (buffer-base-buffer (current-buffer))
                    buffer-file-name))
              nil
              (point)))

      (defun my/helm-org-create-task (candidate)
        "creates the task and returns the location."
        (let ((entry (org-capture-select-template "t")))
          (org-capture-set-plist entry)
          (org-capture-get-template)
          (org-capture-set-target-location)
          (condition-case error
              (progn
                (org-capture-put
                 :template
                 (org-capture-fill-template
                  (my/org-capture-prefill-template (org-capture-get :template)
                                                   candidate)))
                (org-capture-place-template
                 (equal (cl-first (org-capture-get :target)) 'function))
                (setq org-refile-target-table (org-refile-get-targets))
                ;; return the new location
                (my/org-get-current-refile-location))
            ((occ-error quit)
             (if (get-buffer "*capture*") (kill-buffer "*capture*"))
             (occ-error "capture abort: %s" error)))))

      ;; (my/org-refile-get-location-by-substring "try again")

      ;; next, i want to add this to the way that helm prompts me to refile. that
      ;; means that my creation task should return something ready for org-refile.
      ;; actually, maybe i don't have to do that if i know i'm always going to
      ;; call it when i want to jump to something. i might as well add that bit of
      ;; code that sets up clocking in, too.

      (defvar my/helm-org-refile-locations nil)
      (defvar my/org-refile-last-location nil)

      (defun my/helm-org-clock-in-and-track-from-refile (candidate)
        (let ((location (org-refile--get-location candidate my/helm-org-refile-locations)))
          (save-window-excursion
            (org-refile 4 nil location)
            (my/org-clock-in-and-track)
            t)))

      (defun my/org-get-todays-items-as-refile-candidates ()
        "return items scheduled for today, ready for choosing during refiling."
        (delq nil (mapcar #'(lambda (s)
                              (if (get-text-property 0 'org-marker s)
                                  (list s
                                        (buffer-file-name (marker-buffer (get-text-property 0 'org-marker s)))
                                        nil
                                        (marker-position (get-text-property 0 'org-marker s)))))
                          (save-window-excursion (my/org-get-entries-fn (calendar-current-date) (calendar-current-date))))))

      ;; based on http://emacs.stackexchange.com/questions/4063/how-to-get-the-raw-data-for-an-org-mode-agenda-without-an-agenda-view
      (defun my/org-get-entries-fn (begin end)
        "return org schedule items between begin and end.
usage:  (org-get-entries-fn '(6 1 2015) '(6 30 2015))"
        (require 'calendar)
        (require 'org)
        (require 'org-agenda)
        ;; (require 'cl)
        (unless
            (and
             (calendar-date-is-valid-p begin)
             (calendar-date-is-valid-p end))
          (let ((debug-on-quit nil))
            (signal 'quit `("one or both of your gregorian dates are invalid."))))
        (let* ((result nil)
               (org-agenda-prefix-format "  • ")
               (org-agenda-entry-types '(:scheduled))
               (date-after
                (lambda (date num)
                  "return the date after num days from date."
                  (calendar-gregorian-from-absolute
                   (+ (calendar-absolute-from-gregorian date) num))))
               (enumerate-days
                (lambda (begin end)
                  "enumerate date objects between begin and end."
                  (when (> (calendar-absolute-from-gregorian begin)
                           (calendar-absolute-from-gregorian end))
                    (occ-error "invalid period : %s - %s" begin end))
                  (let ((d begin) ret (cont t))
                    (while cont
                      (push (copy-sequence d) ret)
                      (setq cont (not (equal d end)))
                      (setq d (funcall date-after d 1)))
                    (nreverse ret)))))
          (org-agenda-reset-markers)
          (setq org-agenda-buffer
                (when (buffer-live-p org-agenda-buffer)
                  org-agenda-buffer))
          (org-compile-prefix-format nil)
          (setq result
                (cl-loop for date in (funcall enumerate-days begin end) append
                         (cl-loop for file in (org-agenda-files nil 'ifmode)
                                  append
                                  (progn
                                    (org-check-agenda-file file)
                                    (apply 'org-agenda-get-day-entries file date org-agenda-entry-types)))))
          (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
            (get-buffer-create org-agenda-buffer-name))
          (with-current-buffer (get-buffer org-agenda-buffer-name)
            (org-agenda-mode)
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (erase-buffer))
            (mapcar #'(lambda (x)
                        (let ((inhibit-read-only t))
                          (insert (format "%s" x) "\n")))
             result))
          ;;    (display-buffer org-agenda-buffer-name t)
          result))

      (defun my/helm-org-refile-read-location (tbl)
        (setq my/helm-org-refile-locations tbl)
        (helm
         (list
          ;; (helm-build-sync-source "today's tasks"
          ;;   :candidates (mapcar (lambda (a) (cons (cl-first a) a))
          ;;                       (my/org-get-todays-items-as-refile-candidates))
          ;;   :action '(("select" . identity)
          ;;             ("clock in and track" . my/helm-org-clock-in-and-track-from-refile)
          ;;             ("draw index card" . my/helm-org-prepare-index-card-for-subtree))
          ;;   :history 'org-refile-history)
          (helm-build-sync-source "refile targets"
            :candidates (mapcar (lambda (a) (cons (cl-first a) a)) tbl)
            :action '(("select" . identity)
                      ("clock in and track" . my/helm-org-clock-in-and-track-from-refile)
                      ("draw index card" . my/helm-org-prepare-index-card-for-subtree))
            :history 'org-refile-history)
          (helm-build-dummy-source "create task"
            :action (helm-make-actions
                     "create task"
                     'my/helm-org-create-task)))))

      (defun my/org-refile-get-location (&optional prompt default-buffer new-nodes no-exclude)
        "prompt the user for a refile location, using prompt.
  prompt should not be suffixed with a colon and a space, because
  this function appends the default value from
  `org-refile-history' automatically, if that is not empty.
  when no-exclude is set, do not exclude headlines in the current subtree,
  this is used for the goto interface."
        (let ((org-refile-targets org-refile-targets)
              (org-refile-use-outline-path org-refile-use-outline-path)
              excluded-entries)
          (when (and (derived-mode-p 'org-mode)
                     (not org-refile-use-cache)
                     (not no-exclude))
            (org-map-tree
             (lambda()
               (setq excluded-entries
                     (append excluded-entries (list (org-get-heading t t)))))))
          (setq org-refile-target-table
                ;; (org-refile-get-targets default-buffer excluded-entries)
                (org-refile-get-targets default-buffer)))
        (unless org-refile-target-table
          (user-error "no refile targets"))
        (let* ((cbuf (current-buffer))
               (partial-completion-mode nil)
               (cfn (buffer-file-name (buffer-base-buffer cbuf)))
               (cfunc (if (and org-refile-use-outline-path
                               org-outline-path-complete-in-steps)
                          'org-olpath-completing-read
                        'org-icompleting-read))
               (extra (if org-refile-use-outline-path "/" ""))
               (cbnex (concat (buffer-name) extra))
               (filename (and cfn (expand-file-name cfn)))
               (tbl (mapcar
                     (lambda (x)
                       (if (and (not (member org-refile-use-outline-path
                                             '(file full-file-path)))
                                (not (equal filename (nth 1 x))))
                           (cons (concat (cl-first x) extra " ("
                                         (file-name-nondirectory (nth 1 x)) ")")
                                 (cl-rest x))
                         (cons (concat (cl-first x) extra) (cl-rest x))))
                     org-refile-target-table))
               (completion-ignore-case t)
               cdef
               (prompt (concat prompt
                               (or (and (cl-first org-refile-history)
                                        (concat " (default " (cl-first org-refile-history) ")"))
                                   (and (assoc cbnex tbl) (setq cdef cbnex)
                                        (concat " (default " cbnex ")"))) ": "))
               pa answ parent-target child parent old-hist)
          (setq old-hist org-refile-history)
          ;; use helm's sources instead
          (setq answ (my/helm-org-refile-read-location tbl))
          (cond
           ((and (stringp answ)
                 (setq pa (org-refile--get-location answ tbl)))
            (org-refile-check-position pa)
            (when (or (not org-refile-history)
                      (not (eq old-hist org-refile-history))
                      (not (equal (cl-first pa) (cl-first org-refile-history))))
              (setq org-refile-history
                    (cons (cl-first pa) (if (assoc (cl-first org-refile-history) tbl)
                                         org-refile-history
                                       (cl-rest org-refile-history))))
              (if (equal (cl-first org-refile-history) (nth 1 org-refile-history))
                  (pop org-refile-history)))
            (setq my/org-refile-last-location pa)
            pa)
           ((and (stringp answ) (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ))
            (setq parent (match-string 1 answ)
                  child (match-string 2 answ))
            (setq parent-target (org-refile--get-location parent tbl))
            (when (and parent-target
                       (or (eq new-nodes t)
                           (and (eq new-nodes 'confirm)
                                (y-or-n-p (format "create new node \"%s\"? "
                                                  child)))))
              (org-refile-new-child parent-target child)))
           ((listp answ) answ) ;; sacha: helm returned a refile location
           ((not (equal answ t))
            (user-error "invalid target location")))))

      (fset 'org-refile-get-location-bkp 'org-refile-get-location)

      (fset 'org-refile-get-location 'my/org-refile-get-location)))







  (fset 'org-refile-get-location 'org-refile-get-location-bkp))



(when nil
  (cl-defgeneric occ-obj-display-number (i)
    "occ-obj-display-number")

  (cl-defmethod occ-obj-display-number ((i number))
    ;; (cl-call-next-method)
    (occ-debug "plain i=%d" i))

  (cl-defmethod occ-obj-display-number :extra "test0" ((i number))
                (cl-call-next-method)
                (occ-debug "test0 i=%d" i))

  (cl-defmethod occ-obj-display-number :extra "test1" ((i number))
                (cl-call-next-method)
                (occ-debug "test1 i=%d" i))

  (cl-defmethod occ-obj-display-number :extra "test2" ((i number))
                (cl-call-next-method)
                (occ-debug "test2 i=%d" i))

  (occ-obj-display-number 1))



(occ-testing

 (cl-defgeneric occ-test-combine (obj))


 (cl-defmethod occ-test-combine ((x symbol))
   (ignore x)
   (append (list 'symbol)
           (condition-case nil
               (cl-call-next-method)
             ((cl-no-next-method) nil))))
 ;; TODO: occ-prop-base.el: Warning: ‘cl-next-method-p’ is an obsolete macro (as of 25.1); make sure there’s always a next method, or catch ‘cl-no-next-method’ instead [7 times]

 (cl-defmethod occ-test-combine ((x (head z)))
   (ignore x)
   (append (list 'z)
           (condition-case nil
               (cl-call-next-method)
             ((cl-no-next-method) nil))))

 (cl-defmethod occ-test-combine ((x (eql a)))
   (ignore x)
   (append (list 'a)
           (condition-case nil
               (cl-call-next-method)
             ((cl-no-next-method) nil))))

 (let ((generic (cl--generic 'occ-test-combine)))
   (ignore (cl--generic-method-table generic)))

 (occ-test-combine '(z))

 (cl-generic-combine-methods (cl--generic 'occ-test-combine)
                             (let ((generic (cl--generic 'occ-test-combine)))
                               (ignore (cl--generic-method-table generic))))


 (cl-generic-call-method (cl--generic-make 'occz-combine) (list #'occ-test-combine))

 (cl-generic-call-method (cl--generic-make 'occ-test-combine)
                         #'occ-test-combine)


 (cl--generic-make 'occ-test1-combine)

 ())




;; (cl-defstruct bbc-class1
;;   (name1 "bbc-test"))

;; (cl-defstruct bbc-class2
;;   (name2 "bbc-test"))

;; (cl-defstruct (bbc-class-drived (:include bbc-class1 )  (:include bbc-class2 ) )
;;   (name3 "bbc-test"))


;;; occ-scratch-space.el ends here
