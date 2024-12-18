;;; org-scrum.el --- org mode extensions for scrum planning and reporting -*- lexical-binding: t -*-

;; Copyright (C) 2012-2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-scrum
;; Version: 0.1.3
;; Package-Requires: ((emacs "24.5") (org "8.2") (seq "2.3") (cl-lib "1.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides functions that extend org-mode which allow it to generate
;; reports used in the scrum software development process, such as a
;; scrum board and burndown chart.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'org)

;; custom variables

(defgroup org-scrum nil
  "Options for customizing scrum reports"
  :tag "Org Scrum"
  :group 'org)

(defcustom org-scrum-taskid-prefix "T"
  "Prefix added to taskids."
  :type 'string
  :group 'org-scrum)

(defcustom org-scrum-board-links nil
  "If true, make the items in the scrum board links."
  :type 'boolean
  :group 'org-scrum)

(defcustom org-scrum-board-format "%i. %p %t (%o)"
  "Specify the format of the scrum board items.
Provide a format string.  Variables are:
- %i is replaced by the task id
- %p is replaced by the priority
- %t is replaced by the task name
- %o is replaced by the task owner(s)
- %c is replaced by the close date

legacy formats (deprecated):
1. \"id\"
2. \"priority task (closedate)\"
3. \"id. priority task (closedate)\"
4. \"id. owner (closedate)\"
5. \"id. priority task (owner closedate)\""
  :type 'string
  :group 'org-scrum)

(defcustom org-scrum-ascii-graph t
  "If t, render the classic ascii burndown chart, else embed a fancy svg image."
  :type 'boolean
  :group 'org-scrum)

(defcustom org-scrum-ascii-size '(100 . 30)
  "The size (width . height) in characters to make ascii graphs."
  :type '(cons number number)
  :group 'org-scrum)

(defcustom org-scrum-image-size '(700 . 450)
  "The size (width . height) in pixels to make image graphs."
  :type '(cons number number)
  :group 'org-scrum)

;; dynamically scoped constants and variables and associated helper functions

(defvar org-scrum-developers nil
  "This is the list of developers as (name . capacity).")

(defvar org-scrum-tasks nil
  "This is the list of tasks as alists.")

;; functions

(defun org-scrum--get-developers ()
  "Get list of developers as (name . wpd)."
  (mapcar                               ; format return value
   (function
    (lambda (ii)
      (cons
       (capitalize (substring (car ii) 4 (length (car ii))))
       (string-to-number (cdr ii)))))
   (seq-filter                          ; developer entries
    (lambda (ii)
      (and
       (>= (length (car ii)) 5)
       (string= (upcase (substring (car ii) 0 4)) "WPD-")))
    (org-entry-properties (point) 'standard)))) ; entries

(defun org-scrum--load-all ()
  "Load task data into a list of tasks stored as alists."
  (setq org-scrum-developers (car (org-map-entries 'org-scrum--get-developers "ID=\"TASKS\""))
        org-scrum-tasks (org-scrum--visit-all-task-todos
                         #'org-scrum--load-task
                         "TODO<>\"\"")))

(defun org-scrum--load-task ()
  "Load the task at point into an alist."
  ;; taskid, owner, name, priority, state, estimate?, actual?, closedate (later: deps, swimlane)
  (let* ((state (org-entry-get (point) "TODO"))
         (taskid (org-entry-get (point) "TASKID"))
         (name (org-scrum--extract-heading))
         (priority (if (nth 3 (org-heading-components))
                       (make-string 1 (nth 3 (org-heading-components)))
                     "Z"))                      ; default to lowest priority
         (owner (org-entry-get (point) "OWNER"))
         (estimated (org-entry-get (point) "ESTIMATED"))
         (actual (org-entry-get (point) "ACTUAL"))
         (closed (org-entry-get (point) "CLOSED"))
         (nn 0)
         closestr closedate)
    (unless (null closed)          ; only include closedate for DONE items
      (setq closestr (mapcar
                      (function (lambda (ii) (if (< (setq nn (1+ nn)) 4) 0 ii)))
                      (parse-time-string closed)) ; clear time of day
            closedate (format-time-string "%Y-%m-%d" (apply #'encode-time closestr))))
    `((:taskid . ,taskid)
      (:name . ,name)
      (:priority . ,priority)
      (:owner . ,owner)
      (:state . ,state)
      (:estimated . ,estimated)
      (:actual . ,actual)
      (:closedate . ,closedate))))

(defun org-scrum--aggregate-value (all-tasks owner states prop)
  "Sum values of property PROP from tasks for OWNER in state STATES in ALL-TASKS."
  (apply                                ; sum
   #'+
   (mapcar                              ; convert to numbers
    #'string-to-number
    (seq-filter                         ; filter out nils
     #'identity
     (mapcar                            ; convert to value
      (lambda (ii) (alist-get prop ii))
      (org-scrum--find-tasks all-tasks owner states))))))

(defun org-scrum--find-tasks (all-tasks owner states)
  "Find tasks for OWNER in state STATES in ALL-TASKS.
OWNER and STATES will be ignored if nil."
  (if (or owner states)
      (seq-filter                   ; filter by owner and state
       (lambda (ii)
         (and
          (if owner
              (and
               owner
               (alist-get :owner ii)
               (string-match-p owner (alist-get :owner ii)))
            t)
          (if states
              (member (alist-get :state ii) states)
            t)))
       all-tasks)
    all-tasks))

(defun org-scrum--get-finish-date (hours wpd)
  "Count the days to get HOURS work done at WPD hours per day, skipping weekends."
  (let ((ret (current-time))
        (hoursleft hours)
        ctime)
    (while (> hoursleft 0)
      (setq ret (time-add ret (seconds-to-time 86400))
            ctime (decode-time ret))
      (if (and (> (nth 6 ctime) 0) (< (nth 6 ctime) 6))
          (setq hoursleft (- hoursleft wpd))))
    ret))

(defun org-scrum--get-work-left (cdate closed-tasks tot)
  "Get the actual work remaining for the date CDATE.
Computes work remaining given the list of closed items
CLOSED-TASKS and total hours TOT.  Returns (list-of-closed-tasks .
total-remaining-work)"
  (let (nn closetime toremove)
    (dolist (item closed-tasks)
      (setq nn 0
            closetime (apply #'encode-time
                             (mapcar
                              (function (lambda (ii) (if (< (setq nn (1+ nn)) 4) 0 ii))) ; clear time of day
                              (parse-time-string
                               (alist-get :closedate item)))))
      (unless (time-less-p cdate closetime)
        (setq tot (- tot (string-to-number (alist-get :estimated item))))
        (push item toremove)))
    (cons toremove tot)))

(defun org-scrum--draw-progress-bar (est done)
  "Draw a progress bar in the summary table with EST total and DONE complete."
  (let ((width 10)
        (blocksdone 5))
    (setq blocksdone (round (* (if (= 0 est) 0 (/ (* done 1.0) est)) width)))
    (concat
     (apply #'concat (make-list blocksdone "#"))
     (apply #'concat (make-list (- width blocksdone) "-")))))

(defun org-scrum--visit-all-task-todos (fcn match)
  "Call FCN for all tasks in the current tree that match MATCH."
  (save-excursion
    (let (tasks)
      (org-map-entries (lambda () (setq tasks (point))) "ID=\"TASKS\"")
      (goto-char tasks)
      ;;(message "visiting %s" (buffer-substring (point) (line-end-position)))
      (org-map-entries fcn match 'tree))))

(defun org-scrum--extract-heading ()
  "Extract the heading from the current entry"
  (let* ((fullhdg (nth 4 (org-heading-components)))
         (bracket (string-match "\\[" fullhdg)) ; index of bracket character
         (hdg (if bracket
                  (substring fullhdg 0 (min (1- bracket) ))
                fullhdg))
         (maxlen 30)) ; max length of scrum board task name
    (substring fullhdg 0 (min (length hdg) maxlen))))

(defun org-scrum--make-scrum-board-label (task)
  "Make a scrum board entry from the given TASK."
  (let* ((taskid (alist-get :taskid task))
         (name (alist-get :name task))
         (priority (alist-get :priority task))
         (owner (alist-get :owner task))
         (closedate (alist-get :closedate task))
         label)
    (setq label (cond                 ; scrum board label
                 ((string= "1" org-scrum-board-format) taskid)
                 ((string= "2" org-scrum-board-format) (format "[#%s] %s (%s)" priority name closedate))
                 ((string= "3" org-scrum-board-format) (format "%s. [#%s] %s (%s)" taskid priority name closedate))
                 ((string= "4" org-scrum-board-format) (format "%s. %s (%s)" taskid (or owner "not assigned") closedate))
                 ((string= "5" org-scrum-board-format) (format "%s. [#%s] %s (%s %s)" taskid priority name owner closedate))
                 (t (setq label (replace-regexp-in-string "%i" taskid org-scrum-board-format)
                          label (replace-regexp-in-string "%p" (or (and priority (string-trim priority))  "") label)
                          label (replace-regexp-in-string "%t" name label)
                          label (replace-regexp-in-string "%o" (or owner "") label)
                          label (replace-regexp-in-string "%c" (or closedate "") label)))))
    (setq label (replace-regexp-in-string "\\[Z\\] " "" label)                                       ; clean up placeholder low priority
          label (replace-regexp-in-string (rx (* space) (any "([{") (* space) (any "}])")) "" label) ; clean up empty parens
          label (replace-regexp-in-string (rx (group (any "([{")) (+ space)) "\\1" label)            ; clean up space after open paren
          label (replace-regexp-in-string (rx (+ space) (group (any "}])"))) "\\1" label))           ; clean up space before close paren


    (if org-scrum-board-links
        (setq label (org-link-make-string (org-link-heading-search-string) label)))
    label))

(defun org-dblock-write:block-update-board (_params)
  "Generate scrum board."
  (let* ((loadp (not (and org-scrum-developers org-scrum-tasks)))
         (todokwds                      ; list of all todo keywords
          (append org-not-done-keywords org-done-keywords))
         tasks-by-state)
    (when loadp
      (org-scrum--load-all))

    (setq tasks-by-state (seq-group-by (lambda (ii) (alist-get :state ii)) org-scrum-tasks))

    ;; set up the table
    (insert "| " (mapconcat #'identity todokwds "|") "\n|-\n|")
    (org-table-align)
    (org-table-analyze)

    (dolist (item tasks-by-state)
      (let* ((state (car item))
            (index (- (length todokwds)
                      (length (member state todokwds))
                      -1))
            (state-tasks
             (if (member state org-not-done-keywords)
                 (sort              ; sort open tasks by priority
                  (cdr item)
                  (lambda (a b) (string> (alist-get :priority b)
                                         (alist-get :priority a))))
               (sort                ; sort closed tasks by date closed
                (cdr item)
                (lambda (a b) (string> (alist-get :closedate b)
                                       (alist-get :closedate a)))))))
        (org-table-goto-field (format "@2$%d" index))
        (dolist (item state-tasks)
          (unless (eq item (car state-tasks))
            (org-table-next-row))
          (let ((label (org-scrum--make-scrum-board-label item)))
            (insert label)))))
    (org-table-align)

    (when loadp
      (setq org-scrum-developers nil
            org-scrum-tasks nil))))

(defun org-dblock-write:block-update-summary (_params)
  "Generate developer summary table."
  (let ((loadp (not (and org-scrum-developers org-scrum-tasks)))
        (est  0)                ; hours estimated
        (act  0)                ; actual hours spent
        (done 0)                ; hours of estimates that are done
        (rem  0))               ; hours of estimates that are left

    (when loadp
      (org-scrum--load-all))
    (when (= 0 (length org-scrum-developers))
        (error "No developers found (they must have WPD property)"))

    (insert "| NAME | ESTIMATED | ACTUAL | DONE | REMAINING | PENCILS DOWN | PROGRESS |\n|-")
    (dolist (developer org-scrum-developers)
      (setq est  (org-scrum--aggregate-value org-scrum-tasks (car developer) (append org-not-done-keywords org-done-keywords) :estimated))
      (setq act  (org-scrum--aggregate-value org-scrum-tasks (car developer) '()  :actual))
      (setq done (org-scrum--aggregate-value org-scrum-tasks (car developer) org-done-keywords :estimated))
      (setq rem  (org-scrum--aggregate-value org-scrum-tasks (car developer) org-not-done-keywords :estimated))

      (insert "\n| " (car developer)
              " | " (number-to-string est)
              " | " (number-to-string act)
              " | " (number-to-string done)
              " | " (number-to-string rem)
              " | " (format-time-string "%Y-%m-%d" (org-scrum--get-finish-date rem (cdr developer)))
              " | " (org-scrum--draw-progress-bar est done)
              " |"))
    (org-table-align)
    (when loadp
      (setq org-scrum-developers nil
            org-scrum-tasks nil))))

(defun org-scrum--compute-actual-burndown (start sprintlength tot)
  "Compute actual burndown for each day of the sprint.
The sprint starts at date START and lasts SPRINTLENGTH days.  TOT
is the total number of story points for the sprint."
  (let ((loadp (not org-scrum-tasks))
        (left tot)                      ; total actually left
        (today (current-time))          ; know when today is because we can't fill in the future burndown
        (cdate start)                   ; the date of the current date as we iterate
        (day 0)                         ; counts the days as we iterate
        closed-tasks
        actual-burndown)                ; the list of burndown by day
    (when loadp
      (org-scrum--load-all))
    (setq closed-tasks (org-scrum--find-tasks org-scrum-tasks nil org-done-keywords))
    (while (<= day sprintlength)
      (setq cdate (time-add cdate (seconds-to-time 86400))) ; increment current day
      (if (time-less-p cdate today)
          (let* ((ret (org-scrum--get-work-left cdate closed-tasks left))
                (toremove (car ret))) ; save list of completed tasks
            (setq left (cdr ret)      ; save new total
                  closed-tasks (seq-remove (lambda (ii) (seq-contains-p toremove ii)) closed-tasks) ; remove tasks that have been counted
                  actual-burndown (cons (number-to-string left) actual-burndown)))
        (setq actual-burndown (cons "" actual-burndown)))
      (setq day (1+ day)))
    (when loadp
      (setq org-scrum-developers nil
            org-scrum-tasks nil))
    (reverse actual-burndown)))

(defun org-scrum--compute-ideal-burndown (start sprintlength tot)
  "Compute ideal burndown for each day of the sprint.
The sprint starts at START and lasts SPRINTLENGTH days.  TOT is
the total number of story points for the sprint."
  (let ((cdate start)                   ; the current date as we iterate
        (day 0)                         ; counts the days as we iterate
        ctime                           ; holds extracted date components
        ideal-burndown)                 ; the list of burndown by day
    ;; find weekdays. make `ideal-burndown' a list of days where each weekday is t
    (while (< day sprintlength)
      (setq ctime (decode-time cdate)
            ideal-burndown (cons (and (> (nth 6 ctime) 0) (< (nth 6 ctime) 6)) ideal-burndown)
            cdate (time-add cdate (seconds-to-time 86400)) ; increment current day
            day (1+ day)))              ; increment day counter

    ;; compute the ideal burndown rate and use it to fill in `ideal-burndown'
    (let* ((count (seq-reduce (lambda (cc ii) (if ii (1+ cc) cc)) ideal-burndown 0)) ; count weekdays
           (rate  (/ (float tot) count))
           (left tot))
      (dotimes (day (length ideal-burndown))
        (if (nth day ideal-burndown)
            (setq left (max 0 (- left rate))))
        (setcar (nthcdr day ideal-burndown) left)))
    ideal-burndown))

(defun org-scrum--compute-burndown ()
  "Compute ideal and actual burndown for each day of the sprint.
Returns a list of (date actual ideal)."
  (let* ((tot (org-scrum--aggregate-value org-scrum-tasks nil nil :estimated)) ; total estimated hours
         cdate                 ; current date for iterating
         sprintlength)         ; number of calendar days in the sprint

    ;; look up start date and sprint length
    (org-map-entries (lambda ()
                       (setq cdate (time-subtract
                                    (apply
                                     #'encode-time
                                     (org-fix-decoded-time (parse-time-string (org-entry-get (point) "SPRINTSTART"))))
                                    (seconds-to-time 86400)) ; day before sprint start
                             sprintlength (string-to-number (org-entry-get (point) "SPRINTLENGTH"))))
                     "ID=\"TASKS\"")
    (if (or (null cdate) (null sprintlength))
        (error "Couldn't find node with ID=\"TASKS\" containing \"SPRINTLENGTH\" and \"SPRINTSTART\" properties"))

    (cl-mapcar #'list ; this zips the lists together
            (number-sequence 1 sprintlength)
            (org-scrum--compute-actual-burndown cdate sprintlength tot)
            (org-scrum--compute-ideal-burndown cdate sprintlength tot))))

(defun org-scrum--check-gnuplot-exists ()
  "Check if gnuplot is installed on the system."
  (unless (eq 0 (call-process-shell-command "gnuplot --version"))
    (error "Cannot find gnuplot")))

(defun org-scrum--make-gnuplot-config (burndown-data)
  "Write a gnuplot config (including inline data taken from
BURNDOWN-DATA) to the current buffer, which should be empty."
  (cond (org-scrum-ascii-graph
         (insert (format "set term dumb size %d, %d\n\n" (car org-scrum-ascii-size) (cdr org-scrum-ascii-size)))
         (insert "set style line 1 lt 1 lw 1\n")
         (insert "set style line 2 lt 2 lw 1\n"))
        (t
         (insert (format "set term svg size %d, %d background \"white\"\n\n"
                         (car org-scrum-image-size)
                         (cdr org-scrum-image-size)))
         (insert (format "set output \"burndown.svg\"\n"))
         (insert "set style line 1 lt -1 lw 2 lc \"#399320\"\n")
         (insert "set style line 2 lt 0 lw 1\n")))
  (insert "set title \"Burndown\"\n")
  (insert "set xlabel \"day\"\n")
  (insert "set ylabel \"points\"\n")
  (insert (format "set xrange [1:%d]\n" (length burndown-data)))
  (insert "plot \"-\" using 1:2 with lines ls 1 title \"actual\", \"\" using 1:3 with lines ls 2 title \"ideal\"\n")

  (insert (mapconcat (lambda (row) (apply #'format (cons "%d %s %s" row)))
                     burndown-data
                     "\n"))
  (insert "\ne\n")
  ;; gnuplot makes you include the data twice if you want to plot two lines and provide the data inline
  (insert (mapconcat (lambda (row) (apply #'format (cons "%d %s %s" row)))
                     burndown-data
                     "\n"))
  (insert "\ne\n"))

(defun org-dblock-write:block-update-graph (_params)
  "Generate burndown chart."

  (org-scrum--check-gnuplot-exists)

  (goto-char (point-min))
  (re-search-forward "#\\+BEGIN: .*block-update-graph") ; must exist
  (forward-line 1)                                      ; move into dynamic block

  (let ((buffer (current-buffer))
        (burndown-data (org-scrum--compute-burndown))
        (pt (point)))
    (with-temp-buffer
      (org-scrum--make-gnuplot-config burndown-data)
      (call-process-region (point-min) (point-max) "gnuplot" nil buffer)
      (set-buffer buffer)
      (if (not org-scrum-ascii-graph)
          (progn
            (insert "[[./burndown.svg]]")      ; using an org link instead of embedding the image
            (org-display-inline-images))       ; so it gets exported correctly
        (goto-char pt)
        (while (re-search-forward "\f" nil t)  ; delete the formfeed in gnuplot output
          (replace-match ""))
        (while (not (looking-at "#\\+END"))    ; prefix graph lines so org exports them cleanly
          (insert ":")
          (forward-line 1))
        (save-restriction
          (narrow-to-region pt (point))
          (goto-char pt)                       ; the default linestyle with dots also has '+'s at the points
          (while (re-search-forward "#" nil t) ; which make the graph busier, so we use '#' and then swap them
            (replace-match "\.")))))))         ; for dots

(defun org-scrum-reset-taskids ()
  "Replace taskids of all todos in the tasks tree with consecutive values."
  (interactive)
  (save-excursion
    (let ((ii 1))
      (org-scrum--visit-all-task-todos (lambda ()
                                     (org-entry-put (point) "TASKID" (format "%s%02d" org-scrum-taskid-prefix ii))
                                     (setq ii (1+ ii)))
                                   "TODO<>\"\""))))


(defun org-scrum-generate-task-cards ()
  "Generate scrum board task cards in latex format.

This depends on latex and the multirow style installed on the
system. The result is the file \"scrum_cards.pdf\"."
  (interactive)
  (save-excursion
    (let (tex-content)
      (setq tex-content (concat "
\\documentclass[letterpaper,12pt]{article}
\\usepackage[top=0.75in, bottom=0.75in, left=0.75in, right=0.75in]{geometry}
\\setlength{\\columnsep}{0.75in}
\\let\\TAB\\tabular
\\renewcommand\\tabular{\\noindent\\TAB}
\\usepackage{multirow}
\\begin{document}
\\pagestyle{empty}
\\twocolumn
\n"
                                (mapconcat #'org-scrum--generate-task-card
                                           org-scrum-tasks
                                           "\n")
                                "\n\\end{document}\n"))
      (with-temp-file "scrum_cards.tex"
        (insert tex-content))
      (shell-command "texi2pdf scrum_cards.tex"
                     (get-buffer-create "*Standard output*")))))

(defun org-scrum--generate-task-card (task)
  (let* ((name (alist-get :name task))
         (id (or (alist-get :taskid task) "\\_\\_\\_"))
         (owner (or (alist-get :owner task) "\\_\\_\\_\\_\\_"))
         (est (or (alist-get :estimated task) "\\_\\_\\_")))
    (format "
\\vspace{0.4in}
\\filbreak
\\begin{tabular}{l r}
  estimate: %s    & id: %s \\\\
  actual: \\_\\_\\_ & owner: %s \\\\
  \\hline
  \\multicolumn{2}{p{\\columnwidth}}{%s} \\\\
\\end{tabular}
" est id owner name)))

;;;###autoload
(defun org-scrum-update-all ()
  "Update all dynamic blocks in a scrum org file."
  (interactive)
  (save-excursion

    (org-scrum--load-all)

    (let (found)
      (goto-char (point-min))
      (setq found (re-search-forward "#\\+BEGIN: columnview .* :id \"TASKS\"" nil t))
      (if (not found)
          (error "A columnview with \"TASKS\" id not found"))
      (org-ctrl-c-ctrl-c)
      (goto-char (point-min))
      (setq found (re-search-forward "#\\+BEGIN: block-update-summary" nil t))
      (if (not found)
          (error "\"block-update-summary\" not found"))
      (org-ctrl-c-ctrl-c)
      (goto-char (point-min))
      (setq found (re-search-forward "#\\+BEGIN: block-update-board" nil t))
      (if (not found)
          (error "\"block-update-board\" not found"))
      (org-ctrl-c-ctrl-c)
      (goto-char (point-min))
      (setq found (re-search-forward "#\\+BEGIN: block-update-graph" nil t))
      (if (not found)
          (error "\"block-update-graph\" not found"))
      (org-ctrl-c-ctrl-c))

    (setq org-scrum-developers nil
          org-scrum-tasks nil)))

(provide 'org-scrum)

;;; org-scrum.el ends here
