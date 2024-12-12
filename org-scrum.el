;;; org-scrum.el --- org mode extensions for scrum planning and reporting -*- lexical-binding: t -*-

;; Copyright (C) 2012-2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-scrum
;; Version: 0.1.1
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

(defcustom org-scrum-board-format 3
  "Specify the format of the scrum board items.
1. \"id\"
2. \"priority task (closedate)\"
3. \"id. priority task (closedate)\"
4. \"id. owner (closedate)\"
5. \"id. priority task (owner closedate)\""
  :type 'integer
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

(defun org-scrum--get-developers ()
  "Get list of developers as (name . wpd)."
  (mapcar                                       ; format return value
   (function
    (lambda (ii)
      (cons
       (capitalize (substring (car ii) 4 (length (car ii))))
       (string-to-number (cdr ii)))))
   (seq-filter                                  ; developer entries
    (lambda (ii)
      (and
       (>= (length (car ii)) 5)
       (string= (upcase (substring (car ii) 0 4)) "WPD-")))
    (org-entry-properties (point) 'standard)))) ; entries

(defun org-scrum--get-prop-value (match prop)
  "Sum values which match MATCH for property PROP in the TASKS tree."
  (apply
   #'+
   (mapcar                                      ; values
    #'string-to-number
    (seq-filter                                 ; entries with prop
     (lambda (ii) (> (length ii) 0))
     (org-scrum--visit-all-task-todos           ; entries
      (lambda () (org-entry-get (point) prop))
      match)))))

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

(defun org-scrum--get-work-left (cdate closed tot)
  "Get the actual work remaining for the date CDATE.
Computes work remaining given the list of closed items CLOSED and
total hours TOT.  Returns (list-of-closed-tasks
total-remaining-work)"
  (let (toremove)
    (dolist (item closed)
      ;; (message "item %s" (format-time-string "%Y-%m-%d %H:%M:%S" (car item)))
      (unless (time-less-p cdate (nth 0 item))
        (setq tot (- tot (nth 1 item)))
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

(defun org-scrum--make-scrum-board-entry ()
  "Make a scrum board entry from a TODO."
    (let* ((todo (org-entry-get (point) "TODO"))
           (todokwds (append org-not-done-keywords org-done-keywords))
           (hdg (org-scrum--extract-heading))
           (priority (if (nth 3 (org-heading-components)) ; lookup priority
                         (setq priority (concat "[#" (make-string 1 (nth 3 (org-heading-components))) "] "))
                       "[#Z] "))        ; default to lowest priority
           (closedate "")               ; close date without parens
           (closedateparens "")         ; close date with parens
           (indx (- (length todokwds)
                    (length (member todo todokwds))))
           (owner (org-entry-get (point) "OWNER"))
           (closetime (org-entry-get (point) "CLOSED"))
           (nn 0)
           label closestr)
      (unless (null closetime)          ; only include closedate for DONE items
        (setq closestr (mapcar
                        (function (lambda (x) (if (< (setq nn (1+ nn)) 4) 0 x)))
                        (parse-time-string closetime)) ; clear time of day
              closedate (format-time-string "%Y-%m-%d" (apply #'encode-time closestr))
              closedateparens (concat " (" closedate ")")))
      (setq label (cond                 ; scrum board label
         ((= 1 org-scrum-board-format) (org-entry-get (point) "TASKID"))
         ((= 2 org-scrum-board-format) (concat priority hdg closedateparens))
         ((= 3 org-scrum-board-format) (concat (org-entry-get (point) "TASKID") ". " priority hdg closedateparens))
         ((= 4 org-scrum-board-format) (concat (org-entry-get (point) "TASKID") ". " owner closedateparens))
         ((= 5 org-scrum-board-format) (concat (org-entry-get (point) "TASKID") ". " priority hdg " (" owner " " closedate ")"))))
      (if org-scrum-board-links
          (setq label (org-link-make-string (org-link-heading-search-string) label)))
      (cons label indx)))

(defun org-dblock-write:block-update-board (_params)
  "Generate scrum board."
  (interactive)
  (let* ((todos                    ; all todos. list of (label . indx)
          (org-scrum--visit-all-task-todos #'org-scrum--make-scrum-board-entry "TODO<>\"\""))
         (todokwds                      ; list of all todo keywords
          (append org-not-done-keywords org-done-keywords))
         (counts                        ; count for each todo kwd
          (make-list (length todokwds) 0))
         colstr topleft)
    (insert "| " (mapconcat #'identity todokwds "|") " |\n|-")
    (setq topleft (point))

    ;; count the number of scrum board entries in each column
    (dolist (item todos)
      (setcar (nthcdr (cdr item) counts)
              (1+ (nth (cdr item) counts))))

    (let* ((range (number-sequence 1 (length todokwds))) ; range will be '(1 2 3..)
           (newrow                                       ; newrow will be "| |  |   |..."
            (concat "|" (mapconcat (lambda (ii) (make-string ii ? )) range "|") "|")))
      (goto-char topleft)               ; lay out empty table rows
      (dotimes (_ii (seq-reduce (lambda (a b) (max a b)) counts 0))
        (insert (concat "\n" newrow)))) ; different number of spaces for each col

    (let* ((date-pat (rx (= 4 digit) ?- (= 2 digit) ?- (= 2 digit)))
           (capture-date-pat (rx (* nonl) (group (= 4 digit) ?- (= 2 digit) ?- (= 2 digit)) (* nonl)))
           (low-priority-pat (rx "[#Z] "))
           (closedtodos
            (sort                   ; sort closed tasks by date closed
             (seq-filter (lambda (ii) (string-match date-pat (car ii))) todos)
             (lambda (a b) (string< (replace-regexp-in-string capture-date-pat "\\1" (car b))
                                    (replace-regexp-in-string capture-date-pat "\\1" (car a))))))
           (opentodos
            (sort                       ; sort open tasks by priority
             (seq-filter (lambda (ii) (not (string-match date-pat (car ii)))) todos)
             (lambda (a b) (string< (nth 1 (split-string (car a) " ")) (nth 1 (split-string (car b) " "))))))
           (todos (append opentodos closedtodos)))

      (dolist (item todos)                  ; fill in table
        (when item
          (setcar item (replace-regexp-in-string low-priority-pat "" (car item)))
          (goto-char topleft)
          (setq colstr (concat "|" (make-string (1+ (cdr item)) ? ) "|"))
          (search-forward colstr)           ; find col based on number of spaces
          (forward-char -1)
          (insert (car item)))))
    (goto-char topleft)
    (org-ctrl-c-ctrl-c)))

(defun org-scrum--create-match (owner todos)
  "Get a match string for OWNER and sequence of todo keywords TODOS."
  (when (or owner todos)
    (let ((ownerstr (if owner (concat "OWNER={^" owner ".*}") "")))
      (if (> (length todos) 0)
          (mapconcat (lambda (ii) (concat ownerstr "+TODO=\"" ii "\"" )) todos "|")
        ownerstr))))

(defun org-dblock-write:block-update-summary (_params)
  "Generate developer summary table."
  (let ((developers (car (org-map-entries 'org-scrum--get-developers "ID=\"TASKS\"")))
        (est  0)                ; hours estimated
        (act  0)                ; actual hours spent
        (done 0)                ; hours of estimates that are done
        (rem  0))               ; hours of estimates that are left
    (if (= 0 (length developers))
        (error "No developers found (they must have WPD property)"))
    (insert "| NAME | ESTIMATED | ACTUAL | DONE | REMAINING | PENCILS DOWN | PROGRESS |\n|-")
    (dolist (developer developers)
      (setq est  (org-scrum--get-prop-value (org-scrum--create-match (car developer) (append org-not-done-keywords org-done-keywords)) "ESTIMATED"))
      (setq act  (org-scrum--get-prop-value (org-scrum--create-match (car developer) '()) "ACTUAL"))
      (setq done (org-scrum--get-prop-value (org-scrum--create-match (car developer) org-done-keywords) "ESTIMATED"))
      (setq rem  (org-scrum--get-prop-value (org-scrum--create-match (car developer) org-not-done-keywords) "ESTIMATED"))

      (insert "\n| " (car developer)
              " | " (number-to-string est)
              " | " (number-to-string act)
              " | " (number-to-string done)
              " | " (number-to-string rem)
              " | " (format-time-string "%Y-%m-%d" (org-scrum--get-finish-date rem (cdr developer)))
              " | " (org-scrum--draw-progress-bar est done)
              " |"))
    (org-ctrl-c-ctrl-c)))

(defun org-scrum--lookup-closed-tasks ()
  "Find tasks which have been closed.
This returns (time-closed estimated taskid) for each closed task found."
  (let* ((n 0)
         (closetime (org-entry-get (point) "CLOSED"))
         (closestr
          (if (null closetime)
              (error (concat "\"" (nth 4 (org-heading-components)) "\" is marked DONE but doesn't have a CLOSED date"))
            (mapcar
             (function (lambda (x) (if (< (setq n (1+ n)) 4) 0 x))) ; clear time of day
             (parse-time-string closetime)))))

    ;;(message "%s" (format-time-string "%Y-%m-%d" (apply #'encode-time closestr)))
    (list
     (apply #'encode-time closestr)
     (string-to-number (org-entry-get (point) "ESTIMATED"))
     (org-entry-get (point) "TASKID"))))

(defun org-scrum--compute-actual-burndown (start sprintlength tot)
  "Compute actual burndown for each day of the sprint.
The sprint starts at date START and lasts SPRINTLENGTH days.  TOT
is the total number of story points for the sprint."
  (let ((left tot)                      ; total actually left
        (closed (org-map-entries #'org-scrum--lookup-closed-tasks (org-scrum--create-match nil org-done-keywords))) ; list of (date est num) for each task that was completed
        (today (current-time))          ; know when today is because we can't fill in the future burndown
        (cdate start)                   ; the date of the current date as we iterate
        (day 0)                         ; counts the days as we iterate
        actual-burndown)                ; the list of burndown by day
    (while (<= day sprintlength)
      (setq cdate (time-add cdate (seconds-to-time 86400))) ; increment current day
      (if (time-less-p cdate today)
          (let* ((ret (org-scrum--get-work-left cdate closed left))
                (toremove (car ret))) ; save list of completed tasks
            (setq left (cdr ret)      ; save new total
                  closed (seq-remove (lambda (x) (seq-contains-p toremove x)) closed) ; remove tasks that have been counted
                  actual-burndown (cons (number-to-string left) actual-burndown)))
        (setq actual-burndown (cons "" actual-burndown)))
      (setq day (1+ day)))
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
    (let* ((count (seq-reduce (lambda (c ii) (if ii (1+ c) c)) ideal-burndown 0)) ; count weekdays
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
  (let* ((tot (org-scrum--get-prop-value nil "ESTIMATED")) ; total estimated hours
         cdate                 ; current date for iterating
         sprintlength)         ; number of calendar days in the sprint

    ;; look up start date and sprint length
    (org-map-entries (lambda ()
                       (setq cdate (time-subtract (apply #'encode-time (org-fix-decoded-time (parse-time-string (org-entry-get (point) "SPRINTSTART"))))
                                                  (seconds-to-time 86400))) ; day before sprint start
                       (setq sprintlength (string-to-number (org-entry-get (point) "SPRINTLENGTH"))))
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
                                (mapconcat #'identity
                                           (org-scrum--visit-all-task-todos #'org-scrum--generate-task-card "TODO<>\"\"")
                                           "\n")
                                "\n\\end{document}\n"))
      (with-temp-file "scrum_cards.tex"
        (insert tex-content))
      (shell-command "texi2pdf scrum_cards.tex"
                     (get-buffer-create "*Standard output*")))))

(defun org-scrum--generate-task-card ()
  (let* ((hdg (nth 4 (org-heading-components)))
         (bracket (string-match "\\[" hdg))               ; index of bracket character
         (id (or (org-entry-get (point) "TASKID") "\\_\\_\\_"))
         (owner (or (org-entry-get (point) "OWNER") "\\_\\_\\_"))
         (est (or (org-entry-get (point) "ESTIMATED") "\\_\\_\\_")))
    (if bracket
        (setq hdg (substring hdg 0 (1- bracket))))
    (format "
\\vspace{0.4in}
\\filbreak
\\begin{tabular}{l r}
  estimate: %s    & id: %s \\\\
  actual: \\_\\_\\_ & owner: %s \\\\
  \\hline
  \\multicolumn{2}{p{\\columnwidth}}{%s} \\\\
\\end{tabular}
" est id owner hdg)))

;;;###autoload
(defun org-scrum-update-all ()
  "Update all dynamic blocks in a scrum org file."
  (interactive)
  (save-excursion
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
      (org-ctrl-c-ctrl-c))))

(provide 'org-scrum)

;;; org-scrum.el ends here
