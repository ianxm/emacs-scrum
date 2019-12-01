;;; org-scrum.el --- org mode extensions for scrum planning and reporting -*- lexical-binding: t -*-

;; Copyright (C) 2012-2019 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-scrum
;; Version: 0.0.7
;; Package-Requires: ((emacs "24.5") (org "8.2") (gnuplot "0.6") (seq "2.20"))

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
(require 'gnuplot)
(require 'org)

(defgroup org-scrum nil
  "Scrum reporting options"
  :tag "Scrum"
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
(defun org-scrum--get-developers ()
  "Get list of developers as (name . wpd)."
  (let (ret)
    (setq ret (org-entry-properties (point) 'standard))
    (setq ret (seq-filter (lambda (ii)                  ; filter out non-developer properties
                           (and
                            (>= (length (car ii)) 5)
                            (string= (upcase (substring (car ii) 0 4)) "WPD-")))
                         ret))
    (setq ret (mapcar (function (lambda (ii) (cons     ; remove the `wpd-' prefix to get the name
                                              (capitalize (substring (car ii) 4 (length (car ii))))
                                              (string-to-number (cdr ii)))))
                      ret))
    ret))

(defun org-scrum--get-prop-value (match prop)
  "Sum values which match MATCH for property PROP in the TASKS tree."
  (let ((val 0)
        ret)
    (setq ret (org-scrum--visit-all-task-todos (lambda () (org-entry-get (point) prop)) match))
    (setq ret (seq-filter (lambda (ii) (> (length ii) 0)) ret))
    (while ret
      (setq val (+ val (string-to-number (pop ret)))))
    val))

(defun org-scrum--get-finish-date (hours wpd)
  "Count the days to get HOURS work done at WPD hours per day, skipping weekends."
  (let ((ret (current-time))
        (hoursleft hours)
        ctime)
    (while (> hoursleft 0)
      (setq ret (time-add ret (seconds-to-time 86400)))
      (setq ctime (decode-time ret))
      (if (and (> (nth 6 ctime) 0) (< (nth 6 ctime) 6))
          (setq hoursleft (- hoursleft wpd))))
    ret))

(defun org-scrum--get-work-left (cdate closed tot)
  "Get the actual work remaining for the date CDATE given the list of closed items CLOSED and total hours TOT."
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

(defun org-scrum--make-scrum-board-entry ()
  "Make a scrum board entry from a TODO."
  (let* ((todo (org-entry-get (point) "TODO"))
         (todokwds (append org-not-done-keywords org-done-keywords))
         (hdg (nth 4 (org-heading-components)))
         (bracket (string-match "\\[" hdg))               ; index of bracket character
         (maxlen 30)                                      ; max length of scrum board task name
         (priority "[#Z] ")                               ; default to lowest priority
         (closedate "")                                   ; close date without parens
         (closedateparens "")                             ; close date with parens
         owner label indx)
    (if bracket
        (setq hdg (substring hdg 0 (1- bracket))))
    (setq indx (- (length todokwds)
                  (length (member todo todokwds))))
    (setq owner (org-entry-get (point) "OWNER"))
    (setq hdg (substring hdg 0 (min (length hdg) maxlen)))                ; truncate heading
    (if (nth 3 (org-heading-components))                                  ; lookup priority
        (setq priority (concat "[#" (make-string 1 (nth 3 (org-heading-components))) "] ")))
    (let ((n 0)                                                           ; get close date
          closetime closestr)
      (setq closetime (org-entry-get (point) "CLOSED"))
      (unless (null closetime)
        (setq closestr (parse-time-string closetime))
        (setq closestr (mapcar (function (lambda (x) (if (< (setq n (1+ n)) 4) 0 x))) closestr)) ; clear time of day
        (setq closedate (format-time-string " %Y-%m-%d" (apply #'encode-time closestr)))
        (setq closedateparens (format-time-string " (%Y-%m-%d)" (apply #'encode-time closestr)))))
    (cond                                                                 ; scrum board label
     ((= 1 org-scrum-board-format) (setq label (org-entry-get (point) "TASKID")))
     ((= 2 org-scrum-board-format) (setq label (concat priority hdg " " closedateparens)))
     ((= 3 org-scrum-board-format) (setq label (concat (org-entry-get (point) "TASKID") ". " priority hdg closedateparens)))
     ((= 4 org-scrum-board-format) (setq label (concat (org-entry-get (point) "TASKID") ". " owner closedateparens)))
     ((= 5 org-scrum-board-format) (setq label (concat (org-entry-get (point) "TASKID") ". " priority hdg " (" owner closedate ")"))))
    (if org-scrum-board-links
        (setq label (org-make-link-string (org-make-org-heading-search-string) label)))
    (cons label indx)))

(defun org-dblock-write:block-update-board (_params)
  "Generate scrum board."
  (interactive)
  (let* (todos                            ; all todos. list of (label . indx)
         (todokwds (append org-not-done-keywords org-done-keywords)) ; list of all todo keywords
         (counts (make-list (length todokwds) 0)) ; count for each todo kwd
         colstr topleft)
    (insert "| " (mapconcat #'identity todokwds "|") " |\n|-")
    (setq topleft (point))
    (setq todos (org-scrum--visit-all-task-todos #'org-scrum--make-scrum-board-entry "TODO<>\"\""))

    ;; count the number of scrum board entries in each column
    (dolist (item todos)
      (setcar (nthcdr (cdr item) counts)
              (1+ (nth (cdr item) counts))))

    (let ((range (number-sequence 1 (length todokwds))) ; range will be '(1 2 3..)
          newrow)                                     ; newrow will be "| |  |   |..."
      (setq newrow (concat "|" (mapconcat (lambda (ii) (make-string ii ? )) range "|") "|"))
      (goto-char topleft)                 ; lay out empty table rows
      (dotimes (_ii (seq-reduce (lambda (a b) (max a b)) counts 0))
        (insert (concat "\n" newrow))))   ; different number of spaces for each col

    (let (opentodos                       ; todos that arent closed
          closedtodos)                    ; todos that are closed
      (setq closedtodos (seq-filter (lambda (ii) (string-match "[0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}" (car ii))) todos))
      (setq opentodos (seq-filter (lambda (ii) (not (string-match "[0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}" (car ii)))) todos))
                                          ; sort closed tasks by date closed
      (setq closedtodos (sort closedtodos (lambda (a b) (string< (replace-regexp-in-string ".*\\([0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}\\).*" "\\1" (car b))
                                                                 (replace-regexp-in-string ".*\\([0-9]\\{4\\}\\-[0-9]\\{2\\}\\-[0-9]\\{2\\}\\).*" "\\1" (car a))))))
                                          ; sort open tasks by priority
      (setq opentodos (sort opentodos (lambda (a b) (string< (nth 1 (split-string (car a) " ")) (nth 1 (split-string (car b) " "))))))
      (setq todos (append opentodos closedtodos)))

    (dolist (item todos)                  ; fill in table
      (when item
        (setcar item (replace-regexp-in-string "\\[#Z\\] " "" (car item)))
        (goto-char topleft)
        (setq colstr (concat "|" (make-string (1+ (cdr item)) ? ) "|"))
        (search-forward colstr)       ; find col based on number of spaces
        (forward-char -1)
        (insert (car item))))
    (goto-char topleft)
    (org-ctrl-c-ctrl-c)))

(defun org-scrum--create-match (owner todos)
  "Get a match string for OWNER and sequence of todo keywords TODOS."
  (when (or owner todos)
    (let (ownerstr)
      (setq ownerstr (if owner (concat "OWNER={^" owner ".*}") ""))
      (if (> (length todos) 0)
          (mapconcat (lambda (ii) (concat ownerstr "+TODO=\"" ii "\"" )) todos "|")
        ownerstr))))

(defun org-dblock-write:block-update-summary (_params)
  "Generate scrum summary table."
  (let (developers
        (est  0)                ; hours estimated
        (act  0)                ; actual hours spent
        (done 0)                ; hours of estimates that are done
        (rem  0))               ; hours of estimates that are left
    (setq developers (car (org-map-entries 'org-scrum--get-developers "ID=\"TASKS\"")))
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
  (let ((n 0)
        closetime closestr)
    (setq closetime (org-entry-get (point) "CLOSED"))
    (if (null closetime)
        (error (concat "\"" (nth 4 (org-heading-components)) "\" is marked DONE but doesn't have a CLOSED date")))
    (setq closestr (parse-time-string closetime))
    (setq closestr (mapcar (function (lambda (x) (if (< (setq n (1+ n)) 4) 0 x))) closestr)) ; clear time of day
    ;;(message "%s" (format-time-string "%Y-%m-%d" (apply #'encode-time closestr)))
    (list
     (apply #'encode-time closestr)
     (string-to-number (org-entry-get (point) "ESTIMATED"))
     (org-entry-get (point) "TASKID"))))

(defun org-dblock-write:block-update-burndown (_params)
  "Generate burndown table."
  (insert "| DAY | DATE | ACTUAL | IDEAL | TASKS COMPLETED |\n|-")
  (let ((day 1)               ; day index
        (today (current-time))
        tot                   ; total hours of estimates
        totleft               ; total left
        sprintlength          ; number of calendar days in the sprint
        closed                ; list of (date est num) for each task that was completed
        toremove              ; list of (date est num) for each task that has been counted and can be removed
        cdate)                ; current date for iterating

    (setq tot (org-scrum--get-prop-value nil "ESTIMATED"))
    (setq totleft tot)
    (org-map-entries (lambda () ; look up start date and sprint length
                       (setq cdate (time-subtract (apply #'encode-time (org-fix-decoded-time (parse-time-string (org-entry-get (point) "SPRINTSTART"))))
                                                  (seconds-to-time 86400))) ; day before sprint start
                       (setq sprintlength (string-to-number (org-entry-get (point) "SPRINTLENGTH"))))
                     "ID=\"TASKS\"")
    (if (or (null cdate) (null sprintlength))
        (error "Couldn't find node with ID=\"TASKS\" containing \"SPRINTLENGTH\" and \"SPRINTSTART\" properties"))

    (setq closed (org-map-entries #'org-scrum--lookup-closed-tasks (org-scrum--create-match nil org-done-keywords)))
    (while (<= day sprintlength)
      ;; (message "cdate %d %s" day (format-time-string "%Y-%m-%d %H:%M:%S" cdate))
      (setq cdate (time-add cdate (seconds-to-time 86400))) ;; increment current day
      (setq toremove nil)
      (insert "\n| " (number-to-string day)
              " | " (format-time-string "%Y-%m-%d" cdate)
              " | " (if (time-less-p cdate today)
                        (let ((ret (org-scrum--get-work-left cdate closed tot)))
                          (setq toremove (car ret))                   ;; save list of completed tasks
                          (setq tot (cdr ret))                        ;; save new total
                          (if toremove                                ;; remove completed from master list
                              (dolist (item toremove)
                                (setq closed (delq item closed))))
                          (number-to-string tot))
                      "")
              " | " (number-to-string (round (- totleft (* totleft (/ day (* 1.0 sprintlength))))))
              " | " (mapconcat (function (lambda (ii) (nth 2 ii))) toremove " ")
              " | " )
      (setq day (1+ day)))
    (org-ctrl-c-ctrl-c)))

(defun org-dblock-write:block-update-graph (_params)
  "Generate burndown chart."
  (save-excursion
    (let ((fname "burndown.plt")
          pt                    ; the point
          found                 ; true if plot block is found
          sprintlength)         ; calendar days in sprint
      (goto-char (point-min))
      (setq found (re-search-forward "#\\+PLOT: .*title:\"Burndown\"" nil t))
      (if (not found)
          (error "PLOT block not found"))
      (org-map-entries (lambda () ; look up start date and sprint length
                         (setq sprintlength (string-to-number (org-entry-get (point) "SPRINTLENGTH"))))
                       "ID=\"TASKS\"")
      (if (null sprintlength)
          (error "Couldn't find node with ID=\"TASKS\" containing \"SPRINTLENGTH\" and \"SPRINTSTART\" properties"))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "#\\+PLOT: .*title:\"Burndown\"")
        (search-forward "set:\"xrange ")
        (forward-char 3)
        (while (not (looking-at "]"))
          (delete-char 1))
        (insert (number-to-string sprintlength)))
      (org-plot/gnuplot)
      (when (file-exists-p fname)
        (goto-char (point-min))
        (re-search-forward "#\\+BEGIN: .*block-update-graph")   ; must exist
        (forward-line 1)                            ; move into dynamic block
        (setq pt (point))
        (insert-file-contents fname)
        (delete-file fname)                         ; del temp file
        (delete-char 1)                             ; form feed
        (while (not (looking-at "#\\+END"))
          (insert ":")
          (forward-line 1))
        (save-restriction                           ; change ideal to .
          (narrow-to-region pt (point))
          (goto-char pt)
          (while (re-search-forward "#" nil t)
            (replace-match "\.")))
        (save-restriction                           ; change actual to #
          (narrow-to-region pt (point))
          (goto-char pt)
          (while (re-search-forward "\*" nil t)
            (replace-match "#")))))))

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
      (setq found (re-search-forward "#\\+BEGIN: block-update-burndown" nil t))
      (if (not found)
          (error "\"block-update-burndown\" not found"))
      (org-ctrl-c-ctrl-c)
      (goto-char (point-min))
      (setq found (re-search-forward "#\\+BEGIN: block-update-graph" nil t))
      (if (not found)
          (error "\"block-update-graph\" not found"))
      (org-ctrl-c-ctrl-c))))

(provide 'org-scrum)

;;; org-scrum.el ends here
