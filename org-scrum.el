;;; org-scrum.el --- org mode extensions for scrum planning and reporting -*- lexical-binding: t -*-

;; Copyright (C) 2012-2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-scrum
;; Version: 0.2.1
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
  "Options for customizing scrum reports."
  :tag "Org Scrum"
  :group 'org)

(defcustom org-scrum-storyid-prefix "S"
  "Prefix added to story ids."
  :type 'string
  :group 'org-scrum)

(defcustom org-scrum-board-links nil
  "If true, make the items in the scrum board links."
  :type 'boolean
  :group 'org-scrum)

(defcustom org-scrum-board-format "%i. %p %t (%o)"
  "Specify the format of the scrum board items.
Provide a format string.  Variables are:
- %i is replaced by the story id
- %p is replaced by the priority
- %t is replaced by the story title
- %o is replaced by the story owner(s)
- %c is replaced by the close date

legacy formats (deprecated):
1. \"id\"
2. \"priority title (closedate)\"
3. \"id. priority title (closedate)\"
4. \"id. owner (closedate)\"
5. \"id. priority title (owner closedate)\""
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

(defvar org-scrum-data nil
  "This is the list of developers as (name . capacity).
An alist containing :stories, :developers, :sprintnum,
:sprintlength, :sprintstart")

(defun org-scrum--load-all ()
  "Load story data into a list of stories stored as alists."
  (let* ((sprintnum (string-to-number
                     (alist-get        ; read sprintnum from constants
                      "sprintnum"
                      org-table-formula-constants-local
                      nil nil 'equal)))
         (sprintlength (string-to-number
                        (alist-get     ; read sprintnum from constants
                         "sprintlength"
                         org-table-formula-constants-local
                         nil nil 'equal)))
         (capacity (org-scrum--load-capacity sprintnum))
         (stories (org-scrum--visit-all-story-todos
                 #'org-scrum--load-story
                 "TODO<>\"\"")))
    (setq org-scrum-data `((:sprintnum . ,sprintnum)
       (:sprintlength . ,sprintlength)
       (:sprintstart . ,(alist-get :sprintstart capacity))
       (:developers . ,(alist-get :developers capacity))
       (:stories . ,stories)))))

(defun org-scrum--visit-all-story-todos (fcn match)
  "Call FCN for all stories in the current tree that match MATCH."
  (save-excursion
    (let (stories)
      (org-map-entries (lambda () (setq stories (point))) "ID=\"STORIES\"")
      (goto-char stories)
      ;;(message "visiting %s" (buffer-substring (point) (line-end-position)))
      (org-map-entries fcn match 'tree))))

(defun org-scrum--load-capacity (sprintnum)
  "Get list of developers and capcity for sprint SPRINTNUM.
Return sprintstart and a list of `(name . capacity)' for current
sprint from capacity table."
  (save-excursion
    ;; find capacity table
    (goto-char (point-min))
    (when (not (re-search-forward (rx "#+NAME:" (* space) "capacity") nil t))
      (error "The capacity table is required"))
    (forward-line)

    ;; find col of current sprint
    (let ((col 1)
          sprintstart
          developers)
      (while (not (= sprintnum (string-to-number (org-table-get-field col))))
        (setq col (1+ col)))
      (forward-line)
      (setq sprintstart (substring-no-properties (org-table-get-field col)))
      (forward-line 2)
      (while (not (looking-at "|---"))
        (let ((name (substring-no-properties (string-trim (org-table-get-field 1))))
              (capacity (string-to-number (org-table-get-field col))))
          (push (cons name capacity) developers))
        (forward-line))
      `((:sprintstart . ,sprintstart)
        (:developers . ,(reverse developers))))))

(defun org-scrum--load-story ()
  "Load the story at point into an alist."
  ;; storyid, owner, name, priority, state, estimate?, actual?, closedate (later: deps, swimlane)
  (let* ((state (org-entry-get (point) "TODO"))
         (storyid (org-entry-get (point) "STORYID"))
         (name (org-scrum--extract-heading))
         (priority (if (nth 3 (org-heading-components))
                       (make-string 1 (nth 3 (org-heading-components)))
                     "Z"))                      ; default to lowest priority
         (owner (org-entry-get (point) "OWNER"))
         (estimated (string-to-number (or (org-entry-get (point) "ESTIMATED") "")))
         (actual (string-to-number (or (org-entry-get (point) "ACTUAL") "")))
         (closed (org-entry-get (point) "CLOSED"))
         (sprint (string-to-number (or (org-entry-get (point) "SPRINT") "")))
         (nn 0)
         closestr closedate)
    (unless (null closed)          ; only include closedate for DONE items
      (setq closestr (mapcar
                      (function (lambda (ii) (if (< (setq nn (1+ nn)) 4) 0 ii)))
                      (parse-time-string closed)) ; clear time of day
            closedate (format-time-string "%Y-%m-%d" (apply #'encode-time closestr))))
    `((:storyid . ,storyid)
      (:name . ,name)
      (:priority . ,priority)
      (:owner . ,owner)
      (:state . ,state)
      (:estimated . ,estimated)
      (:actual . ,actual)
      (:closedate . ,closedate)
      (:sprint . ,sprint))))

(defun org-scrum--aggregate-value (all-stories owner states prop)
  "Sum values of property PROP for OWNER in state STATES in ALL-STORIES."
  (apply                                ; sum
   #'+
   (seq-filter                          ; filter out zeros
    (lambda (ii) (> ii 0))
    (mapcar                             ; get props
     (lambda (ii) (alist-get prop ii))
     (org-scrum--find-stories all-stories owner states)))))

(defun org-scrum--find-stories (all-stories owner states)
  "Find stories for OWNER in state STATES in ALL-STORIES.
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
       all-stories)
    all-stories))

(defun org-scrum--increment-date (cdate &optional days backp)
  "Increment the date CDATE by DAYS days, accounting for DST.
DAYS defaults to 1 day.  If BACKP, move backwards."
  (let* ((cdst (nth 7 (decode-time cdate)))
         (incr-func (if backp 'time-subtract 'time-add))
         (ndate (funcall incr-func cdate (days-to-time (or days 1)))) ; next date
         (ndst (nth 7 (decode-time ndate))))
    (cond ((and cdst (not ndst))        ; sprint forward
           (setq ndate (time-add ndate (seconds-to-time 3600))))
          ((and (not cdst) ndst)        ; fall back
           (setq ndate (time-subtract ndate (seconds-to-time 3600)))))
    ndate))

;; manage stories

(defun org-scrum-start-next-sprint ()
  "Start the next sprint.
This increments SPRINTNUM and moves all of the open stories that
are in the current sprint to the new sprint."
  (interactive)
  (org-scrum--load-all)
  (save-excursion
    (let* ((last-sprint (alist-get :sprintnum org-scrum-data))
           (next-sprint (1+ last-sprint)))
      ;; increment sprintnum
      (goto-char (point-min))
      (re-search-forward (rx line-start "#+CONSTANTS:" (group (* not-newline)) "sprintnum=" (+ digit)))
      (replace-match (concat "#+CONSTANTS:\\1sprintnum=" (number-to-string next-sprint)))
      (org-save-outline-visibility 'use-markers (org-mode-restart)) ; local refresh to pick up change

      ;; increment sprint in story list
      (goto-char (point-min))
      (re-search-forward (rx line-start "#+BEGIN: columnview" (group (* not-newline))
                             ":match \"SPRINT=" (+ digit) "\"" (group (* not-newline))))
      (replace-match (concat "#+BEGIN: columnview\\1:match \"SPRINT=" (number-to-string next-sprint) "\"\\2"))

      ;; update open stories
      (org-scrum--visit-all-story-todos (lambda ()
                                         (let ((state (org-entry-get (point) "TODO"))
                                               (sprint (string-to-number (or (org-entry-get (point) "SPRINT") ""))))
                                           (when (and (member state org-not-done-keywords)
                                                      (= sprint last-sprint))
                                             (org-entry-put (point) "SPRINT" (number-to-string next-sprint)))))
                                       "TODO<>\"\"")))
  (setq org-scrum-data nil))

(defun org-scrum-reset-storyids ()
  "Replace `STORYIDs' of all stories with consecutive values.
This is deprecated and will be altered or removed in future versions."
  (interactive)
  (save-excursion
    (let ((ii 1))
      (org-scrum--visit-all-story-todos
       (lambda ()
         (org-entry-put (point) "STORYID" (format "%s%02d" org-scrum-storyid-prefix ii))
         (setq ii (1+ ii)))
       "TODO<>\"\""))))

;; developer summary

(defun org-scrum--get-finish-date (points velocity capacity)
  "Count the days to get POINTS work done given VELOCITY and CAPACITY.
VELOCITY is taken from last sprint, or defaults to 1. CAPACITY is
in hours per day.  Count days required to complete work, skipping
weekends.  Return date of completion, or `no capacity' if
capacity is 0 since completion is impossible."
  (let* ((cdate (current-time))
         (hoursleft (ceiling (/ points velocity)))
         ctime)
    (if (= capacity 0)
        "no capacity"
      (while (> hoursleft 0)
        (setq cdate (org-scrum--increment-date cdate)
              ctime (decode-time cdate))
        (if (and (> (nth 6 ctime) 0) (< (nth 6 ctime) 6))
            (setq hoursleft (- hoursleft capacity))))
      (format-time-string "%Y-%m-%d" cdate))))

(defun org-scrum--get-work-left (cdate closed-stories tot)
  "Get the actual work remaining for the date CDATE.
Computes work remaining given the list of closed items
CLOSED-STORIES and total points TOT.  Returns `(list-of-closed-stories .
total-remaining-work)'"
  (let (closetime toremove)
    (dolist (item closed-stories)
      (setq closetime (date-to-time (alist-get :closedate item)))
      (unless (time-less-p cdate closetime)
        (setq tot (- tot (alist-get :estimated item)))
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

(defun org-dblock-write:block-update-summary (_params)
  "Generate developer summary table."
  (let ((loadp (not org-scrum-data))
        (est  0)                ; points estimated
        (act  0)                ; actual points spent
        (done 0)                ; points of estimates that are done
        (rem  0)                ; points of estimates that are left
        developers sprint stories velocity)

    (when loadp
      (org-scrum--load-all))
    (setq developers (alist-get :developers org-scrum-data)
          sprint (alist-get :sprintnum org-scrum-data)
          stories (seq-filter (lambda (ii) (= sprint (or (alist-get :sprint ii) 0)))
                            (alist-get :stories org-scrum-data))
          velocity (org-scrum--get-velocity sprint))
    (when (= 0 (length developers))
      (error "No developers found in the capacity table"))

    (insert "| NAME | ESTIMATED | ACTUAL | DONE | REMAINING | PENCILS DOWN | PROGRESS |\n|-")
    (dolist (developer developers)
      (setq est  (org-scrum--aggregate-value stories (car developer) (append org-not-done-keywords org-done-keywords) :estimated))
      (setq act  (org-scrum--aggregate-value stories (car developer) '()  :actual))
      (setq done (org-scrum--aggregate-value stories (car developer) org-done-keywords :estimated))
      (setq rem  (org-scrum--aggregate-value stories (car developer) org-not-done-keywords :estimated))

      (insert "\n| " (car developer)
              " | " (number-to-string est)
              " | " (number-to-string act)
              " | " (number-to-string done)
              " | " (number-to-string rem)
              " | " (org-scrum--get-finish-date rem velocity (cdr developer))
              " | " (org-scrum--draw-progress-bar est done)
              " |"))
    (org-table-align)
    (when loadp
      (setq org-scrum-data nil))))

(defun org-scrum--get-velocity (sprint)
  "Load velocity from previous SPRINT from the projections table.
Default to `1'."
  (save-excursion
    (goto-char (point-min))
    (if (or (= sprint 1)
            (not (re-search-forward
                  (rx line-start "#+NAME:" (* space) "projections")
                  nil t)))
        1
      (forward-line 5)
      (string-to-number (org-table-get-field sprint)))))

;; scrum board

(defun org-scrum--extract-heading ()
  "Extract the heading from the current entry."
  (let* ((fullhdg (nth 4 (org-heading-components)))
         (bracket (string-match "\\[" fullhdg)) ; index of bracket character
         (hdg (if bracket
                  (substring fullhdg 0 (min (1- bracket) ))
                fullhdg))
         (maxlen 30)) ; max length of scrum board story title
    (substring fullhdg 0 (min (length hdg) maxlen))))

(defun org-scrum--make-scrum-board-label (story)
  "Make a scrum board entry from the given STORY."
  (let* ((storyid (alist-get :storyid story))
         (name (alist-get :name story))
         (priority (alist-get :priority story))
         (owner (alist-get :owner story))
         (closedate (alist-get :closedate story))
         label)
    (setq label (cond                   ; scrum board label
                 ;; legacy formats
                 ((string= "1" org-scrum-board-format) storyid)
                 ((string= "2" org-scrum-board-format) (format "[#%s] %s (%s)" priority name closedate))
                 ((string= "3" org-scrum-board-format) (format "%s. [#%s] %s (%s)" storyid priority name closedate))
                 ((string= "4" org-scrum-board-format) (format "%s. %s (%s)" storyid (or owner "not assigned") closedate))
                 ((string= "5" org-scrum-board-format) (format "%s. [#%s] %s (%s %s)" storyid priority name owner closedate))
                 ;; custom formats
                 (t (setq label (replace-regexp-in-string "%i" storyid org-scrum-board-format)
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
  (let* ((loadp (not org-scrum-data))
         (todokwds                      ; list of all todo keywords
          (append org-not-done-keywords org-done-keywords))
         sprint stories-by-state)
    (when loadp
      (org-scrum--load-all))

    (setq sprint (alist-get :sprintnum org-scrum-data)
          stories-by-state (seq-group-by
                          (lambda (ii) (alist-get :state ii))
                          (seq-filter
                           (lambda (ii) (= sprint (or (alist-get :sprint ii) 0)))
                           (alist-get :stories org-scrum-data))))

    ;; set up the table
    (insert "| " (mapconcat #'identity todokwds "|") "\n|-\n|")
    (org-table-align)
    (org-table-analyze)

    (dolist (item stories-by-state)
      (let* ((state (car item))
            (index (- (length todokwds)
                      (length (member state todokwds))
                      -1))
            (state-stories
             (if (member state org-not-done-keywords)
                 (sort              ; sort open stories by priority
                  (cdr item)
                  (lambda (a b) (string> (alist-get :priority b)
                                         (alist-get :priority a))))
               (sort                ; sort closed stories by date closed
                (cdr item)
                (lambda (a b) (string> (alist-get :closedate b)
                                       (alist-get :closedate a)))))))
        (org-table-goto-field (format "@2$%d" index))
        (dolist (item state-stories)
          (unless (eq item (car state-stories))
            (org-table-next-row))
          (let ((label (org-scrum--make-scrum-board-label item)))
            (insert label)))))
    (org-table-align)

    (when loadp
      (setq org-scrum-data nil))))

;; burndown chart

(defun org-scrum--compute-actual-burndown (start sprintlength tot)
  "Compute actual burndown for each day of the sprint.
The sprint starts at date START and lasts SPRINTLENGTH days.  TOT
is the total number of story points for the sprint."
  (let ((loadp (not org-scrum-data))
        (left tot)                      ; total actually left
        (today (current-time))          ; know when today is because we can't fill in the future burndown
        (cdate start)                   ; the date of the current date as we iterate
        (day 0)                         ; counts the days as we iterate
        sprint                          ; current sprint
        closed-stories                  ; list of closed stories that contribute to burndown
        actual-burndown)                ; the list of burndown by day
    (when loadp
      (org-scrum--load-all))
    (setq sprint (alist-get :sprintnum org-scrum-data)
          closed-stories (org-scrum--find-stories
                        (seq-filter
                         (lambda (ii) (= sprint (or (alist-get :sprint ii) 0)))
                         (alist-get :stories org-scrum-data))
                        nil org-done-keywords))
    (while (< day sprintlength)
      (setq cdate (org-scrum--increment-date cdate)) ; increment current day
      (if (time-less-p cdate today)
          (let* ((ret (org-scrum--get-work-left cdate closed-stories left))
                (toremove (car ret))) ; save list of completed stories
            (setq left (cdr ret)      ; save new total
                  closed-stories (seq-remove (lambda (ii) (seq-contains-p toremove ii)) closed-stories) ; remove stories that have been counted
                  actual-burndown (push (number-to-string left) actual-burndown)))
        (setq actual-burndown (push "" actual-burndown)))
      (setq day (1+ day)))
    (when loadp
      (setq org-scrum-data nil))
    (reverse actual-burndown)))

(defun org-scrum--compute-ideal-burndown (start sprintlength tot)
  "Compute ideal burndown for each day of the sprint.
The sprint starts at START and lasts SPRINTLENGTH days.  TOT is
the total number of story points for the sprint."
  (let ((cdate start)                   ; the current date as we iterate
        (day 0)                         ; counts the days as we iterate
        ctime                           ; holds extracted date components
        weekdays)                       ; list of days, true for weekdays

    ;; find weekdays. make `ideal-burndown' a list of days where each weekday is t
    (while (< day sprintlength)
      (setq ctime (decode-time cdate)
            weekdays (push (and (> (nth 6 ctime) 0) (< (nth 6 ctime) 6)) weekdays)
            cdate (org-scrum--increment-date cdate) ; increment current day
            day (1+ day)))              ; increment day counter

    ;; compute the ideal burndown rate and use it to fill in `ideal-burndown'
    (let* ((count (seq-reduce ; count weekdays
                   (lambda (cc ii) (if ii (1+ cc) cc))
                   weekdays
                   0))
           (rate  (/ (float tot) count))
           (left tot))
      (mapcar
       (lambda (ii)
         (when ii
           (setq left (max 0 (- left rate))))
         left)
       weekdays))))

(defun org-scrum--compute-burndown ()
  "Compute ideal and actual burndown for each day of the sprint.
Returns a list of `(date actual ideal)'."
  (let* ((sprint (alist-get :sprintnum org-scrum-data))
         (tot (org-scrum--aggregate-value
               (seq-filter
                (lambda (ii) (= sprint (or (alist-get :sprint ii) 0)))
                (alist-get :stories org-scrum-data))
               nil nil :estimated)) ; total estimated points
         cdate                 ; current date for iterating
         sprintlength)         ; number of calendar days in the sprint
    ;; look up start date and sprint length
    (org-map-entries
     (lambda ()
       (setq cdate (org-scrum--increment-date ; day before sprint start
                    (date-to-time (alist-get :sprintstart org-scrum-data))
                    nil t)
             sprintlength (alist-get :sprintlength org-scrum-data)))
                     "ID=\"STORIES\"")
    (if (or (null cdate) (null sprintlength))
        (error "Couldn't find #+CONSTANTS setting \"sprintlength\" and \"sprintstart\" in the capacity table"))

    (cl-mapcar #'list ; this zips the lists together
            (number-sequence 1 sprintlength)
            (org-scrum--compute-actual-burndown cdate sprintlength tot)
            (org-scrum--compute-ideal-burndown cdate sprintlength tot))))

(defun org-scrum--check-gnuplot-exists ()
  "Check if gnuplot is installed on the system."
  (unless (eq 0 (call-process-shell-command "gnuplot --version"))
    (error "Cannot find gnuplot")))

(defun org-scrum--make-gnuplot-config (burndown-data)
  "Write a gnuplot config to the current buffer.
The config includes inline data taken from BURNDOWN-DATA."
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
  (insert "set xlabel \"day of sprint\"\n")
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

(defun org-dblock-write:block-update-burndown (_params)
  "Generate burndown chart."

  (org-scrum--check-gnuplot-exists)

  (goto-char (point-min))
  (re-search-forward (rx line-start "#+BEGIN:" (* space) "block-update-burndown"))
  (forward-line)                                        ; move into dynamic block

  (let ((loadp (not org-scrum-data))
        (buffer (current-buffer))
        (pt (point))
        burndown-data)
    (when loadp
      (org-scrum--load-all))
    (setq burndown-data (org-scrum--compute-burndown))
    (with-temp-buffer
      (org-scrum--make-gnuplot-config burndown-data)
      (call-process-region (point-min) (point-max) "gnuplot" nil buffer)
      (set-buffer buffer)
      (if (not org-scrum-ascii-graph)
          (progn
            (insert "[[./burndown.svg]]")      ; using an org link instead of embedding the image
            (org-display-inline-images))       ; so it gets exported correctly
        (goto-char pt)
        (while (search-forward "\f" nil t)     ; delete the formfeed in gnuplot output
          (replace-match ""))
        (while (not (looking-at "#\\+END"))    ; prefix graph lines so org exports them cleanly
          (insert ":")
          (forward-line 1))
        (save-restriction
          (narrow-to-region pt (point))
          (goto-char pt)                       ; the default linestyle with dots also has '+'s at the points
          (while (search-forward "#" nil t)    ; which make the graph busier, so we use '#' and then swap them
            (replace-match "\.")))))           ; for dots
    (when loadp
      (setq org-scrum-data nil))))

;; planning table

(defun org-scrum--set-completed ()
  "Set completed per sprint in the planning table.
Count points completed in each sprint and update the SPRINT
PLANNING table."
  (let ((stories (alist-get :stories org-scrum-data))
        (sprint (alist-get :sprintnum org-scrum-data))
        (hash (make-hash-table)))
    ;; get totals per sprint
    (dolist (story stories)
      (let ((story-sprint (alist-get :sprint story))
            (estimated (alist-get :estimated story))
            (donep (member (alist-get :state story) org-done-keywords)))
        (when (and donep
                   (> story-sprint 0))
          (puthash
           story-sprint
           (+ (gethash story-sprint hash 0) estimated)
           hash))))
    ;; update table
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (rx "#+NAME:" (* space) "projections") nil t)
        (forward-line)
        (org-table-analyze)
        (dotimes (ii (1- org-table-current-ncol))
          (let* ((col (+ ii 2))
                 (colsprint (string-to-number (org-table-get-field col)))
                 (completed (if (gethash colsprint hash)
                                (number-to-string (gethash colsprint hash))
                              (if (<= colsprint sprint)
                                  "0"
                                ""))))
            (org-table-put 3 col completed))))
      (org-table-align))))

;; scrum cards

(defun org-scrum--generate-story-card (story)
  "Generate a scrum card for the given STORY."
  (let* ((name (alist-get :name story))
         (id (or (alist-get :storyid story) "\\_\\_\\_"))
         (owner (or (alist-get :owner story) "\\_\\_\\_\\_\\_"))
         (estnum (alist-get :estimated story))
         (est (if (> estnum 0)
                  (number-to-string estnum)
                "\\_\\_\\_")))
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

(defun org-scrum-generate-story-cards ()
  "Generate scrum board story cards in latex format.
This depends on latex and the multirow style installed on the
system.  The result is the file \"scrum_cards.pdf\"."
  (interactive)
  (org-scrum--load-all)
  (save-excursion
    (let ((sprint (alist-get :sprintnum org-scrum-data))
          tex-content)
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
                                (mapconcat #'org-scrum--generate-story-card
                                           (seq-filter
                                            (lambda (ii) (= sprint (alist-get :sprint ii)))
                                            (alist-get :stories org-scrum-data))
                                           "\n")
                                "\n\\end{document}\n"))
      (with-temp-file "scrum_cards.tex"
        (insert tex-content))
      (shell-command "texi2pdf scrum_cards.tex"
                     (get-buffer-create "*Standard output*"))))
  (setq org-scrum-data nil))

;; update function

;;;###autoload
(defun org-scrum-update-all ()
  "Update all report sections in a scrum org file."
  (interactive)

  ;; check for old version of template
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (rx line-start (* space) ":ID:" (* space) "TASKS" (* space) line-end) nil t)
      (error "This org-scrum file must be upgraded to work with the current version of org-scrum; see the org-scrum readme for more details")))

  (org-scrum--load-all)
  (save-excursion
    (goto-char (point-min))
    (let* ((skipped '())
           (update-block (lambda (pattern name)
                           (goto-char (point-min))
                           (if (re-search-forward pattern nil t)
                               (org-ctrl-c-ctrl-c)
                             (push name skipped)))))
      (funcall update-block (rx line-start "#+NAME:" (* space) "capacity" (*? anything) line-start "#+TBLFM:") "capacity table")
      (org-scrum--set-completed)
      (funcall update-block (rx line-start "#+NAME:" (* space) "projections" (*? anything) line-start "#+TBLFM:") "projections table")
      (funcall update-block (rx line-start "#+BEGIN:" (* space) "columnview" (* not-newline) ":id \"STORIES\"") "STORIES columnview")
      (funcall update-block (rx line-start "#+BEGIN:" (* space) "block-update-summary") "block-update-summary")
      (funcall update-block (rx line-start "#+BEGIN:" (* space) "block-update-board") "block-update-board")
      (funcall update-block (rx line-start "#+BEGIN:" (* space) "block-update-burndown") "block-update-burndown")
      (when skipped
        (message "Skippped blocks that were not found: %s" (string-join (nreverse skipped) ", ")))))
  (setq org-scrum-data nil))

(provide 'org-scrum)

;;; org-scrum.el ends here
