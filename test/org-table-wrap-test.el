;;; org-table-wrap-test.el --- Tests for org-table-wrap -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; ERT tests for org-table-wrap.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(setq load-prefer-newer t)
(require 'org-table-wrap)

;;;; Test helpers

(defmacro org-table-wrap-test-with-buffer (content &rest body)
  "Create a temp Org buffer with CONTENT and evaluate BODY.
The buffer is set to org-mode and has a fixed window width of 40."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ;; Override available width to 40 for deterministic tests
     (cl-letf (((symbol-function 'org-table-wrap--available-width)
                (lambda (&optional _pos) 40)))
       ,@body)))

(defmacro org-table-wrap-test-with-width (width content &rest body)
  "Create a temp Org buffer with CONTENT and evaluate BODY.
Override the available width to WIDTH."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     (cl-letf (((symbol-function 'org-table-wrap--available-width)
                (lambda (&optional _pos) ,width)))
       ,@body)))

;;;; Parsing tests

(ert-deftest org-table-wrap-test-parse-data-row ()
  "Parse a data row into cell contents."
  (let ((result (org-table-wrap--parse-row "| foo | bar | baz |")))
    (should (equal result '("foo" "bar" "baz")))))

(ert-deftest org-table-wrap-test-parse-hline ()
  "Parse an hline row."
  (let ((result (org-table-wrap--parse-row "|---+---+---|")))
    (should (eq result 'hline))))

(ert-deftest org-table-wrap-test-parse-leading-dash-cell ()
  "Rows whose first cell starts with a dash are not parsed as hlines."
  (let ((result (org-table-wrap--parse-row "|-foo|bar|")))
    (should (equal result '("-foo" "bar")))))

(ert-deftest org-table-wrap-test-parse-padded-row ()
  "Parse a row with extra whitespace."
  (let ((result (org-table-wrap--parse-row "|  hello  |  world  |")))
    (should (equal result '("hello" "world")))))

(ert-deftest org-table-wrap-test-parse-empty-cells ()
  "Parse a row with empty cells."
  (let ((result (org-table-wrap--parse-row "| | content | |")))
    (should (equal result '("" "content" "")))))

(ert-deftest org-table-wrap-test-parse-table ()
  "Parse a complete table."
  (org-table-wrap-test-with-buffer
      "| a | b |\n|---+---|\n| c | d |\n"
    (let* ((rows (org-table-wrap--parse-table (point-min) (point-max))))
      (should (= (length rows) 3))
      (should (equal (nth 0 rows) '("a" "b")))
      (should (eq (nth 1 rows) 'hline))
      (should (equal (nth 2 rows) '("c" "d"))))))

;;;; Column width tests

(ert-deftest org-table-wrap-test-natural-widths ()
  "Compute natural column widths."
  (let* ((rows '(("short" "very long content")
                 ("ab" "cd")))
         (widths (org-table-wrap--natural-widths rows 2)))
    (should (= (aref widths 0) 5))   ; "short"
    (should (= (aref widths 1) 17)))) ; "very long content"

(ert-deftest org-table-wrap-test-proportional-allocation ()
  "Wider columns get proportionally more space."
  (let* ((natural (vector 10 30))   ; 1:3 ratio
         (allocated (org-table-wrap--allocate-widths natural 40)))
    ;; Column 1 should be wider than column 0
    (should (> (aref allocated 1) (aref allocated 0)))
    ;; Total should not exceed available
    (should (<= (+ (aref allocated 0) (aref allocated 1)) 40))))

(ert-deftest org-table-wrap-test-minimum-width-enforced ()
  "All columns get at least the minimum width."
  (let* ((natural (vector 1 100))
         (org-table-wrap-min-column-width 5)
         (allocated (org-table-wrap--allocate-widths natural 20)))
    (should (>= (aref allocated 0) 5))
    (should (>= (aref allocated 1) 5))))

;;;; Word wrapping tests

(ert-deftest org-table-wrap-test-no-wrap-needed ()
  "Text fits within width: single line returned."
  (let ((result (org-table-wrap--wrap-text "hello" 10)))
    (should (equal result '("hello")))))

(ert-deftest org-table-wrap-test-word-wrap ()
  "Text wraps at word boundaries."
  (let ((result (org-table-wrap--wrap-text "hello world foo" 11)))
    (should (= (length result) 2))
    (should (equal (nth 0 result) "hello world"))
    (should (equal (nth 1 result) "foo"))))

(ert-deftest org-table-wrap-test-character-wrap ()
  "Long words fall back to character-level wrapping."
  (let ((result (org-table-wrap--wrap-text "abcdefghijklmno" 5)))
    (should (>= (length result) 3))
    ;; Each chunk should be at most 5 chars
    (dolist (line result)
      (should (<= (length line) 5)))))

(ert-deftest org-table-wrap-test-wrap-empty-string ()
  "Empty string wraps to a single empty line."
  (let ((result (org-table-wrap--wrap-text "" 10)))
    (should (equal result '("")))))

;;;; Border character tests

(ert-deftest org-table-wrap-test-unicode-borders ()
  "Unicode border characters are used when enabled."
  (let ((org-table-wrap-use-unicode t))
    (should (equal (org-table-wrap--char 'vertical) "│"))
    (should (equal (org-table-wrap--char 'horizontal) "─"))
    (should (equal (org-table-wrap--char 'cross) "┼"))
    (should (equal (org-table-wrap--char 'top-left) "┌"))
    (should (equal (org-table-wrap--char 'bottom-right) "┘"))))

(ert-deftest org-table-wrap-test-ascii-borders ()
  "ASCII border characters are used when unicode is disabled."
  (let ((org-table-wrap-use-unicode nil))
    (should (equal (org-table-wrap--char 'vertical) "|"))
    (should (equal (org-table-wrap--char 'horizontal) "-"))
    (should (equal (org-table-wrap--char 'cross) "+"))))

;;;; Hline construction tests

(ert-deftest org-table-wrap-test-hline-top ()
  "Top hline uses correct corner characters."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((hline (org-table-wrap--build-hline (vector 3 4) 'top)))
      (should (string-prefix-p "┌" hline))
      (should (string-suffix-p "┐" hline))
      (should (string-match-p "┬" hline)))))

(ert-deftest org-table-wrap-test-hline-middle ()
  "Middle hline uses correct left-t, cross, and right-t characters."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((hline (org-table-wrap--build-hline (vector 3 4) 'middle)))
      (should (string-prefix-p "├" hline))
      (should (string-suffix-p "┤" hline))
      (should (string-match-p "┼" hline)))))

(ert-deftest org-table-wrap-test-hline-bottom ()
  "Bottom hline uses correct corner characters."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((hline (org-table-wrap--build-hline (vector 3 4) 'bottom)))
      (should (string-prefix-p "└" hline))
      (should (string-suffix-p "┘" hline))
      (should (string-match-p "┴" hline)))))

;;;; Display string tests

(ert-deftest org-table-wrap-test-display-string-has-face ()
  "The display string has the org-table face applied."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(hline ("hello" "world") hline))
         (widths (vector 10 10))
         (display (org-table-wrap--build-display-string rows widths)))
    (should (eq (get-text-property 0 'face display) 'org-table))))

(ert-deftest org-table-wrap-test-display-string-structure ()
  "The display string has the expected number of lines."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(hline ("a" "b") hline ("c" "d") hline))
         (widths (vector 5 5))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; 3 hlines + 2 data rows = 5 lines
    (should (= (length lines) 5))))

;;;; Table narrower than window (no overlay)

(ert-deftest org-table-wrap-test-narrow-table-no-overlay ()
  "A table that fits the window gets no overlay."
  (org-table-wrap-test-with-width 80
      "| a | b |\n|---+---|\n| c | d |\n"
    (org-table-wrap-mode 1)
    ;; No overlays should exist since the table is narrow
    (should (null org-table-wrap--overlays))
    (org-table-wrap-mode -1)))

;;;; Table wider than window (overlay applied)

(ert-deftest org-table-wrap-test-wide-table-gets-overlay ()
  "A table wider than the window gets a display overlay."
  (let ((wide-table
         (concat
          "| Column one has a lot of text | Column two also has lots of text | Third column |\n"
          "|------------------------------+----------------------------------+---------------|\n"
          "| Some content here            | More content here                | Short         |\n")))
    (org-table-wrap-test-with-width 40 wide-table
      (org-table-wrap-mode 1)
      (should (= (length org-table-wrap--overlays) 1))
      ;; Verify the overlay has invisible + before-string
      (let* ((entry (car org-table-wrap--overlays))
             (ov (nth 2 entry)))
        (should (overlayp ov))
        (should (eq (overlay-get ov 'invisible) 'org-table-wrap))
        (should (stringp (overlay-get ov 'before-string))))
      (org-table-wrap-mode -1))))

;;;; Entering and leaving table toggles overlay

(ert-deftest org-table-wrap-test-enter-table-removes-overlay ()
  "When point enters a wrapped table, the overlay is removed."
  (let ((wide-table
         (concat
          "Some text before.\n"
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |\n"
          "Some text after.\n")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      ;; Should have one overlay
      (should (= (length org-table-wrap--overlays) 1))
      ;; Move point into the table
      (goto-char (point-min))
      (forward-line 1)
      ;; Simulate post-command-hook
      (org-table-wrap--post-command)
      ;; Overlay should be removed (table is being edited)
      (should (null org-table-wrap--overlays))
      ;; Current table should be tracked
      (should org-table-wrap--current-table)
      ;; Move point out of the table
      (let (scheduled)
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (_secs _repeat fn &rest args)
                     (setq scheduled (cons fn args))
                     'fake-timer)))
          (goto-char (point-min))
          (org-table-wrap--post-command)
          (apply (car scheduled) (cdr scheduled))))
      ;; Overlay should be re-applied
      (should (= (length org-table-wrap--overlays) 1))
      (should (null org-table-wrap--current-table))
      (org-table-wrap-mode -1))))

(ert-deftest org-table-wrap-test-editing-table-shape-keeps-it-revealed ()
  "Editing a revealed table must not reapply a stale overlay."
  (let ((wide-table
         (concat
          "Some text before.\n"
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |\n"
          "Some text after.\n")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      (goto-char (point-min))
      (forward-line 1)
      (org-table-wrap--post-command)
      (should (null org-table-wrap--overlays))
      (end-of-line)
      (insert "\n| extra row                   | another row                 |")
      (org-table-wrap--post-command)
      (should (null org-table-wrap--overlays))
      (should org-table-wrap--current-table)
      (let (scheduled)
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (_secs _repeat fn &rest args)
                     (setq scheduled (cons fn args))
                     'fake-timer)))
          (goto-char (point-min))
          (org-table-wrap--post-command)
          (apply (car scheduled) (cdr scheduled))))
      (should (= (length org-table-wrap--overlays) 1))
      (org-table-wrap-mode -1))))

(ert-deftest org-table-wrap-test-leaving-table-defers-rewrap ()
  "Leaving a table schedules rewrapping instead of doing it in the hook."
  (let ((wide-table
         (concat
          "Some text before.\n"
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |\n"
          "Some text after.\n")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      (goto-char (point-min))
      (forward-line 1)
      (org-table-wrap--post-command)
      (let (scheduled called)
        (cl-letf (((symbol-function 'org-table-wrap--process-table)
                   (lambda (_beg _end)
                     (setq called t)))
                  ((symbol-function 'run-with-idle-timer)
                   (lambda (_secs _repeat fn &rest args)
                     (setq scheduled (cons fn args))
                     'fake-timer)))
          (goto-char (point-min))
          (org-table-wrap--post-command)
          (should-not called)
          (should scheduled)
          (should (null org-table-wrap--current-table))
          (apply (car scheduled) (cdr scheduled))
          (should called)))
      (org-table-wrap-mode -1))))

(ert-deftest org-table-wrap-test-deferred-rewrap-waits-out-recursive-edit ()
  "Deferred rewrapping reschedules while Emacs is in a recursive edit."
  (org-table-wrap-test-with-width 30
      "| Very long column header one | Very long column header two |\n"
    (let ((org-table-wrap-mode t)
          scheduled
          called)
      (cl-letf (((symbol-function 'org-table-wrap--process-table)
                 (lambda (_beg _end)
                   (setq called t)))
                ((symbol-function 'recursion-depth)
                 (lambda ()
                   1))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_secs _repeat fn &rest args)
                   (push (cons fn args) scheduled)
                   'fake-timer)))
        (org-table-wrap--schedule-table-process (point-min) (point-max))
        (apply (caar scheduled) (cdar scheduled))
        (should-not called)
        (should (= (length scheduled) 2))))))

(ert-deftest org-table-wrap-test-deferred-rewrap-contains-errors ()
  "Deferred rewrapping reports failures without signaling."
  (org-table-wrap-test-with-width 30
      "| Very long column header one | Very long column header two |\n"
    (let ((org-table-wrap-mode t)
          (beg-marker (copy-marker (point-min) t))
          (end-marker (copy-marker (point-max)))
          reported)
      (cl-letf (((symbol-function 'org-table-wrap--process-table)
                 (lambda (_beg _end)
                   (error "boom")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq reported (apply #'format format-string args)))))
        (org-table-wrap--process-table-when-idle
         (current-buffer) beg-marker end-marker)
        (should (string-match-p "table re-processing failed: boom" reported))
        (should (null (marker-position beg-marker)))
        (should (null (marker-position end-marker)))))))

(ert-deftest org-table-wrap-test-enter-table-after-edit-before-it ()
  "Overlay bookkeeping stays valid after editing earlier buffer text."
  (let ((wide-table
         (concat
          "Intro line.\n"
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |\n"
          "Some text after.\n")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      (should (= (length org-table-wrap--overlays) 1))
      (goto-char (point-min))
      (insert "Inserted before table.\n")
      (goto-char (point-min))
      (forward-line 2)
      (org-table-wrap--post-command)
      (should (null org-table-wrap--overlays))
      (should org-table-wrap--current-table)
      (org-table-wrap-mode -1))))

;;;; Multiple tables handled independently

(ert-deftest org-table-wrap-test-multiple-tables ()
  "Multiple tables in one buffer each get their own overlay."
  (let ((content
         (concat
          "| Very long column A one | Very long column A two |\n"
          "|------------------------+------------------------|\n"
          "| content A              | more A                 |\n"
          "\nSome text between tables.\n\n"
          "| Very long column B one | Very long column B two |\n"
          "|------------------------+------------------------|\n"
          "| content B              | more B                 |\n")))
    (org-table-wrap-test-with-width 30 content
      (org-table-wrap-mode 1)
      (should (= (length org-table-wrap--overlays) 2))
      (org-table-wrap-mode -1))))

;;;; Hline rows reformatted correctly

(ert-deftest org-table-wrap-test-hline-in-display ()
  "Hline rows appear correctly in the display string."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(hline ("hello" "world") hline))
         (widths (vector 8 8))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; First line should be a top hline
    (should (string-prefix-p "┌" (nth 0 lines)))
    ;; Last line should be a bottom hline
    (should (string-prefix-p "└" (nth 2 lines)))
    ;; All hlines should use ─
    (should (string-match-p "─" (nth 0 lines)))
    (should (string-match-p "─" (nth 2 lines)))))

;;;; Empty cells handled

(ert-deftest org-table-wrap-test-empty-cells ()
  "Empty cells are rendered as blank space."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(("" "content") ("data" "")))
         (widths (vector 5 7))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; Should have 2 data lines (no hlines added since rows don't start/end
    ;; with hline, but top/bottom borders are added)
    (should (= (length lines) 4))  ; top-border + 2 data + bottom-border
    ;; The first data line should contain empty first cell
    (let ((line (nth 1 lines)))
      (should (string-match-p "│" line)))))

;;;; Window resize triggers re-evaluation

(ert-deftest org-table-wrap-test-resize-triggers-reprocess ()
  "Window resize handler schedules re-processing."
  (let ((timer-created nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_secs _repeat _fn)
                 (setq timer-created t)
                 ;; Return a fake timer
                 'fake-timer)))
      (org-table-wrap--window-size-changed nil)
      (should timer-created))))

;;;; Mode cleanup

(ert-deftest org-table-wrap-test-mode-disable-cleans-up ()
  "Disabling the mode removes all overlays and hooks."
  (let ((wide-table
         (concat
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |\n")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      (should (= (length org-table-wrap--overlays) 1))
      (org-table-wrap-mode -1)
      (should (null org-table-wrap--overlays))
      (should (null org-table-wrap--current-table)))))

;;;; Wrapping produces multi-line rows

(ert-deftest org-table-wrap-test-wrapped-row-has-multiple-lines ()
  "A row with content exceeding column width produces multiple display lines."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         ;; Give column 0 width 10, content is 23 chars -> should wrap
         (rows '(("hello world is great ok" "short")))
         (widths (vector 10 5))
         (display (org-table-wrap--build-display-string rows widths))
         (lines (split-string display "\n")))
    ;; Should have top border + multiple data lines + bottom border
    ;; "hello world is great ok" wrapping at width 10:
    ;; "hello" "world is" "great ok" -> 3 lines
    (should (> (length lines) 3))))

;;;; Text properties preservation

(ert-deftest org-table-wrap-test-display-string-preserves-org-table-face ()
  "The display string has org-table face throughout."
  (let* ((org-table-wrap-use-unicode t)
         (org-table-wrap-padding 1)
         (rows '(("bold text" "normal")))
         (widths (vector 10 7))
         (display (org-table-wrap--build-display-string rows widths)))
    ;; The entire string should have org-table face
    (should (eq (get-text-property 0 'face display) 'org-table))
    (should (eq (get-text-property (1- (length display)) 'face display)
                'org-table))))

;;;; Single data line building

(ert-deftest org-table-wrap-test-build-data-line ()
  "A data line has correct structure with borders and padding."
  (let ((org-table-wrap-use-unicode t)
        (org-table-wrap-padding 1))
    (let ((line (org-table-wrap--build-data-line '("hi" "there") (vector 5 5))))
      ;; Should start and end with │
      (should (string-prefix-p "│" line))
      (should (string-suffix-p "│" line))
      ;; Should contain three │ characters (left, middle, right)
      (should (= (cl-count ?│ line) 3)))))

;;;; Table detection

(ert-deftest org-table-wrap-test-find-tables ()
  "Find all tables in a buffer."
  (org-table-wrap-test-with-buffer
      (concat
       "* Heading\n"
       "| a | b |\n"
       "| c | d |\n"
       "\nSome text.\n\n"
       "| x | y |\n"
       "| z | w |\n")
    (let ((tables (org-table-wrap--find-tables)))
      (should (= (length tables) 2)))))

(ert-deftest org-table-wrap-test-find-tables-ignores-example-blocks ()
  "Table discovery ignores non-table lines inside example blocks."
  (org-table-wrap-test-with-buffer
      (concat
       "#+begin_example\n"
       "| not a table |\n"
       "#+end_example\n\n"
       "| a | b |\n"
       "| c | d |\n")
    (let ((tables (org-table-wrap--find-tables)))
      (should (= (length tables) 1)))))

(ert-deftest org-table-wrap-test-table-width ()
  "Compute the width of a table."
  (org-table-wrap-test-with-buffer
      "| short | cell |\n| a     | b    |\n"
    (let ((width (org-table-wrap--table-width (point-min) (point-max))))
      ;; "| short | cell |" = 16 chars
      (should (= width 16)))))

(ert-deftest org-table-wrap-test-overlay-at-eof-keeps-original-newline-shape ()
  "Tables at EOF do not gain a synthetic trailing newline."
  (let ((wide-table
         (concat
          "| Very long column header one | Very long column header two |\n"
          "|-----------------------------+-----------------------------|\n"
          "| content                     | more content                |")))
    (org-table-wrap-test-with-width 30 wide-table
      (org-table-wrap-mode 1)
      (let* ((entry (car org-table-wrap--overlays))
             (ov (nth 2 entry))
             (before (overlay-get ov 'before-string)))
        (should (= (length org-table-wrap--overlays) 1))
        (should-not (string-suffix-p "\n" before)))
      (org-table-wrap-mode -1))))

;;;; Regression tests

(ert-deftest org-table-wrap-test-global-mode-only-in-org-buffers ()
  "The globalized mode must not enable itself in non-Org buffers."
  (unwind-protect
      (progn
        (global-org-table-wrap-mode 1)
        (with-temp-buffer
          (fundamental-mode)
          (should-not org-table-wrap-mode))
        (with-temp-buffer
          (emacs-lisp-mode)
          (should-not org-table-wrap-mode))
        (with-temp-buffer
          (org-mode)
          (should org-table-wrap-mode)))
    (global-org-table-wrap-mode -1)))

;; Shared fixture for the regression tests below.
(defconst org-table-wrap-test--wide-table
  (concat
   "Some text before.\n"
   "| Very long column header one | Very long column header two |\n"
   "|-----------------------------+-----------------------------|\n"
   "| content                     | more content                |\n"
   "Some text after.\n"))

(defun org-table-wrap-test--live-overlays ()
  "Return all org-table-wrap overlays in the current buffer."
  (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-table-wrap))
                    (overlays-in (point-min) (point-max))))

(defmacro org-table-wrap-test--capturing-idle-timers (var &rest body)
  "Run BODY with `run-with-idle-timer' stubbed to push calls onto VAR.
Each element of VAR is (FN . ARGS)."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'run-with-idle-timer)
              (lambda (_secs _repeat fn &rest args)
                (push (cons fn args) ,var)
                'fake-timer))
             ((symbol-function 'cancel-timer) #'ignore))
     ,@body))

(ert-deftest org-table-wrap-test-row-appended-below-wrapped-table ()
  "Appending a row to a wrapped table must not leave or stack overlays."
  (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
    (org-table-wrap-mode 1)
    (should (= (length (org-table-wrap-test--live-overlays)) 1))
    (let (timers)
      (org-table-wrap-test--capturing-idle-timers timers
        ;; Insert a new row at the very end of the table, then step into it.
        (goto-char (point-min))
        (search-forward "Some text after.")
        (beginning-of-line)
        (insert "| new row                     | here                        |\n")
        (forward-line -1)
        (org-table-wrap--post-command)
        ;; Point is in the table: it must be fully revealed.
        (should (null (org-table-wrap-test--live-overlays)))
        (should (null org-table-wrap--overlays))
        ;; Leave the table and run the deferred re-wrap.
        (goto-char (point-min))
        (org-table-wrap--post-command)
        (dolist (call (reverse timers))
          (apply (car call) (cdr call)))))
    (should (= (length (org-table-wrap-test--live-overlays)) 1))
    (should (= (length org-table-wrap--overlays) 1))
    (org-table-wrap-mode -1)))

(ert-deftest org-table-wrap-test-edit-under-overlay-invalidates-it ()
  "Editing a wrapped table from outside must not leave a stale rendering."
  (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
    (org-table-wrap-mode 1)
    (goto-char (point-min))             ; point stays outside the table
    (let (timers)
      (org-table-wrap-test--capturing-idle-timers timers
        (save-excursion
          (goto-char (point-min))
          (search-forward "more content")
          (replace-match "CHANGED CELL"))
        ;; The outdated snapshot is gone immediately...
        (should (null (org-table-wrap-test--live-overlays)))
        (should (null org-table-wrap--overlays))
        ;; ...and the idle refresh renders the new text.
        (should timers)
        (dolist (call (reverse timers))
          (apply (car call) (cdr call)))))
    (let ((ovs (org-table-wrap-test--live-overlays)))
      (should (= (length ovs) 1))
      (should (string-match-p "CHANGED"
                              (overlay-get (car ovs) 'before-string))))
    (should (null org-table-wrap--dirty-markers))
    (org-table-wrap-mode -1)))

(ert-deftest org-table-wrap-test-edits-to-two-tables-refresh-both ()
  "Changes to several wrapped tables in one command re-render all of them."
  (org-table-wrap-test-with-width 30
      (concat org-table-wrap-test--wide-table "\n" org-table-wrap-test--wide-table)
    (org-table-wrap-mode 1)
    (should (= (length (org-table-wrap-test--live-overlays)) 2))
    (goto-char (point-min))
    (let (timers)
      (org-table-wrap-test--capturing-idle-timers timers
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "more content" nil t)
            (replace-match "edited stuff")))
        (dolist (call (reverse timers))
          (apply (car call) (cdr call)))))
    (let ((ovs (org-table-wrap-test--live-overlays)))
      (should (= (length ovs) 2))
      (dolist (ov ovs)
        (should (string-match-p "edited" (overlay-get ov 'before-string)))))
    (org-table-wrap-mode -1)))

(ert-deftest org-table-wrap-test-mode-disable-cancels-dirty-refresh ()
  "Disabling the mode drops queued refreshes."
  (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
    (org-table-wrap-mode 1)
    (let (timers)
      (org-table-wrap-test--capturing-idle-timers timers
        (save-excursion
          (goto-char (point-min))
          (search-forward "more content")
          (replace-match "x"))
        (should org-table-wrap--dirty-markers)
        (org-table-wrap-mode -1)
        (should (null org-table-wrap--dirty-markers))
        (should (null org-table-wrap--dirty-timer))))))

(ert-deftest org-table-wrap-test-rescan-keeps-wrapped-tables ()
  "A full re-scan (as after a resize) keeps already wrapped tables wrapped."
  (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
    (org-table-wrap-mode 1)
    (should (= (length (org-table-wrap--find-tables)) 1))
    (org-table-wrap--process-buffer)
    (should (= (length org-table-wrap--overlays) 1))
    (should (= (length (org-table-wrap-test--live-overlays)) 1))
    (org-table-wrap--process-buffer)
    (should (= (length org-table-wrap--overlays) 1))
    (should (= (length (org-table-wrap-test--live-overlays)) 1))
    (org-table-wrap-mode -1)))

(ert-deftest org-table-wrap-test-find-tables-skips-folded ()
  "Tables inside folded subtrees are still skipped."
  (org-table-wrap-test-with-buffer
      (concat "* Heading\n" "| a | b |\n" "* Other\n")
    (goto-char (point-min))
    (org-fold-subtree t)
    (should (null (org-table-wrap--find-tables)))
    (org-fold-show-all)
    (should (= (length (org-table-wrap--find-tables)) 1))))

(ert-deftest org-table-wrap-test-wrap-wide-chars-narrow-column ()
  "Wrapping double-width characters into a 1-column cell terminates."
  (should (equal (org-table-wrap--wrap-text "漢字漢字" 1)
                 '("漢" "字" "漢" "字"))))

(ert-deftest org-table-wrap-test-unfolding-wraps-hidden-table ()
  "A wide table that was folded when the mode started is wrapped on unfold."
  (org-table-wrap-test-with-width 30
      (concat "* Heading\n"
              "| Very long column header one | Very long column header two |\n"
              "| content                     | more content                |\n"
              "* Other\n")
    (org-fold-subtree t)
    (org-table-wrap-mode 1)
    (should (null org-table-wrap--overlays))
    (let (timers)
      (org-table-wrap-test--capturing-idle-timers timers
        (goto-char (point-min))
        (org-cycle)                     ; folded -> children
        (should timers)
        (dolist (call (reverse timers))
          (apply (car call) (cdr call)))))
    (should (= (length org-table-wrap--overlays) 1))
    (org-table-wrap-mode -1)
    (should-not (memq #'org-table-wrap--after-cycle org-cycle-hook))))

(ert-deftest org-table-wrap-test-natural-widths-ignore-invisible-text ()
  "Natural widths count only visible text, e.g. link descriptions."
  (let* ((link (concat (propertize "[[https://example.com/a/very/long/path][" 'invisible t)
                       "docs"
                       (propertize "]]" 'invisible t)))
         (widths (org-table-wrap--natural-widths (list (list link "abc")) 2)))
    (should (= (aref widths 0) 4))
    (should (= (aref widths 1) 3))))

;;;; Row separators

(defconst org-table-wrap-test--sep-rows
  '(("Header" "Other")
    hline
    ("short" "x")
    ("this cell is long enough to wrap" "y")
    ("tiny" "z")
    ("a" "b"))
  "Rows: header, hline, then four data rows; only the second wraps.")

(defun org-table-wrap-test--count-separators (str)
  "Count middle rules (├...┤) in display string STR."
  (cl-count-if (lambda (line) (string-prefix-p "├" line))
               (split-string str "\n")))

(ert-deftest org-table-wrap-test-row-separators-off-by-default ()
  "Without the option only the table's own hline is drawn."
  (let ((org-table-wrap-row-separators nil))
    (should (= 1 (org-table-wrap-test--count-separators
                  (org-table-wrap--build-display-string
                   org-table-wrap-test--sep-rows (vector 10 5)))))))

(ert-deftest org-table-wrap-test-row-separators-all ()
  "With t, every pair of adjacent data rows gets a separator."
  (let ((org-table-wrap-row-separators t))
    ;; 1 real hline + 3 separators between the 4 data rows; none is
    ;; added between the header and the real hline.
    (should (= 4 (org-table-wrap-test--count-separators
                  (org-table-wrap--build-display-string
                   org-table-wrap-test--sep-rows (vector 10 5)))))))

(ert-deftest org-table-wrap-test-row-separators-wrapped-only ()
  "With `wrapped', separators appear only next to multi-line rows."
  (let ((org-table-wrap-row-separators 'wrapped))
    ;; Real hline + one rule above and one below the wrapped row.
    (should (= 3 (org-table-wrap-test--count-separators
                  (org-table-wrap--build-display-string
                   org-table-wrap-test--sep-rows (vector 10 5)))))))

(ert-deftest org-table-wrap-test-row-separators-not-doubled-at-hline ()
  "No separator is added next to an existing hline."
  (let ((org-table-wrap-row-separators t))
    (should (= 1 (org-table-wrap-test--count-separators
                  (org-table-wrap--build-display-string
                   '(("a" "b") hline ("c" "d")) (vector 5 5)))))))

(ert-deftest org-table-wrap-test-row-separator-char ()
  "Added separators use `org-table-wrap-row-separator-char'; hlines don't."
  (let* ((org-table-wrap-row-separators t)
         (org-table-wrap-row-separator-char "┈")
         (lines (split-string
                 (org-table-wrap--build-display-string
                  org-table-wrap-test--sep-rows (vector 10 5))
                 "\n"))
         (rules (cl-remove-if-not (lambda (l) (string-prefix-p "├" l)) lines)))
    (should (= 4 (length rules)))
    (should (string-match-p "─" (car rules)))          ; the real hline
    (should-not (string-match-p "┈" (car rules)))
    (dolist (r (cdr rules))
      (should (string-match-p "┈" r))
      (should-not (string-match-p "─" r)))))

;;;; Window changes

(ert-deftest org-table-wrap-test-resize-skips-unchanged-width ()
  "A window change that keeps the buffer's window width does not re-scan."
  (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
    (org-table-wrap-mode 1)
    (let ((buf (current-buffer))
          (width 800)
          (processed 0))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) (selected-window)))
                ((symbol-function 'window-body-width)
                 (lambda (&rest _) width)))
        (setq org-table-wrap--last-window-width 800)
        (cl-letf (((symbol-function 'org-table-wrap--process-buffer)
                   (lambda () (when (eq (current-buffer) buf)
                                (cl-incf processed)))))
          ;; Another window switched buffers: our width is unchanged.
          (with-current-buffer buf
            (should-not (org-table-wrap--width-changed-p)))
          ;; Our window got narrower: re-scan needed.
          (setq width 600)
          (with-current-buffer buf
            (should (org-table-wrap--width-changed-p))))))
    (org-table-wrap-mode -1)))

(ert-deftest org-table-wrap-test-measure-does-not-redisplay ()
  "Measuring a rendering must not force a redisplay."
  (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
    (let ((redisplays 0))
      (cl-letf (((symbol-function 'redisplay)
                 (lambda (&rest _) (cl-incf redisplays)))
                ((symbol-function 'window-text-pixel-size)
                 (lambda (&rest _) '(100 . 10))))
        (let ((tb (save-excursion (forward-line 1)
                                  (cons (org-table-begin) (org-table-end)))))
          (org-table-wrap--apply-and-measure
           (selected-window) (car tb) (cdr tb) "x")))
      (should (= redisplays 0))
      (org-table-wrap--remove-overlays))))

;;;; Rendering all tables

(ert-deftest org-table-wrap-test-render-all-renders-narrow-table ()
  "With `org-table-wrap-render-all', a fitting table is rendered unwrapped."
  (let ((org-table-wrap-render-all t))
    (org-table-wrap-test-with-width 40 "| a | bb |\n|---+----|\n| c | d  |\n"
      (org-table-wrap-mode 1)
      (let ((ovs (org-table-wrap-test--live-overlays)))
        (should (= (length ovs) 1))
        (let ((lines (split-string
                      (substring-no-properties
                       (overlay-get (car ovs) 'before-string))
                      "\n" t)))
          ;; Natural widths: no stretching to the window, no wrapping.
          (should (equal lines '("┌───┬────┐"
                                 "│ a │ bb │"
                                 "├───┼────┤"
                                 "│ c │ d  │"
                                 "└───┴────┘")))))
      (org-table-wrap-mode -1))))

(ert-deftest org-table-wrap-test-render-all-off-leaves-narrow-table ()
  "By default a fitting table is left as plain Org text."
  (let ((org-table-wrap-render-all nil))
    (org-table-wrap-test-with-width 40 "| a | bb |\n| c | d  |\n"
      (org-table-wrap-mode 1)
      (should (null (org-table-wrap-test--live-overlays)))
      (org-table-wrap-mode -1))))

(ert-deftest org-table-wrap-test-render-all-still-wraps-wide-table ()
  "With `org-table-wrap-render-all', overflowing tables are still wrapped."
  (let ((org-table-wrap-render-all t))
    (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
      (org-table-wrap-mode 1)
      (let* ((ov (car (org-table-wrap-test--live-overlays)))
             (lines (split-string (overlay-get ov 'before-string) "\n" t)))
        (should (cl-every (lambda (l) (<= (string-width l) 30)) lines)))
      (org-table-wrap-mode -1))))

;;;; Invisibility

(ert-deftest org-table-wrap-test-hidden-table-has-no-ellipsis ()
  "The hidden table text must not be replaced by an ellipsis."
  (org-table-wrap-test-with-width 30 org-table-wrap-test--wide-table
    (org-table-wrap-mode 1)
    (let ((pos (overlay-start (car (org-table-wrap-test--live-overlays)))))
      ;; `invisible-p' returns t for plain invisibility and another
      ;; non-nil value when an ellipsis is shown.
      (should (eq (invisible-p pos) t)))
    (should (memq 'org-table-wrap buffer-invisibility-spec))
    (should-not (member '(org-table-wrap . t) buffer-invisibility-spec))
    (org-table-wrap-mode -1)
    (should-not (memq 'org-table-wrap buffer-invisibility-spec))))

(provide 'org-table-wrap-test)
;;; org-table-wrap-test.el ends here
