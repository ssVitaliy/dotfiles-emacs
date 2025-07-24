;;;; Custom functions
;;; vs-funcs
(defun vs-new-line-from-anyware ()
  "Jump to newline from anywhare in line"
  (interactive)
  (move-end-of-line 1)
  (newline))


(defun vs-select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."

  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))


(defun vs-copy-selection-or-current-line ()
  "Copy selection (kill-ring-save) or whole current line if no region is selected."

  (interactive)
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil)
  (forward-line 1))
  (kill-ring-save (region-beginning) (region-end))
  )


(defun vs-cut-selection-or-current-line ()
  "Cut selection (kill-region) or whole current line if no region is selected."

  (interactive)
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil)
  (forward-line 1))
  (kill-region (region-beginning) (region-end))
  )


;;; Toggle window split from horizontal to vertical.
;; by JeffDwork https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))


;;; xah-funcs
(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when there is no selection,
• If cursor is on any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.

URL `http://xahlee.info/emacs/emacs/emacs_extend_selection.html'
Version: 2020-02-04 2023-08-24 2023-11-14"

  (interactive)
  (cond
   ((region-active-p)
    (let ((xp1 (region-beginning)) (xp2 (region-end)))
      (goto-char xp1)
      (cond
       ((looking-at "\\s(")
        (if (eq (nth 0 (syntax-ppss)) 0)
            (progn
              ;; (message "debug: left bracket, depth 0.")
              (end-of-line) ; select current line
              (push-mark (line-beginning-position) t t))
          (progn
            ;; (message "debug: left bracket, depth not 0")
            (up-list -1 t t)
            (mark-sexp))))
       ((eq xp1 (line-beginning-position))
        (progn
          (goto-char xp1)
          (let ((xfirstLineEndPos (line-end-position)))
            (cond
             ((eq xp2 xfirstLineEndPos)
              (progn
                ;; (message "debug: exactly 1 line. extend to next whole line." )
                (forward-line 1)
                (end-of-line)))
             ((< xp2 xfirstLineEndPos)
              (progn
                ;; (message "debug: less than 1 line. complete the line." )
                (end-of-line)))
             ((> xp2 xfirstLineEndPos)
              (progn
                ;; (message "debug: beginning of line, but end is greater than 1st end of line" )
                (goto-char xp2)
                (if (eq (point) (line-end-position))
                    (progn
                      ;; (message "debug: exactly multiple lines" )
                      (forward-line 1)
                      (end-of-line))
                  (progn
                    ;; (message "debug: multiple lines but end is not eol. make it so" )
                    (goto-char xp2)
                    (end-of-line)))))
             (t (error "%s: logic error 42946" real-this-command))))))
       ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
        (progn
          ;; (message "debug: less than 1 line" )
          (end-of-line) ; select current line
          (push-mark (line-beginning-position) t t)))
       (t
        ;; (message "debug: last resort" )
        nil))))

   ((looking-at "\\s(")
    ;; (message "debug: left bracket")
    (mark-sexp))

   ((looking-at "\\s)")
    ;; (message "debug: right bracket")
    (backward-up-list) (mark-sexp))

   ((looking-at "\\s\"")
    ;; (message "debug: string quote")
    (mark-sexp))

   ((looking-at "[ \t\n]")
    ;; (message "debug: is white space")
    (skip-chars-backward " \t\n")
    (push-mark)
    (skip-chars-forward " \t\n")
    (setq mark-active t))

   ((looking-at "[-_a-zA-Z0-9]")
    ;; (message "debug: left is word or symbol")
    (skip-chars-backward "-_a-zA-Z0-9")
    (push-mark)
    (skip-chars-forward "-_a-zA-Z0-9")
    (setq mark-active t))

   ((and (looking-at "[:blank:]")
         (prog2 (backward-char) (looking-at "[:blank:]") (forward-char)))
    ;; (message "debug: left and right both space" )
    (skip-chars-backward "[:blank:]") (push-mark (point) t t)
    (skip-chars-forward "[:blank:]"))

   ((and (looking-at "\n")
         (eq (char-before) 10))
    ;; (message "debug: left and right both newline")
    (skip-chars-forward "\n")
    (push-mark (point)  t t)
    (re-search-forward "\n[ \t]*\n"))

   (t
    ;; (message "debug: just mark sexp" )
    (mark-sexp)
    (exchange-point-and-mark))))

(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_], independent of syntax table.

URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Created: 2010-05-29
Version: 2025-02-05"
  (interactive)
  (let (xbeg xend)
    (if (region-active-p)
        (setq xbeg (region-beginning) xend (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq xbeg (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq xend (point))))
    (when (< xbeg (point)) (goto-char xbeg))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties xbeg xend))))

