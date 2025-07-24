;;; Used packages:
;; vertico
;; marginalia
;; orderless
;; consult
;; company
;; company-flx
;; company-statistics
;; org
;; vdiff


(setq default-directory "~/")
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq delete-by-moving-to-trash t)
(setq use-short-answers t)
(setq set-mark-command-repeat-pop t)
(setq bookmark-save-flag 1)
(setq auto-save-default nil)
;; Do not create backup files
(setq make-backup-files nil)

;; Theme
;(load-theme 'modus-operandi)            ; Light theme
(load-theme 'modus-vivendi)             ; Dark theme

;;; Custom functions
;; vs-funcs
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


;; xah-funcs
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


;; Keybindings
(define-minor-mode vskeys-mode
  "Minor mode with my keys."
  :global t
  :lighter " vskeys"
  :keymap (let ((vskeys-mode-map (make-sparse-keymap)))
  ; edit
  (keymap-set vskeys-mode-map "S-<return>" #'vs-new-line-from-anyware)
  ; search
  (keymap-set vskeys-mode-map "M-8" #'xah-extend-selection)
  ; select, copy, paste
  (keymap-set vskeys-mode-map "M-l" #'vs-select-current-line-and-forward-line)
  (keymap-set vskeys-mode-map "M-s M-s" #'xah-search-current-word)

  (keymap-set vskeys-mode-map "M-w M-w" #'vs-copy-selection-or-current-line)
  (keymap-set vskeys-mode-map "M-w M-q" #'vs-cut-selection-or-current-line)
  (keymap-set vskeys-mode-map "M-w M-e" #'yank)
  (keymap-set vskeys-mode-map "M-w M-r" #'yank-pop)


  ;;; switch window, another window, next-window
  (keymap-set vskeys-mode-map "M-2" #'other-window)
  ;;; split window vertically
  (keymap-set vskeys-mode-map "M-3" #'split-window-right)
  ;;; delete window
  (keymap-set vskeys-mode-map "M-0 M--" #'delete-window)
  ;;; delete other windows
  (keymap-set vskeys-mode-map "M-0 M-0" #'delete-other-windows)

  (keymap-set vskeys-mode-map "C-v" #'yank)
  (keymap-set vskeys-mode-map "C-z" #'undo)
  (keymap-set vskeys-mode-map "C-S-z" #'undo-redo)
  (keymap-set vskeys-mode-map "C-." #'kmacro-end-and-call-macro)
  (keymap-set vskeys-mode-map "C-c i" #'point-to-register)
  (keymap-set vskeys-mode-map "C-c j" #'pop-global-mark)
  (keymap-set vskeys-mode-map "C-x C-k" 'kill-current-buffer)
  ;(keymap-set vskeys-mode-map "M-n" 'forward-paragraph)
  ;(keymap-set vskeys-mode-map "M-p" 'backward-paragraph)

    vskeys-mode-map))
(vskeys-mode t)


;; grep config
(setq grep-command "grep  -nH --null -i ")

;; ISearch config
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

;; Dired
(file-name-shadow-mode 1)
(add-hook 'dired-mode-hook 'dired-omit-mode)
;;; Automatically hide the detailed listing when visiting a Dired
;;; buffer.  This can always be toggled on/off by calling the
;;; `dired-hide-details-mode' interactively with M-x or its keybindings
;;; (the left parenthesis by default).
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-kill-when-opening-new-dired-buffer t)

;;; When there are two Dired buffers side-by-side make Emacs
;;; automatically suggest the other one as the target of copy or rename
;;; operations.  Remember that you can always use M-p and M-n in the
;;; minibuffer to cycle through the history, regardless of what this
;;; does.  (The "dwim" stands for "Do What I Mean".)
(setq dired-dwim-target t)

(setq package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")
	  ("elpy" . "http://jorgenschaefer.github.io/packages/")
	  ("melpa-stable" . "https://stable.melpa.org/packages/")))

(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1)
  ;; tidy directory, clear old path
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at
;; the top.
(savehist-mode 1)


(use-package marginalia
  :config
  (marginalia-mode 1))


(use-package consult
  :config
  (setq register-preview-delay 0.8
	register-preview-function #'consult-register-format)

  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))


(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))


(use-package company
  :init
  (global-company-mode)
  :config
  (company-mode)
  (setq company-idle-delay 0)
  (setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
  ;; (add-to-list 'company-backends 'company-dabbrev) ;; Backend for header files

  ;; Add space when finish completion with '<SPC>' - aka abort.
  (add-hook 'company-after-completion-hook
            (lambda (&rest _)
	      ;; if the last key pressed was a space character
              (when (eq last-command-event ?\s)
		(insert " "))))
  
  :bind (:map company-search-map  
              ("C-t" . company-search-toggle-filtering)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
	      ("<tab>" . company-complete)
	      ("<SPC>" . company-abort)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))


(defun vs/company-prefix-aware-sorter (candidates)
  "Sort candidates starting with prefix first, and filter :prefix unless needed."
  (let ((prefix (company-grab-symbol)))
    (if (string-prefix-p ":" prefix)
        candidates  ; Keep all if prefix starts with ':'
      ;; Else: filter ':' candidates and sort prefix matches first
      (let ((filtered (cl-remove-if (lambda (c) 
                                     (string-prefix-p ":" (if (consp c) (car c) c)))
                                   candidates)))
        (sort filtered (lambda (a b)
                        (let ((a-str (if (consp a) (car a) a))
                              (b-str (if (consp b) (car b) b)))
                          ;; Prioritize prefix matches
                          (cond
                           ((and (string-prefix-p prefix a-str)
                                 (not (string-prefix-p prefix b-str))) t)
                           ((and (not (string-prefix-p prefix a-str))
                                 (string-prefix-p prefix b-str)) nil)
                           (t (string< a-str b-str))))))))))

(add-to-list 'company-transformers 'vs/company-prefix-aware-sorter)  ; Append to end


(use-package company-flx
  :after company
  :config
  (company-flx-mode)
  (setq company-flx-make-weights '(:prefix 0.8 :substring 0.7 :flex 0.5)))


(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))
  

;; (defun vs/company-after-prefix-filter-colon (candidates)
;;   "Filter out candidates starting with ':' unless the prefix is ':'."
;;   (let ((prefix (company-grab-symbol)))
;;     (if (string-prefix-p ":" prefix)
;;         candidates  ; Keep all candidates if prefix starts with ':'
;;       (cl-remove-if (lambda (c) (string-prefix-p ":" (if (consp c) (car c) c)))
;;                    candidates))))

;; (add-to-list 'company-transformers 'vs/company-after-prefix-filter-colon)


; Org mode
(use-package org
  :config 
  (setq org-log-done 'time)
	   
  :bind (
	 :map org-mode-map
	      ("C-c C-'" . org-mark-ring-goto)
	      ("C-c C-;" . org-mark-ring-push)))


;;; vdiff
(use-package vdiff
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  :bind (
	 :map vdiff-mode-map
              ("M-2" . vdiff-switch-buffer)))

;; ;; denote package
;; (setq denote-directory (expand-file-name "~/odis-api/"))
;; (setq denote-known-keywords '("odisapi"))
;; (add-hook 'dired-mode-hook #'denote-dired-mode)

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

(keymap-set vskeys-mode-map "M-t" 'toggle-window-split)

