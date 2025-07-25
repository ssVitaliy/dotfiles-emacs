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
;; magit
;; vdiff-magit


;;; Common setups
;; Set window size
(setq default-frame-alist
      '((width . 180) (height . 52)))
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
(load-theme 'modus-vivendi)     ; Dark theme
;;(load-theme 'modus-operandi)    ; Light theme


;;; Load custom funcs
(load-file "~/.emacs.d/dotfiles/custom-funcs.el")


;;; Keybindings
(with-eval-after-load 'outline
  (keymap-set outline-minor-mode-map "<TAB>" 'outline-cycle))

;; Define key map
(defvar vs-map (make-sparse-keymap))

;; edit
(keymap-set vs-map "S-<return>" #'vs-new-line-from-anyware)
(keymap-set vs-map "C-z" #'undo)
(keymap-set vs-map "C-S-z" #'undo-redo)

;; search
(keymap-set vs-map "M-8" #'xah-extend-selection)

;; select, copy, paste
(keymap-set vs-map "M-l" #'vs-select-current-line-and-forward-line)
(keymap-set vs-map "M-s M-s" #'xah-search-current-word)
(keymap-set vs-map "M-w M-w" #'vs-copy-selection-or-current-line)
(keymap-set vs-map "M-w M-q" #'vs-cut-selection-or-current-line)
(keymap-set vs-map "C-v" #'yank)
(keymap-set vs-map "M-w M-e" #'yank)
(keymap-set vs-map "M-w M-r" #'yank-pop)

;; switch window, another window, next-window
(keymap-set vs-map "M-2" #'other-window)
(keymap-set vs-map "M-3" #'split-window-right)
(keymap-set vs-map "M-0 M--" #'delete-window)
(keymap-set vs-map "M-0 M-0" #'delete-other-windows)
(keymap-set vs-map "C-x C-k" 'kill-current-buffer)
(keymap-set vs-map "M-t" 'toggle-window-split)

(keymap-set vs-map "C-." #'kmacro-end-and-call-macro)
(keymap-set vs-map "C-c i" #'point-to-register)
(keymap-set vs-map "C-c j" #'pop-global-mark)


(define-minor-mode vs-minor-mode
  "A minor mode with keybindings that override all others.
This includes major mode and other minor mode keybindings."
  :global t  ; Makes it a global minor mode
  :lighter " vs"
  :keymap vs-map
  
  ;; Ensure our keymap has highest priority
  (if vs-minor-mode
      (progn
        ;; Add to the beginning of emulation-mode-map-alists
        (add-to-list 'emulation-mode-map-alists
                     `((vs-minor-mode . ,vs-map)))
        ;; Also add to minor-mode-map-alist for compatibility
        (add-to-list 'minor-mode-map-alist
                     (cons 'vs-minor-mode vs-map)))
    ;; When disabling
    (setq emulation-mode-map-alists
          (remove `((vs-minor-mode . ,vs-map))
                  emulation-mode-map-alists))
    (setq minor-mode-map-alist
          (assq-delete-all 'vs-minor-mode minor-mode-map-alist))))

;; Activate mode
(vs-minor-mode)


(defun reactivate-vs-minor-mode ()
  "Force reactivation of vs-minor-mode."
  (message "vs-minor-mode reactivated")
  (when vs-minor-mode
    (vs-minor-mode -1)
    (vs-minor-mode 1)))


;;; grep
(setq grep-command "grep  -nH --null -i ")


;;; ISearch
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")


;;; Dired
(file-name-shadow-mode 1)
(add-hook 'dired-mode-hook 'dired-omit-mode)
;; Automatically hide the detailed listing when visiting a Dired
;; buffer.  This can always be toggled on/off by calling the
;; `dired-hide-details-mode' interactively with M-x or its keybindings
;; (the left parenthesis by default).
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-kill-when-opening-new-dired-buffer t)

;; When there are two Dired buffers side-by-side make Emacs
;; automatically suggest the other one as the target of copy or rename
;; operations.  Remember that you can always use M-p and M-n in the
;; minibuffer to cycle through the history, regardless of what this
;; does.  (The "dwim" stands for "Do What I Mean".)
(setq dired-dwim-target t)


;;; Config packages ;;;

;; Package archives addresses
(setq package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")
	  ("elpy" . "http://jorgenschaefer.github.io/packages/")
	  ("melpa-stable" . "https://stable.melpa.org/packages/")))


;;; Vertico
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


;;; Marginalia
(use-package marginalia
  :config
  (marginalia-mode 1))


;;; Consult
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


;;; Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))


;;; Company
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

;;; Company transformers
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


;;; Company-flx
(use-package company-flx
  :after company
  :config
  (company-flx-mode)
  (setq company-flx-make-weights '(:prefix 0.8 :substring 0.7 :flex 0.5)))


;;; Company-statistics
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))
  

;;; Org mode
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


;;; magit + vdiff
(use-package magit
  :config
  (keymap-unset magit-status-mode-map "M-2")
  (keymap-unset magit-status-mode-map "M-3")
  (keymap-unset magit-stash-mode-map "M-2")
  (keymap-unset magit-stash-mode-map "M-3"))


(use-package vdiff-magit
  :after magit
  :config
  (keymap-set magit-mode-map "e" 'vdiff-magit-dwim)
  (keymap-set magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)
  ;; This flag will default to using ediff for merges.
  ;; (setq vdiff-magit-use-ediff-for-merges nil)

  ;; Whether vdiff-magit-dwim runs show variants on hunks.  If non-nil,
  ;; vdiff-magit-show-staged or vdiff-magit-show-unstaged are called based on what
  ;; section the hunk is in.  Otherwise, vdiff-magit-dwim runs vdiff-magit-stage
  ;; when point is on an uncommitted hunk.
  ;; (setq vdiff-magit-dwim-show-on-hunks nil)

  ;; Whether vdiff-magit-show-stash shows the state of the index.
  ;; (setq vdiff-magit-show-stash-with-index t)

  ;; Only use two buffers (working file and index) for vdiff-magit-stage
  (setq vdiff-magit-stage-is-2way t)
  )
