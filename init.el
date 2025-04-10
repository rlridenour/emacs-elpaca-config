;;; init.el --- Personal Emacs configuration file -*- lexical-binding: t; -*-

(defconst rr-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst rr-cache-dir "~/.cache/emacs/"
  "The directory for Emacs activity files.")

(defconst rr-backup-dir (concat rr-cache-dir "backup/")
  "The directory for Emacs backup files.")

(defconst rr-org-dir "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/"
  "The directory for my org files.")

(defconst rr-agenda-dir "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
  "The directory for RR-Emacs note storage.")

(defconst rr-notes-dir "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/"
  "The directory for RR-Emacs note storage.")

;;;; Create directories if non-existing
(dolist (dir (list rr-cache-dir
		     rr-backup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; set load path
(add-to-list 'load-path (concat rr-emacs-dir "elisp"))

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				:ref nil :depth 1 :inherit ignore
				:files (:defaults "elpaca-test.el" (:exclude "extensions"))
				:build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
	 (build (expand-file-name "elpaca/" elpaca-builds-directory))
	 (order (cdr elpaca-order))
	 (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	  (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		    ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						    ,@(when-let* ((depth (plist-get order :depth)))
							(list (format "--depth=%d" depth) "--no-single-branch"))
						    ,(plist-get order :repo) ,repo))))
		    ((zerop (call-process "git" nil buffer t "checkout"
					  (or (plist-get order :ref) "--"))))
		    (emacs (concat invocation-directory invocation-name))
		    ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
					  "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		    ((require 'elpaca))
		    ((elpaca-generate-autoloads "elpaca" repo)))
	      (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	    (error "%s" (with-current-buffer buffer (buffer-string))))
	((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca (org :wait t))

(elpaca elpaca-use-package
  (require 'elpaca-use-package)
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(defmacro with-after-elpaca-init (&rest body)
  "Adds BODY to `elpaca-after-init-hook`"
  `(add-hook 'elpaca-after-init-hook (lambda () ,@body)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq sentence-end-double-space nil)

(setq-default tab-width 10)

(setq insert-directory-program "gls")

(setq message-kill-buffer-on-exit t)

(setf use-short-answers t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq ns-function-modifier 'hyper)
;; ns-right-command-modifier 'hyper)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq world-clock-list
	'(
	("America/Chicago" "Oklahoma City")
	("America/Los_Angeles" "Seattle")
	("Pacific/Honolulu" "Honolulu")
	("America/New_York" "New York")
	("Etc/UTC" "UTC")))

(setq world-clock-time-format "%a, %d %b %R %Z")

(setq calendar-location-name "Norman, OK"
	calendar-latitude 35.24371
	calendar-longitude -97.416797
	calendar-mark-holidays-flag t        ;colorize holidays in the calendar
	holiday-bahai-holidays nil           ;these religions have MANY holidays
	holiday-islamic-holidays nil         ;... that I don't get off
	)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(line-number-mode)
(column-number-mode)
(global-visual-line-mode 1)
(global-hl-line-mode)
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

;; Where to save to backup file - in the backup dir
(setq backup-directory-alist (list (cons "."  rr-backup-dir)))
;; Always backup by copying
(setq backup-by-copying t)
;; Delete old backup files
(setq delete-old-versions t)
;; Keep 5 backup files
(setq kept-new-versions 5)
;; Make numeric backup versions
(setq version-control t)
;; Do not automatically save
(setq auto-save-default nil)

;;;;; = saveplace - last position in file
;; Save point position in files between sessions.

;; Where to save the saveplaces file - in the .cache
(setq save-place-file (expand-file-name "saveplaces" rr-cache-dir))
(save-place-mode)

(setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs")

(require 'uniquify)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
	dired-auto-revert-buffer t
	auto-revert-verbose nil)

(setq ibuffer-expert t)

(add-hook 'ibuffer-mode-hook
	    #'(lambda ()
		(ibuffer-auto-mode 1)
		(ibuffer-switch-to-saved-filter-groups "home")))

;;;;; = savehist - last commands used
;; Persist emacs minibuffer history
;; Where to save the savehsit file - in the .cache
(setq savehist-file (expand-file-name "savehist" rr-cache-dir))
(savehist-mode)

(setq large-file-warning-threshold nil)

(add-hook 'before-save-hook 'time-stamp)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))

(setq kill-buffer-query-functions nil)

(add-to-list 'display-buffer-alist
	       (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))
(add-hook 'find-file-not-found-functions #'make-parent-directory)

(defun nuke-all-buffers ()
    "Kill all the open buffers except the current one.
	  Leave *scratch*, *dashboard* and *Messages* alone too."
    (interactive)
    (mapc
     (lambda (buffer)
       (unless (or
		(string= (buffer-name buffer) "*scratch*")
		(string= (buffer-name buffer) "*Org Agenda*")
		(string= (buffer-name buffer) "*Messages*"))
	 (kill-buffer buffer)))
     (buffer-list))
    (delete-other-windows)
(tab-bar-close-other-tabs)
    ;; (goto-dashboard)
    )

(defun rlr/kill-other-buffers ()
(interactive)
(crux-kill-other-buffers)
(tab-bar-close-other-tabs))

(defun goto-emacs-init ()
  (interactive)
  "Find Emacs literate init file."
  (find-file (concat rr-emacs-dir "/init.org")))

(defun goto-shell-init ()
  (interactive)
  "Find Emacs literate init file."
  (find-file "~/.config/fish/functions/"))

(setq save-interprogram-paste-before-kill t)

(setq initial-major-mode 'org-mode)

(setq default-input-method 'TeX)

(delete-selection-mode 1)

(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows))

(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun rlr/find-file-right ()
  "Split window vertically and select recent file."
  (interactive)
  (split-window-right-focus)
  (consult-buffer))

(defun rlr/find-file-below ()
  "Split window horizontally and select recent file."
  (interactive)
  (split-window-below-focus)
  (consult-buffer))

(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
	   (w1 (car windows))
	   (w2 (nth 1 windows))
	   (w1b (window-buffer w1))
	   (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

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

(defun toggle-frame-maximized-undecorated () (interactive) (let* ((frame (selected-frame)) (on? (and (frame-parameter frame 'undecorated) (eq (frame-parameter frame 'fullscreen) 'maximized))) (geom (frame-monitor-attribute 'geometry)) (x (nth 0 geom)) (y (nth 1 geom)) (display-height (nth 3 geom)) (display-width (nth 2 geom)) (cut (if on? (if ns-auto-hide-menu-bar 26 50) (if ns-auto-hide-menu-bar 4 26)))) (set-frame-position frame x y) (set-frame-parameter frame 'fullscreen-restore 'maximized) (set-frame-parameter nil 'fullscreen 'maximized) (set-frame-parameter frame 'undecorated (not on?)) (set-frame-height frame (- display-height cut) nil t) (set-frame-width frame (- display-width 20) nil t) (set-frame-position frame x y)))

(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
If there are only two windows, jump directly to the other window."
  (interactive)
  (let* ((window-list (window-list nil 'no-mini)))
    (if (< (length window-list) 3)
	  ;; If only one window, switch to previous buffer. If only two, jump directly to other window.
	(if (one-window-p)
	      (switch-to-buffer nil)
	    (other-window 1))
	;; Otherwise, show the key selection interface.
	(let* ((my/quick-window-overlays nil)
	       (sorted-windows (sort window-list
				     (lambda (w1 w2)
				       (let ((edges1 (window-edges w1))
					     (edges2 (window-edges w2)))
					 (or (< (car edges1) (car edges2))
					     (and (= (car edges1) (car edges2))
						  (< (cadr edges1) (cadr edges2))))))))
	       (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f")
				      (length sorted-windows)))
	       (window-map (cl-pairlis window-keys sorted-windows)))
	  (setq my/quick-window-overlays
		(mapcar (lambda (entry)
			  (let* ((key (car entry))
				 (window (cdr entry))
				 (start (window-start window))
				 (overlay (make-overlay start start (window-buffer window))))
			    (overlay-put overlay 'after-string
					 (propertize (format "[%s]" key)
						     'face '(:foreground "white" :background "blue" :weight bold)))
			    (overlay-put overlay 'window window)
			    overlay))
			window-map))
	  (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
	    (mapc #'delete-overlay my/quick-window-overlays)
	    (setq my/quick-window-overlays nil)
	    (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
	      (select-window selected-window)))))))

(set-face-attribute 'mode-line nil
		      :foreground "black" :background "wheat3" :box '(:line-width 1 :color "black"))

(setq display-time-24hr-format t)
(display-time-mode)

(setq ring-bell-function 'ignore)

(show-paren-mode)
(setq show-paren-delay 0)

(defun rlr/delete-tab-or-frame ()
  "Delete current tab. If there is only one tab, then delete current frame."
  (interactive)
  (if
	(not (one-window-p))
	(delete-window)
    (condition-case nil
	(tab-close)
	(error (delete-frame)))))

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160 :weight 'medium)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 1.0 :weight 'medium)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0 :weight 'medium)

(setq-default line-spacing 0.25)

(setq case-replace nil)

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

(setq locate-command "mdfind")

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(setq async-shell-command-buffer "new-buffer")

(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
	((display-buffer-alist
	  (list
	   (cons
	    "\\*Async Shell Command\\*.*"
	    (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
	      ;; string escaping madness for applescript
	      (replace-regexp-in-string "\\\\" "\\\\\\\\"
					(shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )

(setq eshell-scroll-to-bottom-on-input "this")

(setq help-window-select t)
(setq Man-notify-method 'aggressive)

(defun insert-date-string ()
  "Insert current date yyyymmdd."
  (interactive)
  (insert (format-time-string "%Y%m%d")))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%B %e, %Y")))

(defun insert-blog-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-")))

(defun reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(defun rr/wrap-at-sentences ()
  "Fills the current paragraph, but starts each sentence on a new line."
  (interactive)
  (save-excursion
    ;; Select the entire paragraph.
    (mark-paragraph)
    ;; Move to the start of the paragraph.
    (goto-char (region-beginning))
    ;; Record the location of the end of the paragraph.
    (setq end-of-paragraph (region-end))
    ;; Wrap lines with hard newlines.
    (let ((use-hard-newlines 't))
	;; Loop over each sentence in the paragraph.
	(while (< (point) end-of-paragraph)
	  ;; Move to end of sentence.
	  (forward-sentence)
	  ;; Delete spaces after sentence.
	(just-one-space)
	;; Delete preceding space.
	  (delete-char -1)
	  ;; Insert a newline before the next sentence.
	  (insert "\n")
	  ))))

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t) (replace-match "\n")))

(defun rr/stretchlink-cc ()
  (interactive)
  (progn
    (setq current-safari-url (do-applescript "tell application \"Safari\" to return URL of document 1"))
    (shell-command
     (concat "curl " "\"https://stretchlink.cc/api/1?u=" current-safari-url "&t=1&c=1&o=text\" | pbcopy"))
    (setq myurl (yank))
    (message myurl)))

(defun rr/open-init-file ()
  (interactive)
  (progn (find-file "~/.config/emacs/init.org")
	 (variable-pitch-mode -1)))

(defun delete-extra-blank-lines ()
(interactive)
(save-excursion)
(beginning-of-buffer)
(replace-regexp "^\n\n+" "\n"))

(use-package general
  :ensure (:wait t)
  :demand
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  (general-unbind
    "C-z"
    "H-w"
    "s-p"
    "s-q"
    "s-w"
    "s-m"
    "s-n"
    "s-h"
    "s-,"))

(use-package aas)

(use-package laas
:hook
(LaTeX-mode . laas-mode))

(use-feature abbrev
  :config
  (load "~/Dropbox/emacs/my-emacs-abbrev"))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package avy
  :config
  (avy-setup-default)
  :general
  ("s-/" #'avy-goto-char-timer)
  ("C-c C-j" #'avy-resume))

(use-feature bookmark
  :config
  (require 'bookmark)
  (bookmark-bmenu-list)
  (setq bookmark-save-flag 1))

(use-package casual
  :ensure
  (:type git :host github :repo "kickingvegas/casual")
  :general
  ("s-." #'casual-editkit-main-tmenu)
  (:keymaps 'reb-mode-map
	      "s-." #'casual-re-builder-tmenu)
  (:keymaps 'calc-mode-map
	      "s-." #'casual-calc-tmenu)
  (:keymaps 'dired-mode-map
	      "s-." #'casual-dired-tmenu)
  (:keymaps 'isearch-mode-map
	      "s-." #'casual-isearch-tmenu)
  (:keymaps 'ibuffer-mode-map
	      "s-." #'casual-ibuffer-tmenu
	      "F" #'casual-ibuffer-filter-tmenu
	      "s" #'casual-ibuffer-sortby-tmenu)
  (:keymaps 'bookmark-bemenu-mode-map
	      "s-." #'casual-bookmarks-tmenu)
  (:keymaps 'org-agenda-mode-map
	      "s-." #'casual-agenda-tmenu)
  (:keymaps 'Info-mode-map
	      "s-." #'casual-info-tmenu)
  (:keymaps 'calendar-mode-map
	      "s-." #'casual-calendar-tmenu)
  )

(use-package casual-avy
  :general
  ("M-g a" #'casual-avy-tmenu))

(use-package cape
  :commands (cape-file)
  :general (:prefix "M-p"
		      "p" 'completion-at-point ;; capf
		      "d" 'cape-dabbrev        ;; or dabbrev-completion
		      "a" 'cape-abbrev
		      "w" 'cape-dict
		      "\\" 'cape-tex
		      "_" 'cape-tex
		      "^" 'cape-tex)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  )

(use-package chordpro-mode)

(use-package citar
  :bind (("C-c C-b" . citar-insert-citation)
	   :map minibuffer-local-map
	   ("M-b" . citar-insert-preset))
  :custom
  (org-cite-global-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (citar-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (org-cite-csl-styles-dir "/usr/local/texlive/2024/texmf-dist/tex/latex/citation-style-language/styles")
  (org-cite-export-processors
   '((md . (csl "chicago-author-date.csl"))
     (latex biblatex)
     (odt . (csl "chicago-author-date.csl"))
     (t . (csl "chicago-author-date.csl")))))

(use-package consult
  :demand
  :config
  (defun rlr/consult-rg ()
    "Function for `consult-ripgrep' with the `universal-argument'."
    (interactive)
    (consult-ripgrep (list 4)))

  (defun rlr/consult-fd ()
    "Function for `consult-find' with the `universal-argument'."
    (interactive)
    (consult-find (list 4)))
  :general
  ("C-x b" #'consult-buffer))

(use-package corfu
  :custom
  (corfu-cycle t)
  :config
  (global-corfu-mode))

(use-package crux
  :general
  ("s-p" #'crux-create-scratch-buffer))

(use-package deadgrep)

(use-package denote
  :config
  (setq denote-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/denote/")
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-date-format nil)
  (require 'denote-journal-extras))

(use-package consult-notes
  :config
  (consult-notes-denote-mode))

(use-package citar-denote
  :after citar denote
  :config
  (citar-denote-mode)
  (setq citar-open-always-create-notes t))

(use-package denote-menu
  :after denote)

(use-package denote-search
  :ensure (:host github :repo "lmq-10/denote-search")
  :custom
  ;; Disable help string (set it once you learn the commands)
  ;; (denote-search-help-string "")
  ;; Display keywords in results buffer
  (denote-search-format-heading-function #'denote-search-format-heading-with-keywords))

(use-package devil
  :init
  (global-devil-mode))

(use-package dired+
  :demand
  :ensure (:host github :repo "emacsmirror/dired-plus"))

(defun hide-dired-details-include-all-subdir-paths ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward dired-subdir-regexp nil t)
	(let* ((match-bounds (cons (match-beginning 1) (match-end 1)))
	       (path (file-name-directory (buffer-substring (car match-bounds)
							    (cdr match-bounds))))
	       (path-start (car match-bounds))
	       (path-end (+ (car match-bounds) (length path)))
	       (inhibit-read-only t))
	  (put-text-property path-start path-end
			     'invisible 'dired-hide-details-information)))))

(use-feature dired
  :hook ((dired-mode . dired-hide-details-mode)
	   (dired-after-readin . hide-dired-details-include-all-subdir-paths)))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode 1))

(use-package dired-x
  :ensure nil
  :config
  (progn
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (setq dired-omit-files
	    (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^\\..+$"))
    (setq-default dired-omit-extensions '(".fdb_latexmk" ".aux" ".bbl" ".blg" ".fls" ".glo" ".idx" ".ilg" ".ind" ".ist" ".log" ".out" ".gz" ".DS_Store" ".xml" ".bcf" ".nav" ".snm" ".toc"))))

(with-after-elpaca-init
 (add-hook 'dired-mode-hook #'dired-omit-mode))

(setq dired-dwim-target t)

(defun my-substspaces (str)
  (subst-char-in-string ?\s ?- str))

(defun my-dired-substspaces (&optional arg)
  "Rename all marked (or next ARG) files so that spaces are replaced with underscores."
  (interactive "P")
  (dired-rename-non-directory #'my-substspaces "Rename by substituting spaces" arg))

(general-define-key
 :keymaps 'dired-mode-map
 "M-<RET>" #'crux-open-with
 "s-j" #'dired-goto-file
 "%s" #'my-dired-substspaces)

(use-package discover
  :config
  (global-discover-mode 1))

(use-package doom-modeline
  :init
  :config
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq display-time-day-and-date t)
  (setq doom-modeline-modal t)
  :hook
  (elpaca-after-init . doom-modeline-mode))

(use-package eat
  :demand
  :ensure
  (:host codeberg
	   :repo "akib/emacs-eat"
	   :files ("*.el" ("term" "term/*.el") "*.texi"
		   "*.ti" ("terminfo/e" "terminfo/e/*")
		   ("terminfo/65" "terminfo/65/*")
		   ("integration" "integration/*")
		   (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package ebib
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  ;;(evil-set-initial-state 'ebib-index-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-entry-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-log-mode 'emacs)
  :custom
  (ebib-preload-bib-files '("~/Dropbox/bibtex/rlr.bib")))

(use-package elfeed
    :demand
    :init
    (setq elfeed-db-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/elfeed")
    :config
    :general
    (:keymaps 'elfeed-search-mode-map
	  "q" #'rlr/elfeed-save-db-and-quit)
(:keymaps 'elfeed-show-mode-map
	  "S-<SPC>" #'scroll-down))

(defun rlr/elfeed-load-db-and-open ()
    "Load elfeed db before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force)
    (elfeed-update))

(defun rlr/open-elfeed-new-tab ()
(interactive)
(tab-new)
(rlr/elfeed-load-db-and-open)
(elfeed-update))

  (defun rlr/elfeed-save-db-and-quit ()
    (interactive)
    (elfeed-db-save)
    (elfeed-search-quit-window)
    (rlr/delete-tab-or-frame))

(defmacro elfeed-tag-selection-as (mytag)
  "Tag elfeed entry as MYTAG"
  `(lambda (&optional user-generic-p)
     (interactive "P")
     (let ((entries (elfeed-search-selected)))
	 (cl-loop for entry in entries
		  do (funcall (if (elfeed-tagged-p ,mytag entry)
				  #'elfeed-untag #'elfeed-tag)
			      entry ,mytag)
		  do (elfeed-untag entry 'unread))
	 (mapc #'elfeed-search-update-entry entries)
	 (unless (use-region-p) (forward-line)))))

(general-define-key
 :keymaps 'elfeed-search-mode-map
 "l" (elfeed-tag-selection-as 'readlater)
 "d" (elfeed-tag-selection-as 'junk)
 "m" (elfeed-tag-selection-as 'starred)
 "M" (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +starred"))
 "L" (lambda () (interactive) (elfeed-search-set-filter "+readlater"))
 )

(use-package elfeed-org
  :after elfeed
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/elfeed/elfeed.org"))
  :config
  ;; (setq rmh-elfeed-org-auto-ignore-invalid-feeds t)
  )

(use-package embark
  :general
  ("C-." #'embark-act)
  ("C-:" #'embark-dwim)
  ("C-h B" #'embark-bindings) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nil
		   (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t
	  evil-track-eol nil
	  evil-want-fine-undo t
	  evil-disable-insert-state-bindings t)
  :config
  (evil-mode -1))

(use-package evil-nerd-commenter
  :general
  ("M-;" #'evilnc-comment-or-uncomment-lines))

(use-feature eww
  :config
  (defun rlr/open-eww-link-new-buffer ()
    (interactive)
    (link-hint-copy-link)
    (tab-new)
    (setq new-buffer-url (current-kill 0 t))
    (switch-to-buffer (generate-new-buffer "*eww*"))
    (eww-mode)
    (eww new-buffer-url))
  (defun rlr/eww-toggle-images ()
    "Toggle whether images are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s"
	       (if shr-inhibit-images "off" "on")))
  ;; (define-key eww-mode-map (kbd "I") #'rlr/eww-toggle-images)
  ;; (define-key eww-link-keymap (kbd "I") #'rlr/eww-toggle-images)
  ;; minimal rendering by default
  (setq-default shr-inhibit-images t)   ; toggle with `I`
  (setq-default shr-use-fonts t)      ; toggle with `F`
  (defun rrnet ()
    (interactive)
    (eww-browse-url "randyridenour.net")
    )
  (defun sep ()
    (interactive)
    (eww-browse-url "plato.stanford.edu")
    )
  :general
  (:keymaps 'eww-mode-map
	  "I" #'rlr/eww-toggle-images
	  "f" #'link-hint-open-link
	  "F" #'rlr/open-eww-link-new-buffer
	  "T" #'eww-toggle-fonts)
  (:keymaps 'eww-link-keymap
	  "I" #'rlr/eww-toggle-images)
  )

(with-after-elpaca-init
 (defun jao-eww-to-org (&optional dest)
   "Render the current eww buffer using org markup.
  If DEST, a buffer, is provided, insert the markup there."
   (interactive)
   (unless (org-region-active-p)
     (let ((shr-width 80)) (eww-readable)))
   (let* ((start (if (org-region-active-p) (region-beginning) (point-min)))
	    (end (if (org-region-active-p) (region-end) (point-max)))
	    (buff (or dest (generate-new-buffer "*eww-to-org*")))
	    (link (eww-current-url))
	    (title (or (plist-get eww-data :title) "")))
     (with-current-buffer buff
	 (insert "#+title: " title "\n#+link: " link "\n\n")
	 (org-mode))
     (save-excursion
	 (goto-char start)
	 (while (< (point) end)
	   (let* ((p (point))
		  (props (text-properties-at p))
		  (k (seq-find (lambda (x) (plist-get props x))
			       '(shr-url image-url outline-level face)))
		  (prop (and k (list k (plist-get props k))))
		  (next (if prop
			    (next-single-property-change p (car prop) nil end)
			  (next-property-change p nil end)))
		  (txt (buffer-substring (point) next))
		  (txt (replace-regexp-in-string "\\*" "·" txt)))
	     (with-current-buffer buff
	       (insert
		(pcase prop
		  ((and (or `(shr-url ,url) `(image-url ,url))
			(guard (string-match-p "^http" url)))
		   (let ((tt (replace-regexp-in-string "\n\\([^$]\\)" " \\1" txt)))
		     (org-link-make-string url tt)))
		  (`(outline-level ,n)
		   (concat (make-string (- (* 2 n) 1) ?*) " " txt "\n"))
		  ('(face italic) (format "/%s/ " (string-trim txt)))
		  ('(face bold) (format "*%s* " (string-trim txt)))
		  (_ txt))))
	     (goto-char next))))
     (pop-to-buffer buff)
     (goto-char (point-min)))))

(defun rlr/open-safari-page-in-eww ()
(interactive)
(org-mac-link-safari-get-frontmost-url)
(setq rlr-org-link (current-kill 0 t))
(setq rlr-org-link (s-chop-left 2 rlr-org-link))
(setq rlr-org-link (s-chop-right 2 rlr-org-link))
(setq rlr-org-link (s-split "\\]\\[" rlr-org-link))
(setq rlr-org-url (pop rlr-org-link))
(eww rlr-org-url))

(use-package emmet-mode
  :general
  (:keymaps 'html-mode-map
	      "C-M-S-s-<right>" #'emmet-next-edit-point
	      "C-M-S-s-<left>" #'emmet-prev-edit-point))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :general ("C-=" #'er/expand-region))

(use-package fish-mode)

(use-package fish-completion
  :ensure (:type git :host github :repo "LemonBreezes/emacs-fish-completion")
  :config
  (when (and (executable-find "fish")
	       (require 'fish-completion nil t))
    (global-fish-completion-mode)))

(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
	  fzf/executable "fzf"
	  fzf/git-grep-args "-i --line-number %s"
	  ;; command used for `fzf-grep-*` functions
	  ;; example usage for ripgrep:
	  fzf/grep-command "rg --no-heading -nH"
	  ;; fzf/grep-command "grep -nrH"
	  ;; If nil, the fzf buffer will appear at the top of the window
	  fzf/position-bottom t
	  fzf/window-height 15))

(use-package helpful)

(use-feature savehist
  :config
  (savehist-mode 1))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(defun rr/insert-unicode (unicode-name)
  "Same as C-x 8 enter UNICODE-NAME."
  (insert-char (gethash unicode-name (ucs-names))))

(use-package major-mode-hydra
  :commands (pretty-hydra-define)
  :general
  ("s-m" #'major-mode-hydra))

(with-after-elpaca-init
 (progn
   (pretty-hydra-define hydra-toggle
     (:color teal :quit-key "q" :title "Toggle")
     (" "
	(("a" abbrev-mode "abbrev" :toggle t)
	 ("b" toggle-debug-on-error "debug" (default value 'debug-on-error))
	 ("d" global-devil-mode "devil" :toggle t)
	 ("e" evil-mode "evil" :toggle t)
	 ("i" aggressive-indent-mode "indent" :toggle t)
	 ("f" auto-fill-mode "fill" :toggle t)
	 ("l" display-line-numbers-mode "linum" :toggle t)
	 ("m" variable-pitch-mode "variable-pitch" :toggle t)
	 ("p" electric-pair-mode "electric-pair" :toggle t)
	 ("t" toggle-truncate-lines "truncate" :toggle t)
	 ("s" whitespace-mode "whitespace" :toggle t))
	" "
	(("c" cdlatex-mode "cdlatex" :toggle t)
	 ("o" olivetti-mode "olivetti" :toggle t)
	 ("r" read-only-mode "read-only" :toggle t)
	 ("v" view-mode "view" :toggle t)
	 ("W" wc-mode "word-count" :toggle t)
	 ("S" auto-save-visited-mode "auto-save" :toggle t)
	 ("C" cua-selection-mode "rectangle" :toggle t))))
   (pretty-hydra-define hydra-buffer
     (:color teal :quit-key "q" :title "Buffers and Files")
     ("Open"
	(("b" ibuffer "ibuffer")
	 ("m" consult-bookmark "bookmark")
	 ("w" consult-buffer-other-window "other window")
	 ("f" consult-buffer-other-frame "other frame")
	 ("d" crux-recentf-find-directory "recent directory")
	 ("a" crux-open-with "open in default app"))
	"Actions"
	(("D" crux-delete-file-and-buffer "delete file")
	 ("R" crux-rename-file-and-buffer "rename file")
	 ("K" rlr/kill-other-buffers "kill other buffers")
	 ("N" nuke-all-buffers "Kill all buffers")
	 ("c" crux-cleanup-buffer-or-region "fix indentation"))
	"Misc"
	(("t" crux-visit-term-buffer "ansi-term")
	 ("T" iterm-goto-filedir-or-home "iTerm2")
	 ("i" crux-find-user-init-file "init.el")
	 ("s" crux-find-shell-init-file "fish config"))
	))
   (pretty-hydra-define hydra-locate
     (:color teal :quit-key "q" title: "Search")
     ("Buffer"
	(("c" pulsar-highlight-dwim "find cursor")
	 ("h" consult-org-heading "org heading")
	 ("l" consult-goto-line "goto-line")
	 ("i" consult-imenu "imenu")
	 ("m" consult-mark "mark")
	 ("o" consult-outline "outline"))
	"Global"
	(("M" consult-global-mark "global-mark")
	 ("n" consult-notes "notes")
	 ("r" consult-ripgrep "ripgrep")
	 ("d" rlr/consult-rg "rg from dir")
	 ("f" rlr/consult-fd "find from dir"))
	"Files"
	(("e" rr/open-init-file "Emacs init")
	 ("s" goto-shell-init "Fish functions"))
	))
   (pretty-hydra-define hydra-window
     (:color teal :quit-key "q" title: "Windows")
     ("Windows"
	(("w" other-window "cycle windows" :exit nil)
	 ("a" ace-window "ace window")
	 ("m" minimize-window "minimize window")
	 ("s" transpose-windows "swap windows")
	 ("S" shrink-window-if-larger-than-buffer "shrink to fit")
	 ("b" balance-windows "balance windows")
	 ("t" toggle-window-split "toggle split")
	 ("T" enlarge-window" grow taller" :exit nil)
	 ("G" enlarge-window-horizontally "grow wider" :exit nil)
	 ("o" delete-other-windows "kill other windows"))
	"Frames"
	(("M" iconify-frame "minimize frame")
	 ("d" delete-other-frames "delete other frames")
	 ("D" delete-frame "delete this frame")
	 ("i" make-frame-invisible "invisible frame")
	 ("f" toggle-frame-fullscreen "fullscreen")
	 ("n" make-frame-command "new frame"))
	"Writeroom"
	(("W" writeroom-mode "toggle writeroom")
	 ("M" writeroom-toggle-mode-line "toggle modeline"))))

   (pretty-hydra-define hydra-new
     (:color teal :quit-key "q" title: "New")
     ("Frame"
	(("f" make-frame-command "new frame"))
	"Denote"
	(("c" org-capture "capture")
	 ("n" denote "note")
	 ("v" denote-menu-list-notes "view notes")
	 ("j" denote-journal-extras-new-or-existing-entry "journal"))
	"Writing"
	(("b" rlrt-new-post "blog post")
	 ("a" rlrt-new-article "article"))
	"Teaching"
	(("l" rlrt-new-lecture "lecture")
	 ("h" rlrt-new-handout "handout")
	 ("s" rlrt-new-syllabus "syllabus"))
	))

   (pretty-hydra-define hydra-logic
     (:color pink :quit-key "0" :title "Logic")
     ("Operators"
	(
	 ;; ("1" (rr/insert-unicode "NOT SIGN") "¬")
	 ("1" (rr/insert-unicode "TILDE OPERATOR") "∼")
	 ;; ("2" (rr/insert-unicode "AMPERSAND") "&")
	 ("2" (rr/insert-unicode "BULLET") "•")
	 ("3" (rr/insert-unicode "LOGICAL OR") "v")
	 ("4" (rr/insert-unicode "SUPERSET OF") "⊃")
	 ;; ("4" (rr/insert-unicode "RIGHTWARDS ARROW") "→")
	 ("5" (rr/insert-unicode "IDENTICAL TO") "≡")
	 ;; ("5" (rr/insert-unicode "LEFT RIGHT ARROW") "↔")
	 ("6" (rr/insert-unicode "THERE EXISTS") "∃")
	 ("7" (rr/insert-unicode "FOR ALL") "∀")
	 ("8" (rr/insert-unicode "WHITE MEDIUM SQUARE") "□")
	 ("9" (rr/insert-unicode "LOZENGE") "◊")
	 ("`" (rr/insert-unicode "NOT EQUAL TO") "≠"))
	"Space"
	(("?" (rr/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space"))
	"Quit"
	(("0" quit-window "quit" :color blue))
	))

   (pretty-hydra-define hydra-math
     (:color pink :quit-key "?" :title "Math")
     ("Operators"
	(("1" (rr/insert-unicode "NOT SIGN") "¬")
	 ("2" (rr/insert-unicode "AMPERSAND") "&")
	 ("3" (rr/insert-unicode "LOGICAL OR") "v")
	 ("4" (rr/insert-unicode "RIGHTWARDS ARROW") "→")
	 ("5" (rr/insert-unicode "LEFT RIGHT ARROW") "↔")
	 ("6" (rr/insert-unicode "THERE EXISTS") "∃")
	 ("7" (rr/insert-unicode "FOR ALL") "∀")
	 ("8" (rr/insert-unicode "WHITE MEDIUM SQUARE") "□")
	 ("9" (rr/insert-unicode "LOZENGE") "◊"))
	"Sets"
	(("R" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL R") "ℝ real")
	 ("N" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL N") "ℕ natural")
	 ("Z" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Z") "ℤ integer")
	 ("Q" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
	 ("Q" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
	 ("Q" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
	 )
	"Space"
	(("?" (rr/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space"))
	"Quit"
	(("?" quit-window "quit" :color blue))
	))

   (pretty-hydra-define hydra-hydras
     (:color teal :quit-key "q" :title "Hydras")
     ("System"
	(("t" hydra-toggle/body)
	 ("b" hydra-buffer/body)
	 ("h" hydra-hugo/body)
	 ("p" powerthesaurus-hydra/body))
	"Unicode"
	(("l" hydra-logic/body "logic")
	 ("m" hydra-math/body))))
   ))

(with-after-elpaca-init
 (progn
   (major-mode-hydra-define dashboard-mode
     (:quit-key "q")
     ("Open"
	(("m" consult-bookmark "bookmarks")
	 ("a" consult-org-agenda "consult-agenda")
	 ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
	 ("b" (find-file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org") "web bookmarks"))))

   (major-mode-hydra-define org-agenda-mode
     (:quit-key "q")
     ("Open"
	(
	 ("a" consult-org-agenda "consult-agenda")
	 ("b" consult-bookmark "bookmarks")
	 ("m" mu4e "rlr/read-mail-news")
	 ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
	 ("w" (find-file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org") "web bookmarks"))
	"Classes"
	(("1" (dired "~/icloud/teaching/intro/lectures") "Intro")
	 ("2" (dired "~/icloud/teaching/medieval/lectures") "Medieval")
	 ("3" (dired "~/icloud/teaching/logic/lectures") "Logic")
	 ("4" (dired "~/icloud/teaching/language/lectures") "Language")
	 )
	"Actions"
	(("s" rlr/save-web-page-as-org-file "Save Safari page as Org")
	 ("e" rlr/open-safari-page-in-eww "Open Safari page in EWW"))
	))

   (major-mode-hydra-define eww-mode
     (:quit-key "q")
     ("A"
	(
	 ;; ("G" eww "Eww Open Browser")
	 ("g" eww-reload "Eww Reload")
	 ("6" eww-open-in-new-buffer "Open in new buffer")
	 ("l" eww-back-url "Back Url")
	 ("r" eww-forward-url "Forward Url")
	 ("N" eww-next-url "Next Url")
	 ("P" eww-previous-url "Previous Url")
	 ("u" eww-up-url "Up Url")
	 ("&" eww-browse-with-external-browser "Open in External Browser")
	 ("d" eww-download "Download")
	 ("w" eww-copy-page-url "Copy Url Page")
	 );end theme
	"B"
	(
	 ("T" rlr/eww-toggle-images "Toggle Image Display")
	 (">" shr-next-link "Shr Next Link")
	 ("<" shr-previous-link "Shr Previous Link")
	 ("n" scroll-down-command "Scroll Down")
	 ("C" url-cookie-list "Url Cookie List")
	 ("v" eww-view-source "View Source")
	 ("R" eww-readable "Make Readable")
	 ("H" eww-list-histories "List History")
	 ("E" eww-set-character-encoding "Character Encoding")
	 ("s" eww-switch-to-buffer "Switch to Buffer")
	 ("S" eww-list-buffers "List Buffers")
	 );end highlighting

	"C"
	(
	 ("1" rrnet "randyridenour.net")
	 ("2" sep "SEP")
	 ("F" eww-toggle-fonts "Toggle Fonts")
	 ("D" eww-toggle-paragraph-direction "Toggle Paragraph Direction")
	 ("c" eww-toggle-colors "Toggle Colors")
	 ("b" eww-add-bookmark "Add Bookmark")
	 ("B" eww-list-bookmarks "List Bookmarks")
	 ("=" eww-next-bookmark "Next Bookmark")
	 ("-" eww-previous-bookmark "Previous Bookmark")
	 ("O" jao-eww-to-org "Make Org Version")
	 ("<SPC>" nil "Quit" :color pink)
	 );end other
	))

   (major-mode-hydra-define markdown-mode
     (:quit-key "q")
     ("Format"
	(("h" markdown-insert-header-dwim "header")
	 ("l" markdown-insert-link "link")
	 ("u" markdown-insert-uri "url")
	 ("f" markdown-insert-footnote "footnote")
	 ("w" markdown-insert-wiki-link "wiki")
	 ("r" markdown-insert-reference-link-dwim "r-link")
	 ("n" markdown-cleanup-list-numbers "clean-lists")
	 ("c" markdown-complete-buffer "complete"))))

   (major-mode-hydra-define LaTeX-mode
     (:quit-key "q")
     ("Bibtex"
	(("r" citar-insert-citation "citation"))
	"LaTeXmk"
	(("m" rlr/tex-mkpdf "PDFLaTeX")
	 ("l" rlr/tex-mklua "LuaLaTeX")
	 ("w" rlr/tex-mktc "watch PDFLaTeX")
	 ("L" rlr/tex-mklua "watch LuaLaTeX")
	 ("c" tex-clean "clean aux")
	 ("C" tex-clean-all "clean all")
	 ("n" latex-word-count "word count"))))

   (major-mode-hydra-define org-mode
     (:quit-key "q")
     ("Export"
	(("m" rlr/org-mkpdf "Make PDF with PDFLaTeX")
	 ("p" rlr/org-open-pdf "View PDF")
	 ("h" make-html "HTML")
	 ("l" rlr/org-mklua "Make PDF with LuaLaTeX")
	 ("el" org-latex-export-to-latex "Org to LaTeX")
	 ("eb" org-beamer-export-to-pdf "Org to Beamer-PDF")
	 ("eB" org-beamer-export-to-latex "Org to Beamer-LaTeX")
	 ("s" lecture-slides "Lecture slides")
	 ("n" lecture-notes "Lecture notes")
	 ("ep" present "Present slides")
	 ("ec" canvas-copy "Copy HTML for Canvas")
	 ("es" canvas-notes "HTML Canvas notes")
	 ("eS" make-syllabus "Syllabus")
	 ("eh" make-handout "Handout")
	 ("c" tex-clean "clean aux")
	 ("C" tex-clean-all "clean all"))
	"Edit"
	(("a" org-appear-mode :toggle t)
	 ("dd" org-deadline "deadline")
	 ("ds" org-schedule "schedule")
	 ("r" org-refile "refile")
	 ("du" rlr/org-date "update date stamp")
	 ;; ("fn" org-footnote-new "insert footnote")
	 ("ff" org-footnote-action "edit footnote")
	 ("fc" citar-insert-citation "citation")
	 ("il" org-mac-link-safari-insert-frontmost-url "insert safari link")
	 ("y" yankpad-set-category "set yankpad"))
	"View"
	(("vi" consult-org-heading "iMenu")
	 ("vu" org-toggle-pretty-entities "org-pretty")
	 ("vI" org-toggle-inline-images "Inline images"))
	"Blog"
	(("bn" rlrt-new-post "New draft")
	 ("bt" orgblog-add-tag "Add tag")
	 ("bi" orgblog-insert-image "Insert image")
	 ("bp" orgblog-publish-draft "Publish draft")
	 ("bb" orgblog-build "Build site")
	 ("bs" orgblog-serve "Serve site")
	 ("bd" orgblog-push "Push to Github"))
	"Notes"
	(("1" denote-link "link to note"))))

   (major-mode-hydra-define dired-mode
     (:quit-key "q")
     ("New"
	(("a" rlrt-new-article "article")
	 ("l" rlrt-new-lecture "lecture")
	 ("h" rlrt-new-handout "handout")
	 ("s" rlrt-new-syllabus "syllabus"))
	"Tools"
	(("d" crux-open-with "Open in default program")
	 ("." dired-omit-mode "Show hidden files")
	 ("p" diredp-copy-abs-filenames-as-kill "Copy filename and path")
	 ("n" dired-toggle-read-only "edit Filenames"))
	"Blog"
	(("bn" rlrt-new-post "New draft")
	 ("bb" orgblog-build "Build Site")
	 ("bs" orgblog-serve "Serve Site")
	 ("bd" orgblog-push "Push to Github"))))

   (major-mode-hydra-define css-mode
     (:quit-key "q")
     ("Blog"
	(("bn" rlrt-new-post "New draft")
	 ("bb" orgblog-build "Build Site")
	 ("bs" orgblog-serve "Serve Site")
	 ("bd" orgblog-push "Push to Github"))))

   (major-mode-hydra-define denote-menu-mode
     (:quit-key "q")
     ("Tools"
	(("f" denote-menu-filter "Filter by regex")
	 ("k" denote-menu-filter-by-keyword "Filter by keyword")
	 ("c" denote-menu-clear-filters "Clear filters")
	 ("d" denote-menu-export-to-dired "Dired"))))

   (major-mode-hydra-define mu4e-view-mode
     (:quit-key "q")
     ("Message"
	(("r" mu4e-compose-reply "Reply")
	 ("a"  mu4e-compose-wide-reply "Reply All"))))

   )
 )

(general-define-key
 "s-h" #'hydra-hydras/body
 "s-n" #'hydra-new/body
 "H-t" #'hydra-toggle/body
 "H-w" #'hydra-window/body
 ;; "s-b" #'hydra-buffer/body
 "C-x 9" #'hydra-logic/body)

(use-package jinx
  :init
  (setenv "PKG_CONFIG_PATH" (concat "/opt/homebrew/opt/glib/lib/pkgconfig/:" (getenv "PKG_CONFIG_PATH")))
  :config
  (setq ispell-silently-savep t)
  :hook (emacs-startup . global-jinx-mode)
  :general
  ([remap ispell-word] #'jinx-correct
   "<f7>" #'jinx-correct
   "S-<f7>" #'jinx-correct-all))

(use-package auctex
  :ensure (auctex :pre-build (("./autogen.sh")
				("./configure"
				 "--without-texmf-dir"
				 "--with-packagelispdir=./"
				 "--with-packagedatadir=./")
				("make"))
		    :build (:not elpaca--compile-info) ;; Make will take care of this step
		    :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
		    :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t
	  TeX-auto-save t
	  TeX-electric-math nil
	  LaTeX-electric-left-right-brace nil
	  TeX-electric-sub-and-superscript nil
	  LaTeX-item-indent 0
	  TeX-quote-after-quote nil
	  TeX-clean-confirm nil
	  TeX-source-correlate-mode t
	  TeX-source-correlate-method 'synctex
	  TeX-view-program-selection '((output-pdf "PDF Viewer"))
	  TeX-view-program-list
	  '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))))

(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' "))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)
(defun tex-clean ()
  (interactive)
  (shell-command "latexmk -c"))

(defun tex-clean-all ()
  (interactive)
  (shell-command "latexmk -C"))

(defun arara-all ()
  (interactive)
  (async-shell-command "mkall"))

(defun rlr/tex-mkpdf ()
  "Compile with pdf latexmk."
  (interactive)
  (save-buffer)
  (async-shell-command-no-window (concat "mkpdf " (shell-quote-argument(file-name-nondirectory buffer-file-name))))
  (TeX-view))

(defun rlr/tex-mktc ()
  "Compile continuously with pdf latexmk."
  (interactive)
  (async-shell-command-no-window (concat "mkpdfc " (shell-quote-argument(file-name-nondirectory buffer-file-name)))))

(defun rlr/tex-mklua ()
  "Compile with lua latexmk."
  (interactive)
  (save-buffer)
  (async-shell-command-no-window (concat "mklua " (shell-quote-argument(file-name-nondirectory buffer-file-name))))
  (TeX-view))

(defun rlr/tex-mkluac ()
  "Compile continuously with lua latexmk."
  (interactive)
  (async-shell-command-no-window (concat "mkluac " (shell-quote-argument(file-name-nondirectory buffer-file-name)))))

(defun latex-word-count ()
  (interactive)
  (let* ((this-file (buffer-file-name))
	   (word-count
	    (with-output-to-string
	      (with-current-buffer standard-output
		(call-process "texcount" nil t nil "-brief" this-file)))))
    (string-match "\n$" word-count)
    (message (replace-match "" nil nil word-count))))

(use-package math-delimiters
  :ensure
  (:type git :host github :repo "oantolin/math-delimiters")
  :after (:any org latex)
  :commands (math-delimiters-no-dollars math-delimiters-mode)
  :hook ((LaTeX-mode . math-delimiters-mode)
	   (org-mode . math-delimiters-mode))
  :config (progn
	      (setq math-delimiters-compressed-display-math nil)
	      (define-minor-mode math-delimiters-mode
		"Math Delimeters"
		:init-value nil
		:lighter " MD"
		:keymap (let ((map (make-sparse-keymap)))
			  (define-key map (kbd "$")  #'math-delimiters-insert)
			  map))))

(use-package link-hint
  :general
  ("s-," #'link-hint-open-link
   "C-c l o" #'link-hint-open-link
   "C-c l c" #'link-hint-copy-link))

(use-package transient)
(use-package hl-todo
  :ensure (:depth nil))

(use-package magit
  :init
  (require 'transient)
  :custom
  (magit-repository-directories (list (cons elpaca-repos-directory 1)))
  (magit-diff-refine-hunk 'all)
  :config
  (transient-bind-q-to-quit))

(use-package marginalia
  :config (marginalia-mode))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
	   ("\\.md\\'" . markdown-mode)
	   ("\\.Rmd\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-asymmetric-header t))

;; Convert markdown files to org format.
(fset 'convert-markdown-to-org
	[?\M-< ?\M-% ?* return ?- return ?! ?\M-< ?\C-\M-% ?# ?* backspace backspace ?  ?# ?* ?$ return return ?! ?\M-< ?\M-% ?# return ?* return ?!])

(fset 'copy-beamer-note
	(kmacro-lambda-form [?\C-r ?: ?E ?N ?D return down ?\C-  ?\C-s ?* ?* ?  ?N ?o ?t ?e ?s return up ?\M-w ?\C-s ?: ?E ?N ?D return down return ?\s-v return] 0 "%d"))

(defun cc/markdown-to-org-region (start end)
  "Convert Markdown formatted text in region (START, END) to Org.

This command requires that pandoc (man page `pandoc(1)') be
installed."
  (interactive "r")
  (shell-command-on-region
   start end
   "pandoc -f markdown -t org --wrap=preserve" t t))

(use-package mastodon
  :config
  (mastodon-discover)
  (setq mastodon-instance-url "https://zirk.us/"
	  mastodon-active-user "randyridenour"))

(defun rlr/open-mastodon ()
  (interactive)
  (tab-new)
  (mastodon))

(use-package modern-tab-bar
  :ensure
  (:host github :repo "aaronjensen/emacs-modern-tab-bar")
  :init
  (setq tab-bar-show t
	  tab-bar-new-button nil
	  tab-bar-close-button-show nil)
  (modern-tab-bar-mode))

(defun rlr/find-file-new-tab ()
  "Open new tab and select recent file."
  (interactive)
  (tab-new)
  (consult-buffer))

(use-package modus-themes
  :demand
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	  modus-themes-mixed-fonts t
	  modus-themes-variable-pitch-ui t
	  modus-themes-italic-constructs t
	  modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
	  modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t)
  :general
  ("<f9>" #'modus-themes-rotate))

(defvar-keymap notepad-mode-map
  "C-c C-c" #'copy-kill-buffer)

(define-derived-mode notepad-mode
  org-mode "Notepad"
  "Major mode for scratch buffers."
  )

(defun rlr/create-notepad-buffer ()
  "Create a new notepad buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*notepad*")))
    (switch-to-buffer buf))
  (notepad-mode)
  (shell-command-on-region (point) (if mark-active (mark) (point)) "pbpaste" nil t))

(defun app-switch ()
  (interactive)
  (shell-command "switch-paste"))

(general-define-key
 "C-s-<tab>" #'app-switch)

(defun copy-kill-buffer ()
  (interactive)
  (goto-char (point-max))
  (newline)
  (mark-whole-buffer)
  (copy-region-as-kill 1 (buffer-size))
  (kill-buffer)
  ;; (app-switch)
  (shell-command "open -a ~/icloud/scripts/beep.app"))

(use-package olivetti)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :ensure nil
  :init
  ;; (setq org-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
  (setq org-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
  :config
  (setq org-list-allow-alphabetical t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  (setq org-hide-emphasis-markers t)
  (setq org-list-indent-offset 2)

  (set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold :inherit 'fixed-pitch)
  (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold :inherit 'fixed-pitch)
  (set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold :inherit 'fixed-pitch)
  (set-face-attribute 'org-level-4 nil :height 1.0 :weight 'bold :inherit 'fixed-pitch)
  (set-face-attribute 'org-level-5 nil :height 1.0 :weight 'bold :inherit 'fixed-pitch)
  (set-face-attribute 'org-level-6 nil :height 1.0 :weight 'bold :inherit 'fixed-pitch)
  (set-face-attribute 'org-level-7 nil :height 1.0 :weight 'bold :inherit 'fixed-pitch)
  (set-face-attribute 'org-level-8 nil :height 1.0 :weight 'bold :inherit 'fixed-pitch)

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)

  ;; Make LaTeX previews larger.
  (plist-put org-format-latex-options :scale 1.5)

  (setq org-support-shift-select t)
  (setq org-special-ctrl-a/e t)
  ;; (setq org-footnote-section nil)
  (setq org-html-validation-link nil)
  (setq org-time-stamp-rounding-minutes '(0 15))
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-log-done t)
  (setq org-todo-keyword-faces
	  '(("DONE" . "green4") ("TODO" . org-warning)))
  (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"))
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-link-frame-setup
	  '((vm . vm-visit-folder-other-frame)
	(vm-imap . vm-visit-imap-folder-other-frame)
	(gnus . org-gnus-no-new-news)
	(file . find-file)
	(wl . wl-other-frame)))
  (require 'org-tempo)
  ;; Open directory links in Dired.
  (add-to-list 'org-file-apps '(directory . emacs)))

(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'markdown-mode-hook #'variable-pitch-mode)

(general-define-key
 "C-M-S-s-v" #'variable-pitch-mode)

(use-package org-appear
  :commands (org-appear-mode)
  ;; :hook     (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)  ; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ; Show bold, italics, verbatim, etc.
	  org-appear-autolinks      t   ; Show links
	  org-appear-autosubmarkers t)) ; Show sub and superscripts

(use-feature mu4e
  :commands (mu4e mu4e-update-mail-and-index)
  :general
  (:keymaps 'mu4e-headers-mode-map
	      "q"  #'kill-current-buffer)
  (:keymaps 'mu4e-main-mode-map
	      "q"  #'rlr/quit-mu4e)
  :after org
  :config
  (setq
   mu4e-split-view 'horizontal
   mu4e-index-update-error-warning nil
   mu4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-use-fancy-chars t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachments-dir "~/Downloads"
   mu4e-maildir       "~/Maildir/"   ;; top-level Maildir
   ;; note that these folders below must start with /
   ;; the paths are relative to maildir root

   ;; this setting allows to re-sync and re-index mail
   ;; by pressing U
   mu4e-get-mail-command "mbsync -a"
   mu4e-update-interval 300 ;; update every 5 minutes
   ;; mu4e-headers-auto-update
   mu4e-completing-read-function 'completing-read
   mu4e-context-policy 'pick-first
   mu4e-contexts (list
		    (make-mu4e-context
		     :name "fastmail"
		     :match-func
		     (lambda (msg)
		       (when msg
			 (string-prefix-p "/fastmail" (mu4e-message-field msg :maildir))))
		     :vars '((user-mail-address . "rlridenour@fastmail.com")
			     (user-full-name    . "Randy Ridenour")
			     (mu4e-drafts-folder  . "/fastmail/Drafts")
			     (mu4e-sent-folder  . "/fastmail/Sent")
			     (mu4e-trash-folder  . "/fastmail/Trash")
			     (mu4e-refile-folder  . "/fastmail/Archive")
			     (sendmail-program . "msmtp")
			     (send-mail-function . smtpmail-send-it)
			     (message-sendmail-f-is-evil . t)
			     (message-sendmail-extra-arguments . ("--read-envelope-from"))
			     (message-send-mail-function . message-send-mail-with-sendmail)
			     (smtpmail-default-smtp-server . "smtp.fastmail.com")
			     (smtpmail-smtp-server  . "smtp.fastmail.com")
			     ))
		    (make-mu4e-context
		     :name "obu"
		     :match-func
		     (lambda (msg)
		       (when msg
			 (string-prefix-p "/obu" (mu4e-message-field msg :maildir))))
		     :vars '((user-mail-address . "randy.ridenour@okbu.edu")
			     (user-full-name    . "Randy Ridenour")
			     (mu4e-drafts-folder  . "/obu/Drafts")
			     (mu4e-sent-folder  . "/obu/Sent")
			     (mu4e-trash-folder . "/obu/Trash")
			     (mu4e-refile-folder  . "/obu/Archive")
			     ;; (sendmail-program . "msmtp")
			     (send-mail-function . smtpmail-send-it)
			     (message-sendmail-f-is-evil . t)
			     (message-sendmail-extra-arguments . ("--read-envelope-from"))
			     (message-send-mail-function . message-send-mail-with-sendmail)
			     (smtpmail-smtp-server  . "localhost")
			     (smtpmail-smtp-user . "randy.ridenour@okbu.edu")
			     (smtpmail-stream-type . plain)
			     (smtpmail-smtp-service . 1025)
			     ))))
  (display-line-numbers-mode -1))

(defun rlr/quit-mu4e ()
  (interactive)
  (mu4e-quit)
  (rlr/delete-tab-or-frame))

(use-package mu4e-alert
  :config
  (mu4e-alert-enable-mode-line-display))

(defun obu-signature ()
  (interactive)
  (insert (concat
	 "\n\n"
	 "--\n"
	 "Randy Ridenour, Ph.D.\n"
	 "Professor of Philosophy\n"
	 "Oklahoma Baptist University\n\n"
	 "500 W. University St.\n"
	 "Shawnee, OK  74804\n"
	 "Office: (405) 585-4432\n")
	))

(defun informal-signature ()
  (interactive)
  (insert (concat
	 "\n\n"
	 "--\n"
	 "Randy"
	 )))

(defun rlr/open-mu4e-new-tab ()
  (interactive)
  (tab-new)
  (mu4e)
  (mu4e-update-mail-and-index 1))

(defun rlr/read-mail-news ()
    (interactive)
    ;; (make-frame-command)
    ;; (agenda-home)
    (tab-new)
    (rlr/elfeed-load-db-and-open)
    (elfeed-update)
    (tab-new)
    (mu4e)
    (mu4e-update-mail-and-index 1)
)

  (general-define-key
   "C-M-s-r" #'rlr/read-mail-news)

(use-package org-modern
  :config
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  )

(require 'ox-beamer)
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
		 '("org-article"
		   "\\documentclass{article}
			      [NO-DEFAULT-PACKAGES]
			      [NO-PACKAGES]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
		 '("org-handout"
		   "\\documentclass{pdfhandout}
			      [NO-DEFAULT-PACKAGES]
			      [NO-PACKAGES]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
		 '("org-beamer"
		   "\\documentclass{beamer}
			      [NO-DEFAULT-PACKAGES]
			      [NO-PACKAGES]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(setq org-export-with-smart-quotes t)
(with-eval-after-load 'ox-latex
  (add-to-list 'org-export-smart-quotes-alist
		 '("en-us"
		   (primary-opening   :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
		   (primary-closing   :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
		   (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
		   (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
		   (apostrophe        :utf-8 "’" :html "&rsquo;"))))

;; (setq org-latex-pdf-process '("arara %f"))
(setq org-latex-pdf-process '("mkpdf %f"))

(defun rlr/org-mkpdf ()
  "Make PDF with pdf latexmk."
  (interactive)
  (save-buffer)
  (org-latex-export-to-latex)
  (async-shell-command-no-window (concat "mkpdf " (shell-quote-argument(file-name-nondirectory (file-name-with-extension buffer-file-name "tex"))))))

(defun rlr/org-open-pdf ()
  "Open PDF in background with default viewer."
  (interactive)
  (async-shell-command-no-window (concat "open -g " (shell-quote-argument(file-name-nondirectory (file-name-with-extension buffer-file-name "pdf"))))))

(defun rlr/org-mklua ()
  "Make PDF with lua latexmk."
  (interactive)
  (save-buffer)
  (org-latex-export-to-latex)
  (async-shell-command-no-window (concat "mklua " (shell-quote-argument(file-name-nondirectory (file-name-with-extension buffer-file-name "tex"))))))

(defun rlr/org-arara ()
  "Make PDF with Arara."
  (interactive)
  (save-buffer)
  (org-arara-export-to-latex)
  (async-shell-command-no-window (concat "mkarara " (shell-quote-argument(file-name-sans-extension (buffer-file-name)))".tex")))

(defun rlr/org-date ()
  "Update existing date: timestamp on a Hugo post."
  (interactive)
  (save-excursion (
		     goto-char 1)
		    (re-search-forward "^#\\+date:")
		    (let ((beg (point)))
		      (end-of-line)
		      (delete-region beg (point)))
		    (insert (concat " " (format-time-string "%B %e, %Y")))))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; Org-capture
(setq org-capture-templates
	'(
	  ("t" "Todo" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org" "Inbox")
	   "** TODO %?\n  %i\n  %a")
	  ("e" "Event" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/events.org" "Future")
	   "** %? %T")
	  ("b" "Bookmark" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org" "Bookmarks")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("c" "Quick note" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/quick-notes.org" "Notes")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  )
	)

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
		 '("n" "New note (with Denote)" plain
		   (file denote-last-path)
		   #'denote-org-capture
		   :no-save t
		   :immediate-finish nil
		   :kill-buffer t
		   :jump-to-captured t)))

(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

(define-key global-map "\C-cc" 'org-capture)

(use-package org-super-agenda
  :after org
  :ensure t
  :config
  (setq org-agenda-skip-scheduled-if-done t
	  org-agenda-skip-deadline-if-done t
	  org-agenda-include-deadlines t
	  org-agenda-block-separator nil
	  org-agenda-compact-blocks t
	  org-agenda-start-day nil ;; i.e. today
	  org-agenda-span 1
	  org-agenda-window-setup "current-window"
	  org-agenda-include-diary nil
	  org-agenda-start-on-weekday nil)
  (setq org-agenda-time-grid
	  '((daily today require-timed remove-match)
	    ()
	    "......"
	    ""))

  (org-super-agenda-mode))

(setq org-agenda-custom-commands
	'(("d" "Agenda for today" agenda ""
	   ((org-agenda-overriding-header "Today's agenda")
	    (org-agenda-span 'day)
	    ))))

(defun today-agenda ()
  "Display today's agenda"
  (interactive)
  (org-agenda nil "d")
  )
(today-agenda)

(with-eval-after-load 'org
  (add-to-list
   'org-agenda-custom-commands
   `("c" "Today - Full View"
     ((agenda ""
		((org-agenda-entry-types '(:timestamp :sexp))
		 (org-agenda-overriding-header
		  (concat "CALENDAR Today "
			  (format-time-string "%a %d" (current-time))))
		 (org-agenda-span 'day)))
	(tags-todo "LEVEL=1+inbox"
		   ((org-agenda-overriding-header "INBOX (Unscheduled)")))
	(tags-todo "DEADLINE<\"<+1d>\"+DEADLINE>\"<-1d>\""
		   ((org-agenda-overriding-header "DUE TODAY")
		    (org-agenda-skip-function
		     '(org-agenda-skip-entry-if 'notdeadline))
		    (org-agenda-sorting-strategy '(priority-down))))
	(tags-todo "DEADLINE<\"<today>\""
		   ((org-agenda-overriding-header "OVERDUE")
		    (org-agenda-skip-function
		     '(org-agenda-skip-entry-if 'notdeadline))
		    (org-agenda-sorting-strategy '(priority-down))))
	(agenda ""
		((org-agenda-entry-types '(:scheduled))
		 (org-agenda-overriding-header "SCHEDULED")
		 (org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-sorting-strategy
		  '(priority-down time-down))
		 (org-agenda-span 'day)
		 (org-agenda-start-on-weekday nil)))
	(todo "DONE"
	      ((org-agenda-overriding-header "COMPLETED"))))
     ((org-agenda-format-date "")
	(org-agenda-start-with-clockreport-mode nil))) t))

(defun agenda-home ()
  (interactive)
  (org-agenda-list 1)
  (delete-other-windows))

(add-hook 'server-after-make-frame-hook 'agenda-home)

(general-define-key
 "s-d" #'agenda-home)

(defun rr/agenda-links ()
  (end-of-buffer)
  (insert-file-contents "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/agenda-links.org")
  (while (org-activate-links (point-max))
    (goto-char (match-end 0)))
  ;; (end-of-buffer)
  ;; (insert (concat "\n\n" (get-votd)))
  (beginning-of-buffer))

(add-hook 'org-agenda-finalize-hook #'rr/agenda-links)

(setq org-return-follows-link t)

(general-define-key
 :keymaps 'org-agenda-mode-map
 "<SPC>" #'link-hint-open-link)

(setopt org-link-elisp-skip-confirm-regexp "rlr.*")

(setq appt-time-msg-list nil)    ;; clear existing appt list
;; (setq appt-message-warning-time '15)  ;; send first warning 15 minutes before appointment
(org-agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt) ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (require 'org-tempo)
  (require 'ox-rss))

(use-package orgonomic
  :ensure
  (:host github :repo "aaronjensen/emacs-orgonomic")
  :hook (org-mode . orgonomic-mode))

(defun my/org-toggle-emphasis (type)
  "Toggle org emphasis TYPE (a character) at point."
  (cl-labels ((in-emph (re)
		  "See if in org emphasis given by RE."
		  (and (org-in-regexp re 2)
		       (>= (point) (match-beginning 3))
		       (<= (point) (match-end 4))))
		(de-emphasize ()
		  "Remove most recently matched org emphasis markers."
		  (save-excursion
		    (replace-match "" nil nil nil 3)
		    (delete-region (match-end 4) (1+ (match-end 4))))))
    (let* ((res (vector org-emph-re org-verbatim-re))
	     (idx (cl-case type (?/ 0) (?* 0) (?_ 0) (?+ 0) (?= 1) (?~ 1)))
	     (re (aref res idx))
	     (other-re (aref res (- 1 idx)))
	     (type-re (string-replace (if (= idx 1) "=~" "*/_+")
				      (char-to-string type) re))
	     add-bounds offset is-word)
	(save-match-data
	  (if (region-active-p)
	      (if (in-emph type-re) (de-emphasize) (org-emphasize type))
	    (if (eq (char-before) type) (backward-char))
	    (if (in-emph type-re)       ;nothing marked, in emph text?
		(de-emphasize)
	      (setq add-bounds          ; check other flavors
		    (if (or (in-emph re) (in-emph other-re))
			(cons (match-beginning 4) (match-end 4))
		      (setq is-word t)
		      (bounds-of-thing-at-point 'symbol))))
	    (if add-bounds
		(let ((off (- (point) (car add-bounds)))
		      (at-end (= (point) (cdr add-bounds))))
		  (set-mark (car add-bounds))
		  (goto-char (cdr add-bounds))
		  (org-emphasize type)  ;deletes marked region!
		  (unless is-word       ; delete extra spaces
		    (goto-char (car add-bounds))
		    (when (eq (char-after) ?\s) (delete-char 1))
		    (goto-char (+ 2 (cdr add-bounds)))
		    (when (eq (char-after) ?\s) (delete-char 1)))
		  (goto-char (+ (car add-bounds) off
				(cond ((= off 0) 0) (at-end 2) (t 1)))))
	      (if is-word (org-emphasize type))))))))

(general-define-key
 :keymaps 'org-mode-map
 "s-<right>" #'org-end-of-line
 "s-<left>" #'org-beginning-of-line
 "s-i" (lambda () (interactive) (my/org-toggle-emphasis ?/))
 "s-b" (lambda () (interactive) (my/org-toggle-emphasis ?*))
 "C-c e e" (lambda () (interactive) (my/org-toggle-emphasis ?~))
 "C-c e =" (lambda () (interactive) (my/org-toggle-emphasis ?=))
 "C-c e _" (lambda () (interactive) (my/org-toggle-emphasis ?_))
 "C-c e +" (lambda () (interactive) (my/org-toggle-emphasis ?+)))

(use-package org-mac-link)

(use-package org-web-tools)

(defun rlr/save-web-page-as-org-file ()
  (interactive)
  (org-mac-link-safari-get-frontmost-url)
  (setq rlr-org-link (current-kill 0 t))
  (setq rlr-org-link (s-chop-left 2 rlr-org-link))
  (setq rlr-org-link (s-chop-right 2 rlr-org-link))
  (setq rlr-org-link (s-split "\\]\\[" rlr-org-link))
  (setq rlr-org-url (pop rlr-org-link))
  (setq rlr-org-title (pop rlr-org-link))
  (setq rlr-org-title (s-replace-all '(("." . "") (":" . "") ("/" . "")) rlr-org-title))
  (setq rlr-org-filename (s-dashed-words rlr-org-title))
  (org-web-tools-read-url-as-org rlr-org-url)
  (write-file (concat "~/icloud/web-saves/" rlr-org-title ".org")))

(defvar rlrt-filename)

(defun rlrt-make-filename (string)
  (s-downcase  (s-join "-" (s-split " " (replace-regexp-in-string "\\bthe \\b\\|\\band \\b\\|\\b[a-z]\\b \\|\\b[a-z][a-z]\\b \\|[[:punct:]]" "" string)))))

(defun rlrt-new-handout (rlrt-title)
  (interactive "sTitle: ")

  ;; Make filename
  (setq rlrt-filename (rlrt-make-filename rlrt-title))

  ;; Create directory
  (make-directory rlrt-filename)

  ;; Create main org file
  (find-file (s-concat rlrt-filename "/" rlrt-filename "-handout.org"))
  (insert (s-concat "#+TITLE: " rlrt-title) ?\n"#+AUTHOR: Dr. Randy Ridenour" ?\n "#+DATE: "(format-time-string "%B %e, %Y") ?\n)
  (insert-file-contents "~/.config/emacs/teaching-templates/handout/handout.org")
  (goto-char (point-max))
  (save-buffer))

(defun rlrt-new-syllabus (rlrt-title)
  (interactive "sTitle: ")

  ;; Make filename
  (setq rlrt-filename (rlrt-make-filename rlrt-title))

  ;; Create directory
  (make-directory rlrt-filename)

  ;; Create main org file
  (find-file (s-concat rlrt-filename "/" rlrt-filename "-syllabus.org"))
  (insert-file-contents "~/.config/emacs/teaching-templates/syllabus/syllabus.org")
  (goto-char (point-max))
  (insert (s-concat "#+include: \"" rlrt-filename "-data.org\" :minlevel 1"))
  (save-buffer)
  (kill-buffer)

  ;; Create Canvas file
  (find-file (s-concat rlrt-filename "/canvas.org"))
  (insert-file-contents "~/.config/emacs/teaching-templates/syllabus/canvas.org")
  (save-buffer)
  (kill-buffer)

  ;; Create data file
  (find-file (s-concat rlrt-filename "/" rlrt-filename "-data.org")))

(defun rlrt-new-lecture (rlrt-title)
  (interactive "sTitle: ")

  ;; Make filename
  (setq rlrt-filename (rlrt-make-filename rlrt-title))

  ;; Create directory
  (make-directory rlrt-filename)

(find-file (s-concat rlrt-filename "/" rlrt-filename "-slides.org"))
(insert-file-contents "~/.config/emacs/teaching-templates/lecture/slides.org")
(goto-char (point-max))
(insert (s-concat "#+include: \"" rlrt-filename "-data.org\" :minlevel 1"))
(save-buffer)
(kill-buffer)

(find-file (s-concat rlrt-filename "/" rlrt-filename "-notes.org"))
(insert-file-contents "~/.config/emacs/teaching-templates/lecture/notes.org")
(goto-char (point-max))
(insert (s-concat "#+include: \"" rlrt-filename "-data.org\" :minlevel 1"))
(save-buffer)
(kill-buffer)

(find-file (s-concat rlrt-filename "/canvas.org"))
(insert-file-contents "~/.config/emacs/teaching-templates/lecture/canvas.org")
(goto-char (point-max))
(save-buffer)
(kill-buffer)

(find-file (s-concat rlrt-filename "/" rlrt-filename "-data.org"))
(insert (s-concat "#+TITLE: " rlrt-title) ?\n)
(yas-expand-snippet (yas-lookup-snippet "beamer-data")))

(defun make-slides ()
  (async-shell-command-no-window "mkslides"))

(defun make-notes ()
  (async-shell-command-no-window "mknotes"))

(defun lecture-slides ()
  "publish org data file as beamer slides"
  (interactive)
  (save-buffer)
  (find-file "*-slides.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (make-slides)
  (find-file "*-data.org" t))

(defun rlr/create-frametitle ()
  "Convert title to frametitle."
  (interactive)
  (goto-char 1)
  (while (ignore-errors
	     (re-search-forward "begin{frame}.*]"))
    (insert "\n \\frametitle")))

(defun lecture-notes ()
  "publish org data file as beamer notes"
  (interactive)
  (save-buffer)
  (find-file "*-notes.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (find-file "*-notes.tex" t)
  (rlr/create-frametitle)
  (save-buffer)
  (kill-buffer)
  (make-notes)
  (find-file "*-data.org" t))

(defun canvas-copy ()
  "Copy html for canvas pages"
  (interactive)
  (save-buffer)
  (org-html-export-to-html)
  (shell-command "canvas"))

(defun canvas-notes ()
  "Copy HTML slide notes for Canvas"
  (interactive)
  (save-buffer)
  (shell-command "canvas-notes")
  (find-file "canvas.org")
  (canvas-copy)
  (kill-buffer)
  (delete-file "canvas-data.org"))

(defun make-handout ()
  "publish org data file as LaTeX handout and Canvas HTML"
  (interactive)
  (save-buffer)
  ;; (find-file "*-handout.org" t)
  (rlr/org-mkpdf)
  ;; (kill-buffer)
  ;; (shell-command "canvas-notes")
  ;; (find-file "canvas.org" t)
  (org-html-export-to-html)
  (shell-command "canvas-handout"))

(defun make-html ()
  (interactive)
  (save-buffer)
  (org-html-export-to-html)
  (shell-command "canvas-handout"))

(defun make-syllabus ()
  "publish org data file as LaTeX syllabus and Canvas HTML"
  (interactive)
  (save-buffer)
  (find-file "*-syllabus.org" t)
  (rlr/org-mkpdf)
  (kill-buffer)
  (shell-command "canvas-notes")
  (find-file "canvas.org" t)
  (org-html-export-to-html)
  (shell-command "canvas")
  (kill-buffer)
  (delete-file "canvas-data.org")
  (find-file "*-data.org" t))

(defun  create-args ()
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (exchange-point-and-mark)
  (yas-expand-snippet (yas-lookup-snippet "arg-wrap-tex"))
  (previous-line)
  ;; (previous-line)
  (org-beginning-of-line)
  (forward-word)
  (forward-char)
  (forward-char)
  (insert "\\underline{")
  (org-end-of-line)
  (insert "}")
  (next-line)
  (org-beginning-of-line)
  (forward-word)
  (insert "[\\phantom{\\(\\therefore\\)}]")
  (next-line)
  (next-line)
  (org-return)
  (org-return)
  (org-yank)
  (exchange-point-and-mark)
  (yas-expand-snippet (yas-lookup-snippet "arg-wrap-html")))

(defun  create-tex-arg ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "arg-wrap-tex"))
  (previous-line)
  (previous-line)
  (forward-word)
  (forward-char)
  (forward-char)
  (insert "\\underline{")
  (org-end-of-line)
  (insert "}")
  (next-line)
  (org-beginning-of-line)
  (forward-word)
  (insert "[\\phantom{\\(\\therefore\\)}]")
  (next-line)
  (next-line)
  (org-return)
  (org-return))

(defun duplicate-slide-note ()
  (interactive)
  (search-backward ":END:")
  (next-line)
  (kill-ring-save (point)
		    (progn
		      (search-forward "** ")
		      (beginning-of-line)
		      (point))
		    )
  (yas-expand-snippet (yas-lookup-snippet "beamer article notes"))
  (yank))

(defun duplicate-all-slide-notes ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (newline)
    (newline)
    ;; Need a blank slide at the end to convert the last note.
    (insert "** ")
    (beginning-of-buffer)
    (while (ignore-errors
	       (search-forward ":BEAMER_ENV: note"))
	(next-line)
	(next-line)
	(kill-ring-save (point)
			(progn
			  (search-forward "** ")
			  (beginning-of-line)
			  (point))
			)
	(yas-expand-snippet (yas-lookup-snippet "beamer article notes"))
	(yank))
    ;; Delete the blank slide that was added earlier.
    (end-of-buffer)
    (search-backward "**")
    (kill-line)
    )
  (save-buffer))

(defun rlrt-new-article (rlrt-title)
  (interactive "sTitle: ")

  ;; Make filename
  (setq rlrt-filename (rlrt-make-filename rlrt-title))

  ;; Create directory
  (make-directory rlrt-filename)

  (find-file (s-concat rlrt-filename "/" rlrt-filename ".org"))
  (insert (s-concat "#+TITLE: " rlrt-title) ?\n)
  (yas-expand-snippet (yas-lookup-snippet "rlrt-pdf-article")))

(defun convert-qti-nyit ()
  (interactive)
  ;; Copy all to a temp buffer and set to text mode.
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
	(insert-buffer-substring old-buffer)
	(text-mode)
	;; convert multiple correct answer and essay questions
	(beginning-of-buffer)
	(while (re-search-forward "^[:space:]*-" nil t)
	  (replace-match ""))
	;; Change correct multiple answer options to "*"
	(beginning-of-buffer)
	(let ((case-fold-search nil))
	  (while (re-search-forward "\[X\]" nil t)
	    (replace-match "*")))
	;; Mark short answer responses with "**"
	(beginning-of-buffer)
	(while (re-search-forward "+" nil t)
	  (replace-match "*"))
	;; remove whitespace at beginning of lines
	(beginning-of-buffer)
	(while (re-search-forward "^\s-*" nil t)
	  (replace-match ""))
	(beginning-of-buffer)
	(while (re-search-forward "\\(^[0-9]\\)" nil t)
	  (replace-match "\n\\1"))
	;; move correct answer symbol to beginning of line
	(beginning-of-buffer)
	(while (re-search-forward "\\(^.*\\)\\(\*$\\)" nil t)
	  (replace-match "\*\\1"))
	(delete-trailing-whitespace)
	;; delete empty line at end and beginning
	(end-of-buffer)
	(delete-char -1)
	(beginning-of-buffer)
	(kill-line)
	;; Copy result to clipboard
	(clipboard-kill-ring-save (point-min) (point-max))
	)
    )
  (browse-url "https://www.nyit.edu/its/canvas_exam_converter")
  )

(defvar orgblog-directory "~/sites/orgblog/" "Path to the Org mode blog.")
(defvar orgblog-public-directory "~/sites/orgblog/docs/" "Path to the blog public directory.")
(defvar orgblog-posts-directory "~/sites/orgblog/posts/" "Path to the blog public directory.")
(defvar orgblog-drafts-directory "~/sites/orgblog/drafts/" "Path to the blog public directory.")

(defun rlrt-new-post (rlrt-title)
  (interactive "sTitle: ")
  ;; Make filename
  (setq rlrt-filename (rlrt-make-filename rlrt-title))
  (find-file (s-concat orgblog-drafts-directory (format-time-string "%Y-%m-%d-") rlrt-filename ".org"))
  (insert (s-concat "#+TITLE: " rlrt-title) ?\n)
  (yas-expand-snippet (yas-lookup-snippet "orgblogt")))

(defun orgblog-insert-image ()
  (interactive)
  (insert "#+begin_center
#+ATTR_HTML: :width 100% :height
")
  (insert "[[" (file-relative-name (read-file-name "Insert file name: " "~/sites/orgblog/images/posts/")) "]]
#+end_center

")
  )

(defun orgblog-publish-draft ()
  (interactive)
  (save-buffer)
  (copy-file (buffer-file-name) "~/sites/orgblog/posts/")
  (delete-file (buffer-file-name) t)
  (kill-buffer)
  (dired "~/sites/orgblog/posts"))

(defun orgblog-build ()
  (interactive)
  (progn
    (find-file "~/sites/orgblog/publish.el")
    (eval-buffer)
    (org-publish-all)
    (webfeeder-build "atom.xml"
		       "./docs"
		       "https://randyridenour.net/"
		       (let ((default-directory (expand-file-name "./docs")))
			 (remove "posts/index.html"
				 (directory-files-recursively "posts"
							      ".*\\.html$")))
		       :title "Randy Ridenour"
		       :description "Blog posts by Randy Ridenour")
    (kill-buffer))
  (message "Build complete!"))

(defun orgblog-serve ()
  (interactive)
  (progn
    (async-shell-command "orgblog-serve")
    (sleep-for 2)
    (async-shell-command "open http://localhost:3000")))

(defun orgblog-push ()
  (interactive)
  (async-shell-command "orgblog-push"))

(setq org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s</h2>
<div id=\"text-footnotes\">
%s
</div>
</div>")

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
	    " height=\"335\""
	    " src=\"https://www.youtube.com/embed/%s\""
	    " frameborder=\"0\""
	    " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
	      handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
		     path (or desc "")))
     (latex (format "\href{%s}{%s}"
		      path (or desc "video"))))))

(defun orgblog-all-tag-lines ()
  "Get filetag lines from all posts."
  (let ((post-dir orgblog-posts-directory)
	  (regex "^#\\+filetags:\\s([a-zA-Z]+)"))
    (shell-command-to-string
     (concat "rg --context 0 --no-filename --no-heading --replace \"\\$1\" -- " (shell-quote-argument regex) " " post-dir))))

(defun orgblog-all-tags ()
  "Return a list of unique tags from all posts."
  (delete-dups
   (split-string (orgblog-all-tag-lines) nil t)))

(defun orgblog-select-tag ()
  "Select and insert a tag from tags in the blog."
  (defvar newtag)
  (setq newtag (completing-read "Tag: " (orgblog-all-tags))))

(defun insert-post-tag ()
  (orgblog-select-tag)
  (beginning-of-buffer)
  (search-forward "#+filetags" nil 1)
  (end-of-line)
  (insert (concat " " newtag))
  (beginning-of-buffer)
  (search-forward "Tagged:")
  (end-of-line)
  (insert (concat " [[file:../tags/" newtag ".org][" (s-titleized-words newtag) "]]")))

(defun add-post-to-tagfile ()
  (defvar tagfile)
  (defvar post-filename)
  (defvar post-title)
  (setq tagfile (concat "../tags/" newtag ".org"))
  (setq post-filename (f-filename (f-this-file)))
  (progn
    (beginning-of-buffer)
    (search-forward "#+title: " nil 1)
    (setq post-title (buffer-substring (point) (line-end-position))))
  (when
	(not (file-exists-p tagfile))
    (f-append-text (concat "#+title: Tagged: " (s-titleized-words newtag) "\n#+setupfile: ../org-templates/post.org\n") 'utf-8 tagfile))
  (f-append-text (concat "\n- [[file:../posts/" post-filename "][" post-title "]]") 'utf-8 tagfile))

(defun orgblog-add-tag ()
  (interactive)
  (orgblog-select-tag)
  (insert-post-tag)
  (add-post-to-tagfile)
  (save-buffer))

(use-package htmlize)

(defun formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
	     (html (with-current-buffer buf (buffer-string))))
	(with-current-buffer buf
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
	(kill-buffer buf))))

;; (global-set-key (kbd "H-w") 'formatted-copy)

(use-package osx-dictionary)

(use-package pandoc-mode)

(use-package pdf-tools
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :init
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  :general
  (:keymaps 'pdf-view-mode-map
	      "C-s" #'isearch-forward)
  )

(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
	(progn
	  (delete-region (point-min) (point-max))
	  nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

(defun goto-scratch ()
  "this sends you to the scratch buffer"
  (interactive)
  (let ((goto-scratch-buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer goto-scratch-buffer)
    (org-mode)))

(use-package persistent-scratch
  :init
  (persistent-scratch-setup-default))

(use-feature project
  :init
  (setq project-vc-ignores '("*.aux" "*.bbl" "*.bcf" "*.blg" "*.fdb_latexmk" "*.fls" "*.log" "*.out" "*.run.xml" "*.run.xml" "*.synctex.gz" "auto/" "*.pdf"))
  (setq project-vc-extra-root-markers '(".proj")))

(use-package pulsar
  :config
  (pulsar-global-mode 1))

(use-feature recentf
  :init
  (recentf-mode)
  :custom
  (recentf-max-menu-items 1000 "Offer more recent files in menu")
  (recentf-max-saved-items 1000 "Save more recent files"))

(use-package reveal-in-osx-finder)

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package shrink-whitespace)

(use-package sly)

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package spacious-padding
  :init (spacious-padding-mode))

(use-package speedrect
  :demand
  :ensure
  (:host github :repo "jdtsmith/speedrect")
  :config (speedrect-mode))

(use-package super-save
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

(use-package terminal-here
  :ensure
  (:host github :repo "davidshepherd7/terminal-here")
  :config
  (setq terminal-here-mac-terminal-command 'ghostty)
  :general
  ("C-`" #'terminal-here-launch)
  )

(use-package term-toggle
  :demand
  :ensure
  (:host github :repo "amno1/emacs-term-toggle")
  :config
  (setq term-toggle-no-confirm-exit t)
  (defun term-toggle-eat ()
    "Toggle `term'."
    (interactive) (term-toggle 'eat))
  :general
  ("<f2>" #'term-toggle-eat
   "<S-f2>" #'term-toggle-eshell)
  )

(use-package titlecase
  :config
  (setq titlecase-style "chicago"))

(use-package unfill)

(use-package vertico
  :demand
  :custom (vertico-cycle t)
  :config
  (setf (car vertico-multiline) "\n") ;; don't replace newlines
  (vertico-mode)
  ;; (setq vertico-multiform-commands
  ;;  '((consult-line
  ;;       posframe
  ;;       (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
  ;;       (vertico-posframe-border-width . 10)
  ;;       ;; NOTE: This is useful when emacs is used in both in X and
  ;;       ;; terminal, for posframe do not work well in terminal, so
  ;;       ;; vertico-buffer-mode will be used as fallback at the
  ;;       ;; moment.
  ;;       (vertico-posframe-fallback-mode . vertico-buffer-mode))
  ;;      (t posframe)))
  (vertico-multiform-mode 1)
  (setq vertico-multiform-categories
	  '((file grid)
	    (jinx grid (vertico-grid-annotate . 20))
	    (citar buffer)))
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :general
  (:keymaps 'vertico-map
	      ;; keybindings to cycle through vertico results.
	      "C-h" #'+minibuffer-up-dir
	      "<backspace>" 'vertico-directory-delete-char
	      "RET" 'vertico-directory-enter))

(use-package visual-regexp
  :general
  ("C-c r" #'vr/replace)
  ("C-c q" #'vr/query-replace))

(defun rlr/votd ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*Verse of the Day*"))
  (insert (concat "Verse of the Day — " (format-time-string "%B %e, %Y") "\n\n" (rlr/get-votd))))

(defun decode-html-entities (text)
  "Decode HTML entities in TEXT."
  (when text
    (let ((entity-map '(("&ldquo;" . "\"")
			  ("&rdquo;" . "\"")
			  ("&#8212;" . "--")
			  ("&#8217;" . "'")
			  ("&#8220;" . "\"")
			  ("&#8221;" . "\"")))
	    (decoded text))
	(dolist (entity entity-map)
	  (setq decoded (replace-regexp-in-string (car entity) (cdr entity) decoded)))
	decoded)))

(defun rlr/fetch-daily-bible-verse ()
  (interactive)
  "Fetch the daily Bible verse from BibleGateway API."
  (let ((url-request-method "GET")
	  (url "https://www.biblegateway.com/votd/get/?format=json&version=NRSVUE"))
    (with-current-buffer (url-retrieve-synchronously url t t)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
		 (json-object-type 'hash-table)
		 (json-array-type 'list)
		 (json-key-type 'string)
		 (json-data (json-read-from-string json-string))
		 (votd (gethash "votd" json-data))
		 (raw-text (gethash "text" votd))
		 (verse-text (decode-html-entities raw-text))
		 (clean-verse (replace-regexp-in-string "[\"]" "" verse-text))
		 ;; (formatted-verse (format-verse-text clean-verse))
		 (verse-reference (gethash "display_ref" votd))
		 (fill-width 80))
	(format "%s\n%s"
		    clean-verse
		    (let ((ref-text verse-reference))
		(concat "\n" (make-string (- fill-width (length ref-text)) ?\s)
			      ref-text " (NRSVue)"))))))))

(defun rlr/get-votd ()
  "Get the daily verse and handle errors."
  (condition-case err
	(rlr/fetch-daily-bible-verse)
    (error
     (format "Today's verse could not be fetched: %s" (error-message-string err)))))

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ("C-x u" . vundo))

(use-package webfeeder)

(use-package website2org
  :ensure (:host github :repo "rtrppl/website2org")
  :config
  (setq website2org-directory "~/icloud/web-saves/website2org/") ;; if needed, see below
(setq website2org-additional-meta nil)
  :bind
  (:map global-map)
  ("C-M-s-<down>" . website2org)
  ("C-M-s-<up>" . website2org-temp))

(use-package wgrep)

(use-package which-key
  :demand
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode)
  )

(use-package yankpad
  :init
  (setq yankpad-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/yankpad.org")
  :general
  ( "<f6>" #'yankpad-insert))

(use-package yasnippet
  :config
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :hook
  (elpaca-after-init . yas-global-mode))

(general-define-key
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease)

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(general-define-key
 "C-x c" #'save-buffers-kill-emacs
 "C-x C-b" #'ibuffer
 "s-o" #'find-file
 "s-k" #'kill-current-buffer
 "M-s-k" #'kill-buffer-and-window
 "s-K" #'nuke-all-buffers
 "s-r" #'consult-buffer
 "M-s-r" #'consult-buffer-other-window
 "C-S-a" #'embark-act
 "C-M-S-s-k" #'copy-kill-buffer
 "C-M-S-s-s" #'goto-scratch)

(general-define-key
 "s-0" #'delete-window
 "s-1" #'delete-other-windows
 "s-2" #'rlr/find-file-below
 "s-3" #'rlr/find-file-right
 "s-4" #'split-window-below-focus
 "s-5" #'split-window-right-focus
 "s-6" #'toggle-window-split
 "S-C-<left>" #'shrink-window-horizontally
 "S-C-<right>" #'enlarge-window-horizontally
 "S-C-<down>" #'shrink-window
 "S-C-<up>" #'enlarge-window
 "C-x w" #'delete-frame
 "M-o" #'crux-other-window-or-switch-buffer
 "M-o" #'my/quick-window-jump
 "s-\"" #'previous-window-any-frame)

(general-define-key
 "s-t" #'tab-new
 "s-T" #'rlr/find-file-new-tab
 "s-w" #'rlr/delete-tab-or-frame)

(general-define-key
 "s-l" #'hydra-locate/body
 "s-f" #'consult-line
 "<f5>" #'deadgrep
 ;; "C-s" #'consult-isearch
 ;; "C-r" #'consult-isearch-reverse
 )

(general-define-key
 "<s-up>" #'beginning-of-buffer
 "<s-down>" #'end-of-buffer
 "<s-right>" #'end-of-visual-line
 "<s-left>" #'beginning-of-visual-line
 "<M-down>" #'forward-paragraph
 "<M-up>" #'backward-paragraph
 "M-u" #'upcase-dwim
 "M-l" #'downcase-dwim
 "M-c" #'capitalize-dwim
 "RET" #'newline-and-indent
 "M-/" #'hippie-expand
 "<s-backspace>" #'kill-whole-line
 "s-j" #'crux-top-join-line
 "<S-return>" #'crux-smart-open-line
 "<C-S-return>" #'crux-smart-open-line-above
 "<C-d d>" #'insert-standard-date

 "M-y" #'consult-yank-pop

 "M-q" #'reformat-paragraph
 "M-#" #'dictionary-lookup-definition
 "M-=" #'shrink-whitespace
 "s-l" #'hydra-locate/body
 "s-f" #'consult-line
 "<f5>" #'deadgrep)

(general-define-key
 ;; Editing
 ;; "s-/" #'avy-goto-char-timer
 "C-x 4 b" #'consult-buffer-other-window
 "C-x 5 b" #'consult-buffer-other-frame
 "C-x r x" #'consult-register
 "M-s m" #'consult-multi-occur
 "<f8>" #'calendar
 )

(defun open-emacs-config ()
  (interactive)
  (find-file "~/.config/emacs/README.org"))

(defun open-fish-functions ()
  (interactive)
  (dired "~/.config/fish/functions"))

(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to #'org-agenda

 "a" #'org-agenda
 "b" #'consult-bookmark
 "c" #'org-capture

 "d s" #'insert-date-string
 "d d" #'insert-standard-date
 "d b" #'insert-blog-date
 "d l" #'dictionary-search
 "D" #'crux-delete-file-and-buffer

 "f f" #'find-file
 "f k" #'rlr/kill-other-buffers
 "f r" #'consult-buffer
 "f R" #'crux-rename-file-and-buffer
 "f P" #'open-emacs-config
 "f S" #'open-fish-functions

 "g l" #'avy-goto-line
 "g w" #'avy-goto-word-1
 "g p" #'pdf-sync-forward-search

 ;; "h" #'consult-history

 ;; Helpful
 "H c" #'helpful-command
 "H F" #'helpful-callable
 "H h" #'helpful-at-point
 "H f" #'helpful-function
 "H v" #'helpful-variable
 "H k" #'helpful-key

 "k" #'rlr/kill-other-buffers

 "m" #'consult-mark
 "n b" #'hugo-draft-post
 "o" #'consult-outline

 ;; Projects
 "p f" #'consult-project-buffer
 "p d" #'project-find-dired

 "r" #'crux-rename-file-and-buffer
 ;; "s" #'rg-menu
 "S" #'crux-cleanup-buffer-or-region
 "t" #'terminal-here-launch
 "u" #'unfill-paragraph
 "w" #'kill-buffer-and-window
 "z" #'reveal-in-osx-finder

 ;; Search
 "s s" #'denote-search
 "s d" #'denote-search-marked-dired-files
 "s r" #'denote-search-files-referenced-in-region
 )

(setq default-directory "~/")

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
