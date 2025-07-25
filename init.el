;;; init.el --- Personal Emacs configuration file -*- lexical-binding: t; -*-

(defvar elpaca-installer-version 0.11)
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
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

(use-package key-chord
  :config
  (key-chord-mode 1))

(use-package major-mode-hydra
  :commands (pretty-hydra-define)
  :general
  ("s-m" #'major-mode-hydra))

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

(use-package discover
  :defer 10
  :config
  (global-discover-mode 1))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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

(add-to-list 'load-path (concat rr-emacs-dir "elisp"))

(setq backup-directory-alist (list (cons "."  rr-backup-dir)))

(setq backup-by-copying t)

(setq delete-old-versions t)

(setq kept-new-versions 5)

(setq version-control t)

(setq auto-save-default nil
	auto-save-visited-interval 60)
(auto-save-visited-mode 1)

(setq create-lockfiles nil)

(use-feature bookmark
  :config
  (require 'bookmark)
  (bookmark-bmenu-list)
  (setq bookmark-save-flag 1))

(setq delete-by-moving-to-trash t
	trash-directory "~/.Trash/emacs")

(defun rr/open-init-file ()
  (interactive)
  (progn (find-file "~/.config/emacs/init.org")
	   (variable-pitch-mode -1)))

(defun open-fish-functions ()
  (interactive)
  (dired "~/.config/fish/functions"))

(use-package vertico
  :demand
  :custom (vertico-cycle t)
  :config
  (setf (car vertico-multiline) "\n") ;; don't replace newlines
  (vertico-mode)
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

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :defer 1
  :config (marginalia-mode))

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
  ("C-x b" #'consult-buffer
   "s-r" #'consult-buffer
   "M-s-r" #'consult-buffer-other-window
   "s-f" #'consult-line
   "M-y" #'consult-yank-pop
   "C-x 4 b" #'consult-buffer-other-window
   "C-x 5 b" #'consult-buffer-other-frame
   "C-x r x" #'consult-register
   "M-s m" #'consult-multi-occur))

(use-package consult-dir
  :general
  ("C-x C-d" #'consult-dir)
  (:keymaps 'vertico-map
	  "C-x C-d" #'consult-dir
	  "C-x C-j" #'consult-dir-jump-file))

(use-package embark
  :general
  ("C-." #'embark-act
   "C-S-a" #'embark-act
   "C-:" #'embark-dwim
   "C-h B" #'embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nil
		   (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package corfu
  :defer 5
  :custom
  (corfu-cycle t)
  :config
  (global-corfu-mode))

(use-feature abbrev
  :config
  (load "~/Dropbox/emacs/my-emacs-abbrev"))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq sentence-end-double-space nil)

(setq-default tab-width 10)

(setq insert-directory-program "gls")

(setq message-kill-buffer-on-exit t)

(setf use-short-answers t)

(setopt ns-right-command-modifier 'hyper)

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

(general-define-key
 "<f8>" #'calendar)

(line-number-mode)
(column-number-mode)

(global-visual-line-mode 1)

(global-hl-line-mode)
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

(setq display-time-24hr-format t)
(display-time-mode)

(setq ring-bell-function 'ignore)

(setq server-use-tcp t)
(server-start)
(require 'org-protocol)

(show-paren-mode)
(setq show-paren-delay 0)

(use-feature savehist
  :config
  (savehist-mode 1))

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

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  ;; (auto-dark-detection-method nil) ;; dangerous to be set manually
  :init (auto-dark-mode))

(use-package doom-modeline
  :init
  :config
  (setopt doom-modeline-enable-word-count t)
  (setopt doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setopt display-time-day-and-date t)
  :hook
  (elpaca-after-init . doom-modeline-mode))

(use-package spacious-padding
  :demand
  :after modus-themes doom-modeline
  :init
  (setq spacious-padding-subtle-mode-line t)
  (setq spacious-padding-widths
	  '( :internal-border-width 30
	     :header-line-width 4
	     :mode-line-width 10
	     :tab-width 4
	     :right-divider-width 30
	     :scroll-bar-width 8
	     :fringe-width 8))
  :general
  ("C-M-s-p" #'spacious-padding-mode))

(add-hook 'server-after-make-frame-hook #'spacious-padding-mode)

(setq tab-bar-show t)                      ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil
	tab-bar-new-button-show nil)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

(defun rlr/find-file-new-tab ()
  "Open new tab and select recent file."
  (interactive)
  (tab-new)
  (consult-buffer))

(use-package pulsar
  :defer 10
  :config
  (pulsar-global-mode 1))

(use-package olivetti)

(general-define-key
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease)

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(use-package ultra-scroll
:ensure (:type git :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
	scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(visual-wrap-prefix-mode)

(use-feature recentf
  :init
  (recentf-mode)
  :custom
  (recentf-max-menu-items 1000 "Offer more recent files in menu")
  (recentf-max-saved-items 1000 "Save more recent files"))

(setq save-place-file (expand-file-name "saveplaces" rr-cache-dir))
(save-place-mode)

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
		(string= (buffer-name buffer) "*Messages*")
		(string= (buffer-name buffer) "*mu4e-main*")
		)
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
  (find-file (concat rr-emacs-dir "/init.org")))

(defun goto-shell-init ()
  (interactive)
  (find-file "~/.config/fish/functions/"))

(setq save-interprogram-paste-before-kill t)

(setq default-input-method 'TeX)

(delete-selection-mode 1)

;; When there is a "Time-stamp: <>" string in the first 10 lines of the file,
;; Emacs will write time-stamp information there when saving the file.
;; (Borrowed from http://home.thep.lu.se/~karlf/emacs.html)
(setq time-stamp-active t          ; Do enable time-stamps.
	time-stamp-line-limit 10     ; Check first 10 buffer lines for Time-stamp: <>
	time-stamp-format "Last changed %Y-%02m-%02d %02H:%02M:%02S by %u")
(add-hook 'write-file-hooks 'time-stamp) ; Update when saving.

(general-define-key
 "C-x c" #'save-buffers-kill-emacs
 "C-x C-b" #'ibuffer
 "s-o" #'find-file
 "s-k" #'kill-current-buffer
 "M-s-k" #'kill-buffer-and-window
 "s-K" #'nuke-all-buffers)

(setq initial-major-mode 'org-mode)

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

(general-define-key
 "C-M-S-s-s" #'goto-scratch)

(use-package persistent-scratch
  :defer 10
  :init
  (persistent-scratch-setup-default))

(use-feature project
  :init
  (setq project-vc-ignores '("*.aux" "*.bbl" "*.bcf" "*.blg" "*.fdb_latexmk" "*.fls" "*.log" "*.out" "*.run.xml" "*.run.xml" "*.synctex.gz" "auto/" "*.pdf"))
  (setq project-vc-extra-root-markers '(".proj")))

(use-package ace-window
  :config
  (setq aw-dispatch-always t)
  :general
  ("M-O" #'ace-window
   "M-o" #'rlr/quick-window-jump))

(defun rlr/quick-window-jump ()
  "If only one window, switch to previous buffer, otherwise call ace-window."
  (interactive)
  (let* ((window-list (window-list nil 'no-mini)))
    (if (< (length window-list) 3)
	  ;; If only one window, switch to previous buffer. If only two, jump directly to other window.
	  (if (one-window-p)
	      (switch-to-buffer nil)
	(other-window 1))
	(ace-window t))))

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

(defun toggle-frame-maximized-undecorated ()
  (interactive)
  (let* (
	   (frame (selected-frame))
	   (on? (and (frame-parameter frame 'undecorated) (eq (frame-parameter frame 'fullscreen) 'maximized)))
	   (geom (frame-monitor-attribute 'geometry))
	   (x (nth 0 geom))
	   (y (nth 1 geom))
	   (display-height (nth 3 geom))
	   (display-width (nth 2 geom))
	   (cut (if on? (if ns-auto-hide-menu-bar 26 50) (if ns-auto-hide-menu-bar 4 26))))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- display-height cut) nil t)
    (set-frame-width frame (- display-width 20) nil t)
    (set-frame-position frame x y)))

(defun rlr/delete-tab-or-frame ()
  "Delete current tab. If there is only one tab, then delete current frame."
  (interactive)
  (if
	(not (one-window-p))
	(delete-window)
    (condition-case nil
	  (tab-close)
	(error (delete-frame)))))

(defun rlr/kill-buffer-delete-tab-or-frame ()
  "Kill current buffer and delete its tab. If there is only one tab, then delete current frame."
  (interactive)
  (kill-buffer)
  (if
	(not (one-window-p))
	(delete-window)
    (condition-case nil
	  (tab-close)
	(error (delete-frame)))))

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
 ;; "M-o" #'crux-other-window-or-switch-buffer
 "s-\"" #'previous-window-any-frame
 "s-t" #'rlr/find-file-new-tab
 "s-w" #'rlr/delete-tab-or-frame
 "s-W" #'rlr/kill-buffer-delete-tab-or-frame)

(setq case-replace nil)

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

(setq locate-command "mdfind")

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(use-package avy
  :config
  (avy-setup-default)
  :general
  ("s-/" #'avy-goto-char-timer)
  ("C-c C-j" #'avy-resume))

(use-package casual-avy
  :general
  ("M-g a" #'casual-avy-tmenu))

(defun brave-api ()
(f-read-text "~/.config/keys/brave-search"))

(use-package consult-omni
  :ensure (:type git :host github :repo "armindarvish/consult-omni" :branch "main" :files (:defaults "sources/*.el"))
  :after consult
  :custom
   ;; General settings that apply to all sources
  (consult-omni-show-preview t) ;;; show previews
  (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
  :config
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)

  ;; Either load all source modules or a selected list

  ;;; Select a list of modules you want to aload, otherwise all sources all laoded
  ; (setq consult-omni-sources-modules-to-load (list 'consult-omni-wkipedia 'consult-omni-notes))
  (consult-omni-sources-load-modules)
  ;;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
  (setq consult-omni-multi-sources '("calc"
				     ;; "File"
				     ;; "Buffer"
				     ;; "Bookmark"
				     "Apps"
				     ;; "gptel"
				     "Brave"
				     "Dictionary"
				     ;; "Google"
				     "Wikipedia"
				     "elfeed"
				     ;; "mu4e"
				     ;; "buffers text search"
				     "Notes Search"
				     "Org Agenda"
				     "GitHub"
				     ;; "YouTube"
				     "Invidious"))

;; Per source customization

  ;;; Set API KEYs. It is recommended to use a function that returns the string for better security.
  (setq consult-omni-google-customsearch-key "YOUR-GOOGLE-API-KEY-OR-FUNCTION")
  (setq consult-omni-google-customsearch-cx "YOUR-GOOGLE-CX-NUMBER-OR-FUNCTION")
  (setq consult-omni-brave-api-key (brave-api))
  (setq consult-omni-stackexchange-api-key "YOUR-STACKEXCHANGE-API-KEY-OR-FUNCTION")
  (setq consult-omni-pubmed-api-key "YOUR-PUBMED-API-KEY-OR-FUNCTION")
  (setq consult-omni-openai-api-key "YOUR-OPENAI-API-KEY-OR-FUNCTION")

;;; Pick you favorite autosuggest command.
  (setq consult-omni-default-autosuggest-command #'consult-omni-dynamic-brave-autosuggest) ;;or any other autosuggest source you define

 ;;; Set your shorthand favorite interactive command
  (setq consult-omni-default-interactive-command #'consult-omni-multi)
:general
("C-M-S-s-o" #'consult-omni-multi))

(use-package easy-find
  :ensure (:type git :host github :repo "emacselements/easy-find"))

(use-package fzf
  :commands (fzf fzf-directory)
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

(use-package rg
  :commands rg
  :config
  (rg-enable-default-bindings))

(use-package wgrep
  :defer 10)

(use-package deadgrep
  :general
  ("<f5>" #'deadgrep))

(use-package zoxide)

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
  :defer 1
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

(setopt dired-keep-marker-rename 82)

(defun rlr/dired-search-and-enter ()
  "Search file or directory with `consult-line' and then visit it."
  (interactive)
  (consult-line)
  (dired-find-file))

(defun my-substspaces (str)
  (subst-char-in-string ?\s ?- str))

(defun my-dired-substspaces (&optional arg)
  "Rename all marked (or next ARG) files so that spaces are replaced with underscores."
  (interactive "P")
  (dired-rename-non-directory #'my-substspaces "Rename by substituting spaces" arg))

(general-define-key
 :keymaps 'dired-mode-map
 "j" #'rlr/dired-search-and-enter
 "J" #'dired-goto-file
 "%s" #'my-dired-substspaces)

(use-package reveal-in-osx-finder
  :defer 10)

(use-package eat
  :defer 10
  :demand
  :ensure
  (:host codeberg
	   :repo "akib/emacs-eat"
	   :files ("*.el" ("term" "term/*.el") "*.texi"
		   "*.ti" ("terminfo/e" "terminfo/e/*")
		   ("terminfo/65" "terminfo/65/*")
		   ("integration" "integration/*")
		   (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package term-toggle
  :ensure
  (:host github :repo "amno1/emacs-term-toggle")
  :defer 5
  :config
  (setq term-toggle-no-confirm-exit t)
  (defun term-toggle-eat ()
    "Toggle `term'."
    (interactive) (term-toggle 'eat))
  :general
  ("<f2>" #'term-toggle-eat
   "<S-f2>" #'term-toggle-eshell)
  )

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

(setq eshell-scroll-to-bottom-on-input "this")

(use-package terminal-here
  :ensure
  (:host github :repo "davidshepherd7/terminal-here")
  :config
  (setq terminal-here-mac-terminal-command 'ghostty)
  :general
  ("C-c t" #'terminal-here-launch))

(use-package tldr
  :commands tldr)

(setq help-window-select t)
(setq Man-notify-method 'aggressive)

(use-package which-key
  :defer 1
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode)
  )

(use-package helpful
  :general
  ("C-h v" #'helpful-variable
   "C-h k" #'helpful-key
   "C-h x" #'helpful-command))

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

(defun delete-extra-blank-lines ()
  (interactive)
  (save-excursion)
  (beginning-of-buffer)
  (replace-regexp "^\n\n+" "\n"))

(defun rr/insert-unicode (unicode-name)
  "Same as C-x 8 enter UNICODE-NAME."
  (insert-char (gethash unicode-name (ucs-names))))

(defun open-line (n)
  "Replacing builtin function"
  (interactive "*p")
  (end-of-line)
  (newline n))

(defun open-line-above (n)
  (interactive "*p")
  (beginning-of-line)
  (newline n)
  (previous-line n))

(general-define-key
 "C-S-o" #'open-line-above)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(general-define-key "C-`" #'push-mark-no-activate)
(general-define-key "M-`" #'consult-mark)

(use-package evil-nerd-commenter
  :general
  ("M-;" #'evilnc-comment-or-uncomment-lines))

(use-package accent
  :config
  (setq accent-position 'after)
  :general
  ("C-x C-a" #'accent-menu))

(use-package aggressive-indent
  :defer 5
  :config
  (global-aggressive-indent-mode 1))

(use-package crux
  :general
  ("s-p" #'crux-create-scratch-buffer
   "s-j" #'crux-top-join-line
   "<S-return>" #'crux-smart-open-line
   "<C-S-return>" #'crux-smart-open-line-above
   "<escape>" #'crux-keyboard-quit-dwim
   [remap keyboard-quit] #'crux-keyboard-quit-dwim)
  (:keymaps 'dired-mode-map
	  "M-<RET>" #'crux-open-with))

(use-package
  god-mode
  :general
  (:keymaps 'god-local-mode-map
	      "."  #'repeat)
  :init (setq god-mode-enable-function-key-translation nil)
  (key-chord-define-global "jk" #'god-mode-all)
  :config
  (add-hook 'god-mode-enabled-hook (lambda () (setq cursor-type 'hbar)))
  (add-hook 'god-mode-disabled-hook (lambda () (setq cursor-type 'box))))

(use-package expand-region
  :general ("C-=" #'er/expand-region))

(use-package fold-and-focus
  :ensure (:type git :host sourcehut :repo "flandrew/fold-and-focus")
  :config
  (global-fold-and-focus-org-mode)
  (global-fold-and-focus-md-mode)
  (global-fold-and-focus-el-mode))

(use-package hungry-delete
  :defer 5
  :config
  (global-hungry-delete-mode))

(use-package transient)
(use-package hl-todo
  :ensure (:depth nil)
  :after magit)

(use-package magit
  :init
  (require 'transient)
  :custom
  (magit-repository-directories (list (cons elpaca-repos-directory 1)))
  (magit-diff-refine-hunk 'all)
  (magit-git-executable "/opt/homebrew/bin/git")
  :config
  (transient-bind-q-to-quit)
  :commands magit-status)

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

(with-after-elpaca-init
 (add-to-list 'vertico-multiform-categories
		'(jinx grid (vertico-grid-annotate . 20))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(text-mode . ("harper-ls" "--stdio"))))

(use-package osx-dictionary
  :defer 10)

(use-package shrink-whitespace
  :general
  ("M-=" #'shrink-whitespace))

(use-package visual-regexp
  :general
  ("C-c r" #'vr/replace)
  ("C-c q" #'vr/query-replace))

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package speedrect
  :ensure
  (:host github :repo "jdtsmith/speedrect")
  :defer 10
  :config (speedrect-mode))

(use-package titlecase
    :config
    (setq titlecase-style "chicago")
:commands titlecase-dwim)

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :general
  ("C-x u" #'vundo))

(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

(use-package unfill
:commands unfill-paragraph)

(use-package ws-butler
:defer 10)

(use-package yasnippet
  :config
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :hook
  (elpaca-after-init . yas-global-mode))

(use-package yankpad
  :init
  (setq yankpad-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/yankpad.org")
  :general
  ( "<f6>" #'yankpad-insert))

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
 "<C-d d>" #'insert-standard-date
 "M-q" #'reformat-paragraph
 "M-#" #'dictionary-lookup-definition)

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
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-use-speed-commands t)

  ;; Hide drawers
  (setopt org-cycle-hide-drawer-startup t)
  (setopt org-startup-folded 'nofold)

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

(defun csm/org-word-count ()
  "Count words in region/buffer, estimate pages, and reading time.
Excludes lines beginning with * or #. Prints result in echo area."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
	 (end (if (use-region-p) (region-end) (point-max)))
	 (word-count
	  (save-excursion
	    (goto-char start)
	    (let ((count 0)
		  (inhibit-field-text-motion t))
	      (while (< (point) end)
		(beginning-of-line)
		(unless (looking-at-p "^[*#<]")
		  (let ((line-end (line-end-position)))
		    (while (re-search-forward "\\w+\\W*" line-end t)
		      (setq count (1+ count)))))
		(forward-line 1))
	      count)))
	 (words-per-page 400)
	 (reading-speed 215)
	 (page-count (/ (+ word-count words-per-page -1) words-per-page))
	 (reading-time (/ (+ word-count reading-speed -1) reading-speed)))
    (message "%d words, ~%d pages, ~%d min read"
	     word-count page-count reading-time)))

(use-package org-appear
:after org
    :commands (org-appear-mode)
    ;; :hook     (org-mode . org-appear-mode)
    :config
    (setq org-hide-emphasis-markers t)  ; Must be activated for org-appear to work
    (setq org-appear-autoemphasis   t   ; Show bold, italics, verbatim, etc.
	  org-appear-autolinks      t   ; Show links
	  org-appear-autosubmarkers t)) ; Show sub and superscripts

(use-package org-modern
:after org
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
		 '("org-obu-letter"
		   "\\documentclass{obuletter}
			      [NO-DEFAULT-PACKAGES]
			      [NO-PACKAGES]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
		 '("org-my-letter"
		   "\\documentclass{myletter}
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

(setopt
 org-latex-to-html-convert-command "latexmlc literal:%i --profile=math 2>/dev/null"
 org-html-with-latex 'html)

(use-package org-auto-tangle
:after org
    :hook (org-mode . org-auto-tangle-mode))

;; Org-capture
(setq org-capture-templates
	'(
	  ("t" "Todo" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org" "Inbox")
	   "** TODO %?\n  %i\n  %a")
	  ("e" "Event" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/events.org" "Future")
	   "** %? %T")
	  ("b" "Bookmark" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org" "Inbox")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("c" "Quick note" entry (file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/quick-notes.org")
	   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	  ("j" "Journelly Entry" entry
	   (file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~xenodium~Journelly/Documents/Journelly.org")
	   "* %U @ -\n%?" :prepend t)))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
		 '("n" "New note (with Denote)" plain
		   (file denote-last-path)
		   #'denote-org-capture
		   :no-save t
		   :immediate-finish nil
		   :kill-buffer t
		   :jump-to-captured t)))

(setq org-refile-targets
	'((nil :maxlevel . 1)
	  (org-agenda-files :maxlevel . 1)))

(define-key global-map "\C-cc" 'org-capture)

(defun rlr/org-sort ()
  (mark-whole-buffer)
  (org-sort-entries nil ?a))

(add-to-list 'safe-local-variable-values
	       '(before-save-hook . (rlr/org-sort)))

(use-package org-super-agenda
  :after org
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

(add-hook 'server-after-make-frame-hook #'agenda-home)

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
:after org
    :config
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))
    (require 'org-tempo)
    ;; (require 'ox-rss)
)

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

(use-package org-mac-link
:defer 1)

(use-package org-web-tools
  :defer 10)

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
  (rlr/org-mklua)
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
  (rlr/org-mklua)
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
  (yas-expand-snippet (yas-lookup-snippet "rlrt-lua-article")))

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

(use-package auctex
  :ensure
  (auctex :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
	    :pre-build (("make" "elpa"))
	    :build (:not elpaca--compile-info) ;; Make will take care of this step
	    :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
	    :version (lambda (_) (require 'auctex) AUCTeX-version))
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

(use-package pandoc-mode
  :defer 30)

(use-package citar
  :general
  ("C-c C-b" #'citar-insert-citation)
  (:keymaps 'minibuffer-local-map
	  "M-b" #'citar-insert-preset)
  :custom
  (org-cite-global-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (citar-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (org-cite-csl-styles-dir "/usr/local/texlive/2024/texmf-dist/tex/latex/citation-style-language/styles")
  (org-cite-export-processors
   '((md . (csl "chicago-author-date.csl"))
     (latex biblatex)
     (odt . (csl "chicago-author-date.csl"))
     (t . (csl "chicago-author-date.csl")))))

(use-package ebib
  :defer 30
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  ;;(evil-set-initial-state 'ebib-index-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-entry-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-log-mode 'emacs)
  :custom
  (ebib-preload-bib-files '("~/Dropbox/bibtex/rlr.bib")))

(use-package denote
    :config
    (setq denote-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/denote/")
    (setq denote-infer-keywords t)
    (setq denote-sort-keywords t)
    (setq denote-prompts '(title keywords))
    (setq denote-date-format nil)
:commands denote)

(use-package consult-denote
  :general
  ("C-c n f"  #'consult-denote-find)
  ("C-c n g"  #'consult-denote-grep)
  :config
  (consult-denote-mode 1))

(use-package denote-org
  :ensure (:type git :host github :repo "protesilaos/denote-org")
  :commands
  ;; I list the commands here so that you can discover them more
  ;; easily.  You might want to bind the most frequently used ones to
  ;; the `org-mode-map'.
  ( denote-org-link-to-heading
    denote-org-backlinks-for-heading

    denote-org-extract-org-subtree

    denote-org-convert-links-to-file-type
    denote-org-convert-links-to-denote-type

    denote-org-dblock-insert-files
    denote-org-dblock-insert-links
    denote-org-dblock-insert-backlinks
    denote-org-dblock-insert-missing-links
    denote-org-dblock-insert-files-as-headings))

(use-package consult-notes
  :after (consult denote)
  :config
  (consult-notes-denote-mode))

(use-package citar-denote
  :after (citar denote)
  :config
  (citar-denote-mode)
  (setq citar-open-always-create-notes t))

(use-package denote-menu
  :after denote)

(use-package denote-search
    :ensure (:host github :repo "lmq-10/denote-search")
:defer 10
    :custom
    ;; Disable help string (set it once you learn the commands)
    ;; (denote-search-help-string "")
    ;; Display keywords in results buffer
    (denote-search-format-heading-function #'denote-search-format-heading-with-keywords))

(general-define-key
 :keymaps 'notepad-mode-map
 "C-c C-c" #'copy-kill-buffer)

(define-derived-mode notepad-mode
  org-mode "Notepad"
  "Major mode for scratch buffers.")

(defun rlr/create-notepad-buffer ()
  "Create a new notepad buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*notepad*")))
    (switch-to-buffer buf))
  (notepad-mode)
  (insert (shell-command-to-string "pbpaste")))

(defun copy-kill-buffer ()
  (interactive)
  (goto-char (point-max))
  (newline)
  (mark-whole-buffer)
  (copy-region-as-kill 1 (buffer-size))
  (kill-buffer)
  (message "Copied to clipboard!"))

(general-define-key
 "C-M-S-s-k" #'copy-kill-buffer)

(use-feature mu4e
  :commands (mu4e mu4e-update-mail-and-index)
  :general
  (:keymaps 'mu4e-headers-mode-map
	  "q"  #'kill-current-buffer
	  "C-<tab>" #'tab-next
	  "g" #'my-mu4e-mark-add-tag)
  (:keymaps 'mu4e-thread-mode-map
	  "C-<tab>" #'tab-next)
  (:keymaps 'mu4e-main-mode-map
	  "q"  #'rlr/quit-mu4e)
  (:keymaps 'mu4e-view-mode-map
	  "," #'link-hint-open-link
	  "C-," #'mu4e-sexp-at-point)
  :after org
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "~/.maildir/")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval 300) ;; update every 5 minutes
  (setq mu4e-read-option-use-builtin nil
	  mu4e-completing-read-function 'completing-read)
  (setq mu4e-split-view 'horizontal)
  (setq mu4e-index-update-error-warning nil)
  (setq mu4e-headers-skip-duplicates  t)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-date-format "%y/%m/%d")
  (setq mu4e-headers-date-format "%Y/%m/%d")
  (setq gnus-article-date-headers '(combined-local-lapsed)) ;; show local time in message header
  (setq mu4e-change-filenames-when-moving t)
  ;; customize the reply-quote-string
  (setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
  ;; choose to use the formatted string
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-context-policy 'pick-first)
  (setq  mu4e-contexts (list
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
			   ))
		    (make-mu4e-context
		     :name "gmail"
		     :name "fastmail"
		     :match-func
		     (lambda (msg)
		       (when msg
			 (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
		     :vars '((user-mail-address . "rlridenour@gmail.com")
			   (user-full-name . "Randy Ridenour")
			   (mu4e-drafts-folder . "/gmail/Drafts")
			   (mu4e-refile-folder . "/gmail/Archive")
			   (mu4e-sent-folder . "/gmail/Sent")
			   (mu4e-trash-folder . "/gmail/Trash")))))

  (setq mu4e-bookmarks
	  '((:name "Unread messages"
	       :query "flag:unread AND NOT flag:trashed AND NOT maildir:/gmail/[Gmail]/Trash"
	       :key ?b)
	( :name "All inboxes"
	  :query "maildir:/obu/INBOX OR maildir:/fastmail/INBOX OR maildir:/gmail/INBOX AND"
	  :key ?A)
	( :name "Today's messages"
	  :query "date:today..now"
	  :key ?t)
	( :name "Last 7 days"
	  :query "date:7d..now"
	  :hide-unread t
	  :key ?w)
	( :name "Messages with images"
	  :query "mime:image/*"
	  :key ?p)))
  (setq mu4e-maildir-shortcuts
	  '((:maildir "/obu/INBOX" :key ?u)
	(:maildir "/fastmail/INBOX" :key ?f)
	(:maildir "/gmail/INBOX" :key ?g)))
  (require 'mu4e-transient))

(defun my-confirm-empty-subject ()
  "Allow user to quit when current message subject is empty."
  (or (message-field-value "Subject")
      (yes-or-no-p "Really send without Subject? ")
      (keyboard-quit)))

(add-hook 'message-send-hook #'my-confirm-empty-subject)

(defun rlr/quit-mu4e ()
  (interactive)
  (mu4e-quit)
  (rlr/delete-tab-or-frame))

(use-package mu4e-alert
  :after mu4e
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
 "C-M-s-r" #'rlr/read-mail-news
 "H-m" #'mu4e-transient-menu
 "C-M-S-s-m" #'rlr/open-mu4e-new-tab)

(use-package consult-mu
  :ensure (:type git :host github :repo "armindarvish/consult-mu" :branch "main" :files (:defaults "extras/*.el"))
  :after (consult mu4e)
  :commands (consult-mu)
  :custom
  ;;maximum number of results shown in minibuffer
  (consult-mu-maxnum 200)
  ;;show preview when pressing any keys
  (consult-mu-preview-key 'any)
  ;;do not mark email as read when previewed. If you turn this to t, be aware that the auto-loaded preview if the preview-key above is 'any would also get marked as read!
  (consult-mu-mark-previewed-as-read nil)
  ;;mark email as read when selected.
  (consult-mu-mark-viewed-as-read t)
  ;;use reply to all when composing reply emails
  (consult-mu-use-wide-reply nil)
  ;; define a template for headers view in minibuffer. The example below adjusts the width based on the width of the screen.
  (consult-mu-headers-template (lambda () (concat "%f" (number-to-string (floor (* (frame-width) 0.15))) "%s" (number-to-string (floor (* (frame-width) 0.5))) "%d13" "%g" "%x")))
  :config
  ;;create a list of saved searches for quick access using `histroy-next-element' with `M-n' in minibuffer. Note the "#" character at the beginning of each query! Change these according to
  (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
  (setq consult-mu-saved-searches-async '("#flag:unread"))
  ;; require embark actions for marking, replying, forwarding, etc. directly from minibuffer
  (require 'consult-mu-embark)
  ;; require extra module for composing (e.g. for interactive attachment) as well as embark actions
  (require 'consult-mu-compose)
  (require 'consult-mu-compose-embark)
  ;; require extra module for searching contacts and runing embark actions on contacts
  (require 'consult-mu-contacts)
  (require 'consult-mu-contacts-embark)
  ;; change the prefiew key for compose so you don't open a preview of every file when selecting files to attach
  (setq consult-mu-compose-preview-key "M-o")
  ;; pick a key to bind to consult-mu-compose-attach in embark-file-map
  (setq consult-mu-embark-attach-file-key "C-a")
  (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*"))
  (setq consult-mu-contacts-ignore-case-fold-search t)
  (consult-mu-compose-embark-bind-attach-file-key)
  ;; choose if you want to use dired for attaching files (choice of 'always, 'in-dired, or nil)
  (setq consult-mu-compose-use-dired-attachment 'in-dired))

(defun rlr/browser-default ()
  (interactive)
  (setq browse-url-browser-function 'browse-url-default-browser))

(defun rlr/browser-qutebrowser ()
  (interactive)
  (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "qutebrowser"))

(defun rlr/browser-eww ()
  (interactive)
  (setq browse-url-browser-function 'eww-browse-url))

(defun rlr/select-browser ()
  (interactive)
  (let* ((choices '(("System Default" . rlr/browser-default)
		      ("Qutebrowser" . rlr/browser-qutebrowser)
		      ("EWW" . rlr/browser-eww)))
	   (choice   (completing-read "Choose one: " choices)))
    (call-interactively (cdr (assoc choice choices)))))

(general-define-key
 "C-M-S-s-b" #'rlr/select-browser)

(use-package elfeed
  :demand
  :init
  (setq elfeed-db-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/elfeed")
  :config
  :general
  ("C-M-S-s-e" #'rlr/open-elfeed-new-tab)
  (:keymaps 'elfeed-search-mode-map
	  "q" #'rlr/elfeed-save-db-and-quit)
  (:keymaps 'elfeed-show-mode-map
	  "S-<SPC>" #'scroll-down
	  "," #'link-hint-open-link)
  :commands elfeed)

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

(defun rlr/elfeed-show-toggle-tag (tag)
  "Toggle tag for elfeed article."
  (interactive)
  (if (elfeed-tagged-p tag elfeed-show-entry)
	(elfeed-show-untag tag)
    (elfeed-show-tag tag)))

(defun rlr/elfeed-show-toggle-starred ()
  "Toggle starred tag for elfeed article"
  (interactive)
  (rlr/elfeed-show-toggle-tag 'starred))

(defun rlr/elfeed-show-toggle-readlater ()
  "Toggle starred tag for elfeed article"
  (interactive)
  (rlr/elfeed-show-toggle-tag 'readlater))

(general-define-key
 :keymaps 'elfeed-show-mode-map
 "l" #'rlr/elfeed-show-toggle-readlater
 "m" #'rlr/elfeed-show-toggle-starred
 )

(use-package elfeed-org
  :after elfeed
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/elfeed/elfeed.org"))
  :config
  ;; (setq rmh-elfeed-org-auto-ignore-invalid-feeds t)
  )

(use-package elfeed-webkit
  :general
  (:keymaps 'elfeed-show-mode-map
	      "%"  #'elfeed-webkit-toggle))

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

(use-package webfeeder
  :defer 30)

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

(use-package website2org
  :ensure (:host github :repo "rtrppl/website2org")
  :config
  (setq website2org-directory "~/icloud/web-saves/website2org/") ;; if needed, see below
  (setq website2org-additional-meta nil)
  :general
  ("C-M-s-<down>" #'website2org
   "C-M-s-<up>" #'website2org-temp))

(use-package htmlize
  :defer 10)

(use-package emmet-mode
  :general
  (:keymaps 'html-mode-map
	      "C-M-S-s-<right>" #'emmet-next-edit-point
	      "C-M-S-s-<left>" #'emmet-prev-edit-point))

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

(defun nrsv-open-eww ()
  (interactive)
  (setq nrsv-passage (read-string "Passage: "))
  (setq nrsv-passage (s-replace " " "%20" oremus-passage))
  (setq oremus-link (concat "https://bible.oremus.org/?version=NRSV&passage=" oremus-passage "&vnum=NO&fnote=NO&omithidden=YES"))
  (eww-browse-url oremus-link))

(defun nrsv-open-default-browser ()
  (interactive)
  (setq nrsv-passage (read-string "Passage: "))
  (setq nrsv-passage (s-replace " " "%20" oremus-passage))
  (setq oremus-link (concat "https://bible.oremus.org/?version=NRSV&passage=" oremus-passage "&vnum=NO&fnote=NO&omithidden=YES"))
  (browse-url oremus-link))

(defun oremus-eww-cleanup ()
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward "" nil t)
    (replace-match "\""))
  (beginning-of-buffer)
  (while (re-search-forward "" nil t)
    (replace-match "\""))
  (beginning-of-buffer)
  (while (re-search-forward "" nil t)
    (replace-match "\'"))
  (beginning-of-buffer)
  (while (re-search-forward "" nil t)
    (replace-match "\'"))
  (beginning-of-buffer)
  (while (re-search-forward "" nil t)
    (replace-match "\'"))
  (beginning-of-buffer)
  (while (re-search-forward "" nil t)
    (replace-match "—"))
  )

(defun nrsv-insert-passage ()
  (interactive)
  (setq oremus-passage (read-string "Passage: "))
  (setq oremus-passage (s-replace " " "%20" oremus-passage))
  (setq oremus-link (concat "https://bible.oremus.org/?version=NRSV&passage=" oremus-passage "&vnum=NO&fnote=NO&omithidden=YES"))
  (switch-to-buffer (url-retrieve-synchronously oremus-link))
  (beginning-of-buffer)
  (search-forward "passageref\">")
  (kill-region (point) 1)
  (search-forward "</div><!-- class=\"bibletext\" -->")
  (beginning-of-line)
  (kill-region (point) (point-max))
  (beginning-of-buffer)
  (while (re-search-forward "<p>" nil t)
    (replace-match "\n"))
  (beginning-of-buffer)
  (while (re-search-forward "<!.+?->" nil t)
    (replace-match ""))
  (beginning-of-buffer)
  (while (re-search-forward "<.+?>" nil t)
    (replace-match ""))
  (beginning-of-buffer)
  (while (re-search-forward "&nbsp;" nil t)
    (replace-match " "))
  (beginning-of-buffer)
  (while (re-search-forward "&#147;" nil t)
    (replace-match "\""))
  (beginning-of-buffer)
  (while (re-search-forward "&#148;" nil t)
    (replace-match "\""))
  (beginning-of-buffer)
  (while (re-search-forward "&#145;" nil t)
    (replace-match "\'"))
  (beginning-of-buffer)
  (while (re-search-forward "&#146;" nil t)
    (replace-match "\'"))
  (beginning-of-buffer)
  (while (re-search-forward "&#151;" nil t)
    (replace-match "---"))
  (delete-extra-blank-lines)
  (clipboard-kill-ring-save (point-min) (point-max))
  (kill-buffer)
  (yank))

(use-package isgd
    :custom
    (isgd-logstats nil)
    (isgd-ask-custom-url t)
:commands (isgd-replace-url-at-point isgd-copy-url-at-point))

(use-package link-hint
  :general
  ("s-," #'link-hint-open-link
   "C-c l o" #'link-hint-open-link
   "C-c l c" #'link-hint-copy-link))

(use-package fish-mode
  :mode ("\\.fish\\'" . Fish-mode))

(use-package fish-completion
  :ensure (:type git :host github :repo "LemonBreezes/emacs-fish-completion")
  :defer 30
  :config
  (when (and (executable-find "fish")
	   (require 'fish-completion nil t))
    (global-fish-completion-mode)))

(use-package sly
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
  :commands sly)

(use-package yaml-mode
  :mode ("\\.yml\\'" . YAML-mode))

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

(use-feature calc
:general
("C-M-S-s-c" #'calc))

(use-package chordpro-mode
:mode ("\\.cho\\'" . chordpro-mode))

(with-after-elpaca-init
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
     ("C" cua-selection-mode "rectangle" :toggle t)))))

(with-after-elpaca-init
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
    )))

(with-after-elpaca-init
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
    )))

(with-after-elpaca-init
 (pretty-hydra-define hydra-window
   (:color teal :quit-key "q" title: "Windows")
   ("Windows"
    (("w m" minimize-window "minimize window")
     ("w s" crux-transpose-windows "swap windows")
     ("w S" shrink-window-if-larger-than-buffer "shrink to fit")
     ("w b" balance-windows "balance windows")
     ("w t" toggle-window-split "toggle split")
     ("w v" enlarge-window" grow taller" :exit nil)
     ("w h" enlarge-window-horizontally "grow wider" :exit nil))
    "Frames"
    (("f m" iconify-frame "minimize frame")
     ("f d" delete-other-frames "delete other frames"))
    "Tabs"
    (("t c" tab-close-other "close other tabs")
     ("t m" tab-move "move tab right")
     ("t b" tab-bar-history-back "restore previous windows")
     ("t f" tab-bar-history-forward "undo restore windows"))
    "Writeroom"
    (("W" writeroom-mode "toggle writeroom")
     ("M" writeroom-toggle-mode-line "toggle modeline")))))

(with-after-elpaca-init
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
    )))

(with-after-elpaca-init
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
    )))

(with-after-elpaca-init
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
    )))

(with-after-elpaca-init
 (pretty-hydra-define hydra-hydras
   (:color teal :quit-key "q" :title "Hydras")
   ("System"
    (("t" hydra-toggle/body)
     ("b" hydra-buffer/body)
     ("h" hydra-hugo/body)
     ("p" powerthesaurus-hydra/body))
    "Unicode"
    (("l" hydra-logic/body "logic")
     ("m" hydra-math/body)))))

(general-define-key
 "H-h" #'hydra-hydras/body
 "s-l" #'hydra-locate/body
 "s-n" #'hydra-new/body
 "H-t" #'hydra-toggle/body
 "H-w" #'hydra-window/body
 "H-b" #'hydra-buffer/body
 "C-x 9" #'hydra-logic/body)

(with-after-elpaca-init
 (major-mode-hydra-define dashboard-mode
   (:quit-key "q")
   ("Open"
    (("m" consult-bookmark "bookmarks")
     ("a" consult-org-agenda "consult-agenda")
     ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
     ("b" (find-file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org") "web bookmarks")))))

(with-after-elpaca-init
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
     ("e" rlr/open-safari-page-in-eww "Open Safari page in EWW")))))

(with-after-elpaca-init
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
     ("<SPC>" nil "Quit" :color pink)))))

(with-after-elpaca-init
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
     ("c" markdown-complete-buffer "complete")))))

(with-after-elpaca-init
 (major-mode-hydra-define LaTeX-mode
   (:quit-key "q")
   ("Bibtex"
    (("r" citar-insert-citation "citation"))
    "LaTeXmk"
    (
     ("m" rlr/tex-mklua "LuaLaTeX")
     ("p" rlr/tex-mkpdf "PDFLaTeX")
     ("w" rlr/tex-mktc "watch PDFLaTeX")
     ("L" rlr/tex-mklua "watch LuaLaTeX")
     ("c" tex-clean "clean aux")
     ("C" tex-clean-all "clean all")
     ("n" latex-word-count "word count")))))

(with-after-elpaca-init
 (major-mode-hydra-define org-mode
   (:quit-key "q")
   ("Export"
    (
     ("m" rlr/org-mklua "Make PDF with LuaLaTeX")
     ("p" rlr/org-mkpdf "Make PDF with PDFLaTeX")
     ("o" rlr/org-open-pdf "View PDF")
     ("h" make-html "HTML")
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
     ("w" csm/org-word-count "word count")
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
    (("1" denote-link "link to note")))))

(with-after-elpaca-init
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
     ("bd" orgblog-push "Push to Github")))))

(with-after-elpaca-init
 (major-mode-hydra-define css-mode
   (:quit-key "q")
   ("Blog"
    (("bn" rlrt-new-post "New draft")
     ("bb" orgblog-build "Build Site")
     ("bs" orgblog-serve "Serve Site")
     ("bd" orgblog-push "Push to Github")))))

(with-after-elpaca-init
 (major-mode-hydra-define denote-menu-mode
   (:quit-key "q")
   ("Tools"
    (("f" denote-menu-filter "Filter by regex")
     ("k" denote-menu-filter-by-keyword "Filter by keyword")
     ("c" denote-menu-clear-filters "Clear filters")
     ("d" denote-menu-export-to-dired "Dired")))))

(with-after-elpaca-init
 (major-mode-hydra-define mu4e-main-mode
   (:quit-key "q")
   ("Message"
    (
     ("n" mu4e-compose-mail "New")
     ("e" mu4e-view-save-attachments "Save attachments")
     ))))

(with-after-elpaca-init
 (major-mode-hydra-define mu4e-headers-mode
   (:quit-key "q")
   ("Message"
    (
     ("n" mu4e-compose-mail "New")
     ("r" mu4e-compose-reply "Reply")
     ("a"  mu4e-compose-wide-reply "Reply All")
     ))))

(with-after-elpaca-init
 (major-mode-hydra-define mu4e-view-mode
   (:quit-key "q")
   ("Message"
    (
     ("n" mu4e-compose-mail "New")
     ("r" mu4e-compose-reply "Reply")
     ("a"  mu4e-compose-wide-reply "Reply All")
     )
    "Browser"
    (
     ("bd" rlr/browser-default "System default")
     ("bq" rlr/browser-qutebrowser "Qutebrowser")
     ("be" rlr/browser-eww "EWW")
     ))))

(with-after-elpaca-init
 (major-mode-hydra-define mu4e-compose-mode
   (:quit-key "q")
   ("Compose with Org"
    (
     ("o" org-mime-edit-mail-in-org-mode "Edit in org")
     ("r" org-mime-htmlize "Org to HTML")
     ))))

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

(defun reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(defun rlr/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	     (format "%.2f seconds"
		     (float-time
		(time-subtract after-init-time before-init-time)))
	     gcs-done))

(add-hook 'emacs-startup-hook #'rlr/display-startup-time)

(setq default-directory "~/")

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
