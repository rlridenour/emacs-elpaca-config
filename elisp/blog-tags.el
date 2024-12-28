(defun orgblog-all-tag-lines ()
  "Get filetag lines from all posts."
  (let ((post-dir (f-dirname (f-this-file)))
	(regex "^#\\+filetags:\\s([a-zA-Z]+)"))
    (shell-command-to-string
     (concat "rg --context 0 --no-filename --no-heading --replace \"\\$1\" -- " (shell-quote-argument regex) " " post-dir))))

(defun orgblog-all-tags ()
  "Return a list of unique tags from all posts."
  (delete-dups
   (split-string (orgblog-all-tag-lines) nil t)))

(defun orgblog-select-tag ()
  "Select and insert a tag from YAML frontmatter tags in the project."
  (defvar newtag)
  (setq newtag (completing-read "Tag: " (orgblog-all-tags))))

(defun insert-post-tag ()
  (interactive)
  (orgblog-select-tag)
  (beginning-of-buffer)
  (search-forward "#+filetags" nil 1)
  (end-of-line)
  (insert (concat " " newtag))
  (search-forward "Tagged:")
  (end-of-line)
  (insert (concat " [[file:../tags/" newtag ".org][" (s-capitalize newtag) "]]")))

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
    (f-append-text (concat "#+title: Tagged: " newtag "\n#+setupfile: ../org-templates/post.org\n") 'utf-8 tagfile))
  (f-append-text (concat "\n- [[file:../posts/" post-filename "][" post-title "]]") 'utf-8 tagfile))

(defun new-orgblog-tag ()
  (interactive)
  (orgblog-select-tag)
  (insert-post-tag)
  (add-post-to-tagfile)
  (save-buffer))
