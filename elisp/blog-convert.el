;; Convert old Hugo posts to new Org blog.


  (defun convert-blog-post ()
    (interactive)
    (beginning-of-buffer)
    (replace-regexp-in-region "tags\\[]" "filetags")
    (beginning-of-buffer)
    (re-search-forward "date.*T")
    (backward-char)
    (kill-line)
    (insert ">")
    (beginning-of-line)
    (re-search-forward "[[:space:]]")
    (insert "<")
    (re-search-forward "lastmod.*T")
    (backward-char)
    (kill-line)
    (insert ">")
    (beginning-of-line)
    (re-search-forward "[[:space:]]")
    (insert "<")
    )



;; Some functions for converting old posts.


  (defun fix-blog-title ()
    (interactive)
    (beginning-of-buffer)
    (re-search-forward "title")
    (beginning-of-line)
    (insert "#+")
    (replace-string-in-region "\'" "" (line-beginning-position) (line-end-position))
    )


  (defun fix-blog-date ()
    (interactive)
    (beginning-of-buffer)
    (re-search-forward "date")
    (replace-string-in-region "'" "" (line-beginning-position) (line-end-position))
    (beginning-of-line)
    (insert "#+")
    (re-search-forward ": ")
    (kill-visual-line)
    (insert (format-time-string "<%Y-%m-%d>" (date-to-time (current-kill 0 t))))
    )

  (defun delete-draft-line ()
    (interactive)
    (beginning-of-buffer)
    (while (re-search-forward "draft:" nil t)
      (beginning-of-line)
      (kill-visual-line)
      (kill-visual-line))
    )

  (defun fix-blog-tags ()
    (interactive)
    (beginning-of-buffer)
    (while (re-search-forward "^tags:" nil t)
      (beginning-of-line)
      (insert "#+file")
      (replace-string-in-region "\'" "" (line-beginning-position) (line-end-position))
      (replace-string-in-region "," "" (line-beginning-position) (line-end-position))
      (replace-string-in-region "[" "" (line-beginning-position) (line-end-position))
      (replace-string-in-region "]" "" (line-beginning-position) (line-end-position))
      (replace-string-in-region "- " "" (line-beginning-position) (line-end-position))
      (downcase-region (line-beginning-position) (line-end-position))
      ))

  (defun delete-unnecessary-header-lines ()
    (interactive)
    (beginning-of-buffer)
    (while (re-search-forward "^---$" nil t)
      (replace-match "")
      (kill-visual-line))
    (beginning-of-buffer)
    (while (re-search-forward "^url:.*$" nil t 1)
      (replace-match "")
      (kill-visual-line))
    (beginning-of-buffer)
    (while (re-search-forward "^comments:.*$" nil t 1)
      (replace-match "")
      (kill-visual-line))
    (beginning-of-buffer)
    (while (re-search-forward "^highlight:.*$" nil t 1)
      (replace-match "")
      (kill-visual-line))
    (beginning-of-buffer)
    (while (re-search-forward "^markup:.*$" nil t 1)
      (replace-match "")
      (kill-visual-line))
    (beginning-of-buffer)
    (while (re-search-forward "^math:.*$" nil t 1)
      (replace-match "")
      (kill-visual-line))
    (beginning-of-buffer)
    (while (re-search-forward "^draft:.*$" nil t 1)
      (replace-match "")
      (kill-visual-line))
    (beginning-of-buffer)
    (while (re-search-forward "^categories:.*$" nil t 1)
      (replace-match "")
      (kill-visual-line))
    )

  (defun convert-markdown-eol ()
    (interactive)
    (beginning-of-line)
    (while (re-search-forward "[[:blank:]]+$" nil t)
      (replace-match "\\\\\\\\")))

  (defun italicize-amen ()
    (interactive)
    (while (re-search-forward "^\*Amen\\*" nil t)
      (replace-match "/Amen/")
      ))

  (defun convert-md-links ()
    (interactive)
    (beginning-of-buffer)
    ;; (while
    ;; (re-search-forward "\\(\\[.*\\]\\)(\\(.*\\))" nil t)
    ;; (replace-match "[[\\2]\\1]")
    ;; )
    (query-replace-regexp "\\(\\[.*\\]\\)(\\(.*\\))"
			"[[\\2]\\1]"))


  (defun convert-md-footnotes ()
    (interactive)
    (beginning-of-buffer)
    (while
	(re-search-forward "\\[\\^")
      (replace-match "[fn:"))
    )

  (defun convert-markdown-header ()
    (interactive)
    (progn
      (delete-unnecessary-header-lines)
      (fix-blog-title)
      (fix-blog-date)
      (fix-blog-tags)
      ))

  (defun fix-prayers ()
    (interactive)
    (progn
      (convert-markdown-eol)
      (italicize-amen)))

  (defun fix-post ()
    (interactive)
    (progn
      (convert-markdown-header)
      (fix-prayers)))


;; Convert HTML posts.


  (defun convert-html-quotes ()
    (interactive)
    (beginning-of-buffer)
    (while (re-search-forward "</blockquote>" nil t)
      (replace-match "
    #\+end_quote"))
    (beginning-of-buffer)
    (while (re-search-forward "<blockquote.*>" nil t)
      (replace-match "#\+begin+quote
	  "))
    (beginning-of-buffer)
    (while (re-search-forward "</cite>" nil t)
      (replace-match ""))
    (beginning-of-buffer)
    (while (re-search-forward "<cite.*>" nil t)
      (replace-match "--- ")))



  (defun convert-html-headings ()
    (interactive)
    (beginning-of-buffer)
    (while (re-search-forward "</h2>" nil t)
      (replace-match "
	  "))
    (beginning-of-buffer)
    (while (re-search-forward "<h2.*>" nil t)
      (replace-match "
    \*\*  "))
    (beginning-of-buffer)
    (while (re-search-forward "</h3>" nil t)
      (replace-match "
	  "))
    (beginning-of-buffer)
    (while (re-search-forward "<h3.*>" nil t)
      (replace-match "
    \*\*\*  ")))

  (defun convert-html-links ()
    (interactive)
    (beginning-of-buffer)
    (while
	(re-search-forward "<a href=\"\\(.*\\)\">" nil t)
      (replace-match "\[\[\\1\]\[")
      )
    (beginning-of-buffer)
    (while
	(re-search-forward "</a>" nil t)
      (replace-match "\]\]")))


  (defun convert-html-lists ()
    (interactive)
    (beginning-of-buffer)
    (while
	(re-search-forward "<li>" nil t)
      (replace-match "- "))
    (beginning-of-buffer)
    (while
	(re-search-forward "</li>" nil t)
      (replace-match ""))
    (beginning-of-buffer)
    (while
	(re-search-forward "</ol>" nil t)
      (replace-match ""))
    (beginning-of-buffer)
    (while
	(re-search-forward "<ol>" nil t)
      (replace-match ""))
    )


  (defun convert-misc-html ()
    (interactive)
    (beginning-of-buffer)
    (while
	(re-search-forward "<p>" nil t)
      (replace-match ""))
    (beginning-of-buffer)
    (while
	(re-search-forward "</p>" nil t)
      (replace-match "
    "))
    (beginning-of-buffer)
    (while
	(re-search-forward "<em>" nil t)
      (replace-match "/"))
    (beginning-of-buffer)
    (while
	(re-search-forward "</em>" nil t)
      (replace-match "/"))
    (beginning-of-buffer)
    (while
	(re-search-forward "&hellip;" nil t)
      (replace-match "..."))
        (beginning-of-buffer)
    (while
	(re-search-forward "&ndash;" nil t)
      (replace-match "--"))
        (beginning-of-buffer)
    (while
	(re-search-forward "&mdash;" nil t)
      (replace-match "---"))
    (while
	(re-search-forward "<br />" nil t)
      (replace-match "\\\\\\\\"))
    )

  (defun strip-html ()
    "Remove HTML tags from the current buffer,
	   (this will affect the whole buffer regardless of the restrictions in effect)."
    (interactive "*")
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "<[^" (point-max) t)
	  (replace-match "\\1"))
	(goto-char (point-min))
	(replace-string "(c)" "(c)")
	(goto-char (point-min))
	(replace-string "&" "&")
	(goto-char (point-min))
	(replace-string "<" "")
	(goto-char (point-min)))))



  (defun convert-html-post ()
    (interactive)
    (convert-misc-html)
    (convert-html-headings)
    (convert-html-quotes)
    (convert-html-links)
    (convert-html-lists)
    )



(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max))
  (goto-char (point-min)))


;; Move converted post.

  (defun orgblog-move-post ()
    (interactive)
    (mark-whole-buffer)
    (crux-cleanup-buffer-or-region)
    (save-buffer)
    (copy-file (buffer-file-name) "~/sites/orgblog/posts/")
    (delete-file (buffer-file-name) t)
    (kill-buffer))
