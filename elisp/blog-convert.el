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


;; Add tagline to bottom of buffer

(defun create-tag-line ()
(interactive)
(beginning-of-buffer)
(while (re-search-forward "filetags: " nil t)
(copy-region-as-kill (point) (line-end-position))
(end-of-buffer)
(insert "

#+begin_tagline
Tagged: ")
(yank)
(insert"
#+end_tagline")
(save-buffer))
(kill-buffer)
)

;; Extract file name and tags

(defun make-file-link ()
  (interactive)
(defvar blog-title)
(beginning-of-buffer)
(re-search-forward "TITLE: " nil t)
(copy-region-as-kill (point) (line-end-position))
(setq blog-title (current-kill 0 t))
;; (insert blog-title)
(defvar blog-filename)
(setq blog-filename (f-filename (buffer-file-name)))
(defvar blog-file-link)
(setq blog-file-link (s-concat "[[file:" blog-filename "][" blog-title "]]")))

(defun get-tags ()
  (interactive)
  (beginning-of-buffer)
(while (re-search-forward "Tagged: " nil t)
(copy-region-as-kill (point) (line-end-position))
(defvar blog-tags)
(setq blog-tags (current-kill 0 t)))
  )

(defun write-blog-tags-title ()
  (interactive)
(defvar blog-tags-title)
(make-file-link)
(get-tags)
(setq blog-tags-title (s-concat blog-tags " " blog-file-link "\n"))
(f-append-text blog-tags-title 'utf-8 "/Users/rlridenour/Desktop/blog-tag-links.txt")
(setq blog-tags "")
  )

;; Convert tagline to links

(defun make-aesthetics-links ()
  (interactive)
  (beginning-of-buffer)
  (search-forward "Tagged: ")
  (while  (search-forward "aesthetics" nil 1)
    (replace-match "[[file:./aesthetics.org][Aesthetics]]")))

(defun make-army-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "army" nil t)
  (replace-match "[[file:./army.org][Army]]")))
  

(defun make-ct-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "critical-thinking" nil t)
  (replace-match "[[file:./critical-thinking.org][Critical-Thinking]]")))
  

(defun make-emacs-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "emacs" nil t)
  (replace-match "[[file:./emacs.org][Emacs]]")))
  

(defun make-ethics-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "ethics" nil t)
  (replace-match "[[file:./ethics.org][Ethics]]")))
  

(defun make-latex-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "latex" nil t)
  (replace-match "[[file:./latex.org][Latex]]")))
  

(defun make-logic-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "logic" nil t)
  (replace-match "[[file:./logic.org][Logic]]")))
  

(defun make-mac-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "mac" nil t)
  (replace-match "[[file:./mac.org][Mac]]")))
  

(defun make-misc-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "misc" nil t)
  (replace-match "[[file:./misc.org][Misc]]")))
  

(defun make-music-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "music" nil t)
  (replace-match "[[file:./music.org][Music]]")))
  

(defun make-org-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "org" nil t)
  (replace-match "[[file:./org.org][Org]]")))
  

(defun make-philosophy-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "philosophy" nil t)
  (replace-match "[[file:./philosophy.org][Philosophy]]")))
  

(defun make-poetry-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "poetry" nil t)
  (replace-match "[[file:./poetry.org][Poetry]]")))
  

(defun make-politics-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "politics" nil t)
  (replace-match "[[file:./politics.org][Politics]]")))
  

(defun make-prayer-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "prayer" nil t)
  (replace-match "[[file:./prayer.org][Prayer]]")))
  

(defun make-quotes-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "quotes" nil t)
  (replace-match "[[file:./quotes.org][Quotes]]")))
  

(defun make-religion-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "religion" nil t)
  (replace-match "[[file:./religion.org][Religion]]")))
  

(defun make-shell-links ()
  (interactive)
  (beginning-of-buffer)
  (search-forward "Tagged:")
  (while (search-forward "shell" nil t)
  (replace-match "[[file:./shell.org][Shell]]")))
  

(defun make-teaching-links ()
  (interactive)
    (beginning-of-buffer)
  (search-forward "Tagged: ")
(while (re-search-forward "teaching" nil t)
  (replace-match "[[file:./teaching.org][Teaching]]")))

;; Make tag link function

(defun make-tag-links ()
  (interactive)
   (setq my-start (mark-marker))
   (make-aesthetics-links)
   (goto-char my-start)
   (make-army-links)
   (goto-char my-start)
   (make-ct-links)
   (goto-char my-start)
   (make-emacs-links)
   (goto-char my-start)
   (make-ethics-links)
   (goto-char my-start)
   (make-latex-links)
   (goto-char my-start)
   (make-logic-links)
   (goto-char my-start)
   (make-mac-links)
   (goto-char my-start)
   (make-misc-links)
   (goto-char my-start)
   (make-music-links)
   (goto-char my-start)
   (make-org-links)
   (goto-char my-start)
   (make-philosophy-links)
   (goto-char my-start)
   (make-poetry-links)
   (goto-char my-start)
   (make-politics-links)
   (goto-char my-start)
   (make-prayer-links)
   (goto-char my-start)
   (make-quotes-links)
   (goto-char my-start)
   (make-religion-links)
   (goto-char my-start)
   (make-shell-links)
   (goto-char my-start)
   (make-teaching-links)
)


(pretty-hydra-define hydra-make-tag-links
  (:color pink :quit-key "z" :title "Make Tag Links")
  (" "
   (("a" (make-aesthetics-links) "aesthetics")
    ("A" (make-army-links) "army")
    ("c" (make-ct-links) "ct")
    ("e" (make-emacs-links) "emacs")
    ("E" (make-ethics-links) "ethics")
    ("l" (make-latex-links) "latex")
    ("L" (make-logic-links) "logic")
    ("M" (make-mac-links) "mac")
    ("m" (make-misc-links) "misc")
    ("o" (make-org-links) "org"))
   " "
    (("w" (make-philosophy-links) "philosophy")
    ("v" (make-poetry-links) "poetry")
    ("P" (make-politics-links) "politics")
    ("p" (make-prayer-links) "prayer")
    ("q" (make-quotes-links) "quotes")
    ("r" (make-religion-links) "religion")
    ("s" (make-shell-links) "shell")
    ("t" (make-teaching-links) "teaching")
    ("i" (insert "misc") "insert misc"))
    ))



(general-define-key
 "H-l" #'hydra-make-tag-links/body)
