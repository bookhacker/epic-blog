;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;; -----------------------------------------------------------------------------
;;;
(require 'find-lisp)
(require 'subr-x)
;;; -----------------------------------------------------------------------------
;;;
(defconst epic-blog-i18n-next-page     "← nächste Seite")
(defconst epic-blog-i18n-previous-page "vorherige Seite →")
(defvar category-link "")
(defvar html-line-break "<br>")
(defvar html-paragraph-prefix "<p>")
(defconst html-paragraph-start-tag "<p>") ;; todo replace with html-paragraph-prefix
(defconst html-paragraph-end-tag "</p>") ;; todo replace with html-paragraph-suffix
(defvar html-paragraph-suffix "</p>")
(defconst html-italics-start-tag "<em>")
(defconst html-italics-end-tag "</em>")
(defconst html-bold-start-tag "<strong>")
(defconst html-bold-end-tag "</strong>")
(defconst category-buffer-name "category-buffer")
(defconst epic-blog-html-buffer-name "html-buffer")
(defconst html-body-start-tag "<body>")
(defconst html-body-end-tag "</body>")
(defvar html-div-end-tag "</div>">)
(defconst html-end-tag "</html>")
(defconst html-main-start-tag "<main>")
(defconst html-main-end-tag "</main>")
(defconst html-title-start-tag "<title>")
(defconst html-title-end-tag   "</title>")
(defconst html-table-start-tag "<table>")
(defconst html-table-end-tag "</table>")
(defvar   epic-blog-headlines-only t)
(defconst epic-blog-index-page-buffer-name "index-page-buffer")
(defconst epic-blog-index-page-title "Übersicht")
(defvar in-paragraph nil "Are we editing a paragraph?")
(defconst post-author-prefix "#+AUTHOR:")
(defconst post-category-prefix "#+CATEGORY:")
(defconst category-title-prefix "#+CATEGORY-TITLE:")
(defconst category-full-title-prefix "#+CATEGORY-FULL-TITLE:")
(defconst post-date-prefix "#+DATE:")
(defvar post-title-prefix "#+TITLE:")
(defvar page-title-prefix "#+TITLE:")
(defconst title-placeholder (concat html-title-start-tag post-title-prefix html-title-end-tag))
(defconst category-file-extension ".category")
(defconst post-extension          ".post")
(defvar html-extension      ".html")
(defvar bold-input nil)
(defvar italics-input nil)
(defvar post-buffer-name nil)
(defvar post-author   "" "Value of line \"#+AUTHOR:\"")
(defvar post-category "" "Value of line \"#+CATEGORY:\"")
(defvar post-date     "" "Value of line \"#+DATE:\"")
(defvar page-title    "" "Value of line \"#+TITLE:\"")
(defvar post-title    "" "Value of line \"#+TITLE:\"")
(defvar post-content  "" "Content of post.")
(defvar epic-blog-posts-per-page 1)
(defvar epic-blog-posts-counter 0)
(defconst epic-blog-next-page-placeholder     "#+NEXTCHAPTER")
(defconst epic-blog-previous-page-placeholder "#+PREVIOUSCHAPTER")
(defvar epic-blog-previous-destination-filename "")
(defconst epic-blog-pages-directory-name "pages")
(defconst epic-blog-new-page-buffer-name "new-page-buffer")
(defvar epic-blog-pages-counter 1 "At startup, we are on the first pages (= page 1).")
(defvar page-files 0)
(defvar post-files 0)
(defvar epic-blog-page-buffer-name "epic-blog-page")
(defvar epic-blog-page-file-buffer-name "")
(defvar epic-blog-post-buffer-name "epic-blog-post")
(defvar formatted-post-date "")
(defvar epic-blog-show-sidebar nil)
(defvar epic-blog-show-featured-image nil)
(defvar epic-blog-categories-list '() "Contains all categories.")
(defvar category-title "")
(defvar destination-file-name "")
(defvar epic-blog-index-file "" "The first post file will also be the index.html.")
(defvar epic-blog-website-title "")
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-create-index-file ()
  "Creates index.html."
  (setq index-file (concat (file-name-as-directory html-directory) "index.html"))
  (copy-file epic-blog-index-file index-file t)
  (unless (string-empty-p epic-blog-website-title)
    (with-current-buffer (find-file index-file)
      (let ((case-fold-search t)) ; or nil
	(goto-char (point-min))
	(search-forward html-title-start-tag nil t) ; title in <head>
	(kill-line)
	(insert epic-blog-website-title html-title-end-tag))
      (write-file index-file)
      (kill-buffer))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-build-all()
  "Builds everything."
  (interactive)
  (epic-blog-initialize)
  ;; (epic-blog-start-index-page)
  (epic-blog-build-all-posts)
  ;; (epic-blog-finish-index-page)
  ;; (epic-blog-finish-new-pages-page)
  (epic-blog-build-all-pages)
  (epic-blog-build-all-categories)
  (epic-blog-create-index-file))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-category-create (category)
  "Builds a category file."
  (with-current-buffer (get-buffer category)
    (goto-char (point-min))
    (insert html-table-start-tag)(newline)
    (goto-char (point-min))
    (insert (epic-blog-get-category-description category))(newline)    
    (goto-char (point-min))
    (insert "<h2>" (epic-blog-get-category-full-title category) "</h2>")(newline)
    (goto-char (point-min))
    (insert html-main-start-tag)(newline)
    (goto-char (point-min))
    (insert "<div id=\"page\">")(newline)
    (goto-char (point-min))
    (insert html-body-start-tag)(newline)
    (goto-char (point-min))
    (insert-file-contents pages-head-file)(newline)
    (epic-blog-category-replace-title category)
    ;; featured-image
    (goto-char (point-max))
    (insert html-table-end-tag)(newline)
    (goto-char (point-max))
    (insert html-main-end-tag)(newline)
    (goto-char (point-max))
    (insert-file-contents pages-footer-file)
    (goto-char (point-max))
    (insert "</div>")(newline)
    (insert html-body-end-tag)(newline)
    (insert html-end-tag)(newline)
    (setq destination-category-file
	  (concat (file-name-as-directory
		   categories-destination-directory)
		  category
		  html-extension))
    (funcall 'html-mode)
    (indent-region (point-min)(point-max))
    (write-file destination-category-file)
    (kill-buffer)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-build-all-categories ()
  "Builds all category files."
  (mapcar 'epic-blog-category-create epic-blog-categories-list))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-build-all-pages()
  "Builds all posts."
  (setq page-files (find-lisp-find-files (file-name-as-directory
					  epic-blog-source-pages-directory)
					 "\\.page$"))
  (mapcar 'epic-blog-page-create page-files))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-build-all-posts()
  "Builds all posts."
  (setq post-files (find-lisp-find-files (file-name-directory buffer-file-name)
					 "\\.post$"))
  (mapcar 'epic-blog-post-create post-files))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-get-category-description (category)
  "Reads category-full-title from category-file."
  (setq category-description "")
  (unless (string-empty-p category)
    ;; create category page if not exists
    (setq category-file (concat (file-name-as-directory categories-directory)
				category category-file-extension))
    (unless (file-exists-p category-file)
      (with-current-buffer (get-buffer-create category-buffer-name)
	(setq category-title (concat category-title-prefix
				     " TODO: "
				     post-category))
	(insert category-title)(newline)
	(write-file category-file)
	(kill-buffer)))
    ;; read from category file
    (with-current-buffer (find-file category-file)
      (goto-char (point-min))
      (while (not (eobp))
	(setq formatted-line (string-trim (epic-blog-get-current-line)))
	(unless (string-empty-p formatted-line)
	  (unless  (string-prefix-p "#+" formatted-line)
	    (setq category-description (concat category-description
					       (epic-blog-get-formatted-html
						formatted-line)
					       "\n"))))
	(forward-line))
      (kill-buffer)))
  category-description)
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-get-category-full-title (category)
  "Reads category-full-title from category-file."
  (unless (string-empty-p category)
    ;; create category page if not exists
    (setq category-file (concat (file-name-as-directory categories-directory)
				category category-file-extension))
    (unless (file-exists-p category-file)
      (with-current-buffer (get-buffer-create category-buffer-name)
	(setq category-title (concat category-title-prefix
				     " TODO: "
				     post-category))
	(insert category-title)(newline)
	(write-file category-file)
	(kill-buffer)))
    ;; read from category file
    (with-current-buffer (find-file category-file)
      (goto-char (point-min))
      (search-forward category-full-title-prefix nil t);
      (setq line (buffer-substring-no-properties (line-beginning-position)
						 (line-end-position)))
      (setq category-full-title (string-trim (string-remove-prefix
					      category-full-title-prefix
					      line)))
      (kill-buffer)
      category-full-title)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-get-category-title (category)
  "Reads category-title from category-file."
  (unless (string-empty-p category)
    ;; create category page if not exists
    (setq category-file (concat (file-name-as-directory categories-directory)
				category category-file-extension))
    (unless (file-exists-p category-file)
      (with-current-buffer (get-buffer-create category-buffer-name)
	(setq category-title (concat category-title-prefix
				     " TODO: "
				     post-category))
	(insert category-title)(newline)
	(write-file category-file)
	(kill-buffer)))
    ;; read from category file
    (with-current-buffer (find-file category-file)
      (goto-char (point-min))
      (setq line (buffer-substring-no-properties (line-beginning-position)
						 (line-end-position)))
      (setq category-title (string-trim (string-remove-prefix
					 category-title-prefix line)))
      (kill-buffer)
      category-title)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-insert-head-file ()
  "Insert head-file content into new post-file."
  (with-current-buffer (get-buffer-create
			epic-blog-post-buffer-name)
    (goto-char 1)
    (insert-file-contents head-file)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-process-post-file (filename)
  "Creates new post from .post-file."
  (setq new-post-file-name-base (file-name-base filename))
  (setq new-post-file-name-base (substring new-post-file-name-base 11 (length new-post-file-name-base)))
  (setq destination-file-name (concat (file-name-as-directory html-directory) new-post-file-name-base html-extension))
  (epic-blog-post-insert-head-file)
  (epic-blog-post-insert-line html-body-start-tag)
  (epic-blog-post-insert-line "<div id=\"page\">")
  (epic-blog-post-insert-featured-image)
  (epic-blog-post-insert-content filename)    
  (epic-blog-post-insert-footer)
  (epic-blog-post-replace-title)
  (epic-blog-post-write-file destination-file-name))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-create (filename)
  "Creates an epic-blog-post."
  (setq post-date (substring (file-name-base filename) 0 10))
  (epic-blog-process-post-file filename)
  ;; insert on category page
  (unless (string-empty-p post-category)
    (unless (get-buffer post-category)
      (setq epic-blog-categories-list (cons post-category epic-blog-categories-list)))    
    (with-current-buffer (get-buffer-create post-category)
      (goto-char (point-min))
      (setq formatted-post-date (epic-blog-get-formatted-post-date))
      (setq date-element (concat "<td class=\"post-data\">" formatted-post-date "</td>"))      
      (setq title-element (concat "<td><a href=\"" (concat (file-name-as-directory "..") (file-name-nondirectory destination-file-name)) "\">" post-title "</a></td>"))
      (insert "<tr>")(newline)
      (insert date-element title-element)(newline)
      (insert "</tr>")(newline)))  
  ;; ;; insert on index-page
  ;; (if (< epic-blog-posts-counter epic-blog-posts-per-page)
  ;;     (progn
  ;; 	(with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
  ;; 	  (goto-char (point-max))
  ;; 	  (insert "<article>")(newline)
  ;; 	  (setq category-element (concat "<span class=\"blog-category-links\"><a href=\"" category-link "\">" category-title "</a></span>"))
  ;; 	  (setq formatted-post-date (epic-blog-get-formatted-post-date))
  ;; 	  (setq date-element (concat "<span class=\"post-data\">Veröffentlicht: " formatted-post-date "</span>")) 
  ;; 	  (insert (concat category-element  " · " date-element))(newline)
  ;; 	  (insert (concat "<h2><a href=\"" destination-file-name "\">" post-title "</a></h2>"))(newline)
  ;; 	  (insert post-content)
  ;; 	  (insert "</article>")(newline)))
  ;;   ;; remaining pages
  ;;   (when (= 0 (mod epic-blog-posts-counter epic-blog-posts-per-page))
  ;;     (setq epic-blog-pages-counter (+ epic-blog-pages-counter 1))
  ;;     (epic-blog-finish-index-page)
  ;;     (when (get-buffer epic-blog-new-page-buffer-name)
  ;; 	(epic-blog-finish-new-pages-page))
  ;;     (epic-blog-create-new-pages-page))
  ;;   (epic-blog-insert-on-pages-page))
  (setq epic-blog-posts-counter (+ epic-blog-posts-counter 1))
  ;; first post is also index.html
  (when (= epic-blog-posts-counter  1)
    (setq epic-blog-index-file destination-file-name))
  (setq epic-blog-previous-destination-filename destination-file-name)
  
  ;; (when (= epic-bloga-posts-counter 0)
  ;;   ;; first post wais also index.html
  ;;   (setq index-file-name (concat (file-name-as-directory html-directory) "index.html"))
  ;;   (copy-file destination-file-name index-file-name t))
  )
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-insert-on-pages-page ()
  "Inserts current post on pages page."
  (with-current-buffer (get-buffer-create epic-blog-new-page-buffer-name)
    (goto-char (point-max))
    (insert "<article>")(newline)
    (setq category-element (concat "<span class=\"blog-category-links\"><a href=\"" category-link "\">" category-title "</a></span>"))
    (setq formatted-post-date (epic-blog-get-formatted-post-date))
    (setq date-element (concat "<span class=\"post-data\">Veröffentlicht: " formatted-post-date "</span>"))
    (insert (concat category-element  " · " date-element))(newline)
    (insert (concat "<h2><a href=\"" destination-file-name "\">" post-title "</a></h2>"))(newline)
    (insert post-content)
    (insert "</article>")(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-buffer-add-line-break ()
  "Adds line break to previous line."
  (with-current-buffer (get-buffer-create epic-blog-page-buffer-name)
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (line-end-position))
    (insert html-line-break)
    (goto-char (point-max))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-buffer-add-paragraph-suffix ()
  "Adds paragraph-suffix to previous line."
  (with-current-buffer (get-buffer-create epic-blog-page-buffer-name)
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (line-end-position))
    (insert html-paragraph-suffix)
    (goto-char (point-max))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-create (filename)
  "Creates an epic-blog-page."
  (epic-blog-page-insert-head)
  (epic-blog-page-insert-content filename)
  (epic-blog-page-insert-footer)  
  (epic-blog-page-replace-title)
  (epic-blog-page-write-file filename))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-insert-content (filename)
  "Inserts file content into page-buffer."
  (with-current-buffer (find-file filename)
    (setq epic-blog-page-file-buffer-name
	  (file-name-nondirectory filename))
    (epic-blog-write-to-page-buffer html-main-start-tag)    
    (epic-blog-iterate-page-buffer)
    (epic-blog-write-to-page-buffer html-main-end-tag)    
    (kill-buffer epic-blog-page-file-buffer-name)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-insert-featured-image ()
  "Insert featured-image part to page-buffer."
  (when epic-blog-show-featured-image
    (epic-blog-write-to-page-buffer "<div class=\"featured-image-container\">")
    (epic-blog-write-to-page-buffer (concat "<img id=\"featured-image\" src=\"" (file-name-as-directory uploads-directory) "rubens-medusa.jpg\" style=\"width:100%;\">"))
    (epic-blog-write-to-page-buffer "<div class=\"bottom-left\"><h1 class=\"site-title\"><a href=\"index.html\" class=\"site-title\">" epic-blog-website-title "</a></h1>\n<p class=\"site-subtitle\">... schreibt.</p></div>")
    (epic-blog-write-to-page-buffer "</div>")))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-insert-featured-image ()
  "Insert featured-image part to post-buffer."
  (when epic-blog-show-featured-image
    (epic-blog-post-insert-line "<div class=\"featured-image-container\">")
    (epic-blog-post-insert-line (concat "<img id=\"featured-image\" src=\"" (file-name-as-directory uploads-directory) "rubens-medusa.jpg\" style=\"width:100%;\">"))
    (epic-blog-post-insert-line "<div class=\"bottom-left\"><h1 class=\"site-title\"><a href=\"index.html\" class=\"site-title\">" epic-blog-website-title "</a></h1>\n<p class=\"site-subtitle\">... schreibt.</p></div>")
    (epic-blog-post-insert-line "</div>")))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-insert-head ()
  "Inserts upper part of page."
  (epic-blog-page-insert-head-file)
  (epic-blog-write-to-page-buffer html-body-start-tag)
  (epic-blog-write-to-page-buffer "<div id=\"page\">")
  (epic-blog-page-insert-featured-image))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-finish-index-page()
  "Writes index-page-buffer to file."
  (when (get-buffer epic-blog-index-page-buffer-name)
    (epic-blog-index-page-insert-pagination)
    (epic-blog-index-page-close-main-tag)
    (epic-blog-index-page-insert-footer)
    (epic-blog-index-page-replace-title)
    (epic-blog-write-index-page-file)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-get-current-line()
  "Returns the current line of cursor position."
  (buffer-substring-no-properties (line-beginning-position)(line-end-position)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-get-formatted-html (line)
  "Returns a html-formatted version of line."
  (setq formatted-line line)
  (when
      (and
       (not (string-prefix-p post-author-prefix formatted-line))
       (not (string-prefix-p post-category-prefix formatted-line))
       (not (string-prefix-p post-date-prefix formatted-line))
       (not (string-prefix-p post-title-prefix formatted-line)))
    (setq formatted-line (epic-blog-html-apply-italics formatted-line))
    (setq formatted-line (epic-blog-html-apply-bold formatted-line))
    (unless (string-prefix-p "<" formatted-line)
      (setq formatted-line (concat html-paragraph-start-tag formatted-line)))
    (unless (string-suffix-p ">" formatted-line)
      (setq formatted-line (concat formatted-line html-paragraph-end-tag))))
  formatted-line)
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-get-formatted-post-date ()
  "Returns the formatted-post-date (dd.mm.yyyy-formatted)."
  (setq date-elements (split-string post-date "-"))
  (setq year (nth 0 date-elements))
  (setq month (nth 1 date-elements))
  (setq day (nth 2 date-elements))
  (setq formatted-post-date (concat day "." month "." year))
  formatted-post-date)
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-get-post-author(line)
  "Return author name from line."
  (setq author-name "")
  (when (string-prefix-p post-author-prefix line)    
    (setq author-name (string-remove-prefix post-author-prefix line))
    (setq author-name (string-trim author-name)))
  author-name)
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-html-apply-bold (line)
  "Replaces * with html-bold-start-tag and html-bold-end-tag."
  (setq index 0)
  (setq new-string "")
  (while (< index (string-width line))
    (setq current-char (aref line index))
    (if (= current-char 42) ; 42 *
	(progn
	  (setq new-string (substring new-string 0))
	  (if (eq bold-input nil)
	      (progn
		(setq new-string (concat new-string html-bold-start-tag))
		(setq bold-input t))
	    (setq new-string (concat new-string html-bold-end-tag))
	    (setq bold-input nil)))
      (setq new-string (concat new-string (char-to-string current-char))))
    (setq index (+ index 1)))
  (setq line new-string)
  line)
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-html-apply-italics (line)
  "Replaces # with italics-prefx and italics-postfix."
  (setq index 0)
  (setq new-string "")
  (while (< index (string-width line))
    (setq current-char (aref line index))
    (if (= current-char 35) ; 35 #
	(progn
	  (setq new-string (substring new-string 0))
	  (if (eq italics-input nil)
	      (progn
		(setq new-string (concat new-string html-italics-start-tag))
		(setq italics-input t))
	    (setq new-string (concat new-string html-italics-end-tag))
	    (setq italics-input nil)))
      (setq new-string (concat new-string (char-to-string current-char))))
    (setq index (+ index 1)))
  (setq line new-string)
  line)
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-insert-line (line)
  "Inserts line into post-buffer."
  (with-current-buffer (get-buffer-create epic-blog-post-buffer-name)
    (goto-char (point-max))
    (insert line)(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-html-insert-line (line)
  "Inserts formatted-line into html-buffer."
  (with-current-buffer (get-buffer-create epic-blog-html-buffer-name)
    (goto-char (point-max))
    (insert line)(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-index-page-close-main-tag()
  "Closes <main>-Tag of index-page."
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (goto-char (point-max))
    (insert html-main-end-tag)(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-index-page-insert-footer()
  "Inserts footer-file into index-page."
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (goto-char (point-max))
    (insert-file-contents footer-file)
    (goto-char (point-max))
    (insert "</div>")(newline)
    (goto-char (point-max))
    (insert html-body-end-tag)(newline)
    (insert html-end-tag)(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-index-page-insert-head()
  "Inserts head-file content."
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (goto-char 1)
    (insert-file-contents head-file)))

;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-index-page-insert-header()
  "Inserts header-file content."
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (goto-char (point-max))
    (insert-file-contents header-file)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-index-page-insert-pagination()
  "Inserts pagination to index-page."
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (goto-char (point-max))
    (setq index-page-next-page (concat (file-name-as-directory
					epic-blog-pages-directory-name)
				       (number-to-string epic-blog-pages-counter)
				       html-extension))
    (setq index-page-next-page-navigation-element (concat
						   "<a rel=\"next\" href=\""
						   index-page-next-page
						   "\" class=\"next-page\">"
						   epic-blog-i18n-previous-page
						   "</a>"))
    (insert (concat "<section id=\"pagination\">\n"
		    index-page-next-page-navigation-element
		    "\n</section>"))
    (newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-index-page-replace-title()
  "Sets title of index-page."
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (let ((case-fold-search t)) ; or nil
      (goto-char (point-min))
      ;; (search-forward title-placeholder nil t) ; title in <head>
      (search-forward html-title-start-tag nil t) ; title in <head>
      (line-beginning-position)
      (kill-line)
      (insert html-title-start-tag epic-blog-website-title html-title-end-tag))
      (goto-char (point-max))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-load-configuration-file ()
  "Loads configuration file."
  (setq epic-blog-configuration-file-name (concat
					  (file-name-as-directory
					   epic-blog-root-directory)					  
					  ".epic-blog"))
       (load epic-blog-configuration-file-name))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-initialize()
  "Initializes the environment for epic-blog-mode."
  (setq epic-blog-posts-counter 0)
  (setq epic-blog-pages-counter 1)
  (defconst epic-blog-root-directory (expand-file-name "../../"))
  (epic-blog-load-configuration-file)
  (defconst html-directory (expand-file-name "../../html"))
  (defconst uploads-directory "uploads")
  (defconst includes-directory (expand-file-name "../includes"))
  (defconst categories-directory (expand-file-name "../categories"))
  (defconst categories-destination-directory (expand-file-name "categories" html-directory))
  (defconst epic-blog-source-pages-directory (expand-file-name "../pages"))
  (defconst head-file (expand-file-name "head.html" includes-directory))
  (defconst pages-head-file (expand-file-name "pages-head.html" includes-directory))
  (defconst header-file (expand-file-name "header.html" includes-directory))
  (defconst footer-file (expand-file-name "footer.html" includes-directory))
  (defconst pages-footer-file (expand-file-name "pages-footer.html" includes-directory))
  (defconst sidebar-file (expand-file-name "sidebar.html" includes-directory))
  (defconst epic-blog-index-page-file-name (expand-file-name "index.html" html-directory)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-insert-content (filename)
  "Inserts post-content."
  (with-current-buffer (find-file filename)
    (setq epic-blog-post-file-buffer-name
	  (file-name-nondirectory filename))
    (epic-blog-post-insert-line html-main-start-tag)
    (epic-blog-iterate-post-buffer)
    (epic-blog-post-insert-pagination)
    (epic-blog-post-insert-line html-main-end-tag)
    (kill-buffer epic-blog-post-file-buffer-name)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-insert-footer()
  "Inserts footer-file content."
  (with-current-buffer (get-buffer-create
			epic-blog-post-buffer-name)
    (goto-char (point-max))
    (insert-file-contents footer-file)
    (epic-blog-post-insert-line "</div>")
    (epic-blog-post-insert-line html-body-end-tag)
    (epic-blog-post-insert-line html-end-tag)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-insert-pagination ()
  "Inserts pagination placeholders in post."
  (with-current-buffer (get-buffer-create
			epic-blog-post-buffer-name)
    (goto-char (point-max))
    (insert "<section id=\"pagination\">")(newline)
    ;;(if (string-empty-p
    (unless (string-empty-p
	     epic-blog-previous-destination-filename)
      ;; insert page in previous-page
      (with-current-buffer
	  (find-file epic-blog-previous-destination-filename)
	(goto-char (point-min))
	(setq next-page-element
	      (concat "<a rel=\"next\" href=\""
		      (file-name-nondirectory destination-file-name)
		      "\" class=\"next-page\">"
		      epic-blog-i18n-previous-page
		      "</a>"))	  
	(search-forward epic-blog-next-page-placeholder nil t)
	(replace-match next-page-element t)
	(write-file epic-blog-previous-destination-filename)
	(kill-buffer))
      (setq previous-page-element
	    (concat "<a rel=\"prev\" href=\""
		    ;; epic-blog-previous-destination-filename
		    (file-name-nondirectory epic-blog-previous-destination-filename)
		    "\" class=\"previous-page\">"
		    epic-blog-i18n-next-page
		    "</a>"))
      ;;)
      (insert previous-page-element " ")
      )
    (unless (= (- (length post-files) 1) epic-blog-posts-counter)
      (insert epic-blog-next-page-placeholder))
    (newline)
    (insert "</section>")(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-insert-head-file ()
  "Inserts head-file content."
  (with-current-buffer (get-buffer-create
			epic-blog-html-buffer-name)
    (goto-char (point-min))
    (insert-file-contents head-file)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-insert-pages-footer()
  "Inserts footer-file content."
  (with-current-buffer (get-buffer-create
			epic-blog-html-buffer-name)
    (goto-char (point-max))
    (insert-file-contents pages-footer-file)
    (epic-blog-html-insert-line html-body-end-tag)
    (epic-blog-html-insert-line html-end-tag)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-iterate-page-buffer ()
  "Iterates over page file."
  (setq last-line "")
  (setq formatted-line "")
  (with-current-buffer (get-buffer-create
			epic-blog-page-file-buffer-name)
    (goto-char (point-min))
    (while (not (eobp))
      (setq formatted-line
	    (string-trim (epic-blog-get-current-line)))
      (if (string-prefix-p page-title-prefix formatted-line)
	  (setq page-title (string-trim
			    (string-remove-prefix
			     page-title-prefix
			     formatted-line)))
	(progn
	  (if (string-empty-p formatted-line)
	      (unless (string-empty-p last-line)
		(epic-blog-page-buffer-add-paragraph-suffix)
		(setq in-paragraph nil))
	    (unless (string-empty-p last-line)
	      (when in-paragraph
		(epic-blog-page-buffer-add-line-break)))
	    (unless (string-prefix-p html-paragraph-prefix
				     formatted-line)
	      (unless in-paragraph
		(setq formatted-line
		      (concat html-paragraph-prefix
			      formatted-line))
		(setq in-paragraph t))))
	  (unless (string-empty-p (string-trim
				   formatted-line))
	    (setq formatted-line
		  (epic-blog-html-apply-bold
		   formatted-line))
	    (setq formatted-line
		  (epic-blog-html-apply-italics
		   formatted-line))
	    (epic-blog-write-to-page-buffer formatted-line))
	  (setq last-line formatted-line)))
      (forward-line))
    (when in-paragraph
      (epic-blog-page-buffer-add-paragraph-suffix)
      (setq in-paragraph nil))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-iterate-post-buffer()
  (setq lines-iterated 0)
  (setq post-content "")
  (setq formatted-line "")
  (with-current-buffer (get-buffer-create epic-blog-post-file-buffer-name)
    (goto-char 1)
    (while (not (eobp))
      (setq formatted-line (epic-blog-get-current-line))
      (setq formatted-line (string-trim formatted-line))
      (cond
       ((string-prefix-p post-author-prefix formatted-line)
	(setq post-author (string-trim (string-remove-prefix
					post-author-prefix
					formatted-line))))
       ((string-prefix-p post-category-prefix formatted-line)
	(setq post-category (string-trim
			     (string-remove-prefix
			      post-category-prefix
			      formatted-line)))



	;; insert on category page
	(setq category-title "")
	(unless (string-empty-p post-category)
	  ;; create category page if not exists
	  (setq category-file (concat (file-name-as-directory categories-directory) post-category category-file-extension))
	  (unless (file-exists-p category-file)
	    (with-current-buffer (get-buffer-create category-buffer-name)
	      (setq category-title (concat category-title-prefix " TODO: " post-category))
	      (insert category-title)(newline)
	      (write-file category-file)
	      (kill-buffer)))
	  ;; read from category file
	  (with-current-buffer (find-file category-file)
	    (goto-char (point-min))
	    (setq line (buffer-substring-no-properties (line-beginning-position)(line-end-position)))
	    (setq category-title (string-trim (string-remove-prefix category-title-prefix line)))
	    (kill-buffer))
	  ;; create category html page
	  ;;    (setq category-link (concat (file-name-as-directory html-directory) (file-name-as-directory "categories") post-category html-extension))
	  (setq category-link (concat (file-name-as-directory "categories") post-category html-extension))
	  ;; create directory "categories" if not exists
	  )
	
	
	




	
	)
       
       ;; ((string-prefix-p post-date-prefix formatted-line)
       ;; 	(setq post-date (string-trim (string-remove-prefix
       ;; 				      post-date-prefix
       ;; 				      formatted-line))))
       
       ((string-prefix-p post-title-prefix formatted-line)
	(setq post-title (string-trim (string-remove-prefix
				       post-title-prefix
				       formatted-line))))	
       ((not (string-empty-p formatted-line))
	(setq formatted-line (epic-blog-get-formatted-html
			      formatted-line))
	(epic-blog-post-insert-line formatted-line)
	(setq post-content (concat post-content
				   formatted-line
				   "\n"))
	(setq last-line formatted-line)))
      (setq lines-iterated (+ lines-iterated 1))
      (forward-line))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-insert-footer()
  "Inserts footer-file content."
  (with-current-buffer (get-buffer-create
			epic-blog-page-buffer-name)
    (goto-char (point-max))
    (insert-file-contents footer-file))
  (epic-blog-write-to-page-buffer html-div-end-tag)
  (epic-blog-write-to-page-buffer html-body-end-tag)
  (epic-blog-write-to-page-buffer html-end-tag))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-insert-head-file ()
  "Inserts head-file into page-buffer."
  (with-current-buffer (get-buffer-create
			epic-blog-page-buffer-name)
    (goto-char 1)
    (insert-file-contents head-file)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-category-replace-title (category)
  "Sets title of category page."
  (unless (string-empty-p category)
    (setq new-title-element (concat html-title-start-tag
				    (epic-blog-get-category-title category)
				    html-title-end-tag))
    (with-current-buffer (get-buffer-create category)
      (let ((case-fold-search t)) ; or nil
	(goto-char (point-min))
					; title in <head>
	(search-forward title-placeholder nil t)
	(replace-match new-title-element)))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-replace-title ()
  "Sets title of target-html-site."
  (unless (string-empty-p page-title)
    (with-current-buffer (get-buffer-create
			  epic-blog-page-buffer-name)
      (let ((case-fold-search t)) ; or nil
	(goto-char (point-min))
					; title in <head>
	(search-forward title-placeholder nil t)
	(replace-match (concat html-title-start-tag
			       page-title
			       html-title-end-tag))
	(setq title-element (concat "<h2>"
				    page-title
				    "</h2>"))
	(setq old-main-element "<main>")
	(setq new-main-element (concat
				"<main class=\"page\">\n"
				title-element))
					; title in <body>
	(search-forward old-main-element nil t)
	(replace-match new-main-element)	
	(goto-char (point-max))))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-page-write-file (filename)
  "Creates destination filename from filename and saves 
page."
  (setq page-file-name-base (file-name-base filename))
  (if (> (length page-file-name-base) 11)
      (setq page-file-name-base
	    (substring
	     page-file-name-base
	     11
	     (length page-file-name-base))))
  (setq destination-file-name (concat (file-name-as-directory
				       html-directory)
				      page-file-name-base
				      html-extension))
  (with-current-buffer (get-buffer-create
			epic-blog-page-buffer-name)
    (funcall 'html-mode)
    (indent-region (point-min)(point-max))
    (write-file destination-file-name nil)
    (kill-buffer)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-replace-title()
  "Sets title of target-html-site."
  (unless (string-empty-p post-title)
    (with-current-buffer (get-buffer-create
			  epic-blog-post-buffer-name)
      (let ((case-fold-search t)) ; or nil
	(goto-char (point-min))
					; title in <head>
	(search-forward title-placeholder nil t)
	(replace-match (concat html-title-start-tag
			       post-title
			       html-title-end-tag))
	(setq category-element
	      (concat
	       "<span class=\"blog-category-links\">"
	       "<a href=\"" category-link "\">"
	       category-title "</a></span>"))
	(setq title-element (concat "<h2>"
				    post-title
				    "</h2>"))
	(setq formatted-post-date
	      (epic-blog-get-formatted-post-date))
	(setq date-element (concat
			    "<span class=\"post-data\">"
			    "Veröffentlicht: "
			    formatted-post-date
			    "</span>")) 
	(setq old-main-element "<main>")
	(setq new-main-element (concat
				old-main-element "\n"
				category-element " · "
				date-element "\n"
				title-element))
					; title in <body>
	(search-forward old-main-element nil t)
	(replace-match new-main-element)	
	(goto-char (point-max))))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-create-new-pages-page()
  "Opens an buffer to creates a new pages page for pagination."
  (with-current-buffer (get-buffer-create epic-blog-new-page-buffer-name)
    (insert-file-contents pages-head-file)
    (goto-char (point-max))
    (insert html-body-start-tag)(newline)
    (insert "<div id=\"page\">")(newline)
    ;; featured image
    (when epic-blog-show-featured-image
      (insert "<div class=\"featured-image-container\">")(newline)
      (insert (concat "<img id=\"featured-image\" src=\"../" (file-name-as-directory uploads-directory) "rubens-medusa.jpg\" style=\"width:100%;\">"))(newline)
      (insert "<div class=\"bottom-left\"><h1 class=\"site-title\"><a href=\"../index.html\" class=\"site-title\">" epic-blog-website-title "</a></h1>\n<p class=\"site-subtitle\">... schreibt.</p></div>")(newline)
      (insert "</div>")(newline))
    ;; sidebar
    (when epic-blog-show-sidebar
      (insert "<aside>")(newline)
      (insert-file-contents sidebar-file)
      (goto-char (point-max))
      (insert "</aside>")(newline))
    (insert html-main-start-tag)(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-finish-new-pages-page()
  "Writes new pages-buffer to file."
  (when (get-buffer epic-blog-new-page-buffer-name)
    (with-current-buffer (get-buffer-create epic-blog-new-page-buffer-name)
      (goto-char (point-max))
      ;; pagination
      ;; previous-page
      (if (= epic-blog-pages-counter 2)
	  (progn
	    (setq next-page (concat (number-to-string epic-blog-pages-counter)
				    html-extension))
	    (setq next-page-navigation-element (concat "<a rel=\"next\" href=\""
						       next-page
						       "\" class=\"next-page\">"
						       epic-blog-i18n-previous-page
						       "</a>"))
	    (insert (concat "<section id=\"pagination\">\n"
			    next-page-navigation-element
			    "\n</section>"))
	    (newline))
	(if (= epic-blog-posts-counter (length post-files))
	    (progn
	      (setq previous-page (concat (number-to-string
					   (- epic-blog-pages-counter 1))
					  html-extension))
	      (setq previous-page-navigation-element
		    (concat
		     "<a rel=\"prev\" href=\""
		     previous-page
		     "\" class=\"previous-page\">"
		     epic-blog-i18n-next-page
		       "</a>"))
	      (insert (concat "<section id=\"pagination\">\n"
			      previous-page-navigation-element
			      "\n</section>"))
	      (newline))
	  (progn
	    (if (= epic-blog-pages-counter 3)
		(setq previous-page epic-blog-index-page-file-name)
	      (setq previous-page (concat (number-to-string
					   (- epic-blog-pages-counter 2))
					  html-extension)))
	    (setq previous-page-navigation-element
		  (concat
		   "<a rel=\"prev\" href=\""
		   previous-page
		   "\" class=\"previous-page\">"
		   epic-blog-i18n-next-page
		     "</a>"))
	    (setq next-page (concat (number-to-string epic-blog-pages-counter)
				    html-extension))
	    (setq next-page-navigation-element (concat "<a rel=\"next\" href=\""
						       next-page
						       "\" class=\"next-page\">"
						       epic-blog-i18n-previous-page
						       "</a>"))
	    (insert (concat "<section id=\"pagination\">\n"
			    previous-page-navigation-element
			    next-page-navigation-element
			    "\n</section>"))
	    (newline))))
      ;;      (insert "</div>")(newline)
      ;; close main-tag
      (insert html-main-end-tag)(newline)
      ;; insert footer
      (insert-file-contents pages-footer-file)
      (goto-char (point-max))
      (insert "</div>")(newline)
      (insert html-body-end-tag)(newline)
      (insert html-end-tag)(newline)
      ;; replace title
      (let ((case-fold-search t)) ; or nil
	(goto-char (point-min))
	(search-forward title-placeholder nil t) ; title in <head>
	(replace-match (concat html-title-start-tag epic-blog-website-title
			       html-title-end-tag))
	(goto-char (point-max)))
      ;; write file
      (funcall 'html-mode)
      (indent-region (point-min)(point-max))
      (setq new-page-filename (concat (file-name-as-directory html-directory)
				      (file-name-as-directory
				       epic-blog-pages-directory-name)
				      (number-to-string
				       (/ epic-blog-posts-counter
					  epic-blog-posts-per-page))
				      html-extension))
      (unless (file-directory-p epic-blog-pages-directory-name)
	(make-directory (file-name-as-directory epic-blog-pages-directory-name)))  
      (write-file new-page-filename nil)
      (kill-buffer))))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-start-index-page()
  "Opens an buffer to create an index-page."
  (epic-blog-index-page-insert-head)
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (goto-char (point-max))
    (insert html-body-start-tag)(newline)
    (insert "<div id=\"page\">")(newline)
    ;; featured image
    (when epic-blog-show-featured-image
      (insert "<div class=\"featured-image-container\">")(newline)
      (insert (concat "<img id=\"featured-image\" src=\"" (file-name-as-directory
							   uploads-directory)
		      "rubens-medusa.jpg\" style=\"width:100%;\">"))
      (newline)
      (insert "<div class=\"bottom-left\"><h1 class=\"site-title\">"
	      "<a href=\"index.html\" class=\"site-title\">" epic-blog-website-title "</a>"
	      "</h1>\n<p class=\"site-subtitle\">... schreibt.</p></div>")
      (newline)
      (insert "</div>")(newline))
    ;; sidebar
    (when epic-blog-show-sidebar      
      (insert "<aside>")(newline)
      (insert-file-contents sidebar-file)
      (goto-char (point-max))
      (insert "</aside>")(newline))
    (insert html-main-start-tag)(newline)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-post-write-file (destination-file-name)
  "Writes html-buffer to a file."
  (with-current-buffer (get-buffer-create epic-blog-post-buffer-name)
    (funcall 'html-mode)
    (indent-region (point-min)(point-max))
    (write-file destination-file-name nil)
    (kill-buffer)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-write-index-page-file()
  "Writes index-page-file."
  (with-current-buffer (get-buffer-create epic-blog-index-page-buffer-name)
    (funcall 'html-mode)
    (indent-region (point-min)(point-max))
    (write-file epic-blog-index-page-file-name nil)
    (kill-buffer)))
;;; -----------------------------------------------------------------------------
;;;
(defun epic-blog-write-to-page-buffer (line)
  "Writes line to page-buffer."
  (with-current-buffer (get-buffer-create epic-blog-page-buffer-name)
    (goto-char (point-max))
    (insert line)(newline)))
;;; -----------------------------------------------------------------------------
;;; Tests
;;;
(load "epic-blog-tests.el")
;;;; epic-blog.el ends here
