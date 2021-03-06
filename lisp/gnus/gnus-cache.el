;;; gnus-cache.el --- cache interface for Gnus
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
;;        Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-int)
(require 'gnus-range)
(require 'gnus-start)
(eval-when-compile
  (require 'gnus-sum))

(defcustom gnus-cache-active-file
  (expand-file-name "active" gnus-cache-directory)
  "*The cache active file."
  :group 'gnus-cache
  :type 'file)

(defcustom gnus-cache-enter-articles '(ticked dormant)
  "Classes of articles to enter into the cache."
  :group 'gnus-cache
  :type '(set (const ticked) (const dormant) (const unread) (const read)))

(defcustom gnus-cache-remove-articles '(read)
  "Classes of articles to remove from the cache."
  :group 'gnus-cache
  :type '(set (const ticked) (const dormant) (const unread) (const read)))

(defcustom gnus-cacheable-groups nil
  "*Groups that match this regexp will be cached.

If you only want to cache your nntp groups, you could set this
variable to \"^nntp\".

If a group matches both gnus-cacheable-groups and gnus-uncacheable-groups
it's not cached."
  :group 'gnus-cache
  :type '(choice (const :tag "off" nil)
		 regexp))

(defcustom gnus-uncacheable-groups nil
  "*Groups that match this regexp will not be cached.

If you want to avoid caching your nnml groups, you could set this
variable to \"^nnml\".

If a group matches both gnus-cacheable-groups and gnus-uncacheable-groups
it's not cached."
  :group 'gnus-cache
  :type '(choice (const :tag "off" nil)
		 regexp))

(defvar gnus-cache-overview-coding-system 'raw-text
  "Coding system used on Gnus cache files.")

(defvar gnus-cache-coding-system 'raw-text
  "Coding system used on Gnus cache files.")



;;; Internal variables.

(defvar gnus-cache-removable-articles nil)
(defvar gnus-cache-buffer nil)
(defvar gnus-cache-active-hashtb nil)
(defvar gnus-cache-active-altered nil)

(eval-and-compile
  (autoload 'nnml-generate-nov-databases-1 "nnml")
  (autoload 'nnvirtual-find-group-art "nnvirtual"))



;;; Functions called from Gnus.

(defun gnus-cache-open ()
  "Initialize the cache."
  (when (or (file-exists-p gnus-cache-directory)
	    (and gnus-use-cache
		 (not (eq gnus-use-cache 'passive))))
    (gnus-cache-read-active)))

;; Complexities of byte-compiling make this kludge necessary.  Eeek.
(ignore-errors
  (gnus-add-shutdown 'gnus-cache-close 'gnus))

(defun gnus-cache-close ()
  "Shut down the cache."
  (gnus-cache-write-active)
  (gnus-cache-save-buffers)
  (setq gnus-cache-active-hashtb nil))

(defun gnus-cache-save-buffers ()
  ;; save the overview buffer if it exists and has been modified
  ;; delete empty cache subdirectories
  (when gnus-cache-buffer
    (let ((buffer (cdr gnus-cache-buffer))
	  (overview-file (gnus-cache-file-name
			  (car gnus-cache-buffer) ".overview")))
      ;; write the overview only if it was modified
      (when (buffer-modified-p buffer)
	(save-excursion
	  (set-buffer buffer)
	  (if (> (buffer-size) 0)
	      ;; Non-empty overview, write it to a file.
	      (let ((coding-system-for-write
		     gnus-cache-overview-coding-system))
		(gnus-write-buffer overview-file))
	    ;; Empty overview file, remove it
	    (when (file-exists-p overview-file)
	      (delete-file overview-file))
	    ;; If possible, remove group's cache subdirectory.
	    (condition-case nil
		;; FIXME: we can detect the error type and warn the user
		;; of any inconsistencies (articles w/o nov entries?).
		;; for now, just be conservative...delete only if safe -- sj
		(delete-directory (file-name-directory overview-file))
	      (error nil)))))
      ;; Kill the buffer -- it's either unmodified or saved.
      (gnus-kill-buffer buffer)
      (setq gnus-cache-buffer nil))))

(defun gnus-cache-possibly-enter-article
  (group article ticked dormant unread &optional force)
  (when (and (or force (not (eq gnus-use-cache 'passive)))
	     (numberp article)
	     (> article 0))		; This might be a dummy article.
    (let ((number article) file headers)
      ;; If this is a virtual group, we find the real group.
      (when (gnus-virtual-group-p group)
	(let ((result (nnvirtual-find-group-art
		       (gnus-group-real-name group) article)))
	  (setq group (car result)
		number (cdr result))))
      (when (and number
		 (> number 0)		; Reffed article.
		 (or force
                     (and (or (not gnus-cacheable-groups)
                              (string-match gnus-cacheable-groups group))
                          (or (not gnus-uncacheable-groups)
			      (not (string-match
				    gnus-uncacheable-groups group)))
			  (gnus-cache-member-of-class
			   gnus-cache-enter-articles ticked dormant unread)))
		 (not (file-exists-p (setq file (gnus-cache-file-name
						 group number)))))
	;; Possibly create the cache directory.
	(gnus-make-directory (file-name-directory file))
	;; Save the article in the cache.
	(if (file-exists-p file)
	    t				; The article already is saved.
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (require 'gnus-art)
	    (let ((gnus-use-cache nil)
		  (gnus-article-decode-hook nil))
	      (gnus-request-article-this-buffer number group))
	    (when (> (buffer-size) 0)
	      (let ((coding-system-for-write gnus-cache-coding-system))
		(gnus-write-buffer file))
	      (setq headers (nnheader-parse-head t))
	      (mail-header-set-number headers number)
	      (gnus-cache-change-buffer group)
	      (set-buffer (cdr gnus-cache-buffer))
	      (goto-char (point-max))
	      (forward-line -1)
	      (while (condition-case ()
			 (when (not (bobp))
			   (> (read (current-buffer)) number))
		       (error
			;; The line was malformed, so we just remove it!!
			(gnus-delete-line)
			t))
		(forward-line -1))
	      (if (bobp)
		  (if (not (eobp))
		      (progn
			(beginning-of-line)
			(when (< (read (current-buffer)) number)
			  (forward-line 1)))
		    (beginning-of-line))
		(forward-line 1))
	      (beginning-of-line)
	      (nnheader-insert-nov headers)
	      ;; Update the active info.
	      (set-buffer gnus-summary-buffer)
	      (gnus-cache-update-active group number)
	      (push article gnus-newsgroup-cached)
	      (gnus-summary-update-secondary-mark article))
	    t))))))

(defun gnus-cache-enter-remove-article (article)
  "Mark ARTICLE for later possible removal."
  (when article
    (push article gnus-cache-removable-articles)))

(defun gnus-cache-possibly-remove-articles ()
  "Possibly remove some of the removable articles."
  (if (not (gnus-virtual-group-p gnus-newsgroup-name))
      (gnus-cache-possibly-remove-articles-1)
    (let ((arts gnus-cache-removable-articles)
	  ga)
      (while arts
	(when (setq ga (nnvirtual-find-group-art
			(gnus-group-real-name gnus-newsgroup-name) (pop arts)))
	  (let ((gnus-cache-removable-articles (list (cdr ga)))
		(gnus-newsgroup-name (car ga)))
	    (gnus-cache-possibly-remove-articles-1)))))
    (setq gnus-cache-removable-articles nil)))

(defun gnus-cache-possibly-remove-articles-1 ()
  "Possibly remove some of the removable articles."
  (unless (eq gnus-use-cache 'passive)
    (let ((articles gnus-cache-removable-articles)
	  (cache-articles gnus-newsgroup-cached)
	  article)
      (gnus-cache-change-buffer gnus-newsgroup-name)
      (while articles
	(when (memq (setq article (pop articles)) cache-articles)
	  ;; The article was in the cache, so we see whether we are
	  ;; supposed to remove it from the cache.
	  (gnus-cache-possibly-remove-article
	   article (memq article gnus-newsgroup-marked)
	   (memq article gnus-newsgroup-dormant)
	   (or (memq article gnus-newsgroup-unreads)
	       (memq article gnus-newsgroup-unselected))))))
    ;; The overview file might have been modified, save it
    ;; safe because we're only called at group exit anyway.
    (gnus-cache-save-buffers)))

(defun gnus-cache-request-article (article group)
  "Retrieve ARTICLE in GROUP from the cache."
  (let ((file (gnus-cache-file-name group article))
	(buffer-read-only nil))
    (when (file-exists-p file)
      (erase-buffer)
      (gnus-kill-all-overlays)
      (let ((coding-system-for-read gnus-cache-coding-system))
	(insert-file-contents file))
      t)))

(defun gnus-cache-possibly-alter-active (group active)
  "Alter the ACTIVE info for GROUP to reflect the articles in the cache."
  (when gnus-cache-active-hashtb
    (let ((cache-active (gnus-gethash group gnus-cache-active-hashtb)))
      (when cache-active
	(when (< (car cache-active) (car active))
	  (setcar active (car cache-active)))
	(when (> (cdr cache-active) (cdr active))
	  (setcdr active (cdr cache-active)))))))

(defun gnus-cache-retrieve-headers (articles group &optional fetch-old)
  "Retrieve the headers for ARTICLES in GROUP."
  (let ((cached
	 (setq gnus-newsgroup-cached (gnus-cache-articles-in-group group))))
    (if (not cached)
	;; No cached articles here, so we just retrieve them
	;; the normal way.
	(let ((gnus-use-cache nil))
	  (gnus-retrieve-headers articles group fetch-old))
      (let ((uncached-articles (gnus-sorted-intersection
				(gnus-sorted-complement articles cached)
				articles))
	    (cache-file (gnus-cache-file-name group ".overview"))
	    type)
	;; We first retrieve all the headers that we don't have in
	;; the cache.
	(let ((gnus-use-cache nil))
	  (when uncached-articles
	    (setq type (and articles
			    (gnus-retrieve-headers
			     uncached-articles group fetch-old)))))
	(gnus-cache-save-buffers)
	;; Then we insert the cached headers.
	(save-excursion
	  (cond
	   ((not (file-exists-p cache-file))
	    ;; There are no cached headers.
	    type)
	   ((null type)
	    ;; There were no uncached headers (or retrieval was
	    ;; unsuccessful), so we use the cached headers exclusively.
	    (set-buffer nntp-server-buffer)
	    (erase-buffer)
	    (let ((coding-system-for-read 
		   gnus-cache-overview-coding-system))
	      (insert-file-contents cache-file))
	    'nov)
	   ((eq type 'nov)
	    ;; We have both cached and uncached NOV headers, so we
	    ;; braid them.
	    (gnus-cache-braid-nov group cached)
	    type)
	   (t
	    ;; We braid HEADs.
	    (gnus-cache-braid-heads group (gnus-sorted-intersection
					   cached articles))
	    type)))))))

(defun gnus-cache-enter-article (&optional n)
  "Enter the next N articles into the cache.
If not given a prefix, use the process marked articles instead.
Returns the list of articles entered."
  (interactive "P")
  (let ((articles (gnus-summary-work-articles n))
	article out)
    (while (setq article (pop articles))
      (gnus-summary-remove-process-mark article)
      (if (natnump article)
	  (when (gnus-cache-possibly-enter-article
		 gnus-newsgroup-name article
		 nil nil nil t)
	    (push article out))
	(gnus-message 2 "Can't cache article %d" article))
      (gnus-summary-update-secondary-mark article))
    (gnus-summary-next-subject 1)
    (gnus-summary-position-point)
    (nreverse out)))

(defun gnus-cache-remove-article (n)
  "Remove the next N articles from the cache.
If not given a prefix, use the process marked articles instead.
Returns the list of articles removed."
  (interactive "P")
  (gnus-cache-change-buffer gnus-newsgroup-name)
  (let ((articles (gnus-summary-work-articles n))
	article out)
    (while articles
      (setq article (pop articles))
      (gnus-summary-remove-process-mark article)
      (when (gnus-cache-possibly-remove-article article nil nil nil t)
	(push article out))
      (gnus-summary-update-secondary-mark article))
    (gnus-summary-next-subject 1)
    (gnus-summary-position-point)
    (nreverse out)))

(defun gnus-cached-article-p (article)
  "Say whether ARTICLE is cached in the current group."
  (memq article gnus-newsgroup-cached))

(defun gnus-summary-insert-cached-articles ()
  "Insert all the articles cached for this group into the current buffer."
  (interactive)
  (let ((cached (sort (copy-sequence gnus-newsgroup-cached) '>))
	(gnus-verbose (max 6 gnus-verbose)))
    (unless cached
      (gnus-message 3 "No cached articles for this group"))
    (while cached
      (gnus-summary-goto-subject (pop cached) t))))

(defalias 'gnus-summary-limit-include-cached
  'gnus-summary-insert-cached-articles)

;;; Internal functions.

(defun gnus-cache-change-buffer (group)
  (and gnus-cache-buffer
       ;; See if the current group's overview cache has been loaded.
       (or (string= group (car gnus-cache-buffer))
	   ;; Another overview cache is current, save it.
	   (gnus-cache-save-buffers)))
  ;; if gnus-cache buffer is nil, create it
  (unless gnus-cache-buffer
    ;; Create cache buffer
    (save-excursion
      (setq gnus-cache-buffer
	    (cons group
		  (set-buffer (gnus-get-buffer-create
			       " *gnus-cache-overview*"))))
      ;; Insert the contents of this group's cache overview.
      (erase-buffer)
      (let ((file (gnus-cache-file-name group ".overview")))
	(when (file-exists-p file)
	  (nnheader-insert-file-contents file)))
      ;; We have a fresh (empty/just loaded) buffer,
      ;; mark it as unmodified to save a redundant write later.
      (set-buffer-modified-p nil))))

;; Return whether an article is a member of a class.
(defun gnus-cache-member-of-class (class ticked dormant unread)
  (or (and ticked (memq 'ticked class))
      (and dormant (memq 'dormant class))
      (and unread (memq 'unread class))
      (and (not unread) (not ticked) (not dormant) (memq 'read class))))

(defun gnus-cache-file-name (group article)
  (expand-file-name
   (if (stringp article) article (int-to-string article))
   (file-name-as-directory
    (expand-file-name
     (nnheader-translate-file-chars
      (if (gnus-use-long-file-name 'not-cache)
	  group
	(let ((group (nnheader-replace-duplicate-chars-in-string
		      (nnheader-replace-chars-in-string group ?/ ?_)
		      ?. ?_)))
	  ;; Translate the first colon into a slash.
	  (when (string-match ":" group)
	    (aset group (match-beginning 0) ?/))
	  (nnheader-replace-chars-in-string group ?. ?/)))
      t)
     gnus-cache-directory))))

(defun gnus-cache-update-article (group article)
  "If ARTICLE is in the cache, remove it and re-enter it."
  (gnus-cache-change-buffer group)
  (when (gnus-cache-possibly-remove-article article nil nil nil t)
    (let ((gnus-use-cache nil))
      (gnus-cache-possibly-enter-article
       gnus-newsgroup-name article
       nil nil nil t))))

(defun gnus-cache-possibly-remove-article (article ticked dormant unread
						   &optional force)
  "Possibly remove ARTICLE from the cache."
  (let ((group gnus-newsgroup-name)
	(number article)
	file)
    ;; If this is a virtual group, we find the real group.
    (when (gnus-virtual-group-p group)
      (let ((result (nnvirtual-find-group-art
		     (gnus-group-real-name group) article)))
	(setq group (car result)
	      number (cdr result))))
    (setq file (gnus-cache-file-name group number))
    (when (and (file-exists-p file)
	       (or force
		   (gnus-cache-member-of-class
		    gnus-cache-remove-articles ticked dormant unread)))
      (save-excursion
	(delete-file file)
	(set-buffer (cdr gnus-cache-buffer))
	(goto-char (point-min))
	(when (or (looking-at (concat (int-to-string number) "\t"))
		  (search-forward (concat "\n" (int-to-string number) "\t")
				  (point-max) t))
	  (delete-region (progn (beginning-of-line) (point))
			 (progn (forward-line 1) (point)))))
      (setq gnus-newsgroup-cached
	    (delq article gnus-newsgroup-cached))
      (gnus-summary-update-secondary-mark article)
      t)))

(defun gnus-cache-articles-in-group (group)
  "Return a sorted list of cached articles in GROUP."
  (let ((dir (file-name-directory (gnus-cache-file-name group 1)))
	articles)
    (when (file-exists-p dir)
      (setq articles
	    (sort (mapcar (lambda (name) (string-to-int name))
			  (directory-files dir nil "^[0-9]+$" t))
		  '<))
      ;; Update the cache active file, just to synch more.
      (when articles
	(gnus-cache-update-active group (car articles) t)
	(gnus-cache-update-active group (car (last articles))))
      articles)))

(defun gnus-cache-braid-nov (group cached &optional file)
  (let ((cache-buf (gnus-get-buffer-create " *gnus-cache*"))
	beg end)
    (gnus-cache-save-buffers)
    (save-excursion
      (set-buffer cache-buf)
      (erase-buffer)
      (let ((coding-system-for-read 
	     gnus-cache-overview-coding-system))
	(insert-file-contents 
	 (or file (gnus-cache-file-name group ".overview"))))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min)))
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (while cached
      (while (and (not (eobp))
		  (< (read (current-buffer)) (car cached)))
	(forward-line 1))
      (beginning-of-line)
      (save-excursion
	(set-buffer cache-buf)
	(if (search-forward (concat "\n" (int-to-string (car cached)) "\t")
			    nil t)
	    (setq beg (progn (beginning-of-line) (point))
		  end (progn (end-of-line) (point)))
	  (setq beg nil)))
      (when beg
	(insert-buffer-substring cache-buf beg end)
	(insert "\n"))
      (setq cached (cdr cached)))
    (kill-buffer cache-buf)))

(defun gnus-cache-braid-heads (group cached)
  (let ((cache-buf (gnus-get-buffer-create " *gnus-cache*")))
    (save-excursion
      (set-buffer cache-buf)
      (erase-buffer))
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (while cached
      (while (and (not (eobp))
		  (looking-at "2.. +\\([0-9]+\\) ")
		  (< (progn (goto-char (match-beginning 1))
			    (read (current-buffer)))
		     (car cached)))
	(search-forward "\n.\n" nil 'move))
      (beginning-of-line)
      (save-excursion
	(set-buffer cache-buf)
	(erase-buffer)
	(let ((coding-system-for-read 
	       gnus-cache-coding-system))
	  (insert-file-contents (gnus-cache-file-name group (car cached))))
	(goto-char (point-min))
	(insert "220 ")
	(princ (car cached) (current-buffer))
	(insert " Article retrieved.\n")
	(search-forward "\n\n" nil 'move)
	(delete-region (point) (point-max))
	(forward-char -1)
	(insert "."))
      (insert-buffer-substring cache-buf)
      (setq cached (cdr cached)))
    (kill-buffer cache-buf)))

;;;###autoload
(defun gnus-jog-cache ()
  "Go through all groups and put the articles into the cache.

Usage:
$ emacs -batch -l ~/.emacs -l gnus -f gnus-jog-cache"
  (interactive)
  (let ((gnus-mark-article-hook nil)
	(gnus-expert-user t)
	(nnmail-spool-file nil)
	(mail-sources nil)
	(gnus-use-dribble-file nil)
	(gnus-novice-user nil)
	(gnus-large-newsgroup nil))
    ;; Start Gnus.
    (gnus)
    ;; Go through all groups...
    (gnus-group-mark-buffer)
    (gnus-group-iterate nil
      (lambda (group)
	(let (gnus-auto-select-next)
	  (gnus-summary-read-group group nil t)
	  ;; ... and enter the articles into the cache.
	  (when (eq major-mode 'gnus-summary-mode)
	    (gnus-uu-mark-buffer)
	    (gnus-cache-enter-article)
	    (kill-buffer (current-buffer))))))))

(defun gnus-cache-read-active (&optional force)
  "Read the cache active file."
  (gnus-make-directory gnus-cache-directory)
  (if (or (not (file-exists-p gnus-cache-active-file))
	  (zerop (nth 7 (file-attributes gnus-cache-active-file)))
	  force)
      ;; There is no active file, so we generate one.
      (gnus-cache-generate-active)
    ;; We simply read the active file.
    (save-excursion
      (gnus-set-work-buffer)
      (nnheader-insert-file-contents gnus-cache-active-file)
      (gnus-active-to-gnus-format
       nil (setq gnus-cache-active-hashtb
		 (gnus-make-hashtable
		  (count-lines (point-min) (point-max)))))
      (setq gnus-cache-active-altered nil))))

(defun gnus-cache-write-active (&optional force)
  "Write the active hashtb to the active file."
  (when (or force
	    (and gnus-cache-active-hashtb
		 gnus-cache-active-altered))
    (gnus-write-active-file gnus-cache-active-file gnus-cache-active-hashtb t)
    ;; Mark the active hashtb as unaltered.
    (setq gnus-cache-active-altered nil)))

(defun gnus-cache-update-active (group number &optional low)
  "Update the upper bound of the active info of GROUP to NUMBER.
If LOW, update the lower bound instead."
  (let ((active (gnus-gethash group gnus-cache-active-hashtb)))
    (if (null active)
	;; We just create a new active entry for this group.
	(gnus-sethash group (cons number number) gnus-cache-active-hashtb)
      ;; Update the lower or upper bound.
      (if low
	  (setcar active number)
	(setcdr active number)))
    ;; Mark the active hashtb as altered.
    (setq gnus-cache-active-altered t)))

;;;###autoload
(defun gnus-cache-generate-active (&optional directory)
  "Generate the cache active file."
  (interactive)
  (let* ((top (null directory))
	 (directory (expand-file-name (or directory gnus-cache-directory)))
	 (files (directory-files directory 'full))
	 (group
	  (if top
	      ""
	    (string-match
	     (concat "^" (regexp-quote
			  (file-name-as-directory
			   (expand-file-name gnus-cache-directory))))
	     (directory-file-name directory))
	    (nnheader-replace-chars-in-string
	     (substring (directory-file-name directory) (match-end 0))
	     ?/ ?.)))
	 nums alphs)
    (when top
      (gnus-message 5 "Generating the cache active file...")
      (setq gnus-cache-active-hashtb (gnus-make-hashtable 123)))
    (when (string-match "^\\(nn[^_]+\\)_" group)
      (setq group (replace-match "\\1:" t t group)))
    ;; Separate articles from all other files and directories.
    (while files
      (if (string-match "^[0-9]+$" (file-name-nondirectory (car files)))
	  (push (string-to-int (file-name-nondirectory (pop files))) nums)
	(push (pop files) alphs)))
    ;; If we have nums, then this is probably a valid group.
    (when (setq nums (sort nums '<))
      (gnus-sethash group (cons (car nums) (gnus-last-element nums))
		    gnus-cache-active-hashtb))
    ;; Go through all the other files.
    (while alphs
      (when (and (file-directory-p (car alphs))
		 (not (string-match "^\\."
				    (file-name-nondirectory (car alphs)))))
	;; We descend directories.
	(gnus-cache-generate-active (car alphs)))
      (setq alphs (cdr alphs)))
    ;; Write the new active file.
    (when top
      (gnus-cache-write-active t)
      (gnus-message 5 "Generating the cache active file...done"))))

;;;###autoload
(defun gnus-cache-generate-nov-databases (dir)
  "Generate NOV files recursively starting in DIR."
  (interactive (list gnus-cache-directory))
  (gnus-cache-close)
  (let ((nnml-generate-active-function 'identity))
    (nnml-generate-nov-databases-1 dir)))

(defun gnus-cache-move-cache (dir)
  "Move the cache tree to somewhere else."
  (interactive "FMove the cache tree to: ")
  (rename-file gnus-cache-directory dir))

(provide 'gnus-cache)

;;; gnus-cache.el ends here
