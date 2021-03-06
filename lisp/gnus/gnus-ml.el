;;; gnus-ml.el --- mailing list minor mode for Gnus

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Julien Gilles  <jgilles@free.fr>
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

;; implement (small subset of) RFC 2369

;;; Usage:

;; (add-hook 'gnus-summary-mode-hook 'turn-on-gnus-mailing-list-mode)

;;; Code:

(require 'gnus)
(require 'gnus-msg)
(eval-when-compile (require 'cl))

;;; Mailing list minor mode

(defvar gnus-mailing-list-mode nil
  "Minor mode for providing mailing-list commands.")

(defvar gnus-mailing-list-mode-map nil)

(defvar gnus-mailing-list-menu)

(unless gnus-mailing-list-mode-map
  (setq gnus-mailing-list-mode-map (make-sparse-keymap))

  (gnus-define-keys gnus-mailing-list-mode-map
    "\C-nh" gnus-mailing-list-help
    "\C-ns" gnus-mailing-list-subscribe
    "\C-nu" gnus-mailing-list-unsubscribe
    "\C-np" gnus-mailing-list-post
    "\C-no" gnus-mailing-list-owner
    "\C-na" gnus-mailing-list-archive
    ))

(defun gnus-mailing-list-make-menu-bar ()
  (unless (boundp 'gnus-mailing-list-menu)
    (easy-menu-define
     gnus-mailing-list-menu gnus-mailing-list-mode-map ""
     '("Mailing-Lists"
       ["Get help" gnus-mailing-list-help t]
       ["Subscribe" gnus-mailing-list-subscribe t]
       ["Unsubscribe" gnus-mailing-list-unsubscribe t]
       ["Post a message" gnus-mailing-list-post t]
       ["Mail to owner" gnus-mailing-list-owner t]
       ["Browse archive" gnus-mailing-list-archive t]))))

;;;###autoload
(defun turn-on-gnus-mailing-list-mode ()
  (when (gnus-group-get-parameter gnus-newsgroup-name 'to-list)
    (gnus-mailing-list-mode 1)))

;;;###autoload
(defun gnus-mailing-list-mode (&optional arg)
  "Minor mode for providing mailing-list commands.

\\{gnus-mailing-list-mode-map}"
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (when (set (make-local-variable 'gnus-mailing-list-mode)
	       (if (null arg) (not gnus-mailing-list-mode)
		 (> (prefix-numeric-value arg) 0)))
      ;; Set up the menu.
      (when (gnus-visual-p 'mailing-list-menu 'menu)
	(gnus-mailing-list-make-menu-bar))
      (gnus-add-minor-mode 'gnus-mailing-list-mode " Mailing-List" gnus-mailing-list-mode-map)
      (gnus-run-hooks 'gnus-mailing-list-mode-hook))))

;;; Commands

(defun gnus-mailing-list-help ()
  "Get help from mailing list server."
  (interactive)  
  (let ((list-help 
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-help"))))
    (cond (list-help (gnus-mailing-list-message list-help))
	  (t (gnus-message 1 "no list-help in this group")))))

(defun gnus-mailing-list-subscribe ()
  "Subscribe"
  (interactive)
  (let ((list-subscribe 
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-subscribe"))))
    (cond (list-subscribe (gnus-mailing-list-message list-subscribe))
	  (t (gnus-message 1 "no list-subscribe in this group")))))

(defun gnus-mailing-list-unsubscribe ()
  "Unsubscribe"
  (interactive)
  (let ((list-unsubscribe 
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-unsubscribe"))))
    (cond (list-unsubscribe (gnus-mailing-list-message list-unsubscribe))
	  (t (gnus-message 1 "no list-unsubscribe in this group")))))

(defun gnus-mailing-list-post ()
  "Post message (really useful ?)"
  (interactive)
  (let ((list-post 
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-post"))))
    (cond (list-post (gnus-mailing-list-message list-post))
	  (t (gnus-message 1 "no list-post in this group")))))

(defun gnus-mailing-list-owner ()
  "Mail to the owner"
  (interactive)
  (let ((list-owner 
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-owner"))))
    (cond (list-owner (gnus-mailing-list-message list-owner))
	  (t (gnus-message 1 "no list-owner in this group")))))

(defun gnus-mailing-list-archive ()
  "Browse archive"
  (interactive)
  (let ((list-archive 
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-archive"))))
    (cond (list-archive (gnus-mailing-list-message list-archive))
	  (t (gnus-message 1 "no list-owner in this group")))))

;;; Utility functions

(defun gnus-mailing-list-message (address)
  ""
  (let ((mailto  "")
	(to ())
	(subject "None")
	(body "")
	)
    (cond 
     ((string-match "<mailto:\\([^>]*\\)>" address)
      (let ((args (match-string 1 address)))
	(cond    				; with param
	 ((string-match "\\(.*\\)\\?\\(.*\\)" args)
	  (setq mailto (match-string 1 args))
	  (let ((param (match-string 2 args)))
	    (if (string-match "subject=\\([^&]*\\)" param)
		(setq subject (match-string 1 param)))
	    (if (string-match "body=\\([^&]*\\)" param)
		(setq body (match-string 1 param)))
	    (if (string-match "to=\\([^&]*\\)" param)
		(push (match-string 1 param) to))
	    ))	 
	 (t (setq mailto args)))))			; without param
     
     ; other case <http://... to be done.
     (t nil))
    (gnus-setup-message 'message (message-mail mailto subject))
    (insert body)
    ))

(provide 'gnus-ml)

;;; gnus-ml.el ends here
