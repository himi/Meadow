;;; mw32reg.el --- Registry operation API.

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; This file is part of MEADOW

;; MEADOW is Multilingual enhancement to GNU Emacs ADvantage Over Windows.
;; MEADOW is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; MEADOW is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with MEADOW; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun mw32-registry-value-to-integer (value)
  "Fit big value DATA into emacs integer.
DATA should be cons of value data and value type of registry which is
like returned value from `mw32-registry-get'.
'registry-dword, 'regstry-dword-big-endian or 'registry-qword can be
converted. Other type of data cause error.
For registry-dword-big-endian, endian conversion is considered here."
  (let ((data (car value))
	(type (cdr value)))
    (cond
     ((eq type 'registry-dword)
      (mw32-registry-words-to-integer (cdr data) (car data)))
     ((eq type 'registry-dword-big-endian)
      ;; with endian conversion
      (mw32-registry-words-to-integer
       (+ (lsh (car data) -8) (lsh (car data) 8)) ; lower word
       (+ (lsh (cdr data) -8) (lsh (cdr data) 8)))) ; higher word
     ((eq type 'registry-qword)
      (mw32-registry-words-to-integer
       (nth 3 data) (nth 2 data) (nth 1 data) (nth 0 data)))
     (t
      (error "Registry data type is not fit for emacs integer")))))

(defun mw32-registry-get-integer (key name)
  "Get integer data in registry KEY from value NAME.
If NAME is nil, value of KEY is retrieved.
Only REG_DWORD, REG_DWORD_BIG_ENDIAN or REG_QWORD type of registry
data is accepted. Others cause error."
  (mw32-registry-value-to-integer (mw32-registry-get key name)))

(defun mw32-registry-get-string (key name &optional coding)
  "Get string data in registry KEY from value NAME with decoding.
If NAME is nil, value of KEY is retrieved.
Returns string which is decoded by w32-system-coding-system or
specified CODING."
  (let ((data (mw32-registry-get key name)))
    (when data
      (let ((type (cdr data))
	    (val (car data)))
      (if (memq type '(registry-sz registry-expand-sz))
	  (decode-coding-string (car data) 
				(or coding w32-system-coding-system))
	(error "Registry data type is not fit for string"))))))

(defun mw32-registry-get-string-list (key name)
  "Get multiple string data in registry KEY from value NAME with decoding.
If NAME is nil, value of KEY is retrieved. 
Returns list of string decoded by `w32-system-coding-system' or
specified CODING."
  (let ((data (mw32-registry-get key name)))
    (when data
      (let ((type (cdr data))
	    (val (car data)))
	(if (eq type 'registry-multi-sz)
	    (mapcar (lambda (str) 
		      (decode-coding-string str (or coding
						    w32-system-coding-system)))
		    val)
	  (error "Registry data type is not fit for string list"))))))


;;
;; this is test code for mw32-registry-words-to-integer
;;
(let ((cnt 0))
  (dolist (test '(((0 0) . 0)
		  ((1 0) . 1)
		  ((?\x7fff 0) . ?\x7fff)
		  ((?\xffff ?\x7ff) . ?\x7ffffff)
		  ((0 ?\x800) . overflow-error)
		  ((?\xffff ?\x800) . overflow-error)
		  ((?\xffff ?\x7ff 1) . wrong-type-argument)
		  ((?\xffff ?\x7ff 1 1) . overflow-error)
		  ((?\xffff ?\x7ff 0 0) . ?\x7ffffff)
		  ((?\xffff ?\xffff ?\xffff ?\xffff) . ?\xfffffff)
		  ((?\xffff 0) . ?\xffff)))
    (let (result)
      (setq cnt (1+ cnt))
      (condition-case e
	  (progn
	    (setq result (apply 'mw32-registry-words-to-integer (car test)))
	    (if (not (eq (cdr test) result))
		(error "Test #%d fail, result=%s, expects=%s" 
		       cnt result (cdr test))))
	(overflow-error
	 (if (not (eq (cdr test) 'overflow-error))
	     (error "Unexpected overflow")))
	(error
	 (if (not (eq (cdr test) (car e)))
	     (error "why error? %s" e)))
	)))
  (message "mw32reg.el: %d tests passed" cnt))


;; mw32reg.el ends here
