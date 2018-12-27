;;;;; meadow.el ---- For Meadow.
;;
;;   Author H.Miyashita
;;
;;;;;

(defgroup Meadow nil
  "Meadow"
  :group 'emacs)

(defvar mw32-last-selection nil
  "It is stored the last data from Emacs.")

;;;
;;; image function
;;;

(defun display-images-p (&optional display)
  "Return non-nil if DISPLAY can display images.

DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display).

This function is overridden by Meadow."
  (and (display-graphic-p display)
       (fboundp 'image-mask-p)
       (fboundp 'image-size)))

(add-hook
 'before-init-hook
 (lambda ()
   ;; BMP support
   (let ((image-type (if (memq 'BMP image-types)
			 'BMP		; ImageMagick image decoder
		       'bmp)))		; built-in image decoder
     (require 'image)
     (require 'image-file)

     (or (rassq image-type image-type-regexps)
	 (setq image-type-regexps
	       (cons (cons "\\`BM" image-type) image-type-regexps)))
     (or (member "bmp" image-file-name-extensions)
	 (setq image-file-name-extensions
	       (cons "bmp" image-file-name-extensions))))

   ;; append extra extensions
   (mapcar
    (lambda (type)
      (unless (or (member type '("txt" "shtml" "html" "htm"))
		  (member type image-file-name-extensions))
	(setq image-file-name-extensions
	      (cons type image-file-name-extensions))))
    (mw32-get-image-magick-extensions))))

;;;
;;; overwrite appearances
;;;

(set-face-background 'modeline "LightBlue")

;;;
;;; overwrite splash handling
;;;

(defvar mw32-splash-masked-p nil
  "If non-nil, show a splash screen of Meadow with a heuristic mask.")

(defun use-fancy-splash-screens-p ()
  "Return t if fancy splash screens should be used."
  (when (image-type-available-p 'bmp)
    (let* ((img (create-image (or fancy-splash-image
				  "meadow.bmp") 'bmp))
	   (image-height (and img (cdr (image-size img))))
	   (window-height (1- (window-height (selected-window)))))
      (> window-height (+ image-height 15)))))

(defun fancy-splash-head ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (or fancy-splash-image
			 "meadow.bmp"))
	 (img (create-image image-file
			    (if (memq 'BMP image-types) 'BMP 'bmp)
			    nil :heuristic-mask mw32-splash-masked-p))
	 (image-width (and img (car (image-size img))))
	 (window-width (window-width (selected-window))))
    (when img
      (when (> window-width image-width)
	;; Center the image in the window.
	(let ((pos (/ (- window-width image-width) 2)))
	  (insert (propertize " " 'display `(space :align-to ,pos))))

	;; Insert the image with a help-echo and a keymap.
	(let ((map (make-sparse-keymap))
	      (help-echo "mouse-2: browse http://www.meadowy.org/"))
	  (define-key map [mouse-2]
	    (lambda ()
	      (interactive)
	      (browse-url "http://www.meadowy.org/")
	      (throw 'exit nil)))
	  (define-key map [down-mouse-2] 'ignore)
	  (define-key map [up-mouse-2] 'ignore)
	  (insert-image img (propertize "xxx" 'help-echo help-echo
					'keymap map)))
	(insert "\n"))))
  (insert "Meadow is based on GNU Emacs.\n")
  (if (eq system-type 'gnu/linux)
      (fancy-splash-insert
       :face '(variable-pitch :foreground "red")
       "GNU Emacs is one component of a Linux-based GNU system.")
    (fancy-splash-insert
     :face '(variable-pitch :foreground "red")
     "GNU Emacs is one component of the GNU operating system."))
  (insert "\n"))

(defun fancy-splash-tail ()
  "Insert the tail part of the splash screen into the current buffer."
  (let ((fg (if (eq (frame-parameter nil 'background-mode) 'dark)
		"cyan" "darkblue")))
    (fancy-splash-insert :face `(variable-pitch :foreground ,fg)
			 "\nThis is "
			 (Meadow-version)
			 "\n based on "
			 (emacs-version)
			 "\n"
			 :face '(variable-pitch :height 0.5)
			 "Copyright (C) 2001 Free Software Foundation, Inc.\n"
			 "Copyright (C) 1995-2001 MIYASHITA Hisashi\n"
			 "Copyright (C) 2002, 2003, 2004, 2005 The Meadow Team")))

;;;
;;; Meadow MW32-IME API.
;;;

(defvar mw32-ime-on-hook nil
  "Functions to eval when IME is turned on at least.\n\
Even if IME state is not changed, these functiona are maybe called.")
(defvar mw32-ime-off-hook nil
  "Functions to eval when IME is turned off at least.\n\
Even if IME state is not changed, these functiona are maybe called.")
(defvar mw32-ime-buffer-switch-p t
  "If this variable is nil, IME control when buffer is switched is disabled.")
(defvar mw32-ime-state nil
  "This shows IME state of the buffer.(buffer local variable)")
(make-variable-buffer-local 'mw32-ime-state)
(put 'mw32-ime-state 'permanent-local t)
(defvar mw32-ime-show-mode-line t
  "When t, mode line indicates IME state.")
(defvar mw32-ime-mode-line-state-indicator "[O]"
  "This is shown at the mode line. It is regarded as state of ime.")
(make-variable-buffer-local 'mw32-ime-mode-line-state-indicator)
(put 'mw32-ime-mode-line-state-indicator 'permanent-local t)
(defvar mw32-ime-mode-line-state-indicator-list '("-" "[|]" "[O]")
  "List of IME state indicator string.")
(defvar mw32-ime-mode-line-format-original nil
  "Original mode line format.")

(setq search-highlight t)

;; isearch ime keymap 

(setq isearch-ime-keymap (copy-keymap minibuffer-local-map))
(nconc isearch-ime-keymap (list (make-vector 26 'isearch-exit-win32ime)))
(define-key isearch-ime-keymap [compend] 'exit-minibuffer)
(define-key isearch-ime-keymap [kanji] 'isearch-exit-win32ime)
(define-key isearch-ime-keymap "\C-s" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-r" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\177" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-g" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-q" 'isearch-command-win32ime)

(define-key isearch-ime-keymap "\r" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-j" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\t" 'isearch-command-win32ime)
(define-key isearch-ime-keymap " " 'isearch-command-win32ime)
    
(define-key isearch-ime-keymap "\C-w" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-y" 'isearch-command-win32ime)

(let ((meta-map (make-sparse-keymap)))
  (define-key isearch-ime-keymap (char-to-string meta-prefix-char) meta-map)
  (define-key isearch-ime-keymap [escape] meta-map))
(define-key isearch-ime-keymap
  (vector meta-prefix-char t)  'isearch-exit-win32ime)
(define-key isearch-ime-keymap "\M-n" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\M-p" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\M-y" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\M-\t" 'isearch-command-win32ime)

;;;
;;; Emulation functions.
;;;

;;
;; Section: General definitions
;;

(defvar w32-fiber-program-name "fiber.exe")
(defvar w32-fiber-process-name "*fiber*")

(defun wildcard-to-regexp (pattern)
  (let ((i 0)
	(len (length pattern))
	(quotestr "")
	(result "")
	char
	result)
    (while (< i len)
      (setq char (aref pattern i)
	    i (1+ i))
      (cond ((= char ?*)
	     (setq result (concat result (regexp-quote quotestr) ".*")
		   quotestr ""))
	    ((= char ??)
	     (setq result (concat result (regexp-quote quotestr) ".")
		   quotestr ""))
	    (t
	     (setq quotestr (concat quotestr (char-to-string char))))))
    (concat "\\`" result (regexp-quote quotestr) "\\'")))

;;
;; Section: X selection
;;

(defalias 'x-selection-exists-p 'w32-clipboard-data-exist-p)

;;
;; Section: Font
;;

(defun w32-list-fonts (pattern &optional face frame max)
  (setq pattern (wildcard-to-regexp pattern))
  (if (null max) (setq max 2000))
  (let ((curfl (w32-font-list))
	curfs
	result)
  (while (and (> max 0)
	      (setq curfs (car curfl)))
      (if (string-match pattern curfs)
	  (setq result (cons curfs result)
		max (1- max)))
      (setq curfl (cdr curfl)))
  result))

(defalias 'x-list-fonts 'w32-list-fonts)

;;
;; Section: X geometry (-g option)
;;

(defun x-parse-geometry (str)
  (let* ((size-regexp "\\([+\\-]?[0-9]+\\)[xX]\\([+\\-]?[0-9]+\\)")
	 (location-regexp "\\([+\\-][+\\-]?[0-9]+\\)")
	 (func (lambda (x)
		 (cond ((= (aref x 0) ?+)
			(cons '+ (string-to-number
				  (substring x 1))))
		       ((= (aref x 0) ?-)
			(cons '- (string-to-number
				  (substring x 1))))
		       (t nil))))
	 location-x location-y size-x size-y result)
    (if (string-match "^=?" str)
	(setq str (substring str (match-end 0))))
    (if (string-match
	 (concat "^" size-regexp)
	 str)
	(setq size-x (string-to-number (match-string 1 str))
	      size-y (string-to-number (match-string 2 str))
	      str (substring str (match-end 0))))
    (if (string-match 
	 (concat "^" location-regexp location-regexp)
	 str)
	(setq location-x (match-string 1 str)
	      location-y (match-string 2 str)
	      location-x (funcall func location-x)
	      location-y (funcall func location-y)))
    (if size-x
	(setq result (cons (cons 'width size-x) result)))
    (if size-y
	(setq result (cons (cons 'height size-y) result)))
    (cond ((eq (car location-x) '+)
	   (setq result
		 (cons (cons 'left (cdr location-x))
		       result)))
	  ((eq (car location-x) '-)
	   (setq result
		 (cons (cons 'right (cdr location-x))
		       result))))
    (cond ((eq (car location-y) '+)
	   (setq result
		 (cons (cons 'top (cdr location-y))
		       result)))
	  ((eq (car location-y) '-)
	   (setq result
		 (cons (cons 'bottom (cdr location-y))
		       result))))
    result))

;;
;; Section: X file dialog
;;

(defalias 'x-file-dialog 'mw32-file-dialog)

;;; Section: focus frame

(defalias 'w32-focus-frame 'x-focus-frame)

;;
;; Section: Shell execute
;;

(defun w32-shell-execute (operation document &optional parameters show-flag)
  (if (and show-flag
	  (not (numberp show-flag)))
      (error "show-flag must be number or nil:%S" show-flag))
  (let ((coding-system-for-write w32-system-coding-system)
	(args (append
	       (list document)
	       (list "-b" operation)
	       (list "-d" default-directory)
	       (if parameters
		   (list "-p" parameters))
	       (if show-flag
		   (list "-n" (number-to-string show-flag))))))
    (apply 'call-process w32-fiber-program-name nil 0 nil
	   args)))

;;
;; Section: IME
;;

;; This is temporal solution.  In the future, we will prepare
;; dynamic configuration.
(defvar mw32-ime-coding-system-language-environment-alist
  '(("Japanese" . japanese-shift-jis)
    ("Chinese-GB" . chinese-iso-8bit)
    ("Chinese-BIG5" . chinese-big5)
    ("Korean" . korean-iso-8bit)))

;;
;; IME state indicator
;;
(global-set-key [kanji] 'ignore)
(global-set-key [compend] 'ignore)

(defun wrap-function-to-control-ime
  (function interactive-p interactive-arg &optional suffix)
  "Wrap FUNCTION, and IME control is enabled when FUNCTION is called. \n\
An original function is saved to FUNCTION-SUFFIX when suffix is string. \n\
If SUFFIX is nil, \"-original\" is added. "
  (let ((original-function
	 (intern (concat (symbol-name function)
			 (if suffix suffix "-original")))))
    (cond ((not (fboundp original-function))
	   (fset original-function
		 (symbol-function function))
	   (fset function
		 (list 
		  'lambda '(&rest arguments)
		  (if interactive-p
		      (list 'interactive interactive-arg))
		  (` (cond ((fep-get-mode)
			    (fep-force-off)
			    (unwind-protect
				(apply '(, original-function) arguments)
			      (if mw32-ime-state
				  (fep-force-on))))
			   (t
			    (apply '(, original-function)
				   arguments))))))))))

(defvar mw32-ime-toroku-region-yomigana nil
  "* if this variable is string, toroku-region regard this value as yomigana.")

(defun mw32-ime-toroku-region (begin end)
  (interactive "r")
  (let ((string (buffer-substring begin end))
	(mw32-ime-buffer-switch-p nil)
	(reading mw32-ime-toroku-region-yomigana))
    (save-excursion
      (set-buffer (window-buffer (minibuffer-window)))
      (fep-force-on nil)
      (mw32-ime-toggle)
      (w32-set-ime-mode 'hiragana)
      (if (not (stringp reading))
	  (setq reading (read-from-minibuffer
			 (format "Input reading of \"%s\":" string))))
      (w32-ime-register-word-dialog reading string))
    (if (not mw32-ime-state) (fep-force-off nil))))

;; for IME management system.

(defun mw32-ime-set-selected-window-buffer-hook (oldbuf newwin newbuf)
  (save-excursion
    (set-buffer newbuf)
    (if mw32-ime-buffer-switch-p
	(if (not (eq (fep-get-mode) mw32-ime-state))
	    (cond
	     (mw32-ime-state
	      (fep-force-on nil)
	      (run-hooks 'mw32-ime-on-hook))
	     (t
	      (if (= (w32-ime-undetermined-string-length) 0)
		  (progn
		    (fep-force-off nil)
		    (run-hooks 'mw32-ime-off-hook)))))))))

(defun mw32-ime-select-window-hook (old new)
  (when mw32-ime-buffer-switch-p
    (with-current-buffer (window-buffer new)
      (when (eq new (minibuffer-window))
	(setq current-input-method nil)
	(setq mw32-ime-state nil))
      (unless (eq (fep-get-mode) mw32-ime-state)
	(cond
	 (mw32-ime-state
	  (fep-force-on nil)
	  (run-hooks 'mw32-ime-on-hook))
	 (t
	  (when (= (w32-ime-undetermined-string-length) 0)
	    (fep-force-off nil)
	    (run-hooks 'mw32-ime-off-hook))))))))

(defun mw32-ime-mode-line-update ()
  (cond (mw32-ime-show-mode-line
	 (if (window-minibuffer-p (selected-window))
;;; for minibuffer ...
	     nil
	   (setq mw32-ime-mode-line-state-indicator
		 (if mw32-ime-state
		     (nth 1 mw32-ime-mode-line-state-indicator-list)
		   (nth 2 mw32-ime-mode-line-state-indicator-list)))))
	(t
	 (setq mw32-ime-mode-line-state-indicator
	       (nth 0 mw32-ime-mode-line-state-indicator-list))))
  (force-mode-line-update))

(defun mw32-ime-init-mode-line-display ()
  (if (not (member 'mw32-ime-mode-line-state-indicator
		   mode-line-format))
      (progn
	(setq mw32-ime-mode-line-format-original
	      (default-value 'mode-line-format))
	(if (and (stringp (car mode-line-format))
		 (string= (car mode-line-format) "-"))
	    (setq-default mode-line-format
			  (cons ""
				(cons 'mw32-ime-mode-line-state-indicator
				      (cdr mode-line-format))))
	  (setq-default mode-line-format
			(cons ""
			      (cons 'mw32-ime-mode-line-state-indicator
				    mode-line-format))))
	(force-mode-line-update t))))
	
(defun mw32-ime-toggle ()
  (interactive)
  (let ((ime-state (fep-get-mode)))
    (if ime-state
	(run-hooks 'mw32-ime-on-hook)
      (run-hooks 'mw32-ime-off-hook))
    (if (not (eq ime-state mw32-ime-state))
	(progn
	  (setq mw32-ime-state ime-state)
	  (mw32-ime-mode-line-update)))))

(defun mw32-ime-initialize ()
  (cond ((and (eq system-type 'windows-nt)
	      (eq window-system 'w32)
	      (featurep 'meadow))
	 (let ((coding-system
		(assoc-ignore-case current-language-environment
		       mw32-ime-coding-system-language-environment-alist)))
	   (mw32-ime-init-mode-line-display)
	   (mw32-ime-mode-line-update)
	   (add-hook 'select-window-functions
		     'mw32-ime-select-window-hook)
	   (add-hook 'set-selected-window-buffer-functions
		     'mw32-ime-set-selected-window-buffer-hook)
	   (define-key global-map [kanji] 'mw32-ime-toggle)
	   (if coding-system
	       (set-keyboard-coding-system (cdr coding-system)))))))

(defun mw32-ime-uninitialize ()
  (cond ((and (eq system-type 'windows-nt)
	      (eq window-system 'w32)
	      (featurep 'meadow))
	 (setq-default mode-line-format
		       mw32-ime-mode-line-format-original)
	 (force-mode-line-update t)
	 (remove-hook 'select-window-functions
		      'mw32-ime-select-window-hook)
	 (remove-hook 'set-selected-window-buffer-functions
		      'mw32-ime-set-selected-window-buffer-hook)
	 (define-key global-map [kanji] 'ignore))))

(defun mw32-ime-state-switch (&optional arg)
  (if arg
      (progn
	(setq inactivate-current-input-method-function
	      'mw32-ime-state-switch)
	(run-hooks 'input-method-activate-hook)
	(setq describe-current-input-method-function nil)
	(fep-force-on t))
    (setq current-input-method nil)
    (run-hooks 'input-method-inactivate-hook)
    (setq describe-current-input-method-function nil)
    (fep-force-off t)
    ))

(register-input-method "MW32-IME" "Japanese" 'mw32-ime-state-switch ""
		       "MW32 System IME")


;;;
;;; Inspect Device Capability/Intrinsic Facilities
;;;                Emulation layer for functions of xfuns.c

(defsubst mw32-emulate-x-display-argument (display)
  (cond ((stringp display) nil)
	((framep display) display)
	((null display) display)
	(t
	 (error "%S must be STRING or FRAME" display))))

(defun x-display-pixel-width (&optional display)
  (mw32-get-device-capability
   'width
   (mw32-emulate-x-display-argument display)))
(defun x-display-pixel-height (&optional display)
  (mw32-get-device-capability
   'height
   (mw32-emulate-x-display-argument display)))
(defun x-display-planes (&optional display)
  (mw32-get-device-capability
   ;; Notice that the meaning of "PLANES" in X are different
   ;; from that in Windows.
   'color-bits
   (mw32-emulate-x-display-argument display)))
(defun x-display-mm-height (&optional display)
  (mw32-get-device-capability
   'height-in-mm
   (mw32-emulate-x-display-argument display)))
(defun x-display-mm-width (&optional display)
  (mw32-get-device-capability
   'width-in-mm
   (mw32-emulate-x-display-argument display)))
(defun x-display-visual-class (&optional display)
  (let ((c (x-display-planes display))
	(n (mw32-get-device-capability
	    'colors)))
    (cond ((eq n 'full) 'true-color)
	  ((= n 1) 'static-gray)
	  ((> c (log c n)) 'pseudo-color)
	  (t 'static-color))))

;; dummy vals.
(defun x-server-max-request-size (&optional display) 65535)
(defun x-server-vendor (&optional display) "MW32")
(defun x-server-version (&optional display) (list 11 0 1))
;; We should use multi-monitor APIs in the future.
(defun x-display-screens (&optional display) 1)
(defun x-display-backing-store (&optional display) 'not-useful)
(defun x-display-save-under (&optional display) nil)

(provide 'meadow)


