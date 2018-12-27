;;;;; mw32mci.el ---- For MCI (Multimedia Control Interface).
;;
;;   Author MIYOSHI Masanori
;;
;;;;;
(defvar mw32-mci-notify-callback-alist nil)


(defun mw32-mci-add-notify-callback (device-id callback-func
					       &optional callback-arg)
  "Add a callback function when a notify event rises."
  (setq mw32-mci-notify-callback-alist
	(cons (list device-id callback-func callback-arg)
	      mw32-mci-notify-callback-alist)))


(defun mw32-mci-remove-notify-callback (device-id callback-func)
  "Remove a callback function."
  (let ((elem (assoc device-id mw32-mci-notify-callback-alist)))
    (while elem
      (let ((func (nth 1 elem)))
	(when (eq func callback-func)
	  (setq mw32-mci-notify-callback-alist
		(delq elem mw32-mci-notify-callback-alist)))
	(setq elem (assoc device-id mw32-mci-notify-callback-alist))))))


(defun mw32-mci-notify-event-handler (device-id exit-state)
  (let ((elem (assoc device-id mw32-mci-notify-callback-alist))
	target-alist)
    (while elem
      (setq target-alist (cons elem target-alist))
      (setq mw32-mci-notify-callback-alist
	    (delq elem mw32-mci-notify-callback-alist))
      (setq elem (assoc device-id mw32-mci-notify-callback-alist)))
    (setq elem (car target-alist))
    (while elem
      (let ((func (nth 1 elem))
	    (arg (nth 2 elem)))
	(funcall func device-id exit-state arg)
	(setq target-alist (cdr target-alist))
	(setq elem (car target-alist))))))


(defun mw32-mci-handle-event (event)
  (interactive "e")
  (let ((event-type (nth 1 event))
	(device-id (nth 2 event))
	(event-arg (nth 3 event)))
    (cond
     ((eq event-type 'mw32-mci-notify)
      (mw32-mci-notify-event-handler device-id event-arg)))))


;; when mw32-mci event rises, handle it with mw32-mci-handle-event().
(define-key special-event-map [mw32-mci] 'mw32-mci-handle-event)


(defun mw32-mci-notify-func (device-id exit-state &optional arg)
  (cond
   ((eq exit-state 'mw32-mci-notify-successful)
    (let (device-name tmp-file)
      (cond
       ((stringp arg)
	(setq device-name arg))
       ((listp arg)
	(setq device-name (nth 0 arg))
	(setq tmp-file (nth 1 arg))))
      (when (stringp device-name)
	(mw32-mci-send-string (format "close %s" device-name)))
      (when (and (stringp tmp-file)
		 (file-writable-p tmp-file))
	(delete-file tmp-file))))
   (t
    (error "abnormal termination"))))


;; Parse sound specification SOUND, and fill ATTRS with what is
;; found.  Value is non-zero if SOUND Is a valid sound specification.
;; A valid sound specification is a list starting with the symbol
;; `sound'.  The rest of the list is a property list which may
;; contain the following key/value pairs:

;;    - `:file FILE'

;;    FILE is the sound file to play.  If it isn't an absolute name,
;;    it's searched under `data-directory'.

;;    - `:data DATA'

;;    DATA is a string containing sound data.  Either :file or :data
;;    may be present, but not both.

;;    - `:device DEVICE'

;;    DEVICE is the name of the device to play on, e.g. "/dev/dsp2".
;;    If not specified, a default device is used.

;;    - `:volume VOL'

;;    VOL must be an integer in the range [0, 100], or a float in the
;;    range [0, 1].

(defun parse-sound (sound)
  (catch 'invalid
    (progn
      ;; SOUND must be a list starting with the symbol `sound'.
      (unless (and (consp sound)
		   (eq (car sound) 'sound))
	(throw 'invalid nil))
      (setq sound (cdr sound))
      (let ((file (plist-get sound :file))
	    (data (plist-get sound :data))
	    (device (plist-get sound :device))
	    (volume (plist-get sound :volume)))

	;; File name or data must be specified.
	(unless (or (stringp file)
		    (stringp data))
	  (throw 'invalid nil))
	  
	;;  Volume must be in the range 0..100 or unspecified.
	(when volume
	  (cond
	   ((integerp volume)
	    (if (or (< volume  0)
		    (> volume 100))
		(throw 'invalid nil)))
	   ((floatp volume)
	    (if (or (< volume  0.0)
		    (> volume 1.0))
		(throw 'invalid nil)))
	   (t
	    (throw 'invalid nil))))
	     
	;; Device must be a string or unspecified.
	(when (and device
		   (not (stringp device)))
	  (throw 'invalid nil))))
    t))


(defun play-sound (sound)
  "Play sound SOUND.\n\
SOUND is a list of the form `(sound KEYWORD VALUE...)'.\n\
The following keywords are recognized:\n\
\n\
  :file FILE.- read sound data from FILE.  If FILE isn't an\n\
absolute file name, it is searched in `data-directory'.\n\
\n\
  :data DATA - read sound data from string DATA.\n\
\n\
Exactly one of :file or :data must be present.\n\
\n\
  :volume VOL - set volume to VOL.  VOL must an integer in the\n\
range 0..100 or a float in the range 0..1.0.  If not specified,\n\
don't change the volume setting of the sound device.\n\
\n\
  :device DEVICE - play sound on DEVICE.  If not specified,\n\
a system-dependent default device name is used."
  ;; Parse the sound specification.  Give up if it is invalid.
  (when (null (parse-sound sound))
    (error "Invalid sound specification"))

  (setq sound (cdr sound))
  (let ((file (plist-get sound :file))
	(data (plist-get sound :data))
	(device (plist-get sound :device))
	(volume (plist-get sound :volume))
	(device-name (make-temp-name "waveaudio"))
	tmp-file arg)
    (if (stringp file)
	(progn
	  (setq file (expand-file-name file data-directory))
	  (setq arg device-name))
      (setq tmp-file (make-temp-file "waveaudio"))
      (setq file (concat tmp-file ".wav"))
      (rename-file tmp-file file)
      (if (file-writable-p file)
	  (with-temp-buffer
	    (let ((coding-system-for-write 'binary))
	      (set-buffer-multibyte nil)
	      (princ data (current-buffer))
	      (write-file file)))
	(error (format "cannot open %s" file)))
      (setq arg (list device-name file)))
    (let (result device-id)
      (setq result
	    (mw32-mci-send-string (format "open \"%s\" alias %s"
					  file device-name)))
      (when (numberp result)
	(error (format "cannot open %s!" file)))
      (setq device-id (string-to-number result))
      (mw32-mci-send-string (format "play %s notify" device-name))
      (mw32-mci-add-notify-callback device-id 'mw32-mci-notify-func arg))
    ;; :volume and :device properties are ignored.
    )
  t)