;;; For Handling W32 Scroll Bar
;;; Created by H.Miyashita,
;;; modifying Emacs19.34 scroll-bar.el and term/w32-win.el
;;; IntelliMouse Support 97/3/29 by H.Miyashita

(defsubst scroll-bar-scale (num-denom whole)
  "Given a pair (NUM . DENOM) and WHOLE, return (/ (* NUM WHOLE) DENOM).
This is handy for scaling a position on a scroll bar into real units,
like buffer positions.  If SCROLL-BAR-POS is the (PORTION . WHOLE) pair
from a scroll bar event, then (scroll-bar-scale SCROLL-BAR-POS
\(buffer-size)) is the position in the current buffer corresponding to
that scroll bar position."
  ;; We multiply before we divide to maintain precision.
  ;; We use floating point because the product of a large buffer size
  ;; with a large scroll bar portion can easily overflow a lisp int.
  (truncate (/ (* (float (car num-denom)) whole) (cdr num-denom))))

(defsubst w32-scroll-event-handle-p (event)
  (and (listp event)
       (eq (nth 4 (event-start event)) 'handle)))

(defun scroll-bar-drag-position (portion-whole)
  "Calculate new window start for drag event."
  (save-excursion
    (goto-char (+ (point-min)
		  (scroll-bar-scale portion-whole
				    (- (point-max) (point-min)))))
    (beginning-of-line)
    (point)))

;;; for normal dragging
(defun w32-scroll-bar-drag (event)
  "Scroll the window by dragging the scroll bar slider.
If you click outside the slider, the window scrolls to bring the slider there."
  (interactive "e")
  (let* (done
	 start-position
	 portion-whole
	 (echo-keystrokes 0)
	 (end-position (event-end event))
	 (window (nth 0 end-position))
	 (before-scroll))
    (with-current-buffer (window-buffer window)
      (setq before-scroll point-before-scroll))
    (save-selected-window
      (select-window window)
      (setq before-scroll
	    (or before-scroll (point))))
    (save-excursion
      (set-buffer (window-buffer window))
      (while (not done)
	(setq event (read-event))
	(cond ((and (w32-scroll-event-handle-p event)
		    (setq start-position (nth 1 event))
		    (eq window (car start-position)))
	       (goto-char (+ (point-min)
			     (scroll-bar-scale
			      (nth 2 start-position)
			      (- (point-max) (point-min)))))
	       (back-to-previous-visible-line-start window)
	       (set-window-start
		window
		(car
		 (compute-motion (line-beginning-position)
				 '(0 . 0)
				 (point-max)
				 (cons 0
				       (nth 2
					    (compute-motion 
					     (line-beginning-position)
					     '(0 . 0)
					     (point)
					     (cons (window-width)
						   (window-height))
					     (window-width)
					     (cons (window-hscroll) 0)
					     window)))
				 (window-width)
				 (cons (window-hscroll) 0)
				 window)))
	       (goto-char
		(car
		 (compute-motion
		  (window-start window)
		  '(0 . 0)
		  (point-max)
		  (cons 0 (/ (window-height) 2))
		  (window-width)
		  (cons (window-hscroll) 0)
		  window))))
	       (t
	       ;; Exit when we get the drag event; store unread-command-events
	       (setq unread-command-events
		     (append unread-command-events (list event))
		     done t)))))
    (sit-for 0)
    (with-current-buffer (window-buffer window)
      (setq point-before-scroll before-scroll))))

;;; This is adequate for pointwise scroll.
(defun scroll-bar-maybe-set-window-start (event)
  "Set the window start according to where the scroll bar is dragged.
Only change window start if the new start is substantially different.
EVENT should be a scroll bar click or drag event."
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (portion-whole (nth 2 end-position))
	 (next-portion-whole (cons (1+ (car portion-whole))
				   (cdr portion-whole)))
	 portion-start
	 next-portion-start
	 (current-start (window-start window)))
    (save-excursion
      (set-buffer (window-buffer window))
      (setq portion-start (scroll-bar-drag-position portion-whole))
      (setq next-portion-start (max
				(scroll-bar-drag-position next-portion-whole)
				(1+ portion-start)))
      (if (or (> current-start next-portion-start)
	      (< current-start portion-start))
	  (set-window-start window portion-start)
	;; Always set window start, to ensure scroll bar position is updated.
	(set-window-start window current-start)))))

; 96.10.7 Created by himi
(defun scroll-but-point (line)
  "Neglecting point, scroll current window. If the point is out of the window, the point is moved to the center of the window."
  (save-excursion
    (goto-char (window-start))
    (vertical-motion line)
    (set-window-start (selected-window) (point))))

(defun w32-handle-scroll-bar-event (event)
  "Handle W32 scroll bar events to do normal Window style scrolling."
  (interactive "e")
  (let ((old-window (selected-window)))
    (unwind-protect
	(let* ((position (event-start event))
	       (window (nth 0 position))
	       (portion-whole (nth 2 position))
	       (bar-part (nth 4 position))
	       (window-scroll-step (- (window-height window) 
				      next-screen-context-lines)))
	  (if (<= window-scroll-step 0)
	      (setq window-scroll-step (window-height window)))
;	  (save-excursion
	    (select-window window)
	    (cond
	     ((eq bar-part 'up)
	      (scroll-but-point -1))
	     ((eq bar-part 'above-handle)
	      (scroll-but-point (- window-scroll-step)))
	     ((eq bar-part 'handle)
	      (w32-scroll-bar-drag event))
	     ((eq bar-part 'below-handle)
	      (scroll-but-point window-scroll-step))
	     ((eq bar-part 'down)
	      (scroll-but-point 1))
	     ))
;)
      (select-window old-window))))

(defun w32-handle-mouse-wheel-event (event)
  "Handle W32 scroll bar events to do normal Window style scrolling."
  (interactive "e")
  (let ((old-window (selected-window)))
    (unwind-protect
	(let* ((position (event-start event))
	       (window (nth 0 position))
	       (lines
		(w32-get-mouse-wheel-scroll-lines
		 (nth 4 position)))
	       (window-scroll-step (- (window-height window) 
				      next-screen-context-lines)))
	  (if (not lines)
	      (if (< (nth 4 position) 0)
		  (setq lines 3)
		(setq lines -3)))
	  (if (<= window-scroll-step 0)
	      (setq window-scroll-step (window-height window)))
	  (cond 
	   ((eq lines 'above-handle)
	    (setq lines (- window-scroll-step)))
	   ((eq lines 'below-handle)
	    (setq lines window-scroll-step)))
	  (if (windowp window)
	      (progn
		(select-window window)
		(scroll-but-point lines))))
	    (select-window old-window))))

(defun w32-handle-mouse-wheel-mode-line (event)
  "Handle W32 scroll bar events to do normal Window style scrolling."
  (interactive "e")
  (let ((old-window (selected-window)))
    (unwind-protect
	(let* ((position (event-start event))
	       (window (nth 0 position))
	       (mousepos (mouse-position))
	       (mouse-frame (car mousepos))
	       (mouse-x (car (cdr mousepos)))
	       (mouse-y (cdr (cdr mousepos)))
	       (delta (if (> (nth 4 position) 0) -1 1))
	       (mouse-delta delta)
	       minibuffer params next-window)
	  (setq params (frame-parameters))
	  (if (and (not (setq minibuffer (cdr (assq 'minibuffer params))))
		   (one-window-p t))
	      (error "Attempt to resize sole window"))
	  (if (and minibuffer
		   (= (nth 1 (window-edges minibuffer))
		      (nth 3 (window-edges window))))
	      (progn
		(setq window minibuffer)
		(setq delta (- delta)))
	    (setq next-window
		  (window-at
		   (car (window-edges window))
		   (1+ (nth 3 (window-edges window))))))
	  (if (and (windowp window)
		   (>= (+ (window-height window) delta)
		       (if (eq window minibuffer) 1 window-min-height))
		   (or (null next-window)
		       (>= (- (window-height next-window) delta)
			   (if (eq next-window minibuffer) 1 window-min-height))))
	      (progn
		(select-window window)
		(enlarge-window delta)
		(set-mouse-position mouse-frame mouse-x (+ mouse-y mouse-delta)))))
	  (select-window old-window))))

(defun scroll-bar-mode (flag)
  "Toggle display of vertical scroll bars on each frame.
This command applies to all frames that exist and frames to be
created in the future.
With a numeric argument, if the argument is negative,
turn off scroll bars; otherwise, turn on scroll bars."
  (interactive "P")
  (if flag (setq flag (prefix-numeric-value flag)))

  ;; Tweedle the variable according to the argument.
  (set-scroll-bar-mode (if (null flag) (not scroll-bar-mode)
			 (and (or (not (numberp flag)) (>= flag 0))
			      (if (eq system-type 'windows-nt) 'right 'left)))))

(defun toggle-scroll-bar (arg)
  "Toggle whether or not the selected frame has vertical scroll bars.
With arg, turn vertical scroll bars on if and only if arg is positive.
The variable `scroll-bar-mode' controls which side the scroll bars are on
when they are turned on; if it is nil, they go on the left."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (cdr (assq 'vertical-scroll-bars
			   (frame-parameters (selected-frame))))
		-1 1)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'vertical-scroll-bars
				       (if (> arg 0)
					   (or scroll-bar-mode 'left))))))

(defun set-scroll-bar-mode-1 (ignore value)
  (set-scroll-bar-mode value))

(defun set-scroll-bar-mode (value)
  "Set `scroll-bar-mode' to VALUE and put the new value into effect."
  (setq scroll-bar-mode value)

    ;; Apply it to default-frame-alist.
    (let ((parameter (assq 'vertical-scroll-bars default-frame-alist)))
      (if (consp parameter)
	  (setcdr parameter scroll-bar-mode)
	(setq default-frame-alist
	      (cons (cons 'vertical-scroll-bars scroll-bar-mode)
		    default-frame-alist)))

    ;; Apply it to existing frames.
    (let ((frames (frame-list)))
      (while frames
	(modify-frame-parameters
	 (car frames)
	 (list (cons 'vertical-scroll-bars scroll-bar-mode)))
	(setq frames (cdr frames))))))

(defcustom scroll-bar-mode 'right
  "*Specify whether to have vertical scroll bars, and on which side.
Possible values are nil (no scroll bars), `left' (scroll bars on left)
and `right' (scroll bars on right).
When you set the variable in a Lisp program, it takes effect for new frames,
and for existing frames when `toggle-scroll-bar' is used.
When you set this with the customization buffer,
it takes effect immediately for all frames."
  :type '(choice (const :tag "none (nil)")
		 (const left)
		 (const right))
  :group 'frames
  :set 'set-scroll-bar-mode-1)

(global-set-key [vertical-scroll-bar mouse-1] 'w32-handle-scroll-bar-event)
(global-set-key [mouse-wheel1] 'w32-handle-mouse-wheel-event)
(global-set-key [nil mouse-wheel1] 'ignore)
(global-set-key [mode-line mouse-wheel1] 
		'w32-handle-mouse-wheel-mode-line)

(provide 'scroll-bar)
