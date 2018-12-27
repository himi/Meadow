;;;;; mw32misc.el ---- For Multilingul Windows.
;;
;;   Author H.Miyashita
;;
;;;;;

(eval-when-compile
  (require 'regexp-opt))

(defvar install-lisp-directory-specific-to-emacs-version ""
  "Directory to store Emacs Lisp libraries specific to Emacs Version.")

(defvar install-lisp-directory-independent-of-emacs-version ""
  "Directory to store Emacs Lisp libraries independent of Emacs Version.")

(defvar w32-num-mouse-buttons 2
  "Number of mouse buttons.  This is for compatibility with NTEmacs.")

(defvar x-bitmap-file-path ""
  "This variable is for compatibility with X.
Don't touch this variable!")

(defun set-clipboard-coding-system (coding-system)
  "Set windows clipboard coding sytem. This coding system is used when
emacs read or write windows clipboard."
  (interactive "zClipboard-coding-system:")
  (check-coding-system coding-system)
  (setq w32-clipboard-coding-system coding-system))

(defun set-w32-system-coding-system (coding-system)
  "Set coding sytem used by windows.  "
  (interactive "zWindows-system-coding-system:")
  (check-coding-system coding-system)
  (setq w32-system-coding-system coding-system))

(fmakunbound 'font-menu-add-default)

(defun w32-generate-font-fontset-menu ()
  (let ((font-list (reverse (sort (w32-font-list) (function string<))))
	items)
    (setq items (mapcar (lambda (x) (list x x)) font-list))
    (list "Font menu"
	  (cons "Font"
		items))))

(defun set-cursor-type (type)
  "Set the text cursor type of the selected frame to TYPE.
When called interactively, prompt for the name of the cursor type to use.
The cursor type supports which `caret', `checkered-caret', `hairline-caret'
, `box' and `bar'.
To get the frame's current cursor type, use `frame-parameters'."
  (interactive "sCursor-Type: ")
  (when (stringp type)
    (setq type (intern type)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'cursor-type type))))

(defun set-cursor-height (height)
  "Set the caret cursor height of the selected frame to HEIGHT.
When called interactively, prompt for the height of the cursor to use.
The cursor height support `0 - 4' integer.
To get the frame's current cursor height, use `frame-parameters'."
  (interactive "nCursor-Height: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'cursor-height height))))

(defun mouse-set-font (&rest fonts)
  "Select an emacs font from a list of known good fonts and fontsets."
  (interactive
   (x-popup-menu last-nonmenu-event
		 (w32-generate-font-fontset-menu)))
  (if fonts
      (let (font)
	(while fonts
	  (condition-case nil
	      (progn
		(set-default-font (car fonts))
		(setq font (car fonts))
		(setq fonts nil))
	    (error
	     (setq fonts (cdr fonts)))))
	(if (null font)
	    (error "Font not found")))))

(defun w32-mouse-operation-init ()
  (setq w32-num-mouse-buttons (w32-get-system-metrics 43))
  (if (>= w32-num-mouse-buttons 3)
      (progn
	(setq w32-lbutton-to-emacs-button 0)
	(setq w32-mbutton-to-emacs-button 1)
	(setq w32-rbutton-to-emacs-button 2)
	)))

(add-hook 'after-init-hook
	  (lambda ()
	    (if (featurep 'meadow)
		(progn
		  (setq keyboard-type (w32-keyboard-type))
		  (setq x-bitmap-file-path mw32-bitmap-file-path)
		  (setq install-lisp-directory-specific-to-emacs-version
			(expand-file-name "../site-lisp" exec-directory)
			install-lisp-directory-independent-of-emacs-version
			(expand-file-name "../../site-lisp" exec-directory))
		  ))))

(defun w32-change-logfont-name (logfont name)
  "change name of logfont."
  (w32-check-logfont logfont)
  (let ((logfontc (copy-sequence logfont)))
    (setcar (nthcdr 1 logfontc) name)
    logfontc))

(defun w32-change-logfont-width (logfont width)
  "change width of logfont."
  (w32-check-logfont logfont)
  (let ((logfontc (copy-sequence logfont)))
    (setcar (nthcdr 2 logfontc) width)
    logfontc))

(defun w32-change-logfont-height (logfont height)
  "change height of logfont."
  (w32-check-logfont logfont)
  (let ((logfontc (copy-sequence logfont)))
    (setcar (nthcdr 3 logfontc) height)
    logfontc))

(defun w32-change-logfont-weight (logfont add)
  "change weight of logfont. Add ADD to weight."
  (w32-check-logfont logfont)
  (let ((weight (nth 4 logfont))
	(logfontc (copy-sequence logfont)))
    (setcar (nthcdr 4 logfontc) (+ weight add))
    logfontc))

(defun w32-change-logfont-italic-p (logfont italic-p)
  "change italic-p of logfont."
  (w32-check-logfont logfont)
  (if (null (or (eq italic-p nil) (eq italic-p t)))
      (error "italic-p must be nil or t."))
  (let ((logfontc (copy-sequence logfont)))
    (setcar (nthcdr 6 logfontc) italic-p)
    logfontc))

(defun w32-logfont-fixed-p (logfont)
  (/= (logand (nth 12 logfont) 1) 0))

(defun w32-change-logfont-charset (logfont charset)
  "change charset of logfont."
  (w32-check-logfont logfont)
  (let ((logfontc (copy-sequence logfont)))
    (setcar (nthcdr 9 logfontc) charset)
    logfontc))

(defun w32-logfont-name (logfont)
  "Return name of logfont."
  (w32-check-logfont logfont)
  (nth 1 logfont))

(defun w32-logfont-width (logfont)
  "Return width of logfont."
  (w32-check-logfont logfont)
  (nth 2 logfont))

(defun w32-logfont-height (logfont)
  "Return height of logfont."
  (w32-check-logfont logfont)
  (nth 3 logfont))

(defun w32-logfont-weight (logfont)
  "Return weight of logfont."
  (w32-check-logfont logfont)
  (nth 4 logfont))

(defun w32-logfont-italic-p (logfont)
  "Return italic-p of logfont."
  (w32-check-logfont logfont)
  (nth 6 logfont))

(defun w32-logfont-charset (logfont)
  "change charset of logfont."
  (w32-check-logfont logfont)
  (nth 9 logfont))

(setq x-fixed-font-alist nil)

;;;
;;; font encoder
;;;

(defun w32-regist-font-encoder (name real-encoder &optional byte)
  (cond ((get real-encoder 'ccl-program-idx)
	 (put name 'ccl-program real-encoder)
	 (put name 'font-unit-byte
	      (cond ((numberp byte) byte)
		    ((null byte) nil)
		    (t
		     (error "BYTE:%S must be a number." byte)))))
	(t
	 (error "Not yet supported encoder! %S" real-encoder))))

(w32-regist-font-encoder
 'encode-koi8-font 'ccl-encode-koi8-font 1)
(w32-regist-font-encoder
 'encode-alternativnyj-font 'ccl-encode-alternativnyj-font 1)
(w32-regist-font-encoder
 'encode-big5-font 'ccl-encode-big5-font 2)
(w32-regist-font-encoder
 'encode-viscii-font 'ccl-encode-viscii-font 1)
(w32-regist-font-encoder
 'encode-ethio-font 'ccl-encode-ethio-font 2)

(define-ccl-program
  ccl-encode-cp1251-font
  '(0
    ((r1 = r1
  [   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 ;; 00-0F
      0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 ;; 10-1F
    160 168 128 129 170 189 178 175 163 138 140 142 141 173 161 143 ;; 20-2F
    192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 ;; 30-3F
    208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 ;; 40-4F
    224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 ;; 50-5F
    240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 ;; 60-6F
    185 184 144 131 186 190 179 191 188 154 156 158 157 167 162 159 ;; 70-7F
  ]))))

(w32-regist-font-encoder
  'encode-cp1251-font 'ccl-encode-cp1251-font 1)

(define-ccl-program
  ccl-encode-cp1250-font
  '(0
    ((r1 = r1
  [   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    160 165 162 163 164 188 140 167 168 138 170 141 143 173 142 175
    176 185 178 179 180 190 156 161 184 154 186 157 159 189 158 191
    192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
    208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
    224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
    240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255
  ]))))

(w32-regist-font-encoder
  'encode-cp1250-font 'ccl-encode-cp1250-font 1)

(define-ccl-program
  ccl-encode-cp1253-font
  '(0
    ((r1 = r1
  [   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      0 145 146 163   0   0 166 167 168 169   0 171 172 173   0 151
    176 177 178 179 180 161 162 183 184 185 186 187 188 189 190 191
    192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
    208 209   0 211 212 213 214 215 216 217 218 219 220 221 222 223
    224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
    240 241 242 243 244 245 246 247 248 249 250 251 252 253 254   0
  ]))))

(w32-regist-font-encoder
  'encode-cp1253-font 'ccl-encode-cp1253-font 1)

(define-ccl-program
  ccl-encode-cp1257-font
  '(0
    ((r1 = r1
  [   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    160 192   0 170 164   0 207 167   0 208 199 204   0 173 222   0
    176 224   0 186   0   0 239   0   0 240 231 236   0   0 254   0
    194   0   0   0 196 197 175 193 200 201 198   0 203   0   0 206
      0 210 212 205   0 213 214 215 168 216   0   0 220   0 219 223
    226   0   0   0 228 229 191 225 232 233 230   0 235   0   0 238
      0 242 244 237   0 245 246 247 184 248   0   0 252   0 251   0
  ]))))

(w32-regist-font-encoder
  'encode-cp1257-font 'ccl-encode-cp1257-font 1)

(define-ccl-program
  ccl-encode-cp1252-font
  '(0
    ((r1 = r1
  [   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    160 161 162 163 128 165 138 167 154 169 170 171 172 173 174 175
    176 177 178 179 142 181 182 183 158 185 186 187 140 156 159 191
    192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
    208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
    224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
    240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255
  ]))))
 
(w32-regist-font-encoder
 'encode-cp1252-font 'ccl-encode-cp1252-font 1)

;;;
;;; Windows logfont information.
;;;

;; <charset> <LOGFONT-charset-num> <encoding> <option-alist>
;; option-alist-key : encoder dim relative-compose

(defconst mw32-charset-windows-font-info-alist
  '((ascii 0 nil)                                 ; ANSI_CHARSET
    (latin-iso8859-1 0 1-byte-set-msb)            ; ANSI_CHARSET
    (ascii-right-to-left 0 nil)                   ; ANSI_CHARSET
    (latin-iso8859-2 238 encode-cp1250-font)      ; EASTEUROPE_CHARSET
    (latin-iso8859-3 1 1-byte-set-msb)            ; DEFAULT_CHARSET
    (latin-iso8859-4 186 encode-cp1257-font)      ; BALTIC_CHARSET
    (cyrillic-iso8859-5 204 encode-cp1251-font)   ; RUSSIAN_CHARSET(1251!=8859)
    (arabic-iso8859-6 178 1-byte-set-msb)         ; ARABIC_CHARSET
    (greek-iso8859-7 161 encode-cp1253-font)      ; GREEK_CHARSET(1253)
    (hebrew-iso8859-8 177 1-byte-set-msb)         ; HEBREW_CHARSET
    (latin-iso8859-9 162 1-byte-set-msb)          ; TURKISH_CHARSET
    (latin-iso8859-15 0 encode-cp1252-font)       ; ANSI_CHARSET(1252)
    (latin-jisx0201 128 nil)                      ; SHIFTJIS_CHARSET
    (katakana-jisx0201 128 shift_jis)             ; SHIFTJIS_CHARSET
    (japanese-jisx0208 128 shift_jis)             ; SHIFTJIS_CHARSET
    (japanese-jisx0212 1 nil)                     ; DEFAULT_CHARSET
    (chinese-big5-1 136 encode-big5-font)         ; CHINESEBIG5_CHARSET
    (chinese-big5-2 136 encode-big5-font)         ; CHINESEBIG5_CHARSET
    (chinese-gb2312 134 2-byte-set-msb)           ; GB2312_CHARSET
    (korean-ksc5601 129 2-byte-set-msb)           ; HANGEUL_CHARSET
    (thai-tis620 222 1-byte-set-msb
     ((relative-compose . -1)))                   ; THAI_CHARSET
    (vietnamese-viscii-lower 163 encode-viscii-font) ; VIETNAMESE_CHARSET
    (vietnamese-viscii-upper 163 encode-viscii-font) ; VIETNAMESE_CHARSET
;    (chinese-cns11643-1 1 nil)           ; DEFAULT_CHARSET
;    (chinese-cns11643-2 1 nil)           ; DEFAULT_CHARSET
;    (chinese-cns11643-3 1 nil)           ; DEFAULT_CHARSET
;    (chinese-cns11643-4 1 nil)           ; DEFAULT_CHARSET
;    (chinese-cns11643-5 1 nil)           ; DEFAULT_CHARSET
;    (chinese-cns11643-6 1 nil)           ; DEFAULT_CHARSET
;    (chinese-cns11643-7 1 nil)           ; DEFAULT_CHARSET
;    (arabic-digit 1 nil)                 ; DEFAULT_CHARSET
;    (arabic-1-column 1 nil)              ; DEFAULT_CHARSET
;    (arabic-2-column 1 nil)              ; DEFAULT_CHARSET
;    (lao 1 nil)                          ; DEFAULT_CHARSET
;    (ipa 1 nil)                          ; DEFAULT_CHARSET
;    (ethiopic 1 nil)                     ; DEFAULT_CHARSET
;    (indian-is13194 1 nil)               ; DEFAULT_CHARSET
;    (indian-2-column 1 nil)              ; DEFAULT_CHARSET
;    (indian-1-column 1 nil)              ; DEFAULT_CHARSET
))

; JOHAB_CHARSET

;;;
;;; Font Request layer: API definition
;;;

;; (:char-spec :width :height :family :weight :slant)
(defun mw32-convert-fr-spec-to-vec (spec)
  (if (eq spec 'any)
      (make-vector 6 'any)
    (let ((vec (make-vector 6 'normal))
	  key val)
      (while spec
	(setq key (car spec)
	      spec (cdr spec)
	      val (car spec)
	      spec (cdr spec))
	(if (not (symbolp key))
	    (error "Key of spec. must be a symbol:%S" key))
	(cond ((eq key :char-spec)
	       (setq val
		     (cond ((charsetp val) (make-char val))
			   ((integerp val) val)
			   ((char-table-p val) val)
			   ((eq val 'any) val)
			   (t (error "Invalid value for :char-spec :%S"
				     val))))
	       (aset vec 0 val))
	      ((eq key :width)
	       (if (not (or (numberp val)
			    (memq val '(any normal
					    condensed semi-expanded
					    expanded extra-condensed
					    extra-expanded ultra-condensed
					    ultra-expanded))))
		   (error "Invalid value for :width :%S"
			  val))
	       (aset vec 1 val))
	      ((eq key :height)
	       (if (not (or (numberp val)
			    (eq val 'any)))
		   (error "Invalid value for :height :%S"
			  val))
	       (aset vec 2 val))
	      ((eq key :family)
	       (if (not (or (stringp val)
			    (eq val 'any)))
		   (error "Invalid value for :family :%S"
			  val))
	       (aset vec 3 val))
	      ((eq key :weight)
	       (if (not (memq val
			      '(any normal
				    bold semi-bold
				    extra-bold light
				    semi-light ultra-light
				    extra-light)))
		   (error "Invalid value for :weight :%S"
			  val))
	       (aset vec 4 val))
	      ((eq key :slant)
	       (if (not (memq val '(any normal
					italic reverse-italic
					oblique reverse-oblique)))
		   (error "Invalid value for :slant :%S"
			  val))
	       (aset vec 5 val))))
      ;; set default value for :char-spec, :height, and :family.
      (if (eq (aref vec 0) 'normal)
	  (aset vec 0 (make-char 'ascii)))
      (if (eq (aref vec 2) 'normal)
	  (aset vec 2 nil))
      (if (eq (aref vec 3) 'normal)
	  (aset vec 3 "\\*"))
      vec)))

;; Note that this function must be DEPRECATED!!!
(defun mw32-convert-font-legacy-strict-spec (alist)
  (if (assq 'strict-spec alist)
      (cons
	(append
	 (or (assq 'spec alist) '(spec))
	 (mapcar
	  (lambda (x)
	    (cons
	     (car x)
	     (cons 'strict
		   (cdr x))))
	  (cdr (assq 'strict-spec alist))))
	alist)
    alist))

(defun mw32-convert-font-request-alist (alist)
  (setq alist (mw32-convert-font-legacy-strict-spec alist))
  (let* ((sslot (assq 'spec alist))
	 (ss (cdr sslot))
	 rs elem spec val)
    (while (setq elem (car ss))
      (setq spec (car elem)
	    val (cdr elem))
      (if (not (listp spec))
	  (error "Invalid Spec %S" spec))
      (setq rs (cons (cons
		      (mw32-convert-fr-spec-to-vec spec)
		      val)
		     rs))
      (setq ss (cdr ss)))
    (if rs (cons (cons 'spec (nreverse rs))
		 (delq sslot alist))
      alist)))

(defun w32-add-font (name alist)
  (w32-add-font-internal
   name (mw32-convert-font-request-alist alist)))

(defun w32-change-font (name alist)
  (w32-change-font-attribute-internal
   name (mw32-convert-font-request-alist alist)))

;;;
;;; Font Request layer: Elisp based FR
;;;

;;;;;
;;;;;
;;;;;  High level font selection API
;;;;;
;;;;;

(defun w32-auto-regist-bdf-font (fontname bdffile &optional encoding)
  (if (null encoding) (setq encoding 0))
  (let ((bdfatt (w32-get-logfont-info (list 'bdf-font bdffile))))
    (if bdfatt
	(progn
	  (if encoding
	      (setq bdfatt (cons (cons 'encoding encoding)
				 bdfatt)))
	  (w32-add-font fontname bdfatt)
	  (w32-change-font-logfont fontname 0
				   (list 'bdf-font bdffile))))))

(defun create-font-from-logfont-list
  (name logfont-list &optional encoding-type alist)
;  (w32-check-logfont logfont)
  (if (null encoding-type) (setq encoding-type 0))
  (let ((prop (append (list (cons 'encoding-type encoding-type))
		      alist 
		      (w32-get-logfont-info (car logfont-list))))
	(i 0)
	logfont)
    (w32-add-font name prop)
    (while (setq logfont (car logfont-list))
      (w32-change-font-logfont name i logfont)
      (setq i (1+ i))
      (setq logfont-list (cdr logfont-list)))))

(defun set-font-from-logfont
  (name logfont charset pnum &optional encoding-type alist)
  (w32-check-logfont logfont)
  (let (w32-alist w32-logfont-info prop)
    (if (eq (car logfont) 'w32-logfont)
	(progn
	  (setq w32-logfont-info
		(assq charset mw32-charset-windows-font-info-alist))
	  (setq w32-alist
		(setq alist (nth 3 w32-logfont-info)))
	  (if (null encoding-type)
	      (setq encoding-type (nth 2 w32-logfont-info)))))
  (setq prop (append 
	      (if (numberp encoding-type)
		  (list (cons 'encoding-type encoding-type))
		(list
		 (cons 'encoding-type 0)
		 (cons 'encoder encoding-type)))
	      alist 
	      w32-alist
	      (w32-get-logfont-info logfont)))
  (condition-case nil
      (w32-add-font name prop)
    (error))
  (w32-change-font-logfont name pnum logfont)))

; request type
;  family, width, height, italic, weight, fixed
;  ?? base ??

(defvar logfont-from-request-functions nil
  "* Functions that return logical font from your request.
These functions are called passing CHARSET-SYMBOL, REQUIRED-ALIST,
RECOMMENDED-ALIST.
These functions must return a logical font or nil 
when no logical fonts are found.")

(defvar w32-font-list-cache-all nil)
(defvar w32-font-list-cache-charset nil)

(defun w32-clear-logfont-list-cache ()
  (setq w32-font-list-cache-all nil
	w32-font-list-cache-charset nil))

(defun w32-enum-logfont-from-charset (charset)
  (let ((font-list-slot (assq charset w32-font-list-cache-charset))
	ms-charset
	curlist
	curelem
	lfname
	cand1
	result)
    (if font-list-slot
	(cdr font-list-slot)
      (if (null w32-font-list-cache-all)
	  (setq w32-font-list-cache-all
		(w32-enum-logfont)))
      (setq ms-charset
	    (nth 1
		 (assq charset mw32-charset-windows-font-info-alist)))
      (if (null ms-charset)
	  nil
	(setq curlist w32-font-list-cache-all)
	(while (setq curelem (car curlist))
	  (setq lfname (nth 1 (nth 3 curelem))
		cand1
		(nconc cand1
		       (and
			(> (length lfname) 0)
			(/= (aref lfname 0) ?@)
			(w32-logfont-valid-charset-p
			 (nth 3 curelem) ms-charset)
			(w32-enum-logfont lfname)))
		curlist (cdr curlist)))
	(setq w32-font-list-cache-charset
	      (cons (cons charset cand1)
		    w32-font-list-cache-charset))
	cand1))))

(defsubst logfont-from-char-and-request (c required recommended)
  (run-hook-with-args-until-success
   'logfont-from-request-functions
   (if (< c 0) 'ascii (char-charset c))
   required recommended nil))
 
(defsubst logfont-list-from-request (required recommended &optional fontset)
  (let* ((charset-list
	  (if (null fontset)
	      (charset-list)
	    (let* ((chlist (aref (fontset-info fontset) 2))
		   (curlist chlist))
	      (while (setq curlist (cdr curlist))
		(setcar curlist
			(car (car curlist))))
	      chlist)))
	 (curchl charset-list)
	 curch logfont result)
    (while (setq curch (car curchl))
      (if (setq logfont (run-hook-with-args-until-success
			 'logfont-from-request-functions
			 curch required recommended fontset))
	  (setq result (cons (cons curch logfont) result)))
      (setq curchl (cdr curchl)))
    result))

(defsubst w32-candidate-scalable-p (cand)
  (eq (nth 2 cand) 'scalable))

(defun w32-candidate-satisfy-request-p (cand request)
  (let* ((item (car request))
	 (cont (cdr request))
	 (logfont (nth 3 cand))
	 (info (w32-get-logfont-info logfont)))
    (cond ((eq item 'width)
	   (or (w32-candidate-scalable-p cand)
	       (= (cdr (assq 'width info)) cont)))
	  ((eq item 'height)
	   (or (w32-candidate-scalable-p cand)
	       (= (cdr (assq 'height info)) cont)))
	  ((eq item 'weight)
	   t)
;	   (or (w32-candidate-scalable-p cand)
;	       (= (cdr (assq 'weight info)) cont)))
	  ((eq item 'italic)
	   (if cont
	       (w32-logfont-italic-p logfont)
	     (not (w32-logfont-italic-p logfont))))
	  ((eq item 'fixed)
	   (if cont
	       (w32-logfont-fixed-p logfont)
	     (not (w32-logfont-fixed-p logfont))))
	  ((eq item 'family)
	   (string= (car cand) cont))
	  (t
	   t))))

(defun w32-select-logfont-from-required (candidate required)
  (let ((scorelist (w32-score-logfont-candidates required candidate))
	(len (length required))
	result)
    (while scorelist
      (if (>= (car scorelist) len)
	  (setq result (cons (car candidate) result)))
      (setq candidate (cdr candidate)
	    scorelist (cdr scorelist)))
    result))

(defun w32-select-logfont-from-recommended (candidate recommended)
  (let* ((scorelist (w32-score-logfont-candidates recommended candidate))
	 (max (car scorelist))
	 (bestcand (car candidate)))
    (setq candidate (cdr candidate)
	  scorelist (cdr scorelist))
    (while scorelist
      (if (> (car scorelist) max)
	  (progn
	    (setq max (car scorelist)
		  bestcand (car candidate))))
      (setq candidate (cdr candidate)
	    scorelist (cdr scorelist)))
    bestcand))

(defsubst w32-logfont-valid-charset-p (logfont charset)
  (=
;   (cdr
;    (assq 'charset-num
;	  (w32-get-logfont-info
;	   (w32-change-logfont-charset
;	    logfont charset))))
   (w32-logfont-charset logfont)
   charset))

(defun w32-modify-logfont-from-request (logfont required recommended)
  (let ((width (or (cdr (assq 'width required))
		   (cdr (assq 'width recommended))
		   (w32-logfont-width logfont)))
	(height (or (assq 'height required)
		    (assq 'height recommended)))
	(weight (or (assq 'weight required)
		    (assq 'weight recommended)))
	(italic (or (assq 'italic required)
		    (assq 'italic recommended)))
	result)

    (setq result (w32-change-logfont-width
		  logfont width))

    ;; In the case of propotional font, we must resize
    ;; font width to ensure the width of this font is less
    ;; than requested `width'.
    (if (not (w32-logfont-fixed-p logfont))
	(let* ((info (w32-get-logfont-info logfont))
	       (max-width (cdr (assq 'max-width info)))
	       (test-width (- (+ width width) max-width)))
	  (if (> max-width width)
	      (if (> test-width 0)
		  (setq result
			(w32-change-logfont-width
			 logfont test-width))
		;; give up resizing correctly, this is heuristic mainly for thai ;_;
		(setq result
		      (w32-change-logfont-width
		       logfont (floor (* width 0.7))))))))

    ;;; for speed, I don't use w32-change-logfont-*
    (if height
	(setcar (nthcdr 3 result)
		(cdr height)))
    (if weight
	(setcar (nthcdr 4 result)
		(cdr weight)))
    (if italic
	(setcar (nthcdr 6 result)
		(cdr italic)))
    result))

(defun w32-logfont-list-from-request (charset required recommended fontset)
  ;; fontset is used as a trivial temporary variable:-P.
  (setq fontset
	(nth 3 (w32-select-logfont-from-recommended
		(w32-select-logfont-from-required
		 (w32-enum-logfont-from-charset charset)
		 required)
		recommended)))
  (and fontset
       (w32-modify-logfont-from-request fontset required recommended)))

(add-hook 'logfont-from-request-functions
	  (function w32-logfont-list-from-request))

;; new version

(defvar mw32-face-attrs-weight-alist
  '((normal . 400)
    (bold . 700)
    (ultra-bold . 800)
    (semi-bold . 600)
    (extra-bold . 800)
    (light . 300)
    (semi-light . 200)
    (extra-light . 200)
    (ultra-light . 200)
    (extra-light . 100)))

(defun mw32-convert-face-attrs-to-request (attrs)
  (let (key req val)
   (while attrs
     (setq key (car attrs)
	   attrs (cdr attrs))
     (cond 
      ;;((eq key :width)
      ;; (setq req (cons (cons 'width (car attrs))
      ;;           req)))
      ((and (eq key :height)
	    (numberp (car attrs)))
       (setq req (cons (cons 'height
			     (/ (* (car attrs) 720) 96) ; pixel -> 0.1 point
			     )
		       req)))
      ((eq key :family)
       (if (not (string= (car attrs) "*"))
	   (setq req (cons (cons 'family (car attrs))
			   req))))
      ((eq key :weight)
       (if (setq val (assq (car attrs)
			   mw32-face-attrs-weight-alist))
	   (if val (setq req (cons (cons 'weight (cdr val)) req)))))
      ((eq key :slant)
       (if (memq (car attrs) '(italic oblique))
	   (setq req (cons (cons 'italic t)
			   req))))))
   req))

(defun mw32-load-lf-from-request (c attrs f required recommended)
  (setq recommended
	(append recommended
		(mw32-convert-face-attrs-to-request attrs)))
  (logfont-from-char-and-request c required recommended))

(defun create-fontset-from-request-with-spec
  (name spec required recommended)
  "Create fontset from your request."
  (w32-add-font-internal
   name
   `((spec
      (,(mw32-convert-fr-spec-to-vec spec)
       function
       (lambda (c attrs f)
	 (mw32-load-lf-from-request
	  c attrs f
	  ',required ',recommended))
       ,(append required recommended))))))

(defun change-fontset-from-request-with-spec
  (name spec required recommended)
  "Create fontset from your request."
  (let* ((finfo
	  (w32-get-font-info name))
	 (fl (assq 'spec finfo))
	 (vec (mw32-convert-fr-spec-to-vec spec))
	 (valf
	  `(function
	    (lambda (c attrs f)
	      (mw32-load-lf-from-request
	       c attrs f
	       ',required ',recommended))
	    ,(append required recommended)))
	 ts)
    (if (null fl)
	(setq finfo (cons (list 'spec (cons vec valf)) finfo))
      (setq ts (assoc vec (cdr fl)))
      (if ts
	  (setcdr ts valf)
	(setcdr fl (cons (cons vec valf) (cdr fl)))))
    (w32-change-font-attribute-internal
     name finfo)))

(defun create-fontset-from-request
  (name required recommended)
  "Create fontset from your request."
  (create-fontset-from-request-with-spec
   name
   'any
   required recommended))

(defun change-fontset-from-request
  (name required recommended &optional property)
  "Create fontset from your request."
  (let ((spec
	 (copy-sequence
	  '(:char-spec any
	    :width any
	    :height any
	    :family any
	    :weight any
	    :slant any))))
    (setq spec
	  (if (= (logand property 1) 0)
	      (plist-put spec :weight 'normal)
	    (plist-put spec :weight 'bold)))
    (setq spec
	  (if (= (logand property 2) 0)
	      (plist-put spec :slant 'normal)
	    (plist-put spec :slant 'italic)))
    (change-fontset-from-request-with-spec
     name spec required recommended)))

;;;;;
;;;;;  For Argument Editing.
;;;;;
;;;;;

(defvar process-argument-editing-alist nil)

(defvar default-process-argument-editing-function
  (lambda (x) (general-process-argument-editing-function
	       x 'msvc t))
  "Default argument editing function.
When any argument editing functions are NOT found,
this function is used for argument editing.")

(defun remove-process-argument-editing (process)
  "Remove argument editing configuration of PROCESS, if exists."
  (let ((curelem process-argument-editing-alist))
    (if (string= (car (car curelem)) process)
	(setq process-argument-editing-alist
	      (cdr process-argument-editing-alist))
      (while (progn
	       (if (not (string= (car (car (cdr curelem))) process))
		   (setq curelem (cdr curelem))
		 (setcdr curelem (cdr (cdr curelem)))
		 nil))))))

(defun define-process-argument-editing
  (process function &optional method)
  "Define argument editing configuration of PROCESS to FUNCTION"
  (indirect-function function)
  (let ((elem (cons process function))
	(oelem (assoc process process-argument-editing-alist)))
    (cond ((eq method 'last)
	   (remove-process-argument-editing process)
	   (nconc process-argument-editing-alist (list elem)))
	  ((eq method 'first)
	   (remove-process-argument-editing process)
	   (setq process-argument-editing-alist
		 (cons elem process-argument-editing-alist)))
	  ((eq method 'append)
	   (if oelem
	       nil
	     (setq process-argument-editing-alist
		   (cons elem process-argument-editing-alist))))
	  ((eq method 'replace)
	   (if oelem
	       (setcdr oelem function)))
	  (t
	   (if oelem
	       (setcdr oelem function)
	     (setq process-argument-editing-alist
		   (cons elem process-argument-editing-alist)))))))

(defun find-process-argument-editing-function (process)
  "Find a function of argument editing to invoke PROCESS."
  (let ((alist process-argument-editing-alist)
	(elem nil))
    (while (and (null elem) (setq elem (car alist)))
      (if (string-match (car elem) process)
	  (setq elem (cdr elem))
	(setq alist (cdr alist))
	(setq elem nil)))
    (if elem
	elem
      default-process-argument-editing-function)))

(defun msvc-process-argument-quoting (arg)
  (mapcar (lambda (x)
	    (let ((start 0) (result "\"") pos end)
	      (while (string-match "\\\\*\"" x start)
		(setq pos (match-beginning 0)
		      end (match-end 0)
		      result (concat result
				     (substring x start pos)
				     (make-string (* (- end pos 1) 2) ?\\ )
				     "\\\"")
		      start end))
	      (concat result  
		      (if (string-match "\\\\*\\'" x start)
			  (concat (substring x start (match-beginning 0))
				  (make-string (* (- (match-end 0)
						     (match-beginning 0))
						  2) ?\\))
			(substring x start))
		      "\"")))
	  arg))

(defun cygnus-process-argument-quoting (arguments)
  (mapcar (lambda (arg)
	     (let ((result "\"") (start 0) pos)
	       (while (string-match "\"" arg start)
		 (setq pos (match-end 0)
		       result (concat result
				      (substring arg start pos) "\"")
		       start pos))
	       (concat result (substring arg start) "\"")))
	  arguments))

(defun general-process-argument-editing-function
  (argument quoting argv0isp &optional ep h2sp qp s2isp)
  (setq argument (cond ((eq quoting 'msvc)
			(msvc-process-argument-quoting argument))
		       ((eq quoting 'cygnus)
			(cygnus-process-argument-quoting argument))
		       (t
			argument)))
  (if (null argv0isp)
      (unix-to-dos-argument (mapconcat (function concat) argument " ")
			    ep h2sp qp s2isp)
    (concat
     (unix-to-dos-filename (car argument)) " "
     (unix-to-dos-argument (mapconcat (function concat) (cdr argument) " ")
			   ep h2sp qp s2isp))))

(defmacro define-argument-editing-from-program-list
  (program-list function &optional method)
  "Define argument editing configuration from PROGRAM-LIST.
PROGRAM-LIST consists of program names, and FUNCTION is used
for argument editing of these programs."
  (list 'define-process-argument-editing
	(concat
	 "/"
	 (regexp-opt (eval program-list) t)
	 "\\'")
	function method))

(define-argument-editing-from-program-list
  '("fiber.exe" "movemail.exe" "ctags.exe" "etags.exe"
    "ftp.exe" "telnet.exe" "tcsh.exe"
    "hexl.exe" "m2ps.exe" "emacsserver.exe"
    "wakeup.exe"  "tcp.exe" "fakemail.exe"
    ;; This is for the wordseg program called swath, the abbreviation
    ;; of Smart Word Analysis for THai, and for marking word boundaries
    ;; for continuous Thai-script sequences.
    "swath.exe")
  (lambda (x) (general-process-argument-editing-function x 'msvc t)))

(define-process-argument-editing
  "\\(/cmd\\.exe\\'\\|/command\\.com\\'\\)"
  (lambda (x) (general-process-argument-editing-function x nil t t nil t t)))

(define-process-argument-editing
  "\\.bat\\'"
  (lambda (x) (general-process-argument-editing-function x nil t t nil t t)))

(define-process-argument-editing
  "/tcsh\\.exe\\'"
  (lambda (x) (general-process-argument-editing-function x 'msvc t)))

(define-process-argument-editing
  "/bash\\.exe\\'"
  (lambda (x) (general-process-argument-editing-function x 'cygnus t)))

;; This is for the typing excersize program called trr.
(define-process-argument-editing
  "/trr.*\\.exe\\'"
  (lambda (x) (general-process-argument-editing-function x 'msvc t)))

;;;
;;; User interface layer that handles WM_SYSCOMMAND.
;;  This is alternative implementation of w32-send-syscommand().
;;;
(defun w32-activate-menu-bar (&optional frame)
  "Activate the menu bar in the frame FRAME.
If FRAME is omitted, the selected frame is used."
  (w32-access-windows-intrinsic-facility 'WM-SYSCOMMAND 'SC-KEYMENU
					 nil frame))

(defun w32-activate-start-menu ()
  "Activate the start menu."
  (w32-access-windows-intrinsic-facility 'WM-SYSCOMMAND 'SC-TASKLIST))

(defun w32-set-monitor-state (state)
  "Sets the state of the monitor.
STATE can have the following values:
 low-power: the monitor is going to low power.
 shut-off: the monitor is being shut off."
  (w32-access-windows-intrinsic-facility 'WM-SYSCOMMAND
					 'SC-MONITORPOWER state))

(defun w32-maximize-frame (&optional frame)
  "Maximize the frame FRAME.
If FRAME is omitted, the selected frame is used."
  (w32-access-windows-intrinsic-facility 'WM-SYSCOMMAND 'SC-MAXIMIZE
					 nil frame))

(defun w32-restore-frame (&optional frame)
  "Restore the maximized frame FRAME.
If FRAME is omitted, the selected frame is used."
  (w32-access-windows-intrinsic-facility 'WM-SYSCOMMAND 'SC-RESTORE nil frame))
