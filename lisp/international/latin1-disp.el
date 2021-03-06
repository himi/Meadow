;;; latin1-disp.el --- display tables for other ISO 8859 on Latin-1 terminals -*- coding: emacs-mule -*-

;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

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

;; This package sets up display of ISO 8859-n for n>1 by substituting
;; Latin-1 characters and sequences of them for characters which can't
;; be displayed, either because we're on a tty or because we don't
;; have the relevant window system fonts available.  For instance,
;; Latin-9 is very similar to Latin-1, so we can display most Latin-9
;; characters using the Latin-1 characters at the same code point and
;; fall back on more-or-less mnemonic ASCII sequences for the rest.

;; For the Latin charsets the ASCII sequences are mostly consistent
;; with the Quail prefix input sequences.  Latin-4 uses the Quail
;; postfix sequences since a prefix method isn't defined for Latin-4.

;; [A different approach is taken in the DOS display tables in
;; term/internal.el, and the relevant ASCII sequences from there are
;; available as an alternative; see `latin1-display-mnemonic'.  Only
;; these sequences are used for Cyrillic, Greek and Hebrew.]

;; If you don't even have Latin-1, see iso-ascii.el and use the
;; complete tables from internal.el.  The ASCII sequences used here
;; are mostly in the same style as iso-ascii.

;;; Code:

;; Ensure `standard-display-table' is set up:
(require 'disp-table)

(defconst latin1-display-sets '(latin-2 latin-3 latin-4 latin-5 latin-8
		                latin-9 cyrillic greek hebrew)
  "The ISO8859 character sets with defined Latin-1 display sequences.
These are the nicknames for the sets and correspond to Emacs language
environments.")

(defgroup latin1-display ()
  "Set up display tables for ISO8859 characters using Latin-1."
  :version "21.1"
  :link '(emacs-commentary-link "latin1-disp")
  :group 'i18n)

(defcustom latin1-display-format "{%s}"
  "A format string used to display the ASCII sequences.
The default encloses the sequence in braces, but you could just use
\"%s\" to avoid the braces."
  :group 'latin1-display
  :type 'string)

;;;###autoload
(defcustom latin1-display nil
  "Set up Latin-1/ASCII display for ISO8859 character sets.
This is done for each character set in the list `latin1-display-sets',
if no font is available to display it.  Characters are displayed using
the corresponding Latin-1 characters where they match.  Otherwise
ASCII sequences are used, mostly following the Latin prefix input
methods.  Some different ASCII sequences are used if
`latin1-display-mnemonic' is non-nil.

Setting this variable directly does not take effect;
use either M-x customize of the function `latin1-display'."
  :group 'latin1-display
  :type 'boolean
  :require 'latin1-disp
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (mapc (if value
		   #'latin1-display-setup
		 #'latin1-display-reset)
	       latin1-display-sets)
	 (redraw-display)))

;;;###autoload
(defun latin1-display (&rest sets)
  "Set up Latin-1/ASCII display for the arguments character SETS.
See option `latin1-display' for the method.  The members of the list
must be in `latin1-display-sets'.  With no arguments, reset the
display for all of `latin1-display-sets'. See also `latin1-display-setup'."
  (if sets
      (progn (mapc #'latin1-display-setup sets)
	     (setq latin1-display t))
    (mapc #'latin1-display-reset latin1-display-sets)
    (setq latin1-display nil))
  (redraw-display))

(defcustom latin1-display-mnemonic nil
  "Non-nil means to display potentially more mnemonic sequences.
These are taken from the tables in `internal.el' rather than the Quail
input sequences."
  :type 'boolean
  :group 'latin1-display)

(defun latin1-display-char (char display &optional alt-display)
  "Make an entry in `standard-display-table' for CHAR using string DISPLAY.
If ALT-DISPLAY is provided, use that instead if
`latin1-display-mnemonic' is non-nil.  The actual string displayed is
formatted using `latin1-display-format'."
  (if (and (stringp alt-display)
	   latin1-display-mnemonic)
      (setq display alt-display))
  (if (stringp display)
      (standard-display-ascii char (format latin1-display-format display))
    (aset standard-display-table char display)))

(defun latin1-display-identities (charset)
  "Display each character in CHARSET as the corresponding Latin-1 character.
CHARSET is a symbol which is the nickname of a language environment
using an ISO8859 character set."
  (if (eq charset 'cyrillic)
      (setq charset 'cyrillic-iso))
  (let ((i 32)
	(set (car (remq 'ascii (get-language-info charset 'charset)))))
    (while (<= i 127)
      (aset standard-display-table
	    (make-char set i)
	    (vector (make-char 'latin-iso8859-1 i)))
      (setq i (1+ i)))))

(defun latin1-display-reset (language)
  "Set up the default display for each character of LANGUAGE's charset.
LANGUAGE is a symbol naming a language environment using an ISO8859
character set."
  (if (eq language 'cyrillic)
      (setq language 'cyrillic-iso))
  (let ((charset (car (remq 'ascii (get-language-info language
							'charset)))))
    (standard-display-default (make-char charset 32)
			      (make-char charset 127)))
  (sit-for 0))

(defun latin1-display-check-font (language)
  "Return non-nil if we have a font with an encoding for LANGUAGE.
LANGUAGE is a symbol naming a language environment using an ISO8859
character set: `latin-2', `hebrew' etc."
  (if (eq language 'cyrillic)
      (setq language 'cyrillic-iso))
  (let* ((info (get-language-info language 'charset))
	 (char (make-char (car (remq 'ascii info)) ?\ )))
    (latin1-char-displayable-p char)))

;; This should be moved into mule-utils or somewhere after 21.1.
(defun latin1-char-displayable-p (char)
  (cond ((< char 256)
	 ;; Single byte characters are always displayable.
	 t)
	((display-multi-font-p)
	 ;; On a window system, a character is displayable if we have
	 ;; a font for that character in the default face of the
	 ;; currently selected frame.
	 (let ((fontset (frame-parameter (selected-frame) 'font))
	       font-pattern)
	   (if (query-fontset fontset)
	       (setq font-pattern (fontset-font fontset char)))
	   (or font-pattern
	       (setq font-pattern (fontset-font "fontset-default" char)))
	   (if font-pattern
	       (progn
		 ;; Now FONT-PATTERN is a string or a cons of family
		 ;; field pattern and registry field pattern.
		 (or (stringp font-pattern)
		     (setq font-pattern (concat "-"
						(or (car font-pattern) "*")
						"-*-"
						(cdr font-pattern))))
		 (x-list-fonts font-pattern 'default (selected-frame) 1)))))
	(t
	 (let ((coding (terminal-coding-system)))
	   (if coding
	       (let ((safe-chars (coding-system-get coding 'safe-chars))
		     (safe-charsets (coding-system-get coding 'safe-charsets)))
		 (or (and safe-chars
			  (aref safe-chars char))
		     (and safe-charsets
			  (memq (char-charset char) safe-charsets)))))))))

(defun latin1-display-setup (set &optional force)
  "Set up Latin-1 display for characters in the given SET.
SET must be a member of `latin1-display-sets'.  Normally, check
whether a font for SET is available and don't set the display if it
is.  If FORCE is non-nil, set up the display regardless."
  (cond
   ((eq set 'latin-2)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?と "'C" "C'")
	 (?ひ "'D" "/D")
	 (?え "'S" "S'")
	 (?よ "'c" "c'")
	 (?を "'d" "/d")
	 (?で "'L" "L'")
	 (?ん "'n" "n'")
	 (?び "'N" "N'")
	 (?も "'r" "r'")
	 (?ぢ "'R" "R'")
	 (?じ "'s" "s'")
	 (?ぞ "'z" "z'")
	 (?ぎ "'Z" "Z'")
	 (?ぃ "`A" "A;")
	 (?ぬ "`E" "E;")
	 (?ぅ "`L" "/L")
	 (?が "`S" ",S")
	 (?む "`T" ",T")
	 (?け "`Z" "Z^.")
	 (?こ "`a" "a;")
	 (?さ "`l" "/l")
	 (?れ "`e" "e;")
	 (?ぜ "`s" ",s")
	 (?�� "`t" ",t")
	 (?ち "`z" "z^.")
	 (?�� "`." "'.")
	 (?づ "~A" "A(")
	 (?な "~C" "C<")
	 (?ぱ "~D" "D<")
	 (?の "~E" "E<")
	 (?ゎ "~e" "e<")
	 (?ぇ "~L" "L<")
	 (?ぴ "~N" "N<")
	 (?ぷ "~O" "O''")
	 (?ぺ "~R" "R<")
	 (?か "~S" "S<")
	 (?き "~T" "T<")
	 (?ぽ "~U" "U''")
	 (?ぐ "~Z" "Z<")
	 (?ゅ "~a" "a(}")
	 (?り "~c" "c<")
	 (?ゑ "~d" "d<")
	 (?し "~l" "l<")
	 (?�� "~n" "n<")
	 (?�� "~o" "o''")
	 (?�� "~r" "r<")
	 (?せ "~s" "s<")
	 (?そ "~t" "t<")
	 (?�� "~u" "u''")
	 (?だ "~z" "z<")
	 (?す "~v" "'<")			; ?い in latin-pre
	 (?い "~~" "'(")
	 (?�� "uu" "u^0")
	 (?ほ "UU" "U^0")
	 (?て "\"A")
	 (?ゆ "\"a")
	 (?ね "\"E" "E:")
	 (?ろ "\"e")
	 (?た "''" "'")
	 (?す "'<")			; Lynx's rendering of caron
	 ))))

   ((eq set 'latin-3)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?Γ "/H")
	 (?Δ "~`" "'(")
	 (?Θ "^H" "H^")
	 (?Ω "^h" "h^")
	 (?Λ ".I" "I^.")
	 (?Μ ",S")
	 (?Ν "~G" "G(")
	 (?Ξ "^J" "J^")
	 (?Ρ ".Z" "Z^.")
	 (?Τ "/h")
	 (?�ｹ ".i" "i^.")
	 (?�ｺ ",s")
	 (?�ｻ "~g" "g(")
	 (?�ｼ "^j" "j^")
	 (?α ".Z" "z^.")
	 (?η ".c" "C^.")
	 (?θ "^C" "C^")
	 (?ψ ".G" "G^.")
	 (?�ﾘ "^G" "G^")
	 (?�ﾝ "~U" "U(")
	 (?�ﾞ "^S" "S^")
	 (?�� ".C" "c^.")
	 (?�� "^c" "c^")
	 (?�� ".g" "g^.")
	 (?�� "^g" "g^")
	 (?�� "~u" "u(")
	 (?�� "^s" "s^")
	 (?�� "/." "^.")))))

   ((eq set 'latin-4)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?┌ "A," "A;")
	 (?┐ "k/" "kk")
	 (?┘ "R," ",R")
	 (?├ "I~" "?I")
	 (?┬ "L," ",L")
	 (?┼ "S~" "S<")
	 (?━ "E-")
	 (?┃ "G," ",G")
	 (?┏ "T/" "/T")
	 (?┛ "Z~" "Z<")
	 (?┳ "a," "a;")
	 (?┫ "';")
	 (?┻ "r," ",r")
	 (?┠ "i~" "~i")
	 (?┯ "l," ",l")
	 (?┨ "'<")
	 (?┿ "s~" "s<")
	 (?┝ "e-")
	 (?┰ "g," ",g")
	 (?┥ "t/" "/t")
	 (?┸ "N/" "NG")
	 (?╂ "z~" "z<")
	 (?�ｿ "n/" "ng")
	 (?�ﾀ "A-")
	 (?�ﾇ "I," "I;")
	 (?�ﾈ "C~" "C<")
	 (?�ﾊ "E," "E;")
	 (?�ﾌ "E." "E^.")
	 (?�ﾏ "I-")
	 (?�ﾑ "N," ",N")
	 (?�ﾒ "O-")
	 (?�ﾓ "K," ",K")
	 (?�ﾙ "U," "U;")
	 (?�ﾝ "U~" "~U")
	 (?�ﾞ "U-")
	 (?�� "a-")
	 (?�� "i," "i;")
	 (?�� "c~" "c<")
	 (?�� "e," "e;")
	 (?�� "e." "e^.")
	 (?�� "i-")
	 (?�� "d/" "/d")
	 (?�� "n," ",n")
	 (?�� "o-")
	 (?�� "k," ",k")
	 (?�� "u," "u;")
	 (?�� "u~" "~u")
	 (?�� "u-")
	 (?�� "^.")))))

   ((eq set 'latin-5)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?昨 "~g" "g(")
	 (?災 "~G" "G(")
	 (?在 ".I" "I^.")
	 (?�� ",s")
	 (?材 ",S")
	 (?碕 "^e" "e<")			; from latin-post
	 (?作 ".e" "e^.")
	 (?搾 "\"i" "i-")		; from latin-post
	 (?�� ".i" "i.")))))

   ((eq set 'latin-8)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?升 ".B" "B`")
	 (?召 ".b" "b`")
	 (?唱 ".c" "c`")
	 (?商 ".C" "C`")
	 (?嘗 ".D" "D`")
	 (?将 ".d" "d`")
	 (?昇 "`w")
	 (?妾 "`W")
	 (?昭 "'w" "w'")
	 (?宵 "'W" "W'")
	 (?松 "`y")
	 (?小 "`Y")
	 (?廠 ".f" "f`")
	 (?床 ".F" "F`")
	 (?承 ".g" "g`")
	 (?彰 ".G" "G`")
	 (?招 ".m" "m`")
	 (?抄 ".M" "M`")
	 (?昌 ".p" "p`")
	 (?捷 ".P" "P`")
	 (?樵 ".s" "s`")
	 (?晶 ".S" "S`")
	 (?樟 "\"w")
	 (?梢 "\"W")
	 (?条 "^w" "w^")
	 (?紹 "^W" "W^")
	 (?譲 ".t" "t`")
	 (?訟 ".T" "T`")
	 (?�� "^y" "y^")
	 (?鉦 "^Y" "Y^")
	 (?庄 "\"Y")))))

   ((eq set 'latin-9)
    (when (or force
	      (not (latin1-display-check-font set)))
      (latin1-display-identities set)
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?耳 "~s" "s<")
	 (?示 "~S" "S<")
	 (?痔 "Euro" "E=")
	 (?失 "~z" "z<")
	 (?雫 "~Z" "Z<")
	 (?疾 "\"Y")
	 (?漆 "oe")
	 (?湿 "OE")))))

   ((eq set 'greek)
    (when (or force
	      (not (latin1-display-check-font set)))
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?�｡ "9'")
	 (?�｢ "'9")
	 (?�ｯ "-M")
	 (?�ｵ "'%")
	 (?�ｶ "'A")
	 (?�ｸ "'E")
	 (?�ｹ "'H")
	 (?�ｺ "'I")
	 (?�ｼ "'O")
	 (?�ｾ "'Y")
	 (?�ｿ "W%")
	 (?�ﾀ "i3")
	 (?�ﾃ "G*")
	 (?�ﾄ "D*")
	 (?�ﾈ "TH")
	 (?�ﾋ "L*")
	 (?�ﾎ "C*")
	 (?�ﾐ "P*")
	 (?�ﾓ "S*")
	 (?�ﾖ "F*")
	 (?�ﾘ "Q*")
	 (?�ﾙ "W*")
	 (?�ﾚ "\"I")
	 (?�ﾛ "\"Y")
	 (?�ﾜ "a%")
	 (?�ﾝ "e%")
	 (?�ﾞ "y%")
	 (?�ﾟ "i%")
	 (?�� "u3")
	 (?�� "a*")
	 (?�� "b*")
	 (?�� "g*")
	 (?�� "d*")
	 (?�� "e*")
	 (?�� "z*")
	 (?�� "y*")
	 (?�� "h*")
	 (?�� "i*")
	 (?�� "k")
	 (?�� "l*")
	 (?�� "m*")
	 (?�� "n*")
	 (?�� "c*")
	 (?�� "p*")
	 (?�� "r*")
	 (?�� "*s")
	 (?�� "s*")
	 (?�� "t*")
	 (?�� "u")
	 (?�� "f*")
	 (?�� "x*")
	 (?�� "q*")
	 (?�� "w*")
	 (?�� "\"i")
	 (?�� "\"u")
	 (?�� "'o")
	 (?�� "'u")
	 (?�� "'w")))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?�ﾁ "A")
	 (?�ﾂ "B")
	 (?�ﾅ "E")
	 (?�ﾆ "Z")
	 (?�ﾇ "H")
	 (?�ﾉ "I")
	 (?�ﾊ "J")
	 (?�ﾌ "M")
	 (?�ﾍ "N")
	 (?�ﾏ "O")
	 (?�ﾑ "P")
	 (?�ﾔ "T")
	 (?�ﾕ "Y")
	 (?�ﾗ "X")
	 (?�� "o")))))

   ((eq set 'hebrew)
    (when (or force
	      (not (latin1-display-check-font set)))
      ;; Don't start with identities, since we don't have definitions
      ;; for a lot of Hebrew in internal.el.  (Intlfonts is also
      ;; missing some glyphs.)
      (let ((i 34))
	(while (<= i 62)
	  (aset standard-display-table
		(make-char 'hebrew-iso8859-8 i)
		(vector (make-char 'latin-iso8859-1 i)))
	  (setq i (1+ i))))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?衣 "=2")
	 (?謂 "A+")
	 (?違 "B+")
	 (?遺 "G+")
	 (?医 "D+")
	 (?井 "H+")
	 (?亥 "W+")
	 (?域 "Z+")
	 (?育 "X+")
	 (?郁 "Tj")
	 (?磯 "J+")
	 (?一 "K%")
	 (?壱 "K+")
	 (?溢 "L+")
	 (?逸 "M%")
	 (?稲 "M+")
	 (?茨 "N%")
	 (?芋 "N+")
	 (?鰯 "S+")
	 (?允 "E+")
	 (?印 "P%")
	 (?咽 "P+")
	 (?員 "Zj")
	 (?因 "ZJ")
	 (?姻 "Q+")
	 (?引 "R+")
	 (?飲 "Sh")
	 (?淫 "T+")))))

   ((eq set 'cyrillic)
    (setq set 'cyrillic-iso)
    (when (or force
	      (not (latin1-display-check-font set)))
      (mapc
       (lambda (l)
	 (apply 'latin1-display-char l))
       '((?犬 "Dj")
	 (?献 "Gj")
	 (?研 "IE")
	 (?見 "Lj")
	 (?謙 "Nj")
	 (?賢 "Ts")
	 (?軒 "Kj")
	 (?鍵 "V%")
	 (?険 "Dzh")
	 (?験 "B=")
	 (?元 "�")
	 (?原 "D")
	 (?幻 "Z%")
	 (?弦 "3")
	 (?減 "U")
	 (?源 "J=")
	 (?現 "L=")
	 (?諺 "P=")
	 (?古 "Y")
	 (?呼 "�")
	 (?姑 "C=")
	 (?孤 "C%")
	 (?己 "S%")
	 (?庫 "Sc")
	 (?弧 "=\"")
	 (?戸 "Y=")
	 (?故 "%\"")
	 (?枯 "Ee")
	 (?湖 "Yu")
	 (?狐 "Ya")
	 (?袴 "b")
	 (?股 "v=")
	 (?胡 "g=")
	 (?菰 "g")
	 (?誇 "z%")
	 (?跨 "z=")
	 (?鈷 "u")
	 (?雇 "j=")
	 (?顧 "k")
	 (?鼓 "l=")
	 (?五 "m=")
	 (?互 "n=")
	 (?午 "n")
	 (?呉 "p")
	 (?娯 "t=")
	 (?御 "f=")
	 (?梧 "c=")
	 (?檎 "c%")
	 (?瑚 "s%")
	 (?碁 "sc")
	 (?語 "='")
	 (?誤 "y=")
	 (?護 "%'")
	 (?醐 "ee")
	 (?乞 "yu")
	 (?鯉 "ya")
	 (?交 "N0")
	 (?侯 "dj")
	 (?候 "gj")
	 (?倖 "ie")
	 (?勾 "lj")
	 (?厚 "nj")
	 (?口 "ts")
	 (?向 "kj")
	 (?�� "v%")
	 (?�� "dzh")))
      (mapc
       (lambda (l)
	 (aset standard-display-table (car l) (string-to-vector (cadr l))))
       '((?牽 "⇒")
	 (?硯 "S")
	 (?絹 "I")
	 (?県 "�ﾏ")
	 (?肩 "J")
	 (?佼 "��")
	 (?�� "〒")
	 (?遣 "-")
	 (?顕 "A")
	 (?鹸 "B")
	 (?厳 "E")
	 (?玄 "K")
	 (?絃 "M")
	 (?舷 "H")
	 (?言 "O")
	 (?限 "P")
	 (?乎 "C")
	 (?個 "T")
	 (?固 "X")
	 (?糊 "a")
	 (?虎 "e")
	 (?伍 "o")
	 (?吾 "c")
	 (?後 "y")
	 (?悟 "x")
	 (?光 "s")
	 (?公 "i")
	 (?功 "��")
	 (?効 "j")))))

   (t (error "Unsupported character set: %S" set)))
   
  (sit-for 0))

(provide 'latin1-disp)

;;; latin1-disp.el ends here
