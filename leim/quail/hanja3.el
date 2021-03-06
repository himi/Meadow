;;; hanja3.el --- Quail-package for Korean Hanja (KSC5601)

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Koaunghi Un <koanughi.un@zdv.uni-tuebingen.de>
;; Keywords: mule, quail, multilingual, input method, Korean, Hanja

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

(require 'quail)

(quail-define-package 
 "korean-hanja3" "Korean" "$(CyS(B3" t
 "3$(C9z=D(BKSC$(CySm.(B: $(Cz1SWGO4B(B $(CySm.@G(B $(Cj$@;(B $(CGQ1[(B3$(C9zcR@87N(B $(C{<usGO?)(B $(C`TwI(B"
		      nil nil nil nil nil nil t)

(quail-define-rules
 ("kf" "$(CJ!J"J#J$J%J&J'J(J)J*J+J,J-J.J/J0J1J2J3J4J5J6J7J8J9J:J;J<J=(B")
 ("kfx" "$(CJ>J?J@JAJBJCJDJEJFJGJH(B")
 ("kfs" "$(CJIJJJKJLJMJNJOJPJQJRJSJTJUJVJWJXJYJZJ[J\J]J^J_J`(B")
 ("kfw" "$(CJaJbJcJdJeJfJgJhJiJj(B")
 ("kfz" "$(CJkJlJmJnJoJpJqJrJsJtJuJvJwJxJyJzJ{J|J}J~(B")
 ("kf3" "$(CK!K"K#K$K%K&(B")
 ("kfa" "$(CK'K(K)K*K+K,K-K.K/K0K1K2K3K4K5K6K7K8K9K:K;K<K=K>(B")
 ("kr" "$(CK?K@KAKBKCKDKEKFKGKHKIKJKKKLKMKNKOKPKQKR(B")
 ("krx" "$(CKSKT(B")
 ("kra" "$(CKUKVKWKX(B")
 ("k6x" "$(CKY(B")
 ("kt" "$(CKZK[K\K]K^K_K`KaKbKcKdKeKfKgKhKiKj(B")
 ("kts" "$(CKkKlKmKnKoKpKqKrKsKtKuKv(B")
 ("ktw" "$(CKwKxKyKz(B")
 ("ktz" "$(CK{K|K}K~L!L"L#(B")
 ("kt3" "$(CL$L%L&(B")
 ("kc" "$(CL'L(L)(B")
 ("kex" "$(CL*L+L,L-L.L/L0(B")
 ("kes" "$(CL1L2L3L4L5L6L7L8L9L:L;(B")
 ("kew" "$(CL<L=L>L?L@LA(B")
 ("kez" "$(CLBLCLDLELFLG(B")
 ("kea" "$(CLHLILJLKLLLMLNLOLPLQLRLSLTLULVLWLXLYLZL[L\L]L^L_L`LaLbLcLdLeLfLgLhLiLjLkLlLmLnLoLpLqLrLsLt(B")
 ("k7" "$(CLuLvLwLxLyLzL{L|L}L~M!M"M#M$M%M&M'M(M)M*M+M,M-M.(B")
 ("k/" "$(CM/M0M1M2M3M4M5M6M7M8M9M:M;M<M=M>M?M@MAMBMCMDMEMFMGMHMIMJMKMLMMMNMOMPMQMRMSMTMU(B")
 ("k/x" "$(CMVMWMXMYMZM[M\(B")
 ("k/s" "$(CM]M^M_M`MaMbMcMdMeMf(B")
 ("k/w" "$(CMgMhMi(B")
 ("k/a" "$(CMjMkMlMmMnMoMpMqMrMsMtMuMvMwMxMy(B")
 ("k/!" "$(CMz(B")
 ("k/f" "$(CM{M|M}M~N!N"N#N$N%N&N'N((B")
 ("k/fx" "$(CN)N*N+N,(B")
 ("k/fs" "$(CN-N.N/N0N1N2N3N4N5N6N7N8N9N:N;N<N=(B")
 ("k/fw" "$(CN>N?N@NA(B")
 ("k/fa" "$(CNBNCNDNENFNGNHNINJNKNLNMNN(B")
 ("k/r" "$(CNONPNQ(B")
 ("k/d" "$(CNRNSNTNUNVNWNXNYNZ(B")
 ("k/da" "$(CN[N\N]N^(B")
 ("k4" "$(CN_N`NaNbNcNdNeNfNgNhNiNjNkNlNmNnNoNpNqNrNsNtNuNvNw(B")
 ("k9" "$(CNxNyNzN{N|N}N~O!O"O#O$O%O&O'O(O)O*O+O,O-O.O/O0O1O2O3O4O5O6O7O8O9O:O;O<O=O>O?O@OAOBOCODOEOFOGOHOIOJOKOLOMONOO(B")
 ("k9x" "$(COPOQOROSOTOU(B")
 ("k9s" "$(COVOWOXOYOZO[(B")
 ("k9w" "$(CO\O]O^O_(B")
 ("k9a" "$(CO`OaObOcOdOe(B")
 ("k9ts" "$(COfOgOhOiOjOkOlOmOnOo(B")
 ("k9tw" "$(COpOqOrOsOt(B")
 ("k9c" "$(COuOvOwOxOyOz(B")
 ("k9d" "$(CO{O|O}O~P!P"(B")
 ("k5" "$(CP#P$P%P&P'P(P)P*P+P,P-P.P/P0P1(B")
 ("k5s" "$(CP2P3P4P5P6P7P8(B")
 ("k5w" "$(CP9(B")
 ("kgx" "$(CP:P;P<P=P>P?P@(B")
 ("kgs" "$(CPAPBPCPDPEPFPGPHPIPJPKPLPMPNPO(B")
 ("kgw" "$(CPP(B")
 ("kgz" "$(CPQPRPSPTPUPVPWPXPYPZP[P\P]P^(B")
 ("kg3" "$(CP_P`PaPbPcPdPe(B")
 ("kga" "$(CPfPgPhPi(B")
 ("kd" "$(CPjPkPlPmPnPoPpPqPrPsPtPuPvPwPxPyPzP{P|P}P~Q!Q"Q#Q$Q%Q&Q'Q(Q)Q*Q+Q,Q-Q.Q/Q0Q1Q2Q3Q4Q5Q6Q7Q8Q9Q:Q;Q<Q=Q>Q?Q@QAQBQCQDQEQFQGQHQIQJQK(B")
 ("kds" "$(CQL(B")
 ("kdw" "$(CQMQNQOQP(B")
 ("kdz" "$(CQQ(B")
 ("kkdx" "$(CQR(B")
 ("hf" "$(CQSQTQUQVQWQXQYQZQ[Q\Q]Q^Q_Q`Qa(B")
 ("hfx" "$(CQbQcQdQeQfQgQhQi(B")
 ("hfs" "$(CQjQkQlQmQnQoQpQqQr(B")
 ("hfw" "$(CQsQt(B")
 ("hfz" "$(CQuQvQwQxQyQzQ{Q|Q}(B")
 ("hf3" "$(CQ~R!R"R#R$(B")
 ("hfa" "$(CR%R&R'R(R)R*R+(B")
 ("hr" "$(CR,R-R.R/R0R1(B")
 ("hra" "$(CR2(B")
 ("he" "$(CR3(B")
 ("hes" "$(CR4R5R6(B")
 ("hez" "$(CR7R8R9R:(B")
 ("hea" "$(CR;R<(B")
 ("h/" "$(CR=R>R?R@RARBRCRDRERFRGRHRIRJRKRLRMRN(B")
 ("h/x" "$(CRORPRQRRRSRT(B")
 ("h/s" "$(CRU(B")
 ("h/a" "$(CRVRWRXRYRZR[R\(B")
 ("h/d" "$(CR]R^R_R`RaRb(B")
 ("h4" "$(CRc(B")
 ("h9" "$(CRdReRfRgRhRiRjRk(B")
 ("h9s" "$(CRl(B")
 ("h9w" "$(CRm(B")
 ("h5" "$(CRnRo(B")
 ("hgx" "$(CRpRq(B")
 ("hgz" "$(CRr(B")
 ("hga" "$(CRsRtRuRvRwRx(B")
 ("hd" "$(CRyRz(B")
 ("hdx" "$(CR{R|(B")
 ("uf" "$(CR}R~(B")
 ("ufs" "$(CS!S"S#S$S%S&S'S(S)S*S+S,S-S.S/S0S1S2S3S4(B")
 ("ufw" "$(CS5S6S7S8S9(B")
 ("ufz" "$(CS:S;S<S=S>S?S@SASBSCSDSESFSGSHSISJ(B")
 ("uf3" "$(CSKSLSMSNSO(B")
 ("ufa" "$(CSPSQSRSSSTSUSVSWSXSYSZ(B")
 ("ur" "$(CS[S\S]S^S_S`SaSbScSdSeSfSgShSiSj(B")
 ("urx" "$(CSk(B")
 ("utx" "$(CSlSm(B")
 ("u/" "$(CSnSoSpSqSrSsStSuSvSwSxSySzS{S|S}S~T!T"T#T$T%T&T'T(T)T*T+T,T-T.T/T0T1T2T3T4T5T6T7(B")
 ("u/x" "$(CT8T9T:T;T<T=T>T?T@TA(B")
 ("u/s" "$(CTBTCTDTETFTGTHTITJTK(B")
 ("u/w" "$(CTLTM(B")
 ("u/a" "$(CTNTOTPTQTRTSTTTUTVTWTXTYTZT[T\T]T^(B")
 ("u9" "$(CT_T`TaTbTcTdTeTfTgThTi(B")
 ("u9s" "$(CTjTkTlTmTnTo(B")
 ("ugx" "$(CTp(B")
 ("uga" "$(CTqTrTsTtTuTvTwTxTy(B")
 ("yf" "$(CTzT{T|T}T~U!U"U#U$(B")
 ("yfx" "$(CU%U&U'U(U)U*U+U,U-(B")
 ("yfs" "$(CU.U/U0U1U2U3U4U5U6(B")
 ("yfw" "$(CU7U8(B")
 ("yfz" "$(CU9U:U;U<U=U>U?U@UAUB(B")
 ("yf3" "$(CUCUDUE(B")
 ("yfa" "$(CUFUGUHUIUJUKULUM(B")
 ("yr" "$(CUNUOUPUQ(B")
 ("yra" "$(CUR(B")
 ("y6x" "$(CUSUT(B")
 ("y6a" "$(CUUUVUWUXUYUZU[U\U]U^U_U`Ua(B")
 ("ye" "$(CUbUcUdUeUfUgUhUiUjUkUlUmUnUoUpUqUrUs(B")
 ("yex" "$(CUtUuUvUwUxUyUz(B")
 ("yes" "$(CU{U|U}U~V!V"V#V$V%V&V'V((B")
 ("yew" "$(CV)V*V+V,V-V.(B")
 ("yez" "$(CV/V0V1V2V3(B")
 ("ye3" "$(CV4(B")
 ("yea" "$(CV5V6V7V8V9V:V;V<V=V>V?V@VAVBVCVDVEVF(B")
 ("y7" "$(CVGVHVIVJVK(B")
 ("y/" "$(CVLVMVNVOVPVQVRVSVTVUVVVWVXVYVZV[V\V](B")
 ("y/x" "$(CV^V_V`VaVbVcVd(B")
 ("y/s" "$(CVe(B")
 ("y/a" "$(CVfVgVhViVjVkVl(B")
 ("y/d" "$(CVmVnVoVpVqVrVsVt(B")
 ("y4" "$(CVuVvVwVxVyVzV{V|V}V~W!W"(B")
 ("y4a" "$(CW#(B")
 ("y9" "$(CW$W%W&W'W(W)W*W+W,W-W.W/W0(B")
 ("y5" "$(CW1W2W3W4W5W6W7W8W9W:W;W<W=W>(B")
 ("y5x" "$(CW?W@WA(B")
 ("y5s" "$(CWBWCWDWEWFWG(B")
 ("y5w" "$(CWHWIWJWK(B")
 ("y5a" "$(CWL(B")
 ("ygx" "$(CWMWN(B")
 ("ygz" "$(CWO(B")
 ("yga" "$(CWPWQWRWSWTWU(B")
 ("yd" "$(CWVWWWXWYWZW[W\W]W^W_W`WaWbWcWdWeWfWgWhWiWjWkWlWmWnWo(B")
 ("yds" "$(CWpWqWrWsWtWuWvWwWx(B")
 ("ydz" "$(CWyWzW{W|W}(B")
 ("yd3" "$(CW~X!X"X#(B")
 ("if" "$(CX$X%X&X'X(X)X*X+(B")
 ("ifx" "$(CX,X-X.X/X0X1(B")
 ("ifs" "$(CX2X3X4X5X6X7X8X9X:X;X<X=X>X?X@XAXBXCXD(B")
 ("ifw" "$(CXEXFXGXHXIXJXK(B")
 ("ifa" "$(CXLXMXNXOXPXQXRXSXTXUXVXW(B")
 ("ir" "$(CXXXYXZX[X\X]X^X_X`XaXbXcXdXe(B")
 ("irx" "$(CXfXgXhXiXj(B")
 ("ira" "$(CXkXlXmXnXoXp(B")
 ("iex" "$(CXqXr(B")
 ("ies" "$(CXsXtXuXvXwXxXyXzX{X|X}(B")
 ("iew" "$(CX~Y!(B")
 ("iea" "$(CY"Y#Y$Y%Y&Y'Y(Y)Y*Y+Y,Y-Y.Y/Y0(B")
 ("i7" "$(CY1(B")
 ("i/" "$(CY2Y3Y4Y5Y6Y7Y8Y9Y:Y;Y<Y=Y>Y?Y@YAYBYCYDYEYFYGYHYI(B")
 ("i/x" "$(CYJYKYLYMYNYOYP(B")
 ("i/w" "$(CYQYR(B")
 ("i/a" "$(CYSYTYU(B")
 ("i4" "$(CYVYWYXYYYZY[Y\Y]Y^Y_Y`Ya(B")
 ("i9" "$(CYbYcYdYeYfYgYhYiYjYkYlYmYnYoYpYqYrYsYtYuYvYw(B")
 ("i9x" "$(CYxYy(B")
 ("i9s" "$(CYzY{Y|Y}Y~Z!Z"Z#Z$Z%Z&Z'(B")
 ("i9w" "$(CZ(Z)Z*(B")
 ("id" "$(CZ+Z,Z-Z.Z/Z0Z1Z2Z3Z4Z5Z6Z7Z8Z9Z:Z;Z<Z=(B")
 ("ids" "$(CZ>Z?Z@ZAZBZCZDZEZFZGZHZIZJ(B")
 ("idw" "$(CZKZLZM(B")
 (";fx" "$(CZNZOZPZQZRZSZTZUZVZWZXZYZZZ[Z\Z]Z^Z_Z`(B")
 (";fs" "$(CZaZbZcZdZeZfZgZhZiZjZkZlZmZnZoZpZqZrZsZtZuZvZwZxZy(B")
 (";fw" "$(CZzZ{Z|Z}Z~[!["[#[$[%[&(B")
 (";fa" "$(C['[([)[*[+[,[-[.[/[0[1[2[3[4[5[6[7[8[9[:[;[<[=[>[?[@[A[B(B")
 (";r" "$(C[C[D[E[F[G[H[I[J[K[L[M[N[O[P[Q[R[S[T[U[V(B")
 (";rx" "$(C[W[X[Y[Z[[[\[][^(B")
 (";ts" "$(C[_[`[a[b[c[d[e[f[g[h(B")
 (";tw" "$(C[i[j[k[l(B")
 (";tz" "$(C[m[n[o[p[q[r[s[t[u(B")
 (";t3" "$(C[v[w(B")
 (";ex" "$(C[x[y[z[{[|[}[~\!\"\#\$(B")
 (";es" "$(C\%\&\'\(\)\*\+(B")
 (";ew" "$(C\,\-\.\/(B")
 (";ea" "$(C\0\1\2\3\4\5\6\7\8\9\:\;\<\=\>\?\@(B")
 (";/" "$(C\A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P(B")
 (";/x" "$(C\Q\R\S\T\U\V\W\X\Y\Z\[\\\]\^\_\`\a(B")
 (";/s" "$(C\b(B")
 (";/w" "$(C\c(B")
 (";/a" "$(C\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s(B")
 (";9" "$(C\t\u\v\w\x\y\z\{\|\}\~]!]"]#]$]%]&]'](])]*]+],]-].]/]0]1]2]3]4]5]6]7]8]9]:];]<]=]>]?]@(B")
 (";9x" "$(C]A(B")
 (";9s" "$(C]B]C]D]E]F]G]H]I]J]K]L]M]N]O]P]Q]R]S]T(B")
 (";9w" "$(C]U]V]W]X]Y(B")
 (";9a" "$(C]Z][]\]]]^]_(B")
 (";d" "$(C]`]a]b]c]d]e]f]g]h]i]j]k]l]m]n]o]p]q]r]s]t]u]v]w]x]y]z]{]|]}]~^!^"^#^$^%^&^'^(^)^*^+^,(B")
 (";ds" "$(C^-^.^/^0^1^2^3^4^5^6^7^8^9^:(B")
 (";da" "$(C^;^<^=^>(B")
 ("nf" "$(C^?^@^A^B^C^D^E^F^G^H^I^J^K^L^M^N^O^P^Q^R^S^T^U^V^W^X^Y^Z^[^\^]^^^_^`^a^b^c^d^e^f^g^h^i^j^k^l^m^n^o^p^q^r^s^t^u^v^w^x^y^z(B")
 ("nfx" "$(C^{^|^}^~(B")
 ("nfs" "$(C_!_"_#_$_%_&_'_(_)_*_+_,(B")
 ("nfw" "$(C_-_._/_0_1(B")
 ("nfz" "$(C_2_3_4_5_6_7_8_9(B")
 ("nf3" "$(C_:_;_<_=(B")
 ("nfa" "$(C_>_?_@_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W_X_Y_Z_[_\(B")
 ("nr" "$(C_]_^__(B")
 ("nrx" "$(C_`_a_b_c_d(B")
 ("nra" "$(C_e_f_g_h_i(B")
 ("nt" "$(C_j_k_l_m_n_o_p_q_r_s_t_u_v_w_x_y_z_{_|_}_~`!`"`#`$`%`&`'`(`)(B")
 ("ntx" "$(C`*`+`,`-`.`/`0`1`2`3`4`5`6`7`8(B")
 ("nts" "$(C`9`:`;`<`=`>`?`@`A`B`C`D`E`F`G`H`I`J`K`L`M`N`O`P`Q`R`S`T`U`V`W`X(B")
 ("ntw" "$(C`Y`Z`[`\`]`^`_```a`b`c`d`e(B")
 ("ntz" "$(C`f`g`h`i`j`k`l`m(B")
 ("nt3" "$(C`n`o`p`q(B")
 ("nta" "$(C`r`s`t`u`v`w`x`y`z`{`|`}`~a!a"a#a$a%(B")
 ("nc" "$(Ca&a'a(a)a*a+a,a-a.(B")
 ("n/" "$(Ca/a0a1a2a3a4a5a6a7a8a9a:a;a<a=a>a?a@aAaBaCaDaEaFaGaHaIaJaKaLaMaNaOaPaQaRaS(B")
 ("n/x" "$(CaTaUaVaWaXaYaZa[a\(B")
 ("n/s" "$(Ca]a^a_a`aaab(B")
 ("n/w" "$(Cac(B")
 ("n/a" "$(Cadaeafagahaiajak(B")
 ("n/r" "$(Calamanaoap(B")
 ("n/d" "$(Caqar(B")
 ("n9" "$(Casatauavawaxayaza{a|a}a~b!b"b#b$b%b&b'b(b)b*b+b,b-b.b/b0b1b2b3b4b5b6b7b8b9b:b;b<b=b>b?b@bAbBbCbDbEbFbGbHbIbJbKbLbMbNbObPbQ(B")
 ("n9x" "$(CbRbSbTbUbVbWbXbYbZb[b\b](B")
 ("n9s" "$(Cb^b_b`babbbcbdbebfbgbhbibjbkblbmbnbobpbqbrbsbtbubvbwbx(B")
 ("n9w" "$(Cbybzb{b|(B")
 ("n9a" "$(Cb}b~c!(B")
 ("ngw" "$(Cc"c#c$(B")
 ("ng3" "$(Cc%c&c'c(c)(B")
 ("nga" "$(Cc*c+c,c-c.c/c0c1c2c3(B")
 ("nd" "$(Cc4c5c6c7c8c9c:c;c<c=c>c?c@cAcBcCcDcEcFcGcHcIcJcKcLcMcNcO(B")
 ("ndx" "$(CcPcQcRcScTcUcVcWcXcYcZc[c\c]c^(B")
 ("nds" "$(Cc_c`cacbcccdcecfcgchcicjckclcmcncocpcqcrcsctcucv(B")
 ("ndw" "$(Ccwcxcycz(B")
 ("ndz" "$(Cc{c|c}c~d!d"d#d$d%d&(B")
 ("nd3" "$(Cd'd(d)(B")
 ("nnfa" "$(Cd*(B")
 ("nnd" "$(Cd+(B")
 ("jf" "$(Cd,d-d.d/d0d1d2d3d4d5d6d7d8d9d:d;d<d=(B")
 ("jfx" "$(Cd>d?d@dAdBdCdDdEdFdGdHdIdJdK(B")
 ("jfs" "$(CdLdMdNdOdPdQdRdSdTdU(B")
 ("jfw" "$(CdVdWdXdY(B")
 ("jfz" "$(CdZd[d\d]d^d_d`da(B")
 ("jf3" "$(Cdbdcddde(B")
 ("jfa" "$(Cdfdgdhdidjdkdl(B")
 ("jr" "$(Cdmdndodpdqdrdsdtdudvdw(B")
 ("jrx" "$(Cdxdydzd{d|d}d~(B")
 ("jra" "$(Ce!e"e#e$(B")
 ("j6" "$(Ce%e&e'e(e)e*e+e,e-e.e/(B")
 ("j6x" "$(Ce0e1e2e3e4e5e6e7e8(B")
 ("j6a" "$(Ce9e:e;e<e=e>e?e@eAeBeCeDeEeFeGeHeIeJeKeLeMeNeOePeQeReSeTeUeVeW(B")
 ("jt" "$(CeXeYeZe[e\e]e^e_e`ea(B")
 ("jtx" "$(Cebecedeeef(B")
 ("jts" "$(Cegeheiejekel(B")
 ("jtw" "$(Cemen(B")
 ("jtz" "$(Ceoepeqereset(B")
 ("jt3" "$(Ceuev(B")
 ("jcs" "$(Cew(B")
 ("je" "$(Cexeyeze{e|e}e~f!f"f#f$f%f&f'f(f)f*f+f,f-f.f/f0f1(B")
 ("jex" "$(Cf2f3f4f5f6f7f8f9f:f;f<f=f>(B")
 ("jes" "$(Cf?f@fAfBfCfDfEfFfGfHfIfJfKfLfMfNfOfPfQfRfSfTfUfVfWfXfYfZf[f\f]f^f_f`fafbfcfdfefffgfhfi(B")
 ("jew" "$(Cfjfkflfmfnfofpfqfrfs(B")
 ("jez" "$(Cftfufvfwfxfyfzf{f|f}f~g!g"g#g$(B")
 ("je3" "$(Cg%g&g'g((B")
 ("jea" "$(Cg)g*g+g,g-g.g/g0g1g2g3g4g5g6g7g8g9g:g;g<g=g>g?g@gAgBgCgDgEgFgGgHgIgJgKgLgMgNgOgP(B")
 ("j7" "$(CgQgRgSgTgUgVgWgXgYgZg[g\g]g^g_g`gagbgcgdgegfgggh(B")
 ("j/" "$(Cgigjgkglgmgngogpgqgrgsgtgugvgwgxgygzg{g|g}g~h!h"h#h$h%h&h'h((B")
 ("j/x" "$(Ch)h*h+h,h-(B")
 ("j/s" "$(Ch.h/h0h1h2h3(B")
 ("j/w" "$(Ch4(B")
 ("j/a" "$(Ch5h6h7h8h9h:h;h<h=(B")
 ("j/f" "$(Ch>h?h@hAhBhChDhE(B")
 ("j/fs" "$(ChFhGhHhIhJhKhLhMhNhOhPhQhRhShThUhVhW(B")
 ("j/fw" "$(ChX(B")
 ("j/fa" "$(ChYhZh[h\h](B")
 ("j/r" "$(Ch^h_h`ha(B")
 ("j/d" "$(Chbhchdhehf(B")
 ("j4" "$(Chghhhihjhkhlhmhnhohphqhrhshthuhvhwhxhyhzh{h|h}h~i!i"i#i$i%i&i'i(i)i*i+i,i-i.(B")
 ("j4x" "$(Ci/i0i1i2i3i4(B")
 ("j4a" "$(Ci5i6i7i8i9i:i;i<i=i>i?i@iAiBiCiDiEiFiGiHiIiJiKiL(B")
 ("j9" "$(CiMiNiOiPiQiRiSiTiUiViWiXiYiZi[i\i]i^i_i`iaibicidieifigihiiijikil(B")
 ("j9x" "$(Ciminioipiqirisitiu(B")
 ("j9s" "$(Civiwixiyizi{i|i}i~j!j"j#j$(B")
 ("j9w" "$(Cj%j&j'(B")
 ("j9a" "$(Cj(j)(B")
 ("j9ts" "$(Cj*j+j,j-j.j/j0j1j2j3j4j5j6j7j8j9j:j;j<j=j>j?j@jAjBjCjD(B")
 ("j9tw" "$(CjEjFjG(B")
 ("j9d" "$(CjHjIjJjKjLjMjNjOjPjQjRjSjTjUjVjWjXjYjZj[j\j]j^j_j`(B")
 ("j5" "$(Cjajbjcjdjejfjgjhjijjjkjljmjnjojpjqjrjsjtjujvjwjxjyjzj{j|j}j~k!k"k#k$k%k&k'k(k)k*k+k,k-k.k/k0k1k2k3k4k5k6k7k8k9k:(B")
 ("j5x" "$(Ck;k<k=k>k?k@kA(B")
 ("j5s" "$(CkBkCkDkEkFkGkHkIkJkKkLkMkN(B")
 ("j5w" "$(CkOkPkQkRkS(B")
 ("j5a" "$(CkTkUkVkWkX(B")
 ("jgs" "$(CkYkZk[k\k]k^k_(B")
 ("jgw" "$(Ck`(B")
 ("jgz" "$(Ckakbkckdkekf(B")
 ("jg3" "$(Ckgkhki(B")
 ("jga" "$(Ckjkkklkm(B")
 ("j8" "$(Cknkokpkqkrksktkukvkwkxkykzk{k|k}k~l!l"(B")
 ("jd" "$(Cl#l$l%l&l'l(l)l*l+l,l-l.l/l0l1l2l3l4l5l6l7l8l9l:l;l<l=l>l?l@lAlBlClDlElFlGlH(B")
 ("jdx" "$(ClIlJlKlLlMlNlOlP(B")
 ("jds" "$(ClQlRlSlTlUlVlWlXlYlZl[l\l]l^l_l`lalblcldlelflglh(B")
 ("jdw" "$(Cliljlklllmlnlolplq(B")
 ("jdz" "$(Clrlsltlulvlwlxlylzl{l|(B")
 ("jd3" "$(Cl}l~m!m"m#(B")
 ("jda" "$(Cm$m%m&m'(B")
 ("lf" "$(Cm(m)m*m+m,m-m.m/m0m1m2m3m4m5m6m7m8m9m:m;m<m=m>m?m@mA(B")
 ("lfx" "$(CmBmCmDmEmFmGmHmImJmKmLmMmN(B")
 ("lfs" "$(CmOmPmQmRmS(B")
 ("lfz" "$(CmTmUmVmWmXmY(B")
 ("lf3" "$(CmZ(B")
 ("lfa" "$(Cm[m\m]m^m_m`mambmcmdmemfmgmhmimjmkmlmmmnmompmqmrmsmtmumvmwmxmymzm{m|m}m~n!(B")
 ("" "$(Cn3n4n5n6(B")
 ("lr" "$(Cn"n#n$n%n&n'n(n)n*n+n,n-n.n/n0n1n2(B")
 ("lt" "$(Cn7n8n9n:n;n<n=n>n?n@nAnBnCnDnEnFnGnHnInJnKnLnMnNnOnPnQnR(B")
 ("ltx" "$(CnSnTnUnVnWnXnYnZn[n\n]n^n_n`nanbncndnenfngnhninjnk(B")
 ("lts" "$(Cnlnmnnnonpnqnrnsntnunvnwnxnynzn{n|n}n~o!o"o#o$o%o&o'o(o)o*o+o,o-o.o/o0o1o2o3o4o5o6(B")
 ("ltw" "$(Co7o8o9o:o;o<o=o>(B")
 ("ltz" "$(Co?o@oAoBoCoDoEoFoG(B")
 ("lt3" "$(CoHoIoJ(B")
 ("lta" "$(CoKoLoMoNoOoPoQoRoSoToUoVoWoXoYoZo[o\o]o^o_o`oaobocodoeofogohoiojokolomonooopoqorosotouovowoxoyozo{o|o}o~p!p"p#(B")
 ("lc" "$(Cp$p%p&p'p(p)p*p+p,p-p.p/p0p1p2p3p4p5p6p7p8p9p:(B")
 ("l/" "$(Cp;p<p=p>p?p@pApBpCpDpEpFpGpHpIpJpKpLpMpNpOpPpQpRpSpTpUpVpWpXpYpZp[p\p]p^p_p`papbpcpdpepfpgph(B")
 ("l/x" "$(Cpipjpkpl(B")
 ("l/s" "$(Cpmpn(B")
 ("l/w" "$(Cpopppq(B")
 ("l/a" "$(Cprpsptpupvpwpxpypzp{p|p}p~q!q"q#q$(B")
 ("l/f" "$(Cq%q&q'q(q)(B")
 ("l/d" "$(Cq*(B")
 ("l9" "$(Cq+q,q-q.q/q0q1q2q3q4q5q6q7q8q9q:q;q<q=q>q?q@qAqBqCqDqEqFqGqHqIqJqKqLqMqNqOqPqQqR(B")
 ("l9x" "$(CqSqT(B")
 ("l9s" "$(CqUqVqWqXqYqZq[q\q]q^q_q`qaqbqcqdqeqfqg(B")
 ("l9w" "$(Cqh(B")
 ("l9a" "$(Cqiqjqkql(B")
 ("lgx" "$(Cqm(B")
 ("lgw" "$(Cqn(B")
 ("lg3" "$(Cqoqpqq(B")
 ("lga" "$(Cqrqsqtquqvqwqxqyqzq{q|(B")
 ("ld" "$(Cq}q~r!r"r#r$r%r&r'r(r)r*r+r,r-r.r/r0r1r2r3r4r5r6r7r8r9r:r;r<r=r>r?r@(B")
 ("ldx" "$(CrArBrCrDrE(B")
 ("lds" "$(CrFrGrHrIrJrKrLrMrNrOrPrQrRrSrTrUrVrWrXrYrZr[r\r]r^r_r`rarbrcrdrerfrgrh(B")
 ("ldw" "$(Crirjrkrlrmrnrorprqrrrsrtrurvrw(B")
 ("ldz" "$(Crxry(B")
 ("ld3" "$(Crzr{r|r}r~s!s"(B")
 ("lda" "$(Cs#s$s%(B")
 ("of" "$(Cs&s's(s)s*s+s,s-s.s/s0s1s2s3s4(B")
 ("ofx" "$(Cs5s6s7s8s9s:s;(B")
 ("ofs" "$(Cs<s=s>s?s@sAsBsCsDsEsFsGsHsIsJ(B")
 ("ofw" "$(CsKsLsMsNsO(B")
 ("ofz" "$(CsPsQsRsSsTsUsVsWsXsY(B")
 ("ofa" "$(CsZs[s\s]s^s_s`sasbscsdsesfsgshsisjskslsmsnso(B")
 ("or" "$(Cspsqsrssstsusvswsxsyszs{(B")
 ("orx" "$(Cs|s}s~t!(B")
 ("ot" "$(Ct"t#t$t%(B")
 ("otx" "$(Ct&t't(t)t*t+t,t-t.t/t0t1t2t3t4(B")
 ("ots" "$(Ct5t6t7t8t9t:t;t<t=t>t?t@tAtBtCtDtEtFtG(B")
 ("otw" "$(CtHtItJtKtLtMtNtOtPtQ(B")
 ("otz" "$(CtRtStTtUtVtWtXtYtZt[(B")
 ("ot3" "$(Ct\t]t^t_t`tatbtctdte(B")
 ("ota" "$(Ctftgthtitjtktltm(B")
 ("oc" "$(Ctntotptqtrtstttutvtw(B")
 ("o/" "$(Ctxtytzt{t|t}t~u!u"u#u$u%u&u'u(u)u*u+u,u-u.u/u0u1u2u3u4(B")
 ("o/x" "$(Cu5u6u7u8u9u:(B")
 ("o/s" "$(Cu;u<u=u>(B")
 ("o/a" "$(Cu?u@uAuBuCuDuEuFuGuH(B")
 ("o/fw" "$(CuI(B")
 ("o/d" "$(CuJuKuL(B")
 ("o9" "$(CuMuNuOuPuQuRuSuTuUuVuWuXuYuZu[u\u]u^u_u`uaubuc(B")
 ("o9x" "$(Cudueufuguhuiujukulumunuo(B")
 ("o9s" "$(Cupuqur(B")
 ("o9w" "$(Cusutuu(B")
 ("o9a" "$(Cuvuwuxuyuzu{(B")
 ("o9c" "$(Cu|u}u~v!(B")
 ("o9d" "$(Cv"v#v$v%v&v'v(v)v*v+v,v-v.v/(B")
 ("ogx" "$(Cv0v1v2v3v4(B")
 ("oga" "$(Cv5(B")
 ("od" "$(Cv6v7v8v9v:v;v<v=v>v?v@vAvBvCvDvEvFvGvHvIvJvKvLvM(B")
 ("odx" "$(CvNvOvP(B")
 ("ods" "$(CvQ(B")
 ("odw" "$(CvRvSvT(B")
 ("odz" "$(CvUvVvWvXvYvZv[v\v](B")
 ("od3" "$(Cv^(B")
 ("oda" "$(Cv_v`(B")
 ("0/r" "$(Cva(B")
 ("'f" "$(Cvbvcvdvevfvgvhvivjvkvlvmvnvo(B")
 ("'fx" "$(Cvpvqvrvsvtvuvvvwvxvyvzv{v|v}v~w!(B")
 ("'fs" "$(Cw"w#w$w%w&w'w(w)w*w+(B")
 ("'fw" "$(Cw,w-(B")
 ("'fz" "$(Cw.w/w0w1(B")
 ("'f3" "$(Cw2w3w4(B")
 ("'fa" "$(Cw5w6w7w8w9(B")
 ("'r" "$(Cw:w;w<w=w>w?w@wAwBwCwDwEwFwG(B")
 ("'rx" "$(CwHwIwJ(B")
 ("'ra" "$(CwK(B")
 ("'t" "$(CwL(B")
 ("'/" "$(CwMwNwOwP(B")
 ("'/a" "$(CwQwRwSwTwUwVwW(B")
 ("'/d" "$(CwXwYwZw[w\w](B")
 ("'9" "$(Cw^w_w`wawbwc(B")
 ("'gx" "$(Cwdwe(B")
 ("'gz" "$(Cwf(B")
 ("pf" "$(Cwgwhwiwjwkwlwmwnwowpwqwrwswtwuwv(B")
 ("pfs" "$(Cwwwxwywzw{w|w}w~x!(B")
 ("pfw" "$(Cx"x#x$(B")
 ("pr" "$(Cx%x&x'x(x)x*x+x,x-x.x/(B")
 ("pra" "$(Cx0x1x2x3(B")
 ("p6x" "$(Cx4(B")
 ("pes" "$(Cx5x6x7x8x9x:x;x<x=x>(B")
 ("pez" "$(Cx?(B")
 ("pea" "$(Cx@xAxBxCxD(B")
 ("p7" "$(CxExFxGxHxIxJxKxLxMxN(B")
 ("p/" "$(CxOxPxQxRxSxTxUxVxWxXxYxZx[x\x]x^x_x`xaxbxcxdxexfxgxhxixj(B")
 ("p/x" "$(Cxkxlxmxnxoxp(B")
 ("p4" "$(Cxqxrxsxtxuxvxwxxxyxzx{x|x}x~(B")
 ("p9z" "$(Cy!y"(B")
 ("p9a" "$(Cy#y$y%y&y'(B")
 ("pd" "$(Cy(y)y*y+y,y-y.(B")
 ("pdw" "$(Cy/y0y1y2y3y4y5y6y7y8(B")
 ("pd3" "$(Cy9y:(B")
 ("mf" "$(Cy;y<y=y>y?y@yAyByCyDyEyFyGyH(B")
 ("mfx" "$(CyIyJyKyLyM(B")
 ("mfs" "$(CyNyOyPyQyRySyTyUyVyWyXyYyZy[(B")
 ("mfw" "$(Cy\y](B")
 ("mfz" "$(Cy^y_y`yaybycydyeyfygyhyi(B")
 ("mf3" "$(Cyjykylymynyoyp(B")
 ("mfa" "$(Cyqyrysytyuyvywyxyyyzy{y|y}y~z!z"z#(B")
 ("mr" "$(Cz$z%z&z'z(z)z*z+z,z-z.z/z0z1z2z3z4z5(B")
 ("mrx" "$(Cz6z7(B")
 ("mra" "$(Cz8z9z:z;z<(B")
 ("m6a" "$(Cz=z>z?z@zAzBzCzDzE(B")
 ("mt" "$(CzFzGzHzI(B")
 ("mts" "$(CzJzKzLzM(B")
 ("mtw" "$(CzN(B")
 ("mtz" "$(CzOzP(B")
 ("mex" "$(CzQzRzSzT(B")
 ("mes" "$(CzUzVzWzXzYzZz[z\z]z^z_z`zazbzczdzezfzgzhzi(B")
 ("mew" "$(Czjzkzlzm(B")
 ("mez" "$(Czn(B")
 ("me3" "$(Czozpzqzrzsztzuzvzwzxzyzz(B")
 ("mea" "$(Cz{z|z}z~{!{"{#{${%{&{'{({){*{+{,{-{.{/{0(B")
 ("m7" "$(C{1{2{3{4{5{6{7{8{9(B")
 ("m/" "$(C{:{;{<{={>{?{@{A{B{C{D{E{F{G{H{I{J{K{L{M{N{O{P{Q{R{S{T{U{V{W{X{Y{Z{[{\{]{^{_{`{a{b(B")
 ("m/x" "$(C{c{d{e(B")
 ("m/s" "$(C{f{g{h{i{j{k(B")
 ("m/w" "$(C{l{m{n(B")
 ("m/a" "$(C{o{p{q{r{s{t{u{v{w{x(B")
 ("m/f" "$(C{y{z{{{|{}{~|!|"|#|$|%|&|'|((B")
 ("m/fx" "$(C|)|*|+|,|-|.(B")
 ("m/fs" "$(C|/|0|1|2|3|4|5|6|7|8|9|:|;|<|=|>|?(B")
 ("m/fw" "$(C|@|A|B|C|D(B")
 ("m/fa" "$(C|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|[|\(B")
 ("m/d" "$(C|]|^|_|`|a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p(B")
 ("m/dx" "$(C|q|r(B")
 ("m/da" "$(C|s|t|u(B")
 ("m4" "$(C|v|w|x|y|z|{|||}|~}!}"}#}$(B")
 ("m9" "$(C}%}&}'}(})}*}+},}-}.}/}0}1(B")
 ("m9s" "$(C}2}3}4}5}6}7}8}9}:};(B")
 ("m9a" "$(C}<(B")
 ("m9ts" "$(C}=}>}?}@(B")
 ("m9c" "$(C}A}B}C(B")
 ("m9d" "$(C}D}E}F}G}H}I}J}K(B")
 ("m5" "$(C}L}M}N}O}P(B")
 ("m5w" "$(C}Q}R}S(B")
 ("m5a" "$(C}T}U}V}W}X(B")
 ("mgx" "$(C}Y(B")
 ("mgs" "$(C}Z}[}\}](B")
 ("mgw" "$(C}^}_}`}a(B")
 ("mgz" "$(C}b}c}d(B")
 ("mg3" "$(C}e}f}g}h(B")
 ("mga" "$(C}i(B")
 ("m8" "$(C}j}k}l}m}n}o}p}q}r}s}t}u}v}w}x}y}z}{}|}}(B")
 ("mdw" "$(C}~(B"))

;;; hanja3.el ends here
