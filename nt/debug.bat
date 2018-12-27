@echo off 
REM !!! Warning: This file automatically generated !!! 
set emacs_dir=d:\wen32a\Meadow

REM Here begins debug.bat.in

REM Set OS specific values.
REM   none needed
  
REM Use new proxy shell by default.
set SHELL=%emacs_dir%\bin\cmdproxy.exe

set EMACSLOADPATH=%emacs_dir%\site-lisp;%emacs_dir%\lisp
set EMACSDATA=%emacs_dir%\etc
set EMACSPATH=%emacs_dir%\bin
set EMACSLOCKDIR=%emacs_dir%\lock
set INFOPATH=%emacs_dir%\info
set EMACSDOC=%emacs_dir%\etc
set TERM=CMD

REM The variable HOME is used to find the startup file, ~\_emacs.  Ideally,
REM this will not be set in this file but should already be set before
REM this file is invoked.  If HOME is not set, use some generic default.

set HOME_SAVE=%HOME%
set HOME_EXISTS=yes
set HOME_DEFAULT=C:\
set HOME=
if "%HOME%" == "%HOME_SAVE%" set HOME_EXISTS=no
if "%HOME_EXISTS%" == "yes" set HOME=%HOME_SAVE%
if "%HOME_EXISTS%" == "no" set HOME=%HOME_DEFAULT%
if "%HOME_EXISTS%" == "no" echo HOME is not set!  Using %HOME% as a default...

start msdev -nologo %emacs_dir%\bin\emacs.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
