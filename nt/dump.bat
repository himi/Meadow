@echo off
if exist Meadow.dmp del Meadow.dmp
md obj
cd obj
md etc
md exe
cd exe

copy ..\..\Meadow.exe .
copy ..\..\..\etc\DOC-X ..\etc\DOC-X
.\Meadow -nd -batch -l loadup dump
copy emacs.dmp ..\..\Meadow.dmp
copy fns-*.el ..\..
cd ..\..
echo y | rd /S obj
