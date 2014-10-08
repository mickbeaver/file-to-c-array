@ECHO ON
PUSHD %~dp0
sbcl.exe --load build.lisp
IF ERRORLEVEL 1 GOTO FATAL_ERROR
.\file-to-c-array.exe -i %0 -o %0%.c
POPD
EXIT /B 0

:FATAL_ERROR
EXIT /B 1
