@echo off

pushd %~dp0

:start

python "OCAT - Overlay Display.py" "%~1" "%~n1" "%~dp1
::	curiously I cannot close for the path
::	this will open the script and pass it those three arguments
::		full file path and name, file name, file path
::	these arguments are then passed through as sys.argv which can be called in Python

shift

if "%~1"=="" goto end
goto start

:end

::pause