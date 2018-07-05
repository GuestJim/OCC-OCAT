@echo off

pushd %~dp0"

:start

python "OCAT - Overlay Frame.py" "%~1" "%~n1" "%~dp1
python "OCAT - Overlay Display.py" "%~1" "%~n1" "%~dp1
::	curiously I cannot close for the path
::	this will open the script and pass it those three arguments
::	these arguments are then passed through as sys.argv which can be called in Python

shift
::	shift moves to the next set of Batch Parameters, allowing this to work when multiple files are dragged and dropped onto it

if "%~1"=="" goto end
goto start

:end

::pause
