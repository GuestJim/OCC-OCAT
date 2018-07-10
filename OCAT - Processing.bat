@echo off
::	keeps the console window clean by suppressing the input commands

pushd %~dp0
::	pushes the working directory

:start
::	a label named start

python "OCAT - Processing.py" "%~1" "%~n1" "%~dp1
::	curiously I cannot close for the path
::	this will open the script and pass it those three arguments
::		full file path and name, file name, file path
::	these arguments are then passed through as sys.argv which can be called in Python

shift
::	shift moves to the next set of Batch Parameters, allowing this to work when multiple files are dragged and dropped onto it

if "%~1"=="" goto end
goto start
::	checks if there are more files that were dragged-and-dropped onto the batch file to process and acts accordingly

:end
::	a label named end that will bring the script to its end

::pause
::	pause is useful for troubleshooting so I tend to leave it commented out for future use
