@echo off
::	prevents the text of every command from being shown
set R_script="%~3"
::	sets the R_script local variable to the location of this script's third argument
::		the third argument from the Python script is the location of the Rscript.exe file
::		this line and this batch file are not necessary if an environment variable directed to the Rscript.exe file, but I do not wish to do that

%R_script% "prec_summary.r" "%~1" "%~2"
::	will open the prec_summary.r script with Rscript.exe and passes it two arguments
::		these arguments, from the Python script, are the path to the file dropped onto it and then a file from that folder for this R script to work on