@echo off
set R_script="%~3"

%R_script% "prec_summary.r" "%~1" "%~2"