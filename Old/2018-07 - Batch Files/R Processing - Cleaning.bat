@echo off
pushd "%~dp1"

:start

set "file=%~1"
set "file=%file:\=/%"

set "graph=%~dp1"
set "graph=%graph:\=/%"

::echo "%file%"
::pause

echo library(readr)> "Cleaning - %~n1.r"
echo library(ggplot2)>> "Cleaning - %~n1.r"
echo setwd("%graph%")>> "Cleaning - %~n1.r"
echo results ^<- read_csv("%~n1.csv")>> "Cleaning - %~n1.r"
echo results = results[which(results$Application!=""),]	#mystery unnamed application >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="explorer.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="OCAT.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="Steam.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="SSOverlay.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="obs64.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="OBS.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="RadeonSettings.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="AMD Ryzen Master.exe"),]	# >> "Cleaning - %~n1.r"
echo results = results[which(results$Application!="waterfox.exe"),]	# >> "Cleaning - %~n1.r"
echo write_csv(results, path=paste0(results$Application[1],substring("%~nx1", 18)))>> "Cleaning - %~n1.r"

::call "Cleaning - %~n1.r"

::del "Cleaning - %~n1.r"

shift

if "%~1"=="" goto end
goto start

::shift moves the %~1 command over, allowing this to iterate through multiple files

:end