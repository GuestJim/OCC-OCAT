pushd "%~dp1"

:start

set "file=%~1"
set "file=%file:\=/%"

set "graph=%~dp1"
set "graph=%graph:\=/%"

::echo "%file%"
::pause

echo library(readr)> "Overlay Display - %~n1.r"
echo library(ggplot2)>> "Overlay Display - %~n1.r"
echo setwd("%graph%")>> "Overlay Display - %~n1.r"
echo results ^<- read_csv("%~n1.csv")>> "Overlay Display - %~n1.r"
echo dir.create("Display - %~n1", showWarnings=FALSE)>> "Overlay Display - %~n1.r"
echo setwd("Display - %~n1")>> "Overlay Display - %~n1.r"
echo FPS ^<- hist(results$TimeInSeconds,breaks=300,plot=FALSE)$counts>> "Overlay Display - %~n1.r"
echo.>> "Overlay Display - %~n1.r"

echo count = 1/60>> "Overlay Display - %~n1.r"
echo #how many previous frames to also show>> "Overlay Display - %~n1.r"
echo back = 60>> "Overlay Display - %~n1.r"
echo.>> "Overlay Display - %~n1.r"

echo theme_update(>> "Overlay Display - %~n1.r"
echo panel.background = element_rect(fill="black"),>> "Overlay Display - %~n1.r"
echo plot.background = element_rect(fill="black"), >> "Overlay Display - %~n1.r"
echo axis.text = element_text(color="white", size=12),>> "Overlay Display - %~n1.r"
echo text = element_text(color="white", size=16),>> "Overlay Display - %~n1.r"
echo panel.grid.major=element_line(color="gray"),>> "Overlay Display - %~n1.r"
echo panel.grid.minor=element_line(color="gray20")>> "Overlay Display - %~n1.r"
echo )>> "Overlay Display - %~n1.r"
echo.>> "Overlay Display - %~n1.r"

echo pdf(NULL)>> "Overlay Display - %~n1.r"
echo.>> "Overlay Display - %~n1.r"

echo for (place in seq(from=0, to=round(max(results$TimeInSeconds)*60,0)-1, by=1)){>> "Overlay Display - %~n1.r"

echo #for multi-threading>> "Overlay Display - %~n1.r"
echo #for (place in seq(from=0, to=round(max(results$TimeInSeconds)*60,0)-1, by=3)){>> "Overlay Display - %~n1.r"
echo #for (place in seq(from=1, to=round(max(results$TimeInSeconds)*60,0)-1, by=3)){>> "Overlay Display - %~n1.r"
echo #for (place in seq(from=2, to=round(max(results$TimeInSeconds)*60,0)-1, by=3)){>> "Overlay Display - %~n1.r"
echo.>> "Overlay Display - %~n1.r"

echo ggplot(results, aes(TimeInSeconds,MsBetweenDisplayChange)) + >> "Overlay Display - %~n1.r"
echo scale_y_continuous(name=NULL, breaks=round(seq(from=1, to=ceiling(max(results$MsBetweenDisplayChange*60/1000)), by=1)*1000/60,2),labels=seq(from=1, to=ceiling(max(results$MsBetweenDisplayChange*60/1000)), by=1), expand=c(0,0), limits=c(0,min(1001/6,max(results$MsBetweenDisplayChange))), sec.axis=dup_axis(), minor_breaks=NULL) + >> "Overlay Display - %~n1.r"
echo scale_x_continuous(name=NULL,breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), labels=round(seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), 3), minor_breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/60), limits=c((place-back)*(count),(place)*(count)), expand=c(0,0)) + >> "Overlay Display - %~n1.r"
echo expand_limits(y=c(0,1000/30)) + geom_step(color="green") + >> "Overlay Display - %~n1.r"
echo geom_point(color="white", shape=9, size=2) #+ geom_path(color="white")>> "Overlay Display - %~n1.r"
echo.>> "Overlay Display - %~n1.r"
echo ggsave(filename=sprintf("%%05d.png",place), device="png", width=20.48, height=2.88, dpi=100)>> "Overlay Display - %~n1.r">> "Overlay Display - %~n1.r"
echo }>> "Overlay Display - %~n1.r"

:Shift

shift

if "%~1"=="" goto end
goto start

::shift moves the %~1 command over, allowing this to iterate through multiple files

:end

exit

set data=MsInPresentAPI
set data=MsUntilRenderComplete
set data=MsUntilDisplayed

::pause