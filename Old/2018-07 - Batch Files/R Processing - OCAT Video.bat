pushd "%~dp1"

:start

set "file=%~1"
set "file=%file:\=/%"

set "graph=%~dp1"
set "graph=%graph:\=/%"

::echo "%file%"
::pause

echo library(readr)> "Video - %~n1.r"
echo library(ggplot2)>> "Video - %~n1.r"
echo setwd("%graph%")>> "Video - %~n1.r"
echo results ^<- read_csv("%~n1.csv")>> "Video - %~n1.r"
echo dir.create("Frames - %~n1", showWarnings=FALSE)>> "Video - %~n1.r"
echo setwd("Frames - %~n1")>> "Video - %~n1.r"
echo FPS ^<- hist(results$TimeInSeconds,breaks=300,plot=FALSE)$counts>> "Video - %~n1.r"
echo.>> "Video - %~n1.r"

echo count = 1/60>> "Video - %~n1.r"
echo #how many previous frames to also show>> "Video - %~n1.r"
echo back = 60>> "Video - %~n1.r"
echo.>> "Video - %~n1.r"

echo pdf(NULL)>> "Video - %~n1.r"
echo.>> "Video - %~n1.r"

echo for (place in 1:round(max(results$TimeInSeconds)*60,0)){>> "Video - %~n1.r"
echo.>> "Video - %~n1.r"

echo ggplot(results, aes(TimeInSeconds,MsBetweenPresents)) + >> "Video - %~n1.r"
echo ggtitle("Frametimes Through Course") + >> "Video - %~n1.r"
echo scale_y_continuous(name="Frametimes (ms)",breaks=round(1000/c(120, 60, 30, 20, 15, 12, 10), 2),expand=c(0,0),limits=c(0,max(results$MsBetweenPresents))) + >> "Video - %~n1.r"
echo scale_x_continuous(name="Time (s)",breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), labels=round(seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), 4), minor_breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/60), limits=c((place-back)*(count),(place)*(count))) + >> "Video - %~n1.r"
echo expand_limits(y=c(0,1000/30)) + >> "Video - %~n1.r"
echo geom_hline(yintercept = c(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + >> "Video - %~n1.r"
echo geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + geom_point() + geom_path()>> "Video - %~n1.r"
echo.>> "Video - %~n1.r"
echo ggsave(filename=sprintf("%%05d.png",place), device="png", width=12.8, height=7.2, dpi=150)>> "Video - %~n1.r"
echo.>> "Overlay Display - %~n1.r"
echo dev.off(dev.prev())>> "Overlay Display - %~n1.r"
echo }>> "Video - %~n1.r"

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