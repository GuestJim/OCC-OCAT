pushd "%~dp1"

:start

set "file=%~1"
set "file=%file:\=/%"

set "graph=%~dp1"
set "graph=%graph:\=/%"

set data=MsBetweenPresents
::set data=MsBetweenDisplayChange
::set data=MsInPresentAPI
::set data=MsUntilRenderComplete
::set data=MsUntilDisplayed

::echo "%file%"
::pause

echo library(readr)> "Overlay Frame - %~n1.r"
echo library(ggplot2)>> "Overlay Frame - %~n1.r"
echo library(foreach)>> "Overlay Frame - %~n1.r"
echo library(doParallel)>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"

echo setwd("%graph%")>> "Overlay Frame - %~n1.r"
echo results ^<- read_csv("%~n1.csv")>> "Overlay Frame - %~n1.r"
echo dir.create("Frames - %~n1", showWarnings=FALSE)>> "Overlay Frame - %~n1.r"
echo setwd("Frames - %~n1")>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"

echo count = 1/60>> "Overlay Frame - %~n1.r"
echo #how many previous frames to also show>> "Overlay Frame - %~n1.r"
echo back = 60>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"


echo pdf(NULL)>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"

echo registerDoParallel(cores=detectCores() - 4)>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"

echo foreach (place=seq(from=0, to=round(max(results$TimeInSeconds)*60,0)-1, by=1), .packages = "ggplot2") %%dopar%% {>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"

echo theme_update(>> "Overlay Frame - %~n1.r"
echo panel.background = element_rect(fill="black"),>> "Overlay Frame - %~n1.r"
echo plot.background = element_rect(fill="black"), >> "Overlay Frame - %~n1.r"
echo axis.text = element_text(color="white", size=12),>> "Overlay Frame - %~n1.r"
echo text = element_text(color="white", size=16),>> "Overlay Frame - %~n1.r"
echo panel.grid.major=element_line(color="gray"),>> "Overlay Frame - %~n1.r"
echo panel.grid.minor=element_line(color="gray20")>> "Overlay Frame - %~n1.r"
echo )>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"

echo ggplot(results, aes(TimeInSeconds,%data%)) + >> "Overlay Frame - %~n1.r"
echo scale_y_continuous(name=NULL,breaks=round(1000/c(120, 60, 30, 20, 15, 12, 10), 2),expand=c(0,0),limits=c(0,min(100, max(results$%data%))), sec.axis=dup_axis(), minor_breaks=NULL) + >> "Overlay Frame - %~n1.r"
echo scale_x_continuous(name=NULL,breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), labels=round(seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), 3), minor_breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/60), expand=c(0,0), limits=c((place-back)*(count),(place)*(count))) + >> "Overlay Frame - %~n1.r"
echo expand_limits(y=c(0,1000/30)) + >> "Overlay Frame - %~n1.r"
echo geom_hline(yintercept = c(quantile(results$%data%, c(.001, .01, .99, 0.999))), color="red") + >> "Overlay Frame - %~n1.r"
echo geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + geom_point(color="white") + geom_path(color="white") >> "Overlay Frame - %~n1.r"
echo.>> "Overlay Frame - %~n1.r"

echo ggsave(filename=sprintf("%%05d.png",place), device="png", width=20.48, height=3.84, dpi=100)>> "Overlay Frame - %~n1.r"
echo.>> "Overlay Display - %~n1.r"
echo dev.off(dev.prev())>> "Overlay Display - %~n1.r"
echo }>> "Overlay Frame - %~n1.r"

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