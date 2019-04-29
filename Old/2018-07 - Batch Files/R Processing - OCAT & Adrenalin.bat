@echo off
pushd "%~dp1"

:start

set "file=%~1"
set "file=%file:\=/%"

set "graph=%~dp1"
set "graph=%graph:\=/%"

set txtout=TRUE
set cleandisout=FALSE

set frametime=FALSE
set framerate=FALSE
set displaytime=FALSE
set framecourse=FALSE
set framecountcourse=FALSE
set frameqq=FALSE
set displaycourse=FALSE
set diffcourse=FALSE
set diffqq=FALSE
set multidata=FALSE
set boxplot=FALSE
set latency=FALSE

set coursePWR=TRUE
set courseTMP=TRUE
set courseFAN=TRUE
set courseSCLK=TRUE

::echo "%file%"
::pause

echo library(readr)> "Processing Hybrid - %~n1 - %~n2.r"
echo library(ggplot2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo setwd("%graph%")>> "Processing Hybrid - %~n1 - %~n2.r"

echo OCAT ^<- read_csv("%~n1.csv")>> "Processing Hybrid - %~n1 - %~n2.r"
echo FPS ^<- hist(OCAT$TimeInSeconds, breaks=300,plot=FALSE)$counts>> "Processing Hybrid - %~n1 - %~n2.r"
echo DIFF = diff(OCAT$MsBetweenPresents)>> "Processing Hybrid - %~n1 - %~n2.r"
echo LOG ^<- read_csv("%~n2.csv")>> "Processing Hybrid - %~n1 - %~n2.r"
echo colnames(LOG) = c("gpuUTIL","gpuSCLK","gpuMCLK","TEMP","PWR","FAN","FPS","cpuUTIL","ramUTIL")>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo pngname = "%~n1">> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo theme_set(theme_grey(base_size = 16))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo Time = data.frame(-1:(dim(LOG)[1]-2))>> "Processing Hybrid - %~n1 - %~n2.r"
echo colnames(Time) = "TimeInSeconds">> "Processing Hybrid - %~n1 - %~n2.r"
echo Log = cbind(Time, LOG)>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo ytimes = c(120, 60, 30, 20, 15, 12, 10)>> "Processing Hybrid - %~n1 - %~n2.r"
echo ytimes = c(ytimes,-ytimes)>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo scaleP = ceiling(max(Log$PWR) / max(OCAT$MsBetweenPresents))>> "Processing Hybrid - %~n1 - %~n2.r"
echo scaleT = ceiling(max(Log$TEMP) / max(OCAT$MsBetweenPresents))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo scaleP = (400 / 70)>> "Processing Hybrid - %~n1 - %~n2.r"
echo scaleT = (090 / 70)>> "Processing Hybrid - %~n1 - %~n2.r"
echo scaleF = (3000 / 70)>> "Processing Hybrid - %~n1 - %~n2.r"
echo scaleC = (1700 / 70)>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo options(error=expression(NULL))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo if(%txtout%) {>> "Processing Hybrid - %~n1 - %~n2.r"

echo sink("Output - %~n1.txt", split=TRUE)>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("%~n1")>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Frame Time/Rate>> "Processing Hybrid - %~n1 - %~n2.r"

echo writeLines("\nMean")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(mean(OCAT$MsBetweenPresents))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nMedian")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(median(OCAT$MsBetweenPresents))>> "Processing Hybrid - %~n1 - %~n2.r"
echo #writeLines("\nAverage FPS")>> "Processing Hybrid - %~n1 - %~n2.r"
echo #print(1000/mean(OCAT$MsBetweenPresents))>> "Processing Hybrid - %~n1 - %~n2.r"
echo #print(mean(FPS))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nAverage of FPS")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(length(OCAT$TimeInSeconds)/max(OCAT$TimeInSeconds))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nRatio Dropped Frames")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(sum(OCAT$Dropped)/length(OCAT$Dropped))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nPercentiles (Frame Time)")>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("0.1%%,\t1%%,\t99%%,\t99.9%%")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(round(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nPercentiles (Frame Rate)")>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("0.1%%,\t1%%,\t99%%,\t99.9%%")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(round(1000/quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nECDF")>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("60 FPS,\t50 FPS,\t30 FPS,\t20 FPS,\t15 FPS")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(100*(1-ecdf(OCAT$MsBetweenPresents)(c(1000/60, 1000/50, 1000/30, 1000/20, 1000/15))))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nMedian")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(median(OCAT$MsBetweenPresents))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nStandard Deviation")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(sd(OCAT$MsBetweenPresents))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo writeLines("\nDiff Percentiles")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(quantile(DIFF, c(.001, .01, .99, 0.999)))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nECDF Diff")>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("-16.667,\t-8.333,\t8.333,\t16.667")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(100*(ecdf(DIFF)(c(-1000/60, -1000/120, 1000/120, 1000/60))))>> "Processing Hybrid - %~n1 - %~n2.r"
echo inc = length(subset(diff(OCAT$MsBetweenPresents), diff(OCAT$MsBetweenPresents) ^> 0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo dec = length(subset(diff(OCAT$MsBetweenPresents), diff(OCAT$MsBetweenPresents) ^<= 0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nDiff Balance")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(dec/(dec+inc));print(inc/(dec+inc))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo writeLines("\nMsUntilDisplayed - MsUntilRenderComplete")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	posonly = OCAT[which(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete ^> 0), ]>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(round(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .50, .99, 0.999)), 2))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo writeLines("\nAdrenalin Data")>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nPower Summary (W)")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(summary(Log$PWR))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nTotal Energy (J, W * s)")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(sum(Log$PWR))>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	#measurements made once per second so no need to multiply by seconds>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nGPU Clock Summary")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(summary(Log$gpuSCLK))>> "Processing Hybrid - %~n1 - %~n2.r"
echo writeLines("\nFan Summary")>> "Processing Hybrid - %~n1 - %~n2.r"
echo print(summary(Log$FAN))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo if(%cleandisout%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	cleanDisplay = OCAT$MsBetweenDisplayChange[!OCAT$MsBetweenDisplayChange==0] >> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	writeLines("\nDisplay Change Percentiles")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	print(quantile(cleanDisplay, c(.001, .01, .99, 0.999)))>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	print(quantile(1000/cleanDisplay, c(.001, .01, .99, 0.999)))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	writeLines("\nECDF")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	writeLines("60 FPS,\t50 FPS,\t30 FPS,\t20 FPS,\t15 FPS")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	print(100*(1-ecdf(cleanDisplay)(c(1000/60, 1000/50, 1000/30, 1000/20, 1000/15))))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	writeLines("\nMedian Display Change")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	print(median(cleanDisplay))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	writeLines("\nStandard Deviation Display Change")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	print(sd(cleanDisplay))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	writeLines("\nQuantile Display Change")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	print(quantile(diff(cleanDisplay), c(.001, .01, .99, 0.999)))>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo sink()>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo pdf(NULL) #prevents rplots.pdf from being generated>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo if(%coursePWR%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo #Frame Time Course and Power>> "Processing Hybrid - %~n1 - %~n2.r"
echo ggplot() + ggtitle("Frame Times Through Course with ASIC Power", subtitle="MsBetweenPresents") +>> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_fill_gradient2("Power (W)", low="blue", mid = "green", midpoint = 225,  high="red", limits = c(50, 400), breaks = c(75,150,225,300,375)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_col(data = Log, aes(x = TimeInSeconds, y = PWR/scaleP, fill=PWR)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleP, name = "ASIC Power (W)", c(75,150,225,300,375))) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo theme(legend.position = c(1, 0.9))>> "Processing Hybrid - %~n1 - %~n2.r"
echo #theme(legend.position="top", legend.justification="right", legend.margin=margin(0, 0, 0, 0), legend.box.margin=margin(-10, 0, -5, -10))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo ggsave(filename=paste(pngname, " - Power.png", sep=""), device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo if(%courseTMP%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo #Frame Time Course and Temperature>> "Processing Hybrid - %~n1 - %~n2.r"
echo ggplot() + ggtitle("Frame Times Through Course with GPU Temperature", subtitle="MsBetweenPresents") +>> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_fill_gradient2(expression(Temp~(degree*C)), low="blue", mid = "green", midpoint = 60, high="red", limits=c(30, 85)) +>> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_col(data = Log, aes(x = TimeInSeconds, y = TEMP/scaleT, fill=TEMP)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleT, name = expression(Temperature~(degree*C)), seq(35, 85, 10))) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + >> "Processing Hybrid - %~n1 - %~n2.r">> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo theme(legend.position = c(1, 0.9))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo ggsave(filename=paste(pngname, " - Temp.png", sep=""), device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo if(%courseFAN%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo #Frame Time Course and Fan Speed (RPM)>> "Processing Hybrid - %~n1 - %~n2.r"
echo ggplot() + ggtitle("Frame Times Through Course with Fan Speed", subtitle="MsBetweenPresents") +>> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_fill_gradient2("Fan Speed (RPM)", low="blue", mid = "green", midpoint = 2000, high="red", limits=c(500, 3000)) +>> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_col(data = Log, aes(x = TimeInSeconds, y = FAN/scaleF, fill=FAN)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleF, name = "Fan Speed (RPM)", seq(500, 3000, 500))) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo theme(legend.position = c(1, 0.9))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo ggsave(filename=paste(pngname," - Fan.png", sep=""), device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo if(%courseSCLK%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo #Frame Time Course and Core Clock>> "Processing Hybrid - %~n1 - %~n2.r"
echo ggplot() + ggtitle("Frame Times Through Course with Core Clock", subtitle="MsBetweenPresents") +>> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_fill_gradient2("Core Clock (MHz)", low="blue", mid = "green", midpoint = 1350, high="red", limits=c(500, 1700)) +>> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_col(data = Log, aes(x = TimeInSeconds, y = gpuSCLK/scaleC, fill=gpuSCLK)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleC, name = "Core Clock (MHz)", seq(500, 1700, 250))) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + >> "Processing Hybrid - %~n1 - %~n2.r"
echo geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + >> "Processing Hybrid - %~n1 - %~n2.r"
echo theme(legend.position = c(1, 0.9))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo ggsave(filename=paste(pngname," - SClock.png", sep=""), device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Frame Time>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%frametime%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(OCAT, aes(MsBetweenPresents)) + geom_freqpoly(binwidth=0.03, size=0) + ggtitle("Frequency Plot of Frame Times", subtitle="MsBetweenPresents") + scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=ceiling(max(OCAT$MsBetweenPresents, 1000/60)), by=1000/120), labels=round(seq(from=0, to=ceiling(max(OCAT$MsBetweenPresents, 1000/60)), by=1000/120), 2), limits=c(NA, min(max(OCAT$MsBetweenPresents), 100)), expand=c(0.02, 0)) + expand_limits(x=c(1000/60, 1000/30)) + scale_y_continuous(name="Count", expand=c(0.02, 0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggsave(filename="Frame Times - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Frame Times - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Frame Rate>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%framerate%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(OCAT, aes(1000/MsBetweenPresents)) + geom_freqpoly(binwidth=1, size=0) + ggtitle("Frequency Plot of Frame Rates", subtitle="1000 * MsBetweenPresents^-1") + scale_x_continuous(name="Frame Rate (FPS)", breaks=c(120, 60, 30, 20, 15, 10,0), expand=c(0.02, 0)) + expand_limits(x=c(30, 60)) + scale_y_continuous(name="Count", expand=c(0.02, 0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggsave(filename="Frame Rates - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Frame Rates - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Display Time>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%displaytime%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(as.data.frame(OCAT$MsBetweenDisplayChange), aes(OCAT$MsBetweenDisplayChange*60/1000)) + geom_freqpoly(binwidth=0.003, size=0) + ggtitle("Frequency Plot of Display Times", subtitle="MsBetweenDisplayChange") + scale_x_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(OCAT$MsBetweenDisplayChange*60/1000)), by=1), minor_breaks=NULL, limits=c(0, min(max(OCAT$MsBetweenDisplayChange*60/1000),15)), expand=c(0.02, 0)) + expand_limits(x=c(0, 2)) + scale_y_continuous(name="Count", expand=c(0.02, 0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggsave(filename="Display Times - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Display Times - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Course - Frame Time>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%framecourse%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(OCAT, aes(TimeInSeconds, MsBetweenPresents)) + geom_point() + geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + ggtitle("Frame Times Through Course", subtitle="MsBetweenPresents") + scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red")>> "Processing Hybrid - %~n1 - %~n2.r"
echo #for a boxplot add this>> "Processing Hybrid - %~n1 - %~n2.r"
echo #	+ geom_boxplot(aes(x=330, y=OCAT$MsBetweenPresents), width=25)>> "Processing Hybrid - %~n1 - %~n2.r"
echo #	for quantile regression use this instead>> "Processing Hybrid - %~n1 - %~n2.r"
echo #	stat_quantile(quantiles = c(0.001, 0.01, 0.99, 0.999), color = "red")>> "Processing Hybrid - %~n1 - %~n2.r"

echo 	ggsave(filename="Course - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Course - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #1s Frame Counts Graph>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%framecountcourse%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	#write.table(FPS, file="%~n1 FPS.txt", sep=",", col.names=FALSE)>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(OCAT, aes(TimeInSeconds, fill=..count..)) + geom_histogram(data=OCAT, binwidth=1, center=0.5, col="black", na.rm=TRUE) + ggtitle("Frames Rendered per Second", subtitle="TimeInSeconds count") + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + scale_fill_gradient2("Frames", low="red", mid="green", high="blue", midpoint=60, expand=c(0.02, 0)) + scale_y_continuous(name="Frames per Second", limits=c(0, max(60,FPS)), breaks=seq(from=0, to=max(60,FPS), by=30), expand=c(0.02, 0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	ggsave(filename="FPS - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="FPS - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #QQ Plot>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%frameqq%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(OCAT, aes(sample=MsBetweenPresents)) + ggtitle("QQ Distrubtion Plot", subtitle="MsBetweenPresents") + scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2), quantile(OCAT$MsBetweenPresents, .9999)), labels=c(0, round(1000/ytimes, 2), paste(round(quantile(OCAT$MsBetweenPresents, .9999), 2), "\n(99.99%%)")), limits=c(0, max(quantile(OCAT$MsBetweenPresents, .9999), 1000/60)), expand=c(0.02, 0)) + scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "1", "50 (Median)", "99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + annotate("rect", ymin=-Inf, ymax=quantile(OCAT$MsBetweenPresents, c(.001, .010, .500, .990, .999)), xmin=-Inf, xmax=qnorm(c(.001, .010, .500, .990, .999)), alpha=0.1, fill=c("blue", "blue", "blue", "red", "red"), color="grey") + geom_point(stat="qq")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggsave(filename="QQ - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="QQ - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Course - Display Time>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%displaycourse%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(OCAT, aes(TimeInSeconds, OCAT$MsBetweenDisplayChange*60/1000)) + ggtitle("Display Times Through Course", subtitle="MsBetweenDisplayChange") + scale_y_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(OCAT$MsBetweenPresents, 1000/60)), by=1), minor_breaks=NULL, limits=c(NA, ceiling(min(max(OCAT$MsBetweenDisplayChange*60/1000), 15))), expand=c(0.02, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 3)) + geom_point() + geom_smooth(method="gam", formula= y ~ s(x, bs = "cs"))>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	ggsave(filename="Course Display - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Course Display - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Course - Diff>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%diffcourse%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(as.data.frame(DIFF), aes(x=OCAT$TimeInSeconds[-1], y=DIFF)) + geom_point() + ggtitle("Difference Course Plot", subtitle="MsBetweenPresent consecutive difference") + expand_limits(y=c(-1000/30, 1000/30)) + scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/50, 1000/50), expand=c(0, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	ggsave(filename="Course Diff - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Course Diff - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #QQ Diff>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%diffqq%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	ggplot(as.data.frame(DIFF), aes(sample=DIFF)) + ggtitle("Difference QQ Distrubtion Plot", subtitle="MsBetweenPresent consecutive differences") + expand_limits(y=c(-1000/50, 1000/50)) + scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/30, 1000/30), expand=c(0, 0)) + scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .25, .5, .75, .99, .999)),labels=c("0.1", "1","25", "50\n(Median)", "75","99","99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + geom_point(stat="qq")>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	ggsave(filename="QQ Diff - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="QQ Diff - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Multi-data Graph>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%multidata%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	`+.uneval` ^<- function(a,b) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	`class^<-`(modifyList(a,b), "uneval")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	}>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo 	redplot = "MsUntilRenderComplete">> "Processing Hybrid - %~n1 - %~n2.r"
echo 	greenplot = "MsUntilDisplayed">> "Processing Hybrid - %~n1 - %~n2.r"
echo 	blueplot = "MsBetweenDisplayChange">> "Processing Hybrid - %~n1 - %~n2.r"
echo 	blackplot = "MsBetweenPresents">> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo 	ggplot(OCAT) + ggtitle("Multi-data Plot") + geom_point(aes_string("TimeInSeconds", paste(y=redplot)) + aes(color="red")) + geom_point(aes_string("TimeInSeconds", paste(y=greenplot)) + aes(color="green")) + geom_point(aes_string("TimeInSeconds", paste(y=blueplot)) + aes(color="blue")) + geom_point(aes_string("TimeInSeconds", paste(y=blackplot)) + aes(color="black")) + scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), expand=c(0, 0)) + scale_color_manual(name=NULL, values=c("black", "blue", "green", "red"), labels=paste(c(blackplot, blueplot, greenplot, redplot)), expand=c(0.02, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + theme(legend.position = "top") + expand_limits(y=c(0, 1000/30))>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	ggsave(filename="Multi - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Multi - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Box Plots - this will show box plots for each second>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%boxplot%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo #note that with limits on the y-axis, box plots might be removed if they go out of bounds
echo 	ggplot(OCAT, aes(OCAT$TimeInSeconds, OCAT$MsBetweenPresents)) + ggtitle("Frame Time Course with Boxplots", subtitle="MsBetweenPresents") + scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, min(1000/30, max(OCAT$MsBetweenPresents)))) + geom_point(color="red") + geom_boxplot(aes(group=cut_width(OCAT$TimeInSeconds, 1)), color="black", outlier.alpha=0)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	ggsave(filename="Box Secs - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Box Secs - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"

::echo.>> "Processing Hybrid - %~n1 - %~n2.r"
::echo #	ggplot(OCAT, aes(OCAT$TimeInSeconds, OCAT$MsBetweenPresents)) + ggtitle("Box Plot of Frame Times") + >> "Processing Hybrid - %~n1 - %~n2.r"
::echo #this will be a boxplot for the entire data set>> "Processing Hybrid - %~n1 - %~n2.r"
::echo #geom_boxplot()>> "Processing Hybrid - %~n1 - %~n2.r"
::echo #boxplot for entire data set, but with different quantiles>> "Processing Hybrid - %~n1 - %~n2.r"
::echo #geom_boxplot(aes(ymin=min(OCAT$MsBetweenPresents), lower=quantile(OCAT$MsBetweenPresents, .01), middle=median(OCAT$MsBetweenPresents), upper=quantile(OCAT$MsBetweenPresents, .99), ymax=max(OCAT$MsBetweenPresents)))>> "Processing Hybrid - %~n1 - %~n2.r"
::echo  	#ggsave(filename="Box - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
::echo  	#ggsave(filename="Box - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"

echo #Frame Latency (maybe) - Difference between time to display the frame and the GPU time to render it>> "Processing Hybrid - %~n1 - %~n2.r"
echo if(%latency%) {>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	framelag = round(100*(1-ecdf(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete)(0)),3)>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	test = c(sum(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete^<0), sum(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete^>0))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	laggraph = 	ggplot(OCAT) + geom_point(aes(TimeInSeconds, MsUntilDisplayed - MsUntilRenderComplete), color="black") + ggtitle("Latency", subtitle="MsUntilDisplayed - MsUntilRenderComplete") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/ytimes, 2)), expand=c(0, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30))>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	poslag = laggraph + geom_hline(yintercept = c(quantile(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete), color="blue")>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	zerolag = laggraph>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	#neglag = laggraph + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag,"%%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag,"%%"), hjust="left", vjust="top")>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	posonly = OCAT[which(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete ^> 0), ]>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	neglag = laggraph + geom_hline(yintercept = c(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete), color="blue") + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag, "%%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag, "%%"), hjust="left", vjust="top")>> "Processing Hybrid - %~n1 - %~n2.r"
echo.>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	testswitch=sum(test==0)+1>> "Processing Hybrid - %~n1 - %~n2.r"
echo 	switch(testswitch, >> "Processing Hybrid - %~n1 - %~n2.r"
echo		ggsave(filename="Latency - %~n1.png", plot=neglag, device="png", width=12.8, height=7.2, dpi=150), >> "Processing Hybrid - %~n1 - %~n2.r"
echo		ggsave(filename="Latency - %~n1.png", plot=poslag, device="png", width=12.8, height=7.2, dpi=150), >> "Processing Hybrid - %~n1 - %~n2.r"
echo		ggsave(filename="Latency - %~n1.png", plot=zerolag, device="png", width=12.8, height=7.2, dpi=150)) >> "Processing Hybrid - %~n1 - %~n2.r"


echo 	#switch(testswitch,neglag,poslag,zerolag)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Latency - %~n1.png", device="png", width=12.8, height=7.2, dpi=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo  	#ggsave(filename="Latency - %~n1.pdf", device="pdf", width=12.8, height=7.2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo }>> "Processing Hybrid - %~n1 - %~n2.r"

::echo 	ggplot(OCAT) + geom_point(aes(TimeInSeconds, MsUntilDisplayed - MsUntilRenderComplete), color="black") + ggtitle("Latency", subtitle="MsUntilDisplayed - MsUntilRenderComplete") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/c(120, 60, 30, 20, 15, 12, 10), 2)), expand=c(0,0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60)) + expand_limits(y=c(0, 1000/30))>> "Processing Hybrid - %~n1 - %~n2.r"
::echo #Frame Latency (maybe) - MsUntilDisplayed is from Present start to Displayed>> "Processing Hybrid - %~n1 - %~n2.r"
::echo #	ggplot(OCAT[which(OCAT$Dropped==0),]) + geom_point(aes(TimeInSeconds, MsUntilDisplayed), color="black") + ggtitle("MsUntilDisplayed", subtitle="Time between present start and frame display") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/c(120, 60, 30, 20, 15, 12, 10), 2)), expand=c(0,0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60)) + expand_limits(y=c(0, 1000/30))>> "Processing Hybrid - %~n1 - %~n2.r"


:Shift

shift
shift

if "%~1"=="" goto end
goto start

::shift moves the %~1 command over, allowing this to iterate through multiple files

:end

exit

set data=MsInPresentAPI
set data=MsUntilRenderComplete
set data=MsUntilDisplayed

::echo plot(OCAT$TimeInSeconds, OCAT$MsBetweenPresents,xlab="Time in Seconds",ylab="Frame Time in ms",h=1000/60) >> "Processing Hybrid - %~n1 - %~n2.r"

echo library(ggplot2)>> "Processing Hybrid - %~n1 - %~n2.r"
echo png(file="%graph%%~n1 - Course.png",width=1920,height=1080,res=150)>> "Processing Hybrid - %~n1 - %~n2.r"
echo ggplot(OCAT, aes(TimeInSeconds, MsBetweenPresents), xlab="Time in Seconds", ylab="Frame Times in ms", main="Frame Times Through Course") + geom_point() + geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + expand_limits(y = c(0, 1000/30))>> "Processing Hybrid - %~n1 - %~n2.r"
echo dev.off()>> "Processing Hybrid - %~n1 - %~n2.r"

::pause