@echo off
pushd "%~dp1"

:start

set "file=%~1"
set "file=%file:\=/%"

set "graph=%~dp1"
set "graph=%graph:\=/%"

set txtout=TRUE
set cleandisout=FALSE

set frametime=TRUE
set framerate=TRUE
set displaytime=TRUE
set framecourse=TRUE
set framecountcourse=FALSE
set frameqq=TRUE
set displaycourse=TRUE
set diffcourse=FALSE
set diffqq=TRUE
set multidata=FALSE
set boxplot=FALSE
set latency=FALSE

::echo "%file%"
::pause

echo library(readr)> "Processing - %~n1.r"
echo library(ggplot2)>> "Processing - %~n1.r"
echo setwd("%graph%")>> "Processing - %~n1.r"
echo #setwd(dirname(sys.frame(1)$ofile))>> "Processing - %~n1.r"
echo #this works in the GUI but not command line>> "Processing - %~n1.r"
echo results ^<- read_csv("%~n1.csv")>> "Processing - %~n1.r"
echo FPS ^<- hist(results$TimeInSeconds, breaks=300,plot=FALSE)$counts>> "Processing - %~n1.r"
echo DIFF = diff(results$MsBetweenPresents)>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo recording = " (Recording #)">> "Processing - %~n1.r"
echo recording = character(0)>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo DPI = 120>> "Processing - %~n1.r"
echo ggscale = 1 >> "Processing - %~n1.r"
echo theme_set(theme_grey(base_size = 16))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"


echo ytimes = c(120, 60, 30, 20, 15, 12, 10)>> "Processing - %~n1.r"
echo ytimes = c(ytimes,-ytimes)>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo options(error=expression(NULL))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo if(%txtout%) {>> "Processing - %~n1.r"

echo sink("Output - %~n1.txt", split=TRUE)>> "Processing - %~n1.r"
echo writeLines("%~n1")>> "Processing - %~n1.r"

echo #Frame Time/Rate>> "Processing - %~n1.r"

echo writeLines("\nMean")>> "Processing - %~n1.r"
echo print(mean(results$MsBetweenPresents))>> "Processing - %~n1.r"
echo writeLines("\nMedian")>> "Processing - %~n1.r"
echo print(median(results$MsBetweenPresents))>> "Processing - %~n1.r"
echo #writeLines("\nAverage FPS")>> "Processing - %~n1.r"
echo #print(1000/mean(results$MsBetweenPresents))>> "Processing - %~n1.r"
echo #print(mean(FPS))>> "Processing - %~n1.r"
echo writeLines("\nAverage of FPS")>> "Processing - %~n1.r"
echo print(length(results$TimeInSeconds)/max(results$TimeInSeconds))>> "Processing - %~n1.r"
echo writeLines("\nRatio Dropped Frames")>> "Processing - %~n1.r"
echo print(sum(results$Dropped)/length(results$Dropped))>> "Processing - %~n1.r"
echo writeLines("\nPercentiles (Frame Time)")>> "Processing - %~n1.r"
echo writeLines("0.1%%,\t1%%,\t99%%,\t99.9%%")>> "Processing - %~n1.r"
echo print(round(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))>> "Processing - %~n1.r"
echo writeLines("\nPercentiles (Frame Rate)")>> "Processing - %~n1.r"
echo writeLines("0.1%%,\t1%%,\t99%%,\t99.9%%")>> "Processing - %~n1.r"
echo print(round(1000/quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))>> "Processing - %~n1.r"
echo writeLines("\nECDF")>> "Processing - %~n1.r"
echo writeLines("60 FPS,\t50 FPS,\t30 FPS,\t20 FPS,\t15 FPS")>> "Processing - %~n1.r"
echo print(100*(1-ecdf(results$MsBetweenPresents)(c(1000/60, 1000/50, 1000/30, 1000/20, 1000/15))))>> "Processing - %~n1.r"
echo writeLines("\nMedian")>> "Processing - %~n1.r"
echo print(median(results$MsBetweenPresents))>> "Processing - %~n1.r"
echo writeLines("\nStandard Deviation")>> "Processing - %~n1.r"
echo print(sd(results$MsBetweenPresents))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo writeLines("\nDiff Percentiles")>> "Processing - %~n1.r"
echo print(quantile(DIFF, c(.001, .01, .99, 0.999)))>> "Processing - %~n1.r"
echo writeLines("\nECDF Diff")>> "Processing - %~n1.r"
echo writeLines("-16.667,\t-8.333,\t8.333,\t16.667")>> "Processing - %~n1.r"
echo print(100*(ecdf(DIFF)(c(-1000/60, -1000/120, 1000/120, 1000/60))))>> "Processing - %~n1.r"
echo inc = length(subset(diff(results$MsBetweenPresents), diff(results$MsBetweenPresents) ^> 0))>> "Processing - %~n1.r"
echo dec = length(subset(diff(results$MsBetweenPresents), diff(results$MsBetweenPresents) ^<= 0))>> "Processing - %~n1.r"
echo writeLines("\nDiff Balance")>> "Processing - %~n1.r"
echo print(dec/(dec+inc));print(inc/(dec+inc))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo writeLines("\nMsUntilDisplayed - MsUntilRenderComplete")>> "Processing - %~n1.r"
echo 	posonly = results[which(results$MsUntilDisplayed - results$MsUntilRenderComplete ^> 0), ]>> "Processing - %~n1.r"
echo print(round(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .50, .99, 0.999)), 2))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo if(%cleandisout%) {>> "Processing - %~n1.r"
echo 	cleanDisplay = results$MsBetweenDisplayChange[!results$MsBetweenDisplayChange==0] >> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	writeLines("\nDisplay Change Percentiles")>> "Processing - %~n1.r"
echo 	print(quantile(cleanDisplay, c(.001, .01, .99, 0.999)))>> "Processing - %~n1.r"
echo 	print(quantile(1000/cleanDisplay, c(.001, .01, .99, 0.999)))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	writeLines("\nECDF")>> "Processing - %~n1.r"
echo 	writeLines("60 FPS,\t50 FPS,\t30 FPS,\t20 FPS,\t15 FPS")>> "Processing - %~n1.r"
echo 	print(100*(1-ecdf(cleanDisplay)(c(1000/60, 1000/50, 1000/30, 1000/20, 1000/15))))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	writeLines("\nMedian Display Change")>> "Processing - %~n1.r"
echo 	print(median(cleanDisplay))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	writeLines("\nStandard Deviation Display Change")>> "Processing - %~n1.r"
echo 	print(sd(cleanDisplay))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	writeLines("\nQuantile Display Change")>> "Processing - %~n1.r"
echo 	print(quantile(diff(cleanDisplay), c(.001, .01, .99, 0.999)))>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo sink()>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"

echo.>> "Processing - %~n1.r"
echo pdf(NULL) #prevents rplots.pdf from being generated>> "Processing - %~n1.r"

echo #Frame Time>> "Processing - %~n1.r"
echo if(%frametime%) {>> "Processing - %~n1.r"
echo 	ggplot(results, aes(MsBetweenPresents)) + >> "Processing - %~n1.r"
echo	ggtitle(paste("Frequency Plot of Frame Times", recording, sep = ""), subtitle="MsBetweenPresents") + >> "Processing - %~n1.r"
echo	geom_freqpoly(binwidth=0.03, size=0) + >> "Processing - %~n1.r"
echo	scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1000/120), labels=round(seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1000/120), 2), limits=c(NA, min(max(results$MsBetweenPresents), 100)), expand=c(0.02, 0)) + expand_limits(x=c(1000/60, 1000/30)) + >> "Processing - %~n1.r"
echo	scale_y_continuous(name="Count", expand=c(0.02, 0))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	ggsave(filename="Frame Times - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Frame Times - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Frame Rate>> "Processing - %~n1.r"
echo if(%framerate%) {>> "Processing - %~n1.r"
echo 	ggplot(results, aes(1000/MsBetweenPresents)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Frequency Plot of Frame Rates", recording, sep = ""), subtitle="1000 * MsBetweenPresents^-1") + >> "Processing - %~n1.r"
echo 	geom_freqpoly(binwidth=1, size=0) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Frame Rate (FPS)", breaks=c(120, 60, 30, 20, 15, 10,0), expand=c(0.02, 0)) + expand_limits(x=c(30, 60)) + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Count", expand=c(0.02, 0))>> "Processing - %~n1.r">> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	ggsave(filename="Frame Rates - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Frame Rates - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Display Time>> "Processing - %~n1.r"
echo if(%displaytime%) {>> "Processing - %~n1.r"
echo 	ggplot(as.data.frame(results$MsBetweenDisplayChange), aes(results$MsBetweenDisplayChange*60/1000)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Frequency Plot of Display Times", recording, sep = ""), subtitle="MsBetweenDisplayChange") + >> "Processing - %~n1.r"
echo 	geom_freqpoly(binwidth=0.003, size=0) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenDisplayChange*60/1000)), by=1), minor_breaks=NULL, limits=c(0, min(max(results$MsBetweenDisplayChange*60/1000),15)), expand=c(0.02, 0)) + expand_limits(x=c(0, 2)) + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Count", expand=c(0.02, 0))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	ggsave(filename="Display Times - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Display Times - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Course - Frame Time>> "Processing - %~n1.r"
echo if(%framecourse%) {>> "Processing - %~n1.r"
echo 	ggplot(results, aes(TimeInSeconds, MsBetweenPresents)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Frame Times Through Course", recording, sep = ""), subtitle="MsBetweenPresents") + >> "Processing - %~n1.r"
echo 	geom_point() + >> "Processing - %~n1.r"
echo 	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + >> "Processing - %~n1.r"
echo 	geom_hline(yintercept = c(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red")>> "Processing - %~n1.r"
echo #for a boxplot add this>> "Processing - %~n1.r"
echo #	+ geom_boxplot(aes(x=330, y=results$MsBetweenPresents), width=25)>> "Processing - %~n1.r"
echo #	for quantile regression use this instead>> "Processing - %~n1.r"
echo #	stat_quantile(quantiles = c(0.001, 0.01, 0.99, 0.999), color = "red")>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo 	ggsave(filename="Course - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Course - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #1s Frame Counts Graph>> "Processing - %~n1.r"
echo if(%framecountcourse%) {>> "Processing - %~n1.r"
echo 	#write.table(FPS, file="%~n1 FPS.txt", sep=",", col.names=FALSE)>> "Processing - %~n1.r"
echo 	ggplot(results, aes(TimeInSeconds, fill=..count..)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Frames Rendered per Second", recording, sep = ""), subtitle="TimeInSeconds count") + >> "Processing - %~n1.r"
echo	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	geom_histogram(data=results, binwidth=1, center=0.5, col="black", na.rm=TRUE) + >> "Processing - %~n1.r"
echo 	scale_fill_gradient2("Frames", low="red", mid="green", high="blue", midpoint=60, expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frames per Second", limits=c(0, max(60,FPS)), breaks=seq(from=0, to=max(60,FPS), by=30), expand=c(0.02, 0))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo  	ggsave(filename="FPS - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="FPS - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #QQ Plot>> "Processing - %~n1.r"
echo if(%frameqq%) {>> "Processing - %~n1.r"
echo 	ggplot(results, aes(sample=MsBetweenPresents)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("QQ Distrubtion Plot", recording, sep = ""), subtitle="MsBetweenPresents") + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2), quantile(results$MsBetweenPresents, .9999)), labels=c(0, round(1000/ytimes, 2), paste(round(quantile(results$MsBetweenPresents, .9999), 2), "\n(99.99%%)")), limits=c(0, max(quantile(results$MsBetweenPresents, .9999), 1000/60)), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "1", "50 (Median)", "99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	annotate("rect", ymin=-Inf, ymax=quantile(results$MsBetweenPresents, c(.001, .010, .500, .990, .999)), xmin=-Inf, xmax=qnorm(c(.001, .010, .500, .990, .999)), alpha=0.1, fill=c("blue", "blue", "blue", "red", "red"), color="grey") + geom_point(stat="qq")>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	ggsave(filename="QQ - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="QQ - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Course - Display Time>> "Processing - %~n1.r"
echo if(%displaycourse%) {>> "Processing - %~n1.r"
echo 	ggplot(results, aes(TimeInSeconds, results$MsBetweenDisplayChange*60/1000)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Display Times Through Course", recording, sep = ""), subtitle="MsBetweenDisplayChange") + 
echo 	geom_point() + >> "Processing - %~n1.r"
echo 	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1), minor_breaks=NULL, limits=c(NA, ceiling(min(max(results$MsBetweenDisplayChange*60/1000), 10))), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 3))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo  	ggsave(filename="Course Display - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Course Display - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Course - Diff>> "Processing - %~n1.r"
echo if(%diffcourse%) {>> "Processing - %~n1.r"
echo 	ggplot(as.data.frame(DIFF), aes(x=results$TimeInSeconds[-1], y=DIFF)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Difference Course Plot", recording, sep = ""), subtitle="MsBetweenPresent consecutive difference") + >> "Processing - %~n1.r"
echo 	geom_point() + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/50, 1000/50), expand=c(0, 0)) + expand_limits(y=c(-1000/30, 1000/30)) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo  	ggsave(filename="Course Diff - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Course Diff - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #QQ Diff>> "Processing - %~n1.r"
echo if(%diffqq%) {>> "Processing - %~n1.r"
echo 	ggplot(as.data.frame(DIFF), aes(sample=DIFF)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Difference QQ Distrubtion Plot", recording, sep = ""), subtitle="MsBetweenPresent consecutive differences") + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/30, 1000/30), expand=c(0, 0)) + expand_limits(y=c(-1000/50, 1000/50)) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .25, .5, .75, .99, .999)),labels=c("0.1", "1","25", "50\n(Median)", "75","99","99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + geom_point(stat="qq")>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo  	ggsave(filename="QQ Diff - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="QQ Diff - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Multi-data Graph>> "Processing - %~n1.r"
echo if(%multidata%) {>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	`+.uneval` ^<- function(a,b) {>> "Processing - %~n1.r"
echo 	`class^<-`(modifyList(a,b), "uneval")>> "Processing - %~n1.r"
echo 	}>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo 	redplot = "MsUntilRenderComplete">> "Processing - %~n1.r"
echo 	greenplot = "MsUntilDisplayed">> "Processing - %~n1.r"
echo 	blueplot = "MsBetweenDisplayChange">> "Processing - %~n1.r"
echo 	blackplot = "MsBetweenPresents">> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo 	ggplot(results) + >> "Processing - %~n1.r"
echo 	ggtitle(recording("Multi-data Plot", recording, sep = "")) + >> "Processing - %~n1.r"
echo 	geom_point(aes_string("TimeInSeconds", paste(y=redplot)) + aes(color="red")) + >> "Processing - %~n1.r"
echo 	geom_point(aes_string("TimeInSeconds", paste(y=greenplot)) + aes(color="green")) + >> "Processing - %~n1.r"
echo 	geom_point(aes_string("TimeInSeconds", paste(y=blueplot)) + aes(color="blue")) + >> "Processing - %~n1.r"
echo 	geom_point(aes_string("TimeInSeconds", paste(y=blackplot)) + aes(color="black")) + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), expand=c(0, 0)) + >> "Processing - %~n1.r"
echo 	scale_color_manual(name=NULL, values=c("black", "blue", "green", "red"), labels=paste(c(blackplot, blueplot, greenplot, redplot)), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	theme(legend.position = "top") + >> "Processing - %~n1.r"
echo 	expand_limits(y=c(0, 1000/30))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo  	ggsave(filename="Multi - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Multi - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Box Plots - this will show box plots for each second>> "Processing - %~n1.r"
echo if(%boxplot%) {>> "Processing - %~n1.r"
echo #note that with limits on the y-axis, box plots might be removed if they go out of bounds
echo 	ggplot(results, aes(results$TimeInSeconds, results$MsBetweenPresents)) + >> "Processing - %~n1.r"
echo 	ggtitle(paste("Frame Time Course with Boxplots", recording, sep = ""), subtitle="MsBetweenPresents") + >> "Processing - %~n1.r"
echo 	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + >> "Processing - %~n1.r"
echo 	expand_limits(y=c(0, min(1000/30, max(results$MsBetweenPresents)))) + >> "Processing - %~n1.r"
echo 	geom_point(color="red") + >> "Processing - %~n1.r"
echo 	geom_boxplot(aes(group=cut_width(results$TimeInSeconds, 1)), color="black", outlier.alpha=0)>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo  	ggsave(filename="Box Secs - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Box Secs - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"

::echo.>> "Processing - %~n1.r"
::echo #	ggplot(results, aes(results$TimeInSeconds, results$MsBetweenPresents)) + ggtitle("Box Plot of Frame Times") + >> "Processing - %~n1.r"
::echo #this will be a boxplot for the entire data set>> "Processing - %~n1.r"
::echo #geom_boxplot()>> "Processing - %~n1.r"
::echo #boxplot for entire data set, but with different quantiles>> "Processing - %~n1.r"
::echo #geom_boxplot(aes(ymin=min(results$MsBetweenPresents), lower=quantile(results$MsBetweenPresents, .01), middle=median(results$MsBetweenPresents), upper=quantile(results$MsBetweenPresents, .99), ymax=max(results$MsBetweenPresents)))>> "Processing - %~n1.r"
::echo  	#ggsave(filename="Box - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
::echo  	#ggsave(filename="Box - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"

echo #Frame Latency (maybe) - Difference between time to display the frame and the GPU time to render it>> "Processing - %~n1.r"
echo if(%latency%) {>> "Processing - %~n1.r"
echo 	framelag = round(100*(1-ecdf(results$MsUntilDisplayed - results$MsUntilRenderComplete)(0)),3)>> "Processing - %~n1.r"
echo 	test = c(sum(results$MsUntilDisplayed - results$MsUntilRenderComplete^<0), sum(results$MsUntilDisplayed - results$MsUntilRenderComplete^>0))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	laggraph = 	ggplot(results) + geom_point(aes(TimeInSeconds, MsUntilDisplayed - MsUntilRenderComplete), color="black") + ggtitle(paste("Latency", recording, sep = ""), subtitle="MsUntilDisplayed - MsUntilRenderComplete") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/ytimes, 2)), expand=c(0, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30))>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	poslag = laggraph + geom_hline(yintercept = c(quantile(results$MsUntilDisplayed - results$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(results$MsUntilDisplayed - results$MsUntilRenderComplete), color="blue")>> "Processing - %~n1.r"
echo 	zerolag = laggraph>> "Processing - %~n1.r"
echo 	#neglag = laggraph + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag,"%%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag,"%%"), hjust="left", vjust="top")>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	posonly = results[which(results$MsUntilDisplayed - results$MsUntilRenderComplete ^> 0), ]>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	neglag = laggraph + geom_hline(yintercept = c(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete), color="blue") + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag, "%%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag, "%%"), hjust="left", vjust="top")>> "Processing - %~n1.r"
echo.>> "Processing - %~n1.r"
echo 	testswitch=sum(test==0)+1>> "Processing - %~n1.r"
echo 	switch(testswitch, >> "Processing - %~n1.r"
echo		ggsave(filename="Latency - %~n1.png", plot=neglag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), >> "Processing - %~n1.r"
echo		ggsave(filename="Latency - %~n1.png", plot=poslag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), >> "Processing - %~n1.r"
echo		ggsave(filename="Latency - %~n1.png", plot=zerolag, device="png", width=16, height=9, dpi=DPI, scale=ggscale)) >> "Processing - %~n1.r"


echo 	#switch(testswitch,neglag,poslag,zerolag)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Latency - %~n1.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)>> "Processing - %~n1.r"
echo  	#ggsave(filename="Latency - %~n1.pdf", device="pdf", width=16, height=9, scale=ggscale)>> "Processing - %~n1.r"
echo }>> "Processing - %~n1.r"

::echo 	ggplot(results) + geom_point(aes(TimeInSeconds, MsUntilDisplayed - MsUntilRenderComplete), color="black") + ggtitle("Latency", subtitle="MsUntilDisplayed - MsUntilRenderComplete") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/c(120, 60, 30, 20, 15, 12, 10), 2)), expand=c(0,0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60)) + expand_limits(y=c(0, 1000/30))>> "Processing - %~n1.r"
::echo #Frame Latency (maybe) - MsUntilDisplayed is from Present start to Displayed>> "Processing - %~n1.r"
::echo #	ggplot(results[which(results$Dropped==0),]) + geom_point(aes(TimeInSeconds, MsUntilDisplayed), color="black") + ggtitle("MsUntilDisplayed", subtitle="Time between present start and frame display") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/c(120, 60, 30, 20, 15, 12, 10), 2)), expand=c(0,0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60)) + expand_limits(y=c(0, 1000/30))>> "Processing - %~n1.r"


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

::echo plot(results$TimeInSeconds, results$MsBetweenPresents,xlab="Time in Seconds",ylab="Frame Time in ms",h=1000/60) >> "Processing - %~n1.r"

echo library(ggplot2)>> "Processing - %~n1.r"
echo png(file="%graph%%~n1 - Course.png",width=1920,height=1080,res=150)>> "Processing - %~n1.r"
echo ggplot(results, aes(TimeInSeconds, MsBetweenPresents), xlab="Time in Seconds", ylab="Frame Times in ms", main="Frame Times Through Course") + geom_point() + geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + expand_limits(y = c(0, 1000/30))>> "Processing - %~n1.r"
echo dev.off()>> "Processing - %~n1.r"

::pause