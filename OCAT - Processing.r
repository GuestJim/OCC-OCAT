library(readr)
#	loads the library for reading CSVs into R
library(ggplot2)
#	loads the GGPlot2 library for generating graphs
setwd("!PATH!")
#	sets the working directory
results <- read_csv("!FILEX!")
#	reads the CSV to the results dataframe
FPS <- hist(results$TimeInSeconds, breaks=300,plot=FALSE)$counts
#	is a count of the number of frames each second
DIFF = diff(results$MsBetweenPresents)
#	the difference between consecutive MsBetweenPresents values

game = ""
game = "!FILE!"
#	sets a variable for easier identification of what is being worked on

recording = " - (Recording 1)"
recording = character(0)
#	to specify the specific recording, as there are often multiple
#		character(0) keeps it empty as currently it will use the CSV file name in the game variable, making this unnecessary in general

setname = paste(" - \n", game, recording, sep = "")
setname = character(0)
#	for identifying the game and recording set in the graphs, but should be manually set, so leaving blank is preferred here

pdf = TRUE
#	TRUE to save graphs as PDFs and FALSE for PNGs
DPI = 120
ggscale = 1 
theme_set(theme_grey(base_size = 16))
#	options for the size of the output graphs

ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)
#	lists for frame rates of interest, both positive and negative for some of the graph axes

options(error=expression(NULL))
#	supresses errors if there are issue when running the script

if(TRUE) {
sink("Output - !FILE!.txt", split=TRUE)
#	creates a device for saving output to a text file
writeLines("!FILE!")
#Frame Time/Rate
writeLines("\nMean")
print(mean(results$MsBetweenPresents))
writeLines("\nMedian")
print(median(results$MsBetweenPresents))
#writeLines("\nAverage FPS")
#print(1000/mean(results$MsBetweenPresents))
#print(mean(FPS))
writeLines("\nAverage of FPS")
print(length(results$TimeInSeconds)/max(results$TimeInSeconds))
writeLines("\nRatio Dropped Frames")
print(sum(results$Dropped)/length(results$Dropped))
writeLines("\nPercentiles (Frame Time)")
writeLines("0.1%,\t1%,\t99%,\t99.9%")
print(round(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))
writeLines("\nPercentiles (Frame Rate)")
writeLines("0.1%,\t1%,\t99%,\t99.9%")
print(round(1000/quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))
writeLines("\nECDF")
writeLines("60 FPS,\t50 FPS,\t30 FPS,\t20 FPS,\t15 FPS")
print(100*(1-ecdf(results$MsBetweenPresents)(c(1000/60, 1000/50, 1000/30, 1000/20, 1000/15))))
writeLines("\nMedian")
print(median(results$MsBetweenPresents))
writeLines("\nStandard Deviation")
print(sd(results$MsBetweenPresents))

writeLines("\nDiff Percentiles")
print(quantile(DIFF, c(.001, .01, .99, 0.999)))
writeLines("\nECDF Diff")
writeLines("-16.667,\t-8.333,\t8.333,\t16.667")
print(100*(ecdf(DIFF)(c(-1000/60, -1000/120, 1000/120, 1000/60))))
inc = length(subset(diff(results$MsBetweenPresents), diff(results$MsBetweenPresents) > 0))
dec = length(subset(diff(results$MsBetweenPresents), diff(results$MsBetweenPresents) <= 0))
writeLines("\nDiff Balance")
print(dec/(dec+inc));print(inc/(dec+inc))

writeLines("\nMsUntilDisplayed - MsUntilRenderComplete")
	posonly = results[which(results$MsUntilDisplayed - results$MsUntilRenderComplete > 0), ]
print(round(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .50, .99, 0.999)), 2))

if(FALSE) {
#	looks at the MsBetweenDisplayChange data with dropped frames skipped
#	if(FALSE) means it will not be done
	cleanDisplay = results$MsBetweenDisplayChange[!results$MsBetweenDisplayChange==0] 

	writeLines("\nDisplay Change Percentiles")
	print(quantile(cleanDisplay, c(.001, .01, .99, 0.999)))
	print(quantile(1000/cleanDisplay, c(.001, .01, .99, 0.999)))

	writeLines("\nECDF")
	writeLines("60 FPS,\t50 FPS,\t30 FPS,\t20 FPS,\t15 FPS")
	print(100*(1-ecdf(cleanDisplay)(c(1000/60, 1000/50, 1000/30, 1000/20, 1000/15))))

	writeLines("\nMedian Display Change")
	print(median(cleanDisplay))

	writeLines("\nStandard Deviation Display Change")
	print(sd(cleanDisplay))

	writeLines("\nQuantile Display Change")
	print(quantile(diff(cleanDisplay), c(.001, .01, .99, 0.999)))
}

sink()
#	closes the text file, applying all of this information to it
}

pdf(NULL)
#	prevents rplots.pdf from being generated

#Frame Time
if(TRUE) {
	ggplot(results, aes(MsBetweenPresents)) + 
	ggtitle(paste("Frequency Plot of Frame Times", setname, sep = ""), subtitle="MsBetweenPresents") + 
	geom_freqpoly(binwidth=0.03, size=0) + 
	scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1000/120), labels=round(seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1000/120), 2), limits=c(NA, min(max(results$MsBetweenPresents), 100)), expand=c(0.02, 0)) + expand_limits(x=c(1000/60, 1000/30)) + 
	scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
 	ggsave(filename=paste(game, " - Frame Times", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - Frame Times", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Frame Rate
if(TRUE) {
	ggplot(results, aes(1000/MsBetweenPresents)) + 
	ggtitle(paste("Frequency Plot of Frame Rates", setname, sep = ""), subtitle="1000 * MsBetweenPresents^-1") + 
	geom_freqpoly(binwidth=1, size=0) + 
	scale_x_continuous(name="Frame Rate (FPS)", breaks=c(120, 60, 30, 20, 15, 10,0), expand=c(0.02, 0)) + expand_limits(x=c(30, 60)) + 
	scale_y_continuous(name="Count", expand=c(0.02, 0))

	ggsave(filename=paste(game, " - Frame Rates", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
#	ggsave(filename=paste(game, " - Frame Rates", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
}

#Display Time
if(TRUE) {
	ggplot(as.data.frame(results$MsBetweenDisplayChange), aes(results$MsBetweenDisplayChange*60/1000)) + 
	ggtitle(paste("Frequency Plot of Display Times", setname, sep = ""), subtitle="MsBetweenDisplayChange") + 
	geom_freqpoly(binwidth=0.003, size=0) + 
	scale_x_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenDisplayChange*60/1000)), by=1), minor_breaks=NULL, limits=c(0, min(max(results$MsBetweenDisplayChange*60/1000),15)), expand=c(0.02, 0)) + expand_limits(x=c(0, 2)) + 
	scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
	ggsave(filename=paste(game, " - Display Times", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - Display Times", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Frame Time
if(TRUE) {
	ggplot(results, aes(TimeInSeconds, MsBetweenPresents)) + 
	ggtitle(paste("Frame Times Through Course", setname, sep = ""), subtitle="MsBetweenPresents") + 
	geom_point() + 
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
	geom_hline(yintercept = c(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red")
#for a boxplot added this
#	+ geom_boxplot(aes(x=330, y=results$MsBetweenPresents), width=25)
#	for quantile regression use this instead
#	stat_quantile(quantiles = c(0.001, 0.01, 0.99, 0.999), color = "red")

if (pdf) {
	ggsave(filename=paste(game, " - Course", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - Course", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#1s Frame Counts Graph
if(FALSE) {
	#write.table(FPS, file="Agony-Win64-Shipping.exe_20180530-104054_RecordingResults-0 FPS.txt", sep=",", col.names=FALSE)
	ggplot(results, aes(TimeInSeconds, fill=..count..)) + 
	ggtitle(paste("Frames Rendered per Second", setname, sep = ""), subtitle="TimeInSeconds count") + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	geom_histogram(data=results, binwidth=1, center=0.5, col="black", na.rm=TRUE) + 
	scale_fill_gradient2("Frames", low="red", mid="green", high="blue", midpoint=60, expand=c(0.02, 0)) + 
	scale_y_continuous(name="Frames per Second", limits=c(0, max(60,FPS)), breaks=seq(from=0, to=max(60,FPS), by=30), expand=c(0.02, 0))

if (pdf) {
	ggsave(filename=paste(game, " - FPS", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - FPS", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#QQ Plot
if(TRUE) {
	ggplot(results, aes(sample=MsBetweenPresents)) + 
	ggtitle(paste("QQ Distrubtion Plot", setname, sep = ""), subtitle="MsBetweenPresents") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2), quantile(results$MsBetweenPresents, .9999)), labels=c(0, round(1000/ytimes, 2), paste(round(quantile(results$MsBetweenPresents, .9999), 2), "\n(99.99%)")), limits=c(0, max(quantile(results$MsBetweenPresents, .9999), 1000/60)), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "1", "50 (Median)", "99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + 
	annotate("rect", ymin=-Inf, ymax=quantile(results$MsBetweenPresents, c(.001, .010, .500, .990, .999)), xmin=-Inf, xmax=qnorm(c(.001, .010, .500, .990, .999)), alpha=0.1, fill=c("blue", "blue", "blue", "red", "red"), color="grey") + geom_point(stat="qq")

if (pdf) {
	ggsave(filename=paste(game, " - QQ", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - QQ", recording, ".png", sep = ""), width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Display Time
if(TRUE) {
	ggplot(results, aes(TimeInSeconds, results$MsBetweenDisplayChange*60/1000)) + 
	ggtitle(paste("Display Times Through Course", setname, sep = ""), subtitle="MsBetweenDisplayChange") + 
	geom_point() + 
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
	scale_y_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1), minor_breaks=NULL, limits=c(NA, ceiling(min(max(results$MsBetweenDisplayChange*60/1000), 10))), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 3))

if (pdf) {
	ggsave(filename=paste(game, " - Course Display", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
 	ggsave(filename=paste(game, " - Course Display", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Diff
if(FALSE) {
	ggplot(as.data.frame(DIFF), aes(x=results$TimeInSeconds[-1], y=DIFF)) + 
	ggtitle(paste("Difference Course Plot", setname, sep = ""), subtitle="MsBetweenPresent consecutive difference") + 
	geom_point() + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/50, 1000/50), expand=c(0, 0)) + expand_limits(y=c(-1000/30, 1000/30)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0))

if (pdf) {
	ggsave(filename=paste(game, " - Course Diff", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
 	ggsave(filename=paste(game, " - Course Diff", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#QQ Diff
if(TRUE) {
	ggplot(as.data.frame(DIFF), aes(sample=DIFF)) + 
	ggtitle(paste("Difference QQ Distrubtion Plot", setname, sep = ""), subtitle="MsBetweenPresent consecutive differences") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/30, 1000/30), expand=c(0, 0)) + expand_limits(y=c(-1000/50, 1000/50)) + 
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .25, .5, .75, .99, .999)),labels=c("0.1", "1","25", "50\n(Median)", "75","99","99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + geom_point(stat="qq")

if (pdf) {
	ggsave(filename=paste(game, " - QQ Diff", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - QQ Diff", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Multi-data Graph
if(FALSE) {

	`+.uneval` <- function(a,b) {
	`class<-`(modifyList(a,b), "uneval")
	}

	redplot = "MsUntilRenderComplete"
	greenplot = "MsUntilDisplayed"
	blueplot = "MsBetweenDisplayChange"
	blackplot = "MsBetweenPresents"

	ggplot(results) + 
	ggtitle(recording("Multi-data Plot", setname, sep = "")) + 
	geom_point(aes_string("TimeInSeconds", paste(y=redplot)) + aes(color="red")) + 
	geom_point(aes_string("TimeInSeconds", paste(y=greenplot)) + aes(color="green")) + 
	geom_point(aes_string("TimeInSeconds", paste(y=blueplot)) + aes(color="blue")) + 
	geom_point(aes_string("TimeInSeconds", paste(y=blackplot)) + aes(color="black")) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), expand=c(0, 0)) + 
	scale_color_manual(name=NULL, values=c("black", "blue", "green", "red"), labels=paste(c(blackplot, blueplot, greenplot, redplot)), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	theme(legend.position = "top") + 
	expand_limits(y=c(0, 1000/30))

if (pdf) {
	ggsave(filename=paste(game, " - Multi", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - Multi", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Box Plots - this will show box plots for each second
if(FALSE) {
	ggplot(results, aes(results$TimeInSeconds, results$MsBetweenPresents)) + 
	ggtitle(paste("Frame Time Course with Boxplots", setname, sep = ""), subtitle="MsBetweenPresents") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	expand_limits(y=c(0, min(1000/30, max(results$MsBetweenPresents)))) + 
	geom_point(color="red") + 
	geom_boxplot(aes(group=cut_width(results$TimeInSeconds, 1)), color="black", outlier.alpha=0)

if (pdf) {
	ggsave(filename=paste(game, " - Box Secs", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	} else {
	ggsave(filename=paste(game, " - Box Secs", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
	}
}

#Frame Latency (maybe) - Difference between time to display the frame and the GPU time to render it
if(FALSE) {
	framelag = round(100*(1-ecdf(results$MsUntilDisplayed - results$MsUntilRenderComplete)(0)),3)
	test = c(sum(results$MsUntilDisplayed - results$MsUntilRenderComplete<0), sum(results$MsUntilDisplayed - results$MsUntilRenderComplete>0))

	laggraph = 	ggplot(results) + geom_point(aes(TimeInSeconds, MsUntilDisplayed - MsUntilRenderComplete), color="black") + ggtitle(paste("Latency", setname, sep = ""), subtitle="MsUntilDisplayed - MsUntilRenderComplete") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/ytimes, 2)), expand=c(0, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30))

	poslag = laggraph + geom_hline(yintercept = c(quantile(results$MsUntilDisplayed - results$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(results$MsUntilDisplayed - results$MsUntilRenderComplete), color="blue")
	zerolag = laggraph
	#neglag = laggraph + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag,"%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag,"%"), hjust="left", vjust="top")

	posonly = results[which(results$MsUntilDisplayed - results$MsUntilRenderComplete > 0), ]

	neglag = laggraph + geom_hline(yintercept = c(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete), color="blue") + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag, "%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag, "%"), hjust="left", vjust="top")

	testswitch=sum(test==0)+1
if (pdf) {
	switch(testswitch, 
	ggsave(filename=paste(game, " - Latency", recording, ".pdf", sep = ""), plot=neglag, device="pdf", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste(game, " - Latency", recording, ".pdf", sep = ""), plot=poslag, device="pdf", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste(game, " - Latency", recording, ".pdf", sep = ""), plot=zerolag, device="pdf", width=16, height=9, dpi=DPI, scale=ggscale)) 
	} else {
	switch(testswitch, 
	ggsave(filename=paste(game, " - Latency", recording, ".png", sep = ""), plot=neglag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste(game, " - Latency", recording, ".png", sep = ""), plot=poslag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste(game, " - Latency", recording, ".png", sep = ""), plot=zerolag, device="png", width=16, height=9, dpi=DPI, scale=ggscale)) 
	}
#	switch(testswitch,neglag,poslag,zerolag)
#	ggsave(filename=paste(game, " - Latency", recording, ".png", sep = ""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
#	ggsave(filename=paste(game, " - Latency", recording, ".pdf", sep = ""), device="pdf", width=16, height=9, scale=ggscale)
}
