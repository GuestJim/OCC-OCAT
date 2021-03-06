library(readr)
library(ggplot2)

if (interactive()) { 
	setwd("!PATH!")
} else {
	pdf(NULL) #prevents rplots.pdf from being generated
}

results <- read_csv("!FILEX!")
FPS <- hist(results$TimeInSeconds, breaks=300,plot=FALSE)$counts
DIFF = diff(results$MsBetweenPresents)

game = "!GAME!"

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]

titled = FALSE
graphs = FALSE

if(titled) {
	recording = "Recording 1"
	recordnam = paste0(" - (", recording, ")")
	setname = paste0(" - \n", game, recordnam)
} else {
	recording = character(0)
	setname = character(0)
}

pdf = FALSE
DPI = 120
ggscale = 1 
theme_set(theme_grey(base_size = 16))

ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)

labelRound = function(x) sprintf("%.2f", x)

options(error=expression(NULL), width = 1000)

percFPS = function(x, listPERC = c(0.1, 1, 99, 99.9), r = 2) {
	if (max(listPERC) > 1) listPERC = listPERC/100
	out = c()
	for (i in listPERC) {
		temp = c(1000/quantile(x, i), quantile(x, i))
		names(temp) = paste0(i * 100, c("% (ms)", "% (FPS)"))
		out = append(out, temp)
	}
	return(round(out, r))
}

percFPSv = function(x, listPERC = c(0.1, 1, 99, 99.9), r = 2) {
	if (max(listPERC) > 1) listPERC = listPERC/100
	out = c()
	for (i in listPERC) {
		temp = rbind(1000/quantile(x, i), quantile(x, i))
		names(temp) = paste0(i * 100, "%")
		rownames(temp) = c("FPS", "ms")
		out = cbind(out, temp)
	}
	return(round(out, r))
}

ecdfFPS = function(x, listFPS=c(60, 50, 30, 20, 15), r = 2) {
	out = 100*(1-ecdf(x)(1000/listFPS))
	names(out) = paste0(listFPS, " FPS")
	return(round(out, r))
}

ecdfMSd = function(x, listFPS=1000/c(-60, -120, 120, 60), r = 7) {
	out = 100*(ecdf(x)(listFPS))
	names(out) = paste0(round(listFPS, 2), " ms")
	return(round(out, r))
}

if(TRUE) {
sink("Output - !FILE!.txt", split=TRUE)
writeLines("!FILE!")
if (titled) {
	writeLines(paste0(game, recordnam))
}
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
writeLines("\nPercentiles")
print(percFPSv(results$MsBetweenPresents))
writeLines("\nECDF")
print(ecdfFPS(results$MsBetweenPresents))
writeLines("\nMedian")
print(median(results$MsBetweenPresents))
writeLines("\nStandard Deviation")
print(sd(results$MsBetweenPresents))

writeLines("\nDiff Percentiles")
print(quantile(DIFF, c(.001, .01, .99, 0.999)))
writeLines("\nECDF Diff")
print(ecdfMSd(DIFF, 1000/c(-60, -120, 120, 60)))
inc = length(subset(diff(results$MsBetweenPresents), diff(results$MsBetweenPresents) > 0))
dec = length(subset(diff(results$MsBetweenPresents), diff(results$MsBetweenPresents) <= 0))
writeLines("\nDiff Balance")
print(dec/(dec+inc));print(inc/(dec+inc))

writeLines("\nMsUntilDisplayed - MsUntilRenderComplete")
	posonly = results[which(results$MsUntilDisplayed - results$MsUntilRenderComplete > 0), ]
print(round(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .50, .99, 0.999)), 2))

if(FALSE) {
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
}

if (!graphs){
	q()
}

#Frame Time
if(TRUE) {
	ggplot(results, aes(MsBetweenPresents)) + 
	ggtitle(paste0("Frequency Plot of Frame Times", setname), subtitle="MsBetweenPresents") + 
	geom_freqpoly(binwidth=0.03, size=0) + 
	scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1000/120), labels=labelRound, limits=c(NA, min(max(results$MsBetweenPresents), 100)), expand=c(0.02, 0)) + 
	expand_limits(x=c(1000/60, 1000/30)) + 
	scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
 	ggsave(filename=paste0(gameF, " - ", recording, " - Frame Times.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Frame Times.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Frame Rate
if(TRUE) {
	ggplot(results, aes(1000/MsBetweenPresents)) + 
	ggtitle(paste0("Frequency Plot of Frame Rates", setname), subtitle="1000 * MsBetweenPresents^-1") + 
	geom_freqpoly(binwidth=1, size=0) + 
	scale_x_continuous(name="Frame Rate (FPS)", breaks=c(120, 60, 30, 20, 15, 10,0), expand=c(0.02, 0)) + expand_limits(x=c(30, 60)) + 
	scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Frame Rates.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Frame Rates.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Display Time
if(TRUE) {
	ggplot(as.data.frame(results$MsBetweenDisplayChange), aes(results$MsBetweenDisplayChange*60/1000)) + 
	ggtitle(paste0("Frequency Plot of Display Times", setname), subtitle="MsBetweenDisplayChange") + 
	geom_freqpoly(binwidth=0.003, size=0) + 
	scale_x_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenDisplayChange*60/1000)), by=1), minor_breaks=NULL, limits=c(0, min(max(results$MsBetweenDisplayChange*60/1000),15)), expand=c(0.02, 0)) + 
	expand_limits(x=c(0, 2)) + 
	scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Display Times.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Display Times.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Frame Time
if(TRUE) {
	ggplot(results, aes(TimeInSeconds, MsBetweenPresents)) + 
	ggtitle(paste0("Frame Times Through Course", setname), subtitle="MsBetweenPresents") + 
	geom_point() + 
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA, 100), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
	geom_hline(yintercept = c(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red")
#for a boxplot added this
#	+ geom_boxplot(aes(x=330, y=results$MsBetweenPresents), width=25)
#	for quantile regression use this instead
#	stat_quantile(quantiles = c(0.001, 0.01, 0.99, 0.999), color = "red")

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Course.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Course.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#1s Frame Counts Graph
if(FALSE) {
	#write.table(FPS, file="Agony-Win64-Shipping.exe_20180530-104054_RecordingResults-0 FPS.txt", sep=",", col.names=FALSE)
	ggplot(results, aes(TimeInSeconds, fill=..count..)) + 
	ggtitle(paste0("Frames Rendered per Second", setname), subtitle="TimeInSeconds count") + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	geom_histogram(data=results, binwidth=1, center=0.5, col="black", na.rm=TRUE) + 
	scale_fill_gradient2("Frames", low="red", mid="green", high="blue", midpoint=60, expand=c(0.02, 0)) + 
	scale_y_continuous(name="Frames per Second", limits=c(0, max(60,FPS)), breaks=seq(from=0, to=max(60,FPS), by=30), expand=c(0.02, 0))

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - FPS.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - FPS.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#QQ Plot
if(TRUE) {
	ggplot(results, aes(sample=MsBetweenPresents)) + 
	ggtitle(paste("QQ Distrubtion Plot", setname, sep = ""), subtitle="MsBetweenPresents") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(0, max(quantile(results$MsBetweenPresents, .9999), 1000/60)), expand=c(0.02, 0), sec.axis = sec_axis(~., breaks = quantile(results$MsBetweenPresents, .9999), labels = paste0(round(quantile(results$MsBetweenPresents, .9999), 2), "\n(99.99%)"))) + 
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "1", "50 (Median)", "99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + 
	annotate("rect", ymin=-Inf, ymax=quantile(results$MsBetweenPresents, c(.001, .010, .500, .990, .999)), xmin=-Inf, xmax=qnorm(c(.001, .010, .500, .990, .999)), alpha=0.1, fill=c("blue", "blue", "blue", "red", "red"), color="grey") + 
	geom_point(stat="qq")

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - QQ.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - QQ.png"), width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Display Time
if(FALSE) {
	ggplot(results, aes(TimeInSeconds, results$MsBetweenDisplayChange*60/1000)) + 
	ggtitle(paste0("Display Times Through Course", setname), subtitle="MsBetweenDisplayChange") + 
	geom_point() + 
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
	scale_y_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/60)), by=1), minor_breaks=NULL, limits=c(NA, 6), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 3))

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Course Display.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
 	ggsave(filename=paste0(gameF, " - ", recording, " - Course Display.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Diff
if(FALSE) {
	ggplot(as.data.frame(DIFF), aes(x=results$TimeInSeconds[-1], y=DIFF)) + 
	ggtitle(paste0("Difference Course Plot", setname), subtitle="MsBetweenPresent consecutive difference") + 
	geom_point(alpha = 0.1) + 
	geom_hline(yintercept = 0, color = "red") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/50, 1000/50), expand=c(0, 0)) + expand_limits(y=c(-1000/30, 1000/30)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0))

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Course Diff.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
 	ggsave(filename=paste0(gameF, " - ", recording, " - Course Diff.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

quant = c(0.1, 1, 25, 50, 75, 99, 99.9) / 100

#QQ Diff
if(FALSE) {
	ggplot(as.data.frame(DIFF), aes(sample=DIFF)) + 
	ggtitle(paste0("Difference QQ Distrubtion Plot", setname), subtitle="MsBetweenPresent consecutive differences") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/30, 1000/30), expand=c(0, 0)) + expand_limits(y=c(-1000/50, 1000/50)) + 
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .25, .5, .75, .99, .999)),labels=c("0.1", "1","25", "50\n(Median)", "75","99","99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + 
	geom_point(stat="qq") + 
	annotate("label", x = qnorm(quant), y = quantile(DIFF, quant, names = FALSE) + 2, label = round(quantile(DIFF, quant, names = FALSE), 2))

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - QQ Diff.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - QQ Diff.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Plotting Frame Time and Diff
if(TRUE){
	ggplot(data=results, aes(x=results$MsBetweenPresents, y=rbind(c(diff(results$MsBetweenPresents), 0))[1,])) + 
	ggtitle(paste0("Frame Times vs Frame Time Difference (next frame)", setname), subtitle="MsBetweenPresent consecutive differences") + 
	annotate("rect", ymin=quantile(diff(results$MsBetweenPresents), c(.001, .010)), ymax=quantile(diff(results$MsBetweenPresents), c(.999, .990)), xmin=quantile(results$MsBetweenPresents, c(.001, .010)), xmax=quantile(results$MsBetweenPresents, c(.999, .990)), alpha=c(0.1, 0.075), fill=c("red", "blue")) + 
	geom_point(alpha = 0.1) + 
	stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
	geom_point(x=median(results$MsBetweenPresents), y=median(diff(results$MsBetweenPresents)), color = "magenta", shape ="x") + 
	scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenPresents, 1000/30)), by=1000/120), labels=labelRound, limits=c(0, 1000/10), expand=c(0.02, 0)) + 
	scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))
	
if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Time vs Diff.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Time vs Diff.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

resultsClean = results[!(results$MsBetweenDisplayChange ==0), ]
#	removes the dropped frames, which have a 0 for MsBetweenDisplayChange

#Plotting Display Time and Diff
if(FALSE){
	ggplot(data=results, aes(x=results$MsBetweenDisplayChange, y=rbind(c(diff(results$MsBetweenDisplayChange), 0))[1,])) + 
	ggtitle(paste0("Display Times vs Display Time Difference (next frame)", setname), subtitle="MsBetweenDisplayChange consecutive differences") + 
	annotate("rect", ymin=quantile(diff(results$MsBetweenDisplayChange), c(.001, .010)), ymax=quantile(diff(results$MsBetweenDisplayChange), c(.999, .990)), xmin=quantile(results$MsBetweenDisplayChange, c(.001, .010)), xmax=quantile(results$MsBetweenDisplayChange, c(.999, .990)), alpha=c(0.1, 0.075), fill=c("red", "blue")) + 
	geom_point(alpha = 0.1) + 
	stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
	geom_point(x=median(results$MsBetweenDisplayChange), y=median(diff(results$MsBetweenDisplayChange)), color = "magenta", shape ="x") + 
	scale_x_continuous(name="Refresh Cycles Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(results$MsBetweenDisplayChange, 1000/30)), by=1000/120), labels=labelRound, limits=c(0, 1000/10), expand=c(0.02, 0)) + 
	scale_y_continuous(name="Consecutive Display Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))
	
if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Time vs Diff - Display.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Time vs Diff - Display.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
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
	ggtitle(recording("Multi-data Plot", setname)) + 
	geom_point(aes_string("TimeInSeconds", paste0(y=redplot)) + aes(color="red")) + 
	geom_point(aes_string("TimeInSeconds", paste0(y=greenplot)) + aes(color="green")) + 
	geom_point(aes_string("TimeInSeconds", paste0(y=blueplot)) + aes(color="blue")) + 
	geom_point(aes_string("TimeInSeconds", paste0(y=blackplot)) + aes(color="black")) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), expand=c(0, 0)) + 
	scale_color_manual(name=NULL, values=c("black", "blue", "green", "red"), labels=paste0(c(blackplot, blueplot, greenplot, redplot)), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	theme(legend.position = "top") + 
	expand_limits(y=c(0, 1000/30))

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Multi.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Multi.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Box Plots - this will show box plots for each second
if(FALSE) {
	ggplot(results, aes(results$TimeInSeconds, results$MsBetweenPresents)) + 
	ggtitle(paste0("Frame Time Course with Boxplots", setname), subtitle="MsBetweenPresents") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	expand_limits(y=c(0, min(1000/30, max(results$MsBetweenPresents)))) + 
	geom_point(color="red") + 
	geom_boxplot(aes(group=cut_width(results$TimeInSeconds, 1)), color="black", outlier.alpha=0)

if (pdf) {
	ggsave(filename=paste0(gameF, " - ", recording, " - Box Secs.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	} else {
	ggsave(filename=paste0(gameF, " - ", recording, " - Box Secs.pdf"), device="pdf", width=16, height=9, scale=ggscale)
	}
}

#Frame Latency (maybe) - Difference between time to display the frame and the GPU time to render it
if(FALSE) {
	framelag = round(100*(1-ecdf(results$MsUntilDisplayed - results$MsUntilRenderComplete)(0)),3)
	test = c(sum(results$MsUntilDisplayed - results$MsUntilRenderComplete<0), sum(results$MsUntilDisplayed - results$MsUntilRenderComplete>0))

	laggraph = 	ggplot(results) + geom_point(aes(TimeInSeconds, MsUntilDisplayed - MsUntilRenderComplete), color="black") + ggtitle(paste0("Latency", setname), subtitle="MsUntilDisplayed - MsUntilRenderComplete") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/ytimes, 2)), expand=c(0, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30))

	poslag = laggraph + geom_hline(yintercept = c(quantile(results$MsUntilDisplayed - results$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(results$MsUntilDisplayed - results$MsUntilRenderComplete), color="blue")
	zerolag = laggraph
	#neglag = laggraph + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste0(framelag,"%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste0(100-framelag,"%"), hjust="left", vjust="top")

	posonly = results[which(results$MsUntilDisplayed - results$MsUntilRenderComplete > 0), ]

	neglag = laggraph + geom_hline(yintercept = c(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete), color="blue") + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste0(framelag, "%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste0(100-framelag, "%"), hjust="left", vjust="top")

	testswitch=sum(test==0)+1
if (pdf) {
	switch(testswitch, 
	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.pdf"), plot=neglag, device="pdf", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.pdf"), plot=poslag, device="pdf", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.pdf"), plot=zerolag, device="pdf", width=16, height=9, dpi=DPI, scale=ggscale)) 
	} else {
	switch(testswitch, 
	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.png"), plot=neglag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.png"), plot=poslag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.png"), plot=zerolag, device="png", width=16, height=9, dpi=DPI, scale=ggscale)) 
	}	
#	switch(testswitch,neglag,poslag,zerolag)
#	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.png"), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
#	ggsave(filename=paste0(gameF, " - ", recording, " - Latency.pdf"), device="pdf", width=16, height=9, scale=ggscale)
}
