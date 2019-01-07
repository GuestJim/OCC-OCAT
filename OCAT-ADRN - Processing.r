library(readr)
#	loads the library for reading CSVs into R
library(ggplot2)
#	loads the GGPlot2 library for generating graphs
#setwd("!PATH!")
#	sets the working directory
#		checked and when not using the GUI, the scripts location is the working directory, so this is not necessary and impairs working across computers
#		keeping it though for when working in the GUI though
ADRN <- read_csv("!FILEADRNX!")
OCAT <- read_csv("!FILEOCATX!")
#	read the input CSVs to appropriate data frames
FPS <- hist(results$TimeInSeconds, breaks=300,plot=FALSE)$counts
#	is a count of the number of frames each second
DIFF = diff(results$MsBetweenPresents)
#	the difference between consecutive MsBetweenPresents values
colnames(ADRN) = c("gpuUTIL","gpuSCLK","gpuMCLK","TEMP","PWR","FAN","FPS","cpuUTIL","ramUTIL")
#	sets better column names for the Adrenalin data frame
#		it is critical to remove spaces from the column names

game = character(0)
game = "!FILE!"
#	sets a variable for easier identification of what is being worked on

titled = FALSE
#	indicates if a title has been given to the data, besides the filename
graphs = FALSE
#	should the graphs be made or not because sometimes you only want the characteristics

if(titled) {
#	control for if a title for the dataset has been provided or not
	recording = "Recording 1"
	recordnam = paste(" - (", recording, ")",sep="")
	#	to specify the specific recording, as there are often multiple
	#		character(0) keeps it empty as currently it will use the CSV file name in the game variable, making this unnecessary in general
	#	by separating the Recording # from the string used for naming, it makes the filenames and such cleaner
	setname = paste(" - \n", game, recordnam, sep = "")
	#	for identifying the game and recording set in the graphs, but should be manually set, so leaving blank is preferred here
} else {
	recording = character(0)
	setname = character(0)
	#	gives default empty values for these variables so they can still be used without error
}

pdf = TRUE
#	TRUE to save graphs as PDFs and FALSE for PNGs
DPI = 120
ggscale = 1 
theme_set(theme_grey(base_size = 16))
#	options for the size of the output graphs

Time = data.frame(-1:(dim(ADRN)[1]-2))
colnames(Time) = "TimeInSeconds"
ADRN = cbind(Time, ADRN)
#	adds a TimeInSeconds column to the Adrenalin data for syncing with the OCAT data
#		Adrenalin polls once per second at best

ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)
#	lists for frame rates of interest, both positive and negative for some of the graph axes

options(error=expression(NULL))
#	supresses errors if there are issue when running the script

if(TRUE) {
sink("OCAT-ADRN - !FILEOCAT!.txt", split=TRUE)
#	creates a device for saving output to a text file
writeLines("OCAT-ADRN - !FILEOCAT!")
#Frame Time/Rate
writeLines("\nMean")
print(mean(OCAT$MsBetweenPresents))
writeLines("\nMedian")
print(median(OCAT$MsBetweenPresents))
#writeLines("\nAverage FPS")
#print(1000/mean(OCAT$MsBetweenPresents))
#print(mean(FPS))
writeLines("\nAverage of FPS")
print(length(OCAT$TimeInSeconds)/max(OCAT$TimeInSeconds))
writeLines("\nRatio Dropped Frames")
print(sum(OCAT$Dropped)/length(OCAT$Dropped))
writeLines("\nPercentiles (Frame Time)")
writeLines("0.1%,\t1%,\t99%,\t99.9%")
print(round(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))
writeLines("\nPercentiles (Frame Rate)")
writeLines("0.1%,\t1%,\t99%,\t99.9%")
print(round(1000/quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999)), 2))
writeLines("\nECDF")
writeLines("60 FPS,\t50 FPS,\t30 FPS,\t20 FPS,\t15 FPS")
print(100*(1-ecdf(OCAT$MsBetweenPresents)(c(1000/60, 1000/50, 1000/30, 1000/20, 1000/15))))
writeLines("\nMedian")
print(median(OCAT$MsBetweenPresents))
writeLines("\nStandard Deviation")
print(sd(OCAT$MsBetweenPresents))

writeLines("\nDiff Percentiles")
print(quantile(DIFF, c(.001, .01, .99, 0.999)))
writeLines("\nECDF Diff")
writeLines("-16.667,\t-8.333,\t8.333,\t16.667")
print(100*(ecdf(DIFF)(c(-1000/60, -1000/120, 1000/120, 1000/60))))
inc = length(subset(diff(OCAT$MsBetweenPresents), diff(OCAT$MsBetweenPresents) > 0))
dec = length(subset(diff(OCAT$MsBetweenPresents), diff(OCAT$MsBetweenPresents) <= 0))
writeLines("\nDiff Balance")
print(dec/(dec+inc));print(inc/(dec+inc))

writeLines("\nMsUntilDisplayed - MsUntilRenderComplete")
	posonly = OCAT[which(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete > 0), ]
print(round(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .50, .99, 0.999)), 2))

writeLines("\nAdrenalin Data")
writeLines("\nPower Summary (W)")
print(summary(ADRN$PWR))
writeLines("\nTotal Energy (J, W * s)")
print(sum(ADRN$PWR))
	#measurements made once per second so no need to multiply by seconds
writeLines("\nGPU Clock Summary")
print(summary(ADRN$gpuSCLK))
writeLines("\nFan Summary")
print(summary(ADRN$FAN))

if(FALSE) {
#	looks at the MsBetweenDisplayChange data with dropped frames skipped
#	if(FALSE) means it will not be done
	cleanDisplay = OCAT$MsBetweenDisplayChange[!OCAT$MsBetweenDisplayChange==0] 

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

if(TRUE) {
#Frame Time Course and Power

scaleP = (400 / 70)
#	for the graphs combining different kinds of data, it can be necessary to

ggplot() + ggtitle(paste("Frame Times Through Course with ASIC Power", setname, sep = ""), subtitle="MsBetweenPresents") +
scale_fill_gradient2("Power (W)", low="blue", mid = "green", midpoint = 225,  high="red", limits = c(50, 400), breaks = c(75,150,225,300,375)) + 
geom_col(data = ADRN, aes(x = TimeInSeconds, y = PWR/scaleP, fill=PWR)) + 
geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + 
geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleP, name = "ASIC Power (W)", c(75,150,225,300,375))) + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + 
theme(legend.position = c(1, 0.9))
#theme(legend.position="top", legend.justification="right", legend.margin=margin(0, 0, 0, 0), legend.box.margin=margin(-10, 0, -5, -10))

if (pdf) {
		ggsave(filename=paste(game, " - ", recording, " - Power.pdf", sep=""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste(game, " - ", recording, " - Power.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

if(TRUE) {
#Frame Time Course and Temperature

scaleT = (090 / 70)
#	for the graphs combining different kinds of data, it can be necessary to

ggplot() + ggtitle(paste("Frame Times Through Course with GPU Temperature", setname, sep = ""), subtitle="MsBetweenPresents") +
scale_fill_gradient2(expression(Temp~(degree*C)), low="blue", mid = "green", midpoint = 60, high="red", limits=c(30, 85)) +
geom_col(data = ADRN, aes(x = TimeInSeconds, y = TEMP/scaleT, fill=TEMP)) + 
geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + 
geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleT, name = expression(Temperature~(degree*C)), seq(35, 85, 10))) + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + 
theme(legend.position = c(1, 0.9))

if (pdf) {
		ggsave(filename=paste(game, " - ", recording, " - Temp.pdf", sep=""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste(game, " - ", recording, " - Temp.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

if(TRUE) {
#Frame Time Course and Fan Speed (RPM)

scaleF = (3000 / 70)
#	for the graphs combining different kinds of data, it can be necessary to

ggplot() + ggtitle(paste("Frame Times Through Course with Fan Speed", setname, sep = ""), subtitle="MsBetweenPresents") +
scale_fill_gradient2("Fan Speed (RPM)", low="blue", mid = "green", midpoint = 2000, high="red", limits=c(500, 3000)) +
geom_col(data = ADRN, aes(x = TimeInSeconds, y = FAN/scaleF, fill=FAN)) + 
geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + 
geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleF, name = "Fan Speed (RPM)", seq(500, 3000, 500))) + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + 
theme(legend.position = c(1, 0.9))

if (pdf) {
		ggsave(filename=paste(game, " - ", recording, " - Fan.pdf", sep=""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste(game, " - ", recording, " - Fan.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

if(TRUE) {
#Frame Time Course and Core Clock

scaleC = (1700 / 70)
#	for the graphs combining different kinds of data, it can be necessary to

ggplot() + ggtitle(paste("Frame Times Through Course with Core Clock", setname, sep = ""), subtitle="MsBetweenPresents") +
scale_fill_gradient2("Core Clock (MHz)", low="blue", mid = "green", midpoint = 1350, high="red", limits=c(500, 1700)) +
geom_col(data = ADRN, aes(x = TimeInSeconds, y = gpuSCLK/scaleC, fill=gpuSCLK)) + 
geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black") + 
geom_smooth(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), method="gam", formula= y ~ s(x, bs = "cs")) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0), sec.axis = sec_axis(~.*scaleC, name = "Core Clock (MHz)", seq(500, 1700, 250))) + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + 
theme(legend.position = c(1, 0.9))

if (pdf) {
		ggsave(filename=paste(game, " - ", recording, " - SClock.pdf", sep=""), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste(game, " - ", recording, " - SClock.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Frame Time
if(FALSE) {
	ggplot(OCAT, aes(MsBetweenPresents)) + 
	ggtitle(paste("Frequency Plot of Frame Times", setname, sep = ""), subtitle="MsBetweenPresents") + 
	geom_freqpoly(binwidth=0.03, size=0) + 
	scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=ceiling(max(OCAT$MsBetweenPresents, 1000/60)), by=1000/120), labels=round(seq(from=0, to=ceiling(max(OCAT$MsBetweenPresents, 1000/60)), by=1000/120), 2), limits=c(NA, min(max(OCAT$MsBetweenPresents), 100)), expand=c(0.02, 0)) + 
	expand_limits(x=c(1000/60, 1000/30)) + scale_y_continuous(name="Count", expand=c(0.02, 0))
	
if (pdf) {
		ggsave(filename=paste("Frame Times - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Frame Times - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Frame Rate
if(FALSE) {
	ggplot(OCAT, aes(1000/MsBetweenPresents)) + 
	ggtitle(paste("Frequency Plot of Frame Rates", setname, sep = ""), subtitle="1000 * MsBetweenPresents^-1") + 
	geom_freqpoly(binwidth=1, size=0) + 
	scale_x_continuous(name="Frame Rate (FPS)", breaks=c(120, 60, 30, 20, 15, 10,0), expand=c(0.02, 0)) + 
	expand_limits(x=c(30, 60)) + scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
		ggsave(filename=paste("Frame Rates - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Frame Rates - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Display Time
if(FALSE) {
	ggplot(as.data.frame(OCAT$MsBetweenDisplayChange), aes(OCAT$MsBetweenDisplayChange*60/1000)) + 
	geom_freqpoly(binwidth=0.003, size=0) + 
	ggtitle(paste("Frequency Plot of Display Times", setname, sep = ""), subtitle="MsBetweenDisplayChange") + 
	scale_x_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(OCAT$MsBetweenDisplayChange*60/1000)), by=1), minor_breaks=NULL, limits=c(0, min(max(OCAT$MsBetweenDisplayChange*60/1000),15)), expand=c(0.02, 0)) + 
	expand_limits(x=c(0, 2)) + 
	scale_y_continuous(name="Count", expand=c(0.02, 0))
	
if (pdf) {
		ggsave(filename=paste("Display Times - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Display Times - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Frame Time
if(FALSE) {
	ggplot(OCAT, aes(TimeInSeconds, MsBetweenPresents)) + 
	ggtitle(paste("Frame Times Through Course", setname, sep = ""), subtitle="MsBetweenPresents") + 
	geom_point() + 
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	expand_limits(y=c(0, 1000/30)) + 
	geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red")
#for a boxplot add this
#	+ geom_boxplot(aes(x=330, y=OCAT$MsBetweenPresents), width=25)
#	for quantile regression use this instead
#	stat_quantile(quantiles = c(0.001, 0.01, 0.99, 0.999), color = "red")

if (pdf) {
		ggsave(filename=paste("Course - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Course - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#1s Frame Counts Graph
if(FALSE) {
	#write.table(FPS, file="Default - Adrenalin FPS.txt", sep=",", col.names=FALSE)
	ggplot(OCAT, aes(TimeInSeconds, fill=..count..)) + 
	geom_histogram(data=OCAT, binwidth=1, center=0.5, col="black", na.rm=TRUE) + 
	ggtitle(paste("Frames Rendered per Second", setname, sep = ""), subtitle="TimeInSeconds count") + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	scale_fill_gradient2("Frames", low="red", mid="green", high="blue", midpoint=60, expand=c(0.02, 0)) + 
	scale_y_continuous(name="Frames per Second", limits=c(0, max(60,FPS)), breaks=seq(from=0, to=max(60,FPS), by=30), expand=c(0.02, 0))
	
if (pdf) {
		ggsave(filename=paste("FPS - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("FPS - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#QQ Plot
if(FALSE) {
	ggplot(OCAT, aes(sample=MsBetweenPresents)) + 
	ggtitle(paste("QQ Distrubtion Plot", setname, sep = ""), subtitle="MsBetweenPresents") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2), quantile(OCAT$MsBetweenPresents, .9999)), labels=c(0, round(1000/ytimes, 2), paste(round(quantile(OCAT$MsBetweenPresents, .9999), 2), "\n(99.99%)")), limits=c(0, max(quantile(OCAT$MsBetweenPresents, .9999), 1000/60)), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "1", "50 (Median)", "99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + 
	annotate("rect", ymin=-Inf, ymax=quantile(OCAT$MsBetweenPresents, c(.001, .010, .500, .990, .999)), xmin=-Inf, xmax=qnorm(c(.001, .010, .500, .990, .999)), alpha=0.1, fill=c("blue", "blue", "blue", "red", "red"), color="grey") + geom_point(stat="qq")
	
if (pdf) {
		ggsave(filename=paste("QQ - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("QQ - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Display Time
if(FALSE) {
	ggplot(OCAT, aes(TimeInSeconds, OCAT$MsBetweenDisplayChange*60/1000)) + 
	ggtitle(paste("Display Times Through Course", setname, sep = ""), subtitle="MsBetweenDisplayChange") + 
	scale_y_continuous(name="Frames Later (1/60 s)", breaks=seq(from=0, to=ceiling(max(OCAT$MsBetweenPresents, 1000/60)), by=1), minor_breaks=NULL, limits=c(NA, ceiling(min(max(OCAT$MsBetweenDisplayChange*60/1000), 15))), expand=c(0.02, 0)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	expand_limits(y=c(0, 3)) + 
	geom_point() + 
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs"))
	
if (pdf) {
		ggsave(filename=paste("Course Display - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Course Display - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Course - Diff
if(FALSE) {
	ggplot(as.data.frame(DIFF), aes(x=OCAT$TimeInSeconds[-1], y=DIFF)) + 
	ggtitle(paste("Difference Course Plot", setname, sep = ""), subtitle="MsBetweenPresent consecutive difference") + 
	geom_point() + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/50, 1000/50), expand=c(0, 0)) + 
	expand_limits(y=c(-1000/30, 1000/30)) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0))
	
if (pdf) {
		ggsave(filename=paste("Course Diff - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Course Diff - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#QQ Diff
if(FALSE) {
	ggplot(as.data.frame(DIFF), aes(sample=DIFF)) + 
	ggtitle(paste("Difference QQ Distrubtion Plot", setname, sep = ""), subtitle="MsBetweenPresent consecutive differences") + 
	expand_limits(y=c(-1000/50, 1000/50)) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), limits=c(-1000/30, 1000/30), expand=c(0, 0)) + 
	scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .25, .5, .75, .99, .999)),labels=c("0.1", "1","25", "50\n(Median)", "75","99","99.9"), minor_breaks=NULL, expand=c(0.02, 0)) + 
	geom_point(stat="qq")
	
if (pdf) {
		ggsave(filename=paste("QQ Diff - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("QQ Diff - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
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

	ggplot(OCAT) + 
	ggtitle(paste("Multi-data Plot", setname, sep='')) + 
	geom_point(aes_string("TimeInSeconds", paste(y=redplot)) + aes(color="red")) + 
	geom_point(aes_string("TimeInSeconds", paste(y=greenplot)) + aes(color="green")) + 
	geom_point(aes_string("TimeInSeconds", paste(y=blueplot)) + aes(color="blue")) + 
	geom_point(aes_string("TimeInSeconds", paste(y=blackplot)) + aes(color="black")) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=round(1000/ytimes, 2), expand=c(0, 0)) + 
	scale_color_manual(name=NULL, values=c("black", "blue", "green", "red"), labels=paste(c(blackplot, blueplot, greenplot, redplot)), expand=c(0.02, 0)) + s
	cale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	theme(legend.position = "top") + 
	expand_limits(y=c(0, 1000/30))
	
if (pdf) {
		ggsave(filename=paste("Multi - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Multi - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Box Plots - this will show box plots for each second
if(FALSE) {
	ggplot(OCAT, aes(OCAT$TimeInSeconds, OCAT$MsBetweenPresents)) + 
	ggtitle(paste("Frame Time Course with Boxplots", setname, sep = ""), subtitle="MsBetweenPresents") + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,100), expand=c(0.02, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + 
	expand_limits(y=c(0, min(1000/30, max(OCAT$MsBetweenPresents)))) + geom_point(color="red") + 
	geom_boxplot(aes(group=cut_width(OCAT$TimeInSeconds, 1)), color="black", outlier.alpha=0)
	
if (pdf) {
		ggsave(filename=paste("Box Secs - ", game, " - ", recording, ".pdf", sep=''), device="pdf", width=16, height=9, scale=ggscale)
	} else {
		ggsave(filename=paste("Box Secs - ", game, " - ", recording, ".png", sep=''), device="png", width=16, height=9, dpi=DPI, scale=ggscale)
	}
}

#Frame Latency (maybe) - Difference between time to display the frame and the GPU time to render it
if(FALSE) {
	framelag = round(100*(1-ecdf(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete)(0)),3)
	test = c(sum(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete<0), sum(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete>0))

	laggraph = 	ggplot(OCAT) + geom_point(aes(TimeInSeconds, MsUntilDisplayed - MsUntilRenderComplete), color="black") + ggtitle(paste("Latency", setname, sep = ""), subtitle="MsUntilDisplayed - MsUntilRenderComplete") + scale_y_continuous(name="Frametimes (ms)", breaks=c(0, round(1000/ytimes, 2)), expand=c(0, 0)) + scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30))

	poslag = laggraph + geom_hline(yintercept = c(quantile(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete), color="blue")
	zerolag = laggraph
	#neglag = laggraph + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag,"%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag,"%"), hjust="left", vjust="top")

	posonly = OCAT[which(OCAT$MsUntilDisplayed - OCAT$MsUntilRenderComplete > 0), ]

	neglag = laggraph + geom_hline(yintercept = c(quantile(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete, c(.001, .01, .99, 0.999))), color="red") + geom_hline(yintercept = median(posonly$MsUntilDisplayed - posonly$MsUntilRenderComplete), color="blue") + geom_hline(yintercept=0) + geom_label(data=data.frame(x=0, y=0), x=0, y=1, label=paste(framelag, "%"), hjust="left", vjust="bottom") + geom_label(data=data.frame(x=0, y=0), x=0, y=-1, label=paste(100-framelag, "%"), hjust="left", vjust="top")

	testswitch=sum(test==0)+1
	
if (pdf) {
	switch(testswitch, 
	ggsave(filename=paste("Latency - ", game, " - ", recording, ".pdf", sep=''), plot=neglag, device="pdf", width=16, height=9, scale=ggscale), 
	ggsave(filename=paste("Latency - ", game, " - ", recording, ".pdf", sep=''), plot=poslag, device="pdf", width=16, height=9, scale=ggscale), 
	ggsave(filename=paste("Latency - ", game, " - ", recording, ".pdf", sep=''), plot=zerolag, device="pdf", width=16, height=9, scale=ggscale)) 
	} else {
	switch(testswitch, 
	ggsave(filename=paste("Latency - ", game, " - ", recording, ".png", sep=''), plot=neglag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste("Latency - ", game, " - ", recording, ".png", sep=''), plot=poslag, device="png", width=16, height=9, dpi=DPI, scale=ggscale), 
	ggsave(filename=paste("Latency - ", game, " - ", recording, ".png", sep=''), plot=zerolag, device="png", width=16, height=9, dpi=DPI, scale=ggscale)) 
	}
	#switch(testswitch,neglag,poslag,zerolag)
 	#ggsave(filename="Latency - Default - Adrenalin.png", device="png", width=16, height=9, dpi=DPI, scale=ggscale)
 	#ggsave(filename="Latency - Default - Adrenalin.pdf", device="pdf", width=16, height=9, scale=ggscale)
}
