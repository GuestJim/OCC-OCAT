library(readr)

args	=	commandArgs(trailingOnly=TRUE)

PATH	=	args[1]
FILE	=	args[2]

setwd(PATH)
dataIN	=	read_csv(FILE)

cols_SUM	=	c(
	"File",
	"Application Name",
#	"Compositor",
#	"Date and Time",
	"Average FPS (Application)",
	"Average frame time (ms) (Application)",
	"95th-percentile frame time (ms) (Application)",
	"99th-percentile frame time (ms) (Application)",
	"99.9th-percentile frame time (ms) (Application)",
#	"Missed frames (Application)",
#	"Average number of missed frames (Application)",
#	"Maximum number of consecutive missed frames (Application)",
#	"Missed frames (Compositor)",
#	"Average number of missed frames (Compositor)",
#	"Maximum number of consecutive missed frames (Compositor)",
	"Average Estimated Driver Lag (ms)",
	"Width",
	"Height",
#	"User Note",
	"Motherboard",
	"OS",
	"Processor",
	"System RAM",
	"Base Driver Version",
	"Driver Package",
	"GPU #",
	"GPU",
	"GPU Core Clock (MHz)",
	"GPU Memory Clock (MHz)",
	"GPU Memory (MB)"
)
#	these are the column names from the original summary file
#		some cannot be calculated new, so they are commented out

dataOUT	=	c(
	FILE,
	dataIN$Application[1],
	round(1000/mean(dataIN$MsBetweenPresents), 2),
	round(mean(dataIN$MsBetweenPresents), 4),
	quantile(dataIN$MsBetweenPresents, 0.95),
	quantile(dataIN$MsBetweenPresents, 0.99),
	quantile(dataIN$MsBetweenPresents, 0.999),
	mean(dataIN$MsEstimatedDriverLag),
	dataIN[1, grep("Width", colnames(dataIN)):ncol(dataIN)]
	)

dataOUT	=	data.frame(dataOUT)
names(dataOUT)	=	cols_SUM

prec_sum	=	data.frame()
if	(file.exists("prec_summary.csv"))	{
	prec_sum	=	read_csv("prec_summary.csv")
}
prec_sum	=	rbind(prec_sum, dataOUT)
colnames(prec_sum)	=	cols_SUM

write_csv(prec_sum, "prec_summary.csv")