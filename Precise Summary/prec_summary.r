library(readr)
#	loads the readr library for R to use

args	=	commandArgs(trailingOnly=TRUE)
#	gets the arguments passed to this script, which are the path of the file dropped on the Python script and the name of a file in that folder

PATH	=	args[1]
FILE	=	args[2]
#	more convenient names for the two arguments

setwd(PATH)
#	sets the working directory for this script
dataIN	=	read_csv(FILE)
#	reads the current CSV into a variable so its data can be processed

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
#	list of values that will be placed in the prec_summary.csv output
#		file name
#		application name
#		mean FPS
#		mean frame time (ms)
#		95% of frame time
#		99% of frame time
#		99.9% of frame time
#		mean of estimated driver lag
#		system configuration information also recorded

dataOUT	=	data.frame(dataOUT)
#	makes dataOUT into a data frame
names(dataOUT)	=	cols_SUM
#	sets the column names for dataOUT

prec_sum	=	data.frame()
#	creates an empty data frame named prec_sum
if	(file.exists("prec_summary.csv"))	{
#	checks if there is already a prec_summary.csv file in the directory
	prec_sum	=	read_csv("prec_summary.csv")
#		contents of the prec_summary.csv file are read into prec_sum
}
prec_sum	=	rbind(prec_sum, dataOUT)
#	the contents of prec_sum and the results of processing the current file are bound together
colnames(prec_sum)	=	cols_SUM
#	the column names are set for the new prec_sum

write_csv(prec_sum, "prec_summary.csv")
#	prec_sum is written to a CSV