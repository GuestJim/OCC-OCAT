library(readr)
#	loads the library for reading CSVs into R
library(ggplot2)
#	loads the GGPlot2 library for generating graphs
library(foreach)
library(doParallel)
#	both foreach and doParallel are for making this multithreaded

#setwd("!PATH!")
#	sets the working directory
#		checked and when not using the GUI, the scripts location is the working directory, so this is not necessary and impairs working across computers
#		keeping it though for when working in the GUI though
results <- read_csv("!FILEX!")
#	reads the CSV to the results data frame
dir.create("Frames - !FILE!", showWarnings=FALSE)
#	creates a directory for the overlay graphs to be saved to
setwd("Frames - !FILE!")
#	sets the working directory to the folder for the graphs

count = 1/60
back = 60
#	how many previous frames to also show

pdf(NULL)
#	to prevent an unnecessary PDF from being made

registerDoParallel(cores=detectCores() - 4)
#	sets how many threads can be used by detecting the number present and subtracting 4, for continued system usability

foreach (place=seq(from=0, to=round(max(results$TimeInSeconds)*60,0)-1, by=1), .packages = "ggplot2") %dopar% {
#	the foreach command with %dopar% will spread the work across multiply threads
#	foreach does require any necessary packages by identified, in this case ggplot2
	
theme_update(
panel.background = element_rect(fill="black"),
plot.background = element_rect(fill="black"), 
axis.text = element_text(color="white", size=12),
text = element_text(color="white", size=16),
panel.grid.major=element_line(color="gray"),
panel.grid.minor=element_line(color="gray20")
)
#	sets a custom theme for all of the graphs
	
ggplot(results, aes(TimeInSeconds,MsBetweenPresents)) + 
scale_y_continuous(name=NULL,breaks=round(1000/c(120, 60, 30, 20, 15, 12, 10), 2),expand=c(0,0),limits=c(0,min(100, max(results$MsBetweenPresents))), sec.axis=dup_axis(), minor_breaks=NULL) + 
scale_x_continuous(name=NULL,breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), labels=round(seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), 3), minor_breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/60), expand=c(0,0), limits=c((place-back)*(count),(place)*(count))) + 
expand_limits(y=c(0,1000/30)) + 
geom_hline(yintercept = c(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + 
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + geom_point(color="white") + geom_path(color="white") 
#	makes the graphs
	
ggsave(filename=sprintf("%05d.png",place), device="png", width=20.48, height=3.84, dpi=100)
#	I record at 2048x1152 so these images will be 2048x384 (one-third height)

dev.off(dev.prev())
#	this closes the previous device, which is how the images are made and not closing them properly breaks R with too many devices
}

dev.off(dev.prev())
#	also outside to make sure the devices are cleared
