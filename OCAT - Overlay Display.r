library(readr)
library(ggplot2)
library(foreach)
library(doParallel)

if (interactive()) { 
	setwd("!PATH!")
} else {
	pdf(NULL) #prevents rplots.pdf from being generated
}
results <- read_csv("!FILEX!")
dir.create("Display - !FILE!", showWarnings=FALSE)
setwd("Display - !FILE!")

count = 1/60
back = 60
#	how many previous frames to also show

labelRound = function(x) sprintf("%.3f", x)

registerDoParallel(cores=detectCores() - 4)

foreach (place=seq(from=0, to=round(max(results$TimeInSeconds)*60,0)-1, by=1), .packages = "ggplot2") %dopar% {

theme_update(
panel.background = element_rect(fill="black"),
plot.background = element_rect(fill="black"), 
axis.text = element_text(color="white", size=12),
text = element_text(color="white", size=16),
panel.grid.major=element_line(color="gray"),
panel.grid.minor=element_line(color="gray20")
)

ggplot(results, aes(TimeInSeconds,MsBetweenDisplayChange)) + 
scale_y_continuous(name=NULL, breaks=round(seq(from=1, to=ceiling(max(results$MsBetweenDisplayChange*60/1000)), by=1)*1000/60,2),labels=seq(from=1, to=ceiling(max(results$MsBetweenDisplayChange*60/1000)), by=1), expand=c(0,0), limits=c(0,min(1001/6,max(results$MsBetweenDisplayChange))), sec.axis=dup_axis(), minor_breaks=NULL) + 
scale_x_continuous(name=NULL,breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), labels=labelRound, minor_breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/60), limits=c((place-back)*(count),(place)*(count)), expand=c(0,0)) + 
expand_limits(y=c(0,1000/30)) + geom_step(color="green") + 
geom_point(color="white", shape=9, size=2) #+ geom_path(color="white")

ggsave(filename=sprintf("%05d.png",place), device="png", width=20.48, height=3.84, dpi=100)

dev.off(dev.prev())
}

dev.off(dev.prev())