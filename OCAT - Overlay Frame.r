library(readr)
library(ggplot2)
library(foreach)
library(doParallel)

#setwd("!PATH!")
results <- read_csv("!FILEX!")
dir.create("Frames - !FILE!", showWarnings=FALSE)
setwd("Frames - !FILE!")

count = 1/60
back = 60
#	how many previous frames to also show

pdf(NULL)

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

ggplot(results, aes(TimeInSeconds,MsBetweenPresents)) + 
scale_y_continuous(name=NULL,breaks=round(1000/c(120, 60, 30, 20, 15, 12, 10), 2),expand=c(0,0),limits=c(0,min(100, max(results$MsBetweenPresents))), sec.axis=dup_axis(), minor_breaks=NULL) + 
scale_x_continuous(name=NULL,breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), labels=round(seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/6), 3), minor_breaks=seq(from=0,to=round(max(results$TimeInSeconds), 0),by=1/60), expand=c(0,0), limits=c((place-back)*(count),(place)*(count))) + 
expand_limits(y=c(0,1000/30)) + 
geom_hline(yintercept = c(quantile(results$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + 
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + geom_point(color="white") + geom_path(color="white") 

ggsave(filename=sprintf("%05d.png",place), device="png", width=20.48, height=3.84, dpi=100)
}
