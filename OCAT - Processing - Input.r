library(readr)
library(ggplot2)

if (interactive()) { 
	setwd("!PATH!")
} else {
	pdf(NULL) #prevents rplots.pdf from being generated
}

results <- read_csv("!FILEX!")
fileName = "!FILE!"
FPS <- hist(results$TimeInSeconds, breaks=300,plot=FALSE)$counts
DIFF = diff(results$MsBetweenPresents)

game = "!GAME!"

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]

recording = "Recording !REC!"

graphs = FALSE
ggdevice = "png"

if(exists("recording")) {
	recordnam = paste0(" - (", recording, ")")
	setname = paste0(" - \n", game, recordnam)
} else {
	recording = character(0)
	setname = character(0)
}

textOUT = TRUE

textFRAM = TRUE
graphFRAM = TRUE

textDISP = FALSE
graphDISP = FALSE

textDIFF = FALSE
graphDIFF = FALSE

if (!graphs){
	graphFRAM = FALSE
	graphDISP = FALSE
	graphDIFF = FALSE
}

source("OCAT - Processing - Output.r")