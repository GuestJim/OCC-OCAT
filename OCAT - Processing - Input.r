library(readr)
#	loads the library for reading CSVs into R
library(ggplot2)
#	loads the GGPlot2 library for generating graphs

if (interactive()) { 
	setwd("!PATH!")
} else {
	pdf(NULL)
}
#	interactive() will be TRUE when in the GUI and FALSE when not
#		it is necessary to set the working directory when in the GUI, but the directory of the script is the working directory when run otherwise
#	when not run in the GUI, rplots.pdf is made but PDF(null) prevents this

results <- read_csv("!FILEX!")
#	reads the CSV to the results dataframe
fileName = "!FILE!"
#	the filename is needed in the Output.r script, so it is assigned to an R variable, preserving the script needing changes
FPS <- hist(results$TimeInSeconds, breaks=300,plot=FALSE)$counts
#	is a count of the number of frames each second
DIFF = diff(results$MsBetweenPresents)
#	the difference between consecutive MsBetweenPresents values

game = "!GAME!"
#	variable for identifying the game the data is for
GPU = "!GPU!"
#	variable for identifying the GPU the data is for

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]
#	version of the 'game' variable for use in filenames
if (GPU != paste0("!GPU","!"))	{
	gameGPU = paste0(game, " (", GPU, ")")
}	else	{
	gameGPU = game
}
#	checks if the GPU is set and if so will create a string combining it and the game name

recording = "Recording !REC!"
#	identifies the recording number

graphs = FALSE
#	for diabling the creationg of graphs
ggdevice = "png"
#	sets whether you want graphs output as PNG or as PDF

if(exists("recording")) {
#	checks if the recording variable exists, which it should, but just in case
	recordnam = paste0(" - (", recording, ")")
#		to specify the specific recording, as there are often multiple
#		by separating the Recording # from the string used for naming, it makes the filenames and such cleaner
	setname = paste0(" - \n", game, recordnam)
#		for identifying the game and recording set in the graphs, but should be manually set, so leaving blank is preferred here
} else {
	recording = character(0)
	setname = character(0)
#		gives default empty values for these variables so they can still be used without error
}

textOUT		=	TRUE
#	control for if there should be a TXT output including various statistics

textFRAM	=	TRUE
graphFRAM	=	TRUE
#	controls for if you want statistics and graphs for the Frame Time data

textDISP	=	FALSE
graphDISP	=	FALSE
#	controls for if you want statistics and graphs for the Display Time data

textDIFF	=	FALSE
graphDIFF	=	FALSE
#	controls for if you want statistics and graphs for the consecutive difference data
#		it shouold be noted the Frame/Display Time vs. Consecutive Frame/Display Time graphs are not controlled by this

listFPS		=	NULL
#	this is for the ecdfFPS function function, allowing one to more easily add to the list

if (!graphs){
	graphFRAM = FALSE
	graphDISP = FALSE
	graphDIFF = FALSE
}
#	instead of having an exit for graphs, this will just disable the creation of all of the graphs

source("OCAT - Processing - Output.r")
#	reads the Output.r script in the same folder as this script