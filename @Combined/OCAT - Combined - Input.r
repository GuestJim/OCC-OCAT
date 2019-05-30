library(readr)
library(ggplot2)
#	the libraries for R to load
#		readr	is for reading the CSVs
#		ggplot2	is for generating the graphs

game = "!GAME!"
cGPU = "!GPU!"
#	variables for the game name and current GPU
#		the !...! is replaced by the Python script

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]
#	changes characters in the game name to create a version safe for filenames

theme_set(theme_grey(base_size = 16))
DPI = 120
#	graph configuration information for font size and DPI for the produced images
ggdevice = "png"
#	sets that the graphs should be saved as PNGs. Can also be PDFs

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	FALSE
graphs_all	=	FALSE
#	these are controls for if there whould be TXT files produced with statistics, HTML files with tables, and the graphs

textFRAM	=	TRUE
graphFRAM	=	TRUE
#	specific controls for frame time data

textDISP	=	FALSE
graphDISP	=	FALSE
#	specific controls for display time data

textAPI		=	FALSE
textLOC		=	FALSE
#	will generate TXT and HTML, if HTML is enabled, files for each API/Location

textDIFF	=	FALSE
graphDIFF	=	FALSE
#	specific controls for consecutive frame time difference data

listFPS		=	NULL
#	for adding to the FPS Percentile list

if (!textOUT)	{
	textFRAM	=	FALSE
	textDISP	=	FALSE
	HTMLOUT		=	FALSE
	textDIFF	=	FALSE
}

if (!graphs){
	graphFRAM	=	FALSE
	graphDISP	=	FALSE
	graphDIFF	=	FALSE
}
#	control so all text or graphs can be disabled more easily

if (interactive()) {
	setwd("!PATH!")
} else {
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#	prevents rplots.pdf from being generated

resultsFull <- read_csv("@Combined - !QUA!.csv", col_types = "????????????????????c")
#	reads the CSV data in and states the last column, the API column, is of type Character
#		the !QUA! is changed by the Python script
results = resultsFull
#	so other scripts that are called can continue using the results variable, but a complete copy of the CSV is still kept in memory

listGPU = c(
"RX 580",
"RX Vega 64",
"GTX 770",
"GTX 980",
"GTX 1070",
"GTX 1080",
"RTX 2060",
"RTX 2080"
)
#	list of GPUs

listQUA = c(
"!QUA!"
)
#	the list of qualities, which the Python script fills

listLOC = c(
!LOC!
)
#	list of locations for the data, with !LOC! being replaced by the Python script

listAPI = c(
!API!
)
#	list of APIs used, with !API! replaced by the Python script

results$GPU = factor(results$GPU, levels = listGPU)
results$Quality = factor(results$Quality, levels = listQUA)
if (length(listLOC[1]) != 0) {
	results$Location = factor(results$Location, levels = listLOC)
}
results$API = factor(results$API, levels = listAPI, ordered = TRUE)
#	makes the lists into type Factor, which is necessary for some things like the graph facets

if (levels(results$Quality)[1] != "Review")	{
	QUA = paste0(levels(results$Quality)[1], " Quality")
	qua = paste0(levels(results$Quality)[1])
}	else	{
	QUA = "Review"
}
#	unless the data is for a review instead of a performance analysis, it will create a copy of the list that adds Quality to the end
#		'High' becomes 'High Quality'

gameQUA = paste0(game, " - ", QUA)
#	game name and quality name together
gameFQUA = paste0(gameF, " - ", QUA)
#	filename version of game with quality including 'Quality' at end
gameFqua = paste0(gameF, " - ", qua)
#	filename version of game with quality
gameGPU = paste0(game, " (", cGPU, ")")
#	game name with GPU

if (length(unique(results$GPU)) == 1 & nchar(listAPI[1]) > 1)	{
	gameFQUA = paste0(gameFQUA, " - ", listAPI[1])
	gameFqua = paste0(gameFqua, " - ", listAPI[1])
}
#	this is only necessary for the GPU scripts, not the High scripts, so this checks for if multiple GPUs are in the data
source("@Combined - Output - !TYPE!.r")
#	calls the appropriate output R script
#		!TYPE! is set by the Python script

if	(graphs_all)	{
#	the following code will work through each location, running the output script for these individual runs

for	(loc in listLOC)	{
	results = resultsFull[resultsFull$Location == loc, ]
#		subsets the data frame for just those entries of the current location
	recording = paste0(qua, " - ", loc)
	if (nchar(listAPI[1]) > 1)	{
		recording = paste0(recording, " - ", listAPI[1])
	}
	fileName = paste0(gameF, " - ", recording)
#	creates 'recording' and 'fileName' variables that are needed for the script

	if(exists("recording")) {
		recordnam = paste0(" - (", recording, ")")
		setname = paste0(" - \n", game, recordnam)
#		checks if the 'recording' variable exists and then creates appropriate variables using it
#			likely not necessary, but here just in case
	} else {
		recording = character(0)
		setname = character(0)
#		if 'recording' does not exist, it makes empty variablse for the script to use
	}
	
	source("OCAT - Processing - Output.r")
#		calls the output script for the individual data runs
}
}