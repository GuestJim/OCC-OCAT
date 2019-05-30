library(readr)
library(ggplot2)

game = "!GAME!"
cGPU = "!GPU!"

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]

theme_set(theme_grey(base_size = 16))
DPI = 120
ggdevice = "png"

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	FALSE
graphs_all	=	FALSE

textFRAM	=	TRUE
graphFRAM	=	TRUE

textDISP	=	FALSE
graphDISP	=	FALSE

textAPI		=	FALSE
textLOC		=	FALSE
#	will generate TXT and HTML, if HTML is enabled, files for each API/Location

textDIFF	=	FALSE
graphDIFF	=	FALSE

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

if (interactive()) {
	setwd("!PATH!")
} else {
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#	prevents rplots.pdf from being generated

resultsFull <- read_csv("@Combined - !QUA!.csv", col_types = "????????????????????c")
results = resultsFull

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

listQUA = c(
"!QUA!"
)

listLOC = c(
!LOC!
)
#	add the necessary scripting to replace this using a Location.txt file or to make it ""

listAPI = c(
!API!
)

results$GPU = factor(results$GPU, levels = listGPU)
results$Quality = factor(results$Quality, levels = listQUA)
if (length(listLOC[1]) != 0) {
	results$Location = factor(results$Location, levels = listLOC)
}
results$API = factor(results$API, levels = listAPI, ordered = TRUE)

if (levels(results$Quality)[1] != "Review")	{
	QUA = paste0(levels(results$Quality)[1], " Quality")
	qua = paste0(levels(results$Quality)[1])
}	else	{
	QUA = "Review"
}

gameQUA = paste0(game, " - ", QUA)
gameFQUA = paste0(gameF, " - ", QUA)
gameFqua = paste0(gameF, " - ", qua)
gameGPU = paste0(game, " (", cGPU, ")")

if (length(unique(results$GPU)) == 1 & nchar(listAPI[1]) > 1)	{
	gameFQUA = paste0(gameFQUA, " - ", listAPI[1])
	gameFqua = paste0(gameFqua, " - ", listAPI[1])
}
#	this is only necessary for the GPU scripts, not the High scripts, so this checks for if multiple GPUs are in the data
source("@Combined - Output - !TYPE!.r")

if	(graphs_all)	{

for	(loc in listLOC)	{
	results = resultsFull[resultsFull$Location == loc, ]
	
	recording = paste0(qua, " - ", loc)
	if (nchar(listAPI[1]) > 1)	{
		recording = paste0(recording, " - ", listAPI[1])
	}
	fileName = paste0(gameF, " - ", recording)

	if(exists("recording")) {
		recordnam = paste0(" - (", recording, ")")
		setname = paste0(" - \n", game, recordnam)
	} else {
		recording = character(0)
		setname = character(0)
	}
	
	source("OCAT - Processing - Output.r")
}
}