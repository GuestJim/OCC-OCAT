library(readr)
library(ggplot2)

game = "!GAME!"

gameF = gsub(":", "-", game)
gameF = unlist(strsplit(gameF, split=" [(]"))[1]

theme_set(theme_grey(base_size = 16))
DPI = 120
ggdevice = "png"

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	TRUE

textFRAM	=	TRUE
graphFRAM	=	TRUE

textDISP	=	FALSE
graphDISP	=	FALSE

textDIFF	=	FALSE
graphDIFF	=	FALSE

if (!textOUT)	{
	textFRAM = FALSE
	textDISP = FALSE
	HTMLOUT = FALSE
	textDIFF = FALSE
}

if (!graphs){
	graphFRAM = FALSE
	graphDISP = FALSE
	graphDIFF = FALSE
}

if (interactive()) {
	setwd("!PATH!")
} else {
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#	prevents rplots.pdf from being generated

results <- read_csv("@Combined - !QUA!.csv", col_types = "????????????????????c")
#	VERY IMPORTANT
#		remember to add to the Python script to change the !TYPE! based on if it is a PA or a review

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
""
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

source("@Combined - Output - GPU.r")