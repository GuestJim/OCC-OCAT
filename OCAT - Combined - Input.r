library(readr)
library(ggplot2)
library(moments)

game	=	"!GAME!"
cGPU	=	!GPU!

gameF	=	gsub(":", "-", game)
gameF	=	unlist(strsplit(gameF, split=" [(]"))[1]

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

useSHORT	=	TRUE
testAPI		=	FALSE
overAPI		=	FALSE
listFPS		=	NULL
#	for adding to the FPS Percentile list
diffLim		=	NULL
QUAN		=	c(0.01, 0.99)
FtimeLimit	=	1000/15
yratesEXT	=	NULL

gWIDTH	=	8
gHEIGH	=	9
app.BREAK	=	FALSE
#	switch for if line breaks should be used in the labels
#		can be changed prior to graphs being created for selective application

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	TRUE
graphs_all	=	FALSE

textFRAM	=	TRUE
graphFRAM	=	TRUE

textDISP	=	FALSE
graphDISP	=	FALSE

textREND	=	FALSE
graphREND	=	FALSE

textDRIV	=	FALSE
graphDRIV	=	FALSE

textAPI		=	FALSE
textLOC		=	FALSE
#	will generate TXT and HTML, if HTML is enabled, files for each API/Location

textDiff	=	FALSE
graphDiff	=	FALSE
#	cannot be DIFF because of naming conflict in Output

if (!textOUT)	{
	textFRAM	=	FALSE
	textDISP	=	FALSE
	textREND	=	FALSE
	HTMLOUT		=	FALSE
	textDiff	=	FALSE
}

if (!graphs)	{
	graphFRAM	=	FALSE
	graphDISP	=	FALSE
	graphREND	=	FALSE
	graphDRIV	=	FALSE
	graphDiff	=	FALSE
}

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#		prevents rplots.pdf from being generated

resultsFull	=	read_csv("@Combined - !QUA!.csv")

listGPU		=	c(
"RX 580",
"RX Vega 64",
"GTX 770",
"GTX 980",
"GTX 1070",
"GTX 1080",
"RTX 2060",
"RTX 2080"
)

listQUA		=	c(
"!QUA!"
)

listLOC		=	c(
!LOC!
)

shortLOC	=	c(
!LOCSHO!
)

listAPI		=	c(
!API!
)

shortAPI	=	c(
!APISHO!
)

resultsFull$GPU		=	ordered(resultsFull$GPU,		levels = listGPU)
resultsFull$Quality	=	ordered(resultsFull$Quality,	levels = listQUA)
if (length(listLOC[1]) != 0) {
	resultsFull$Location	=	ordered(resultsFull$Location,	levels = listLOC)
}
resultsFull$API		=	ordered(resultsFull$API,	levels = listAPI)

results = resultsFull
#	protects the original data, after having been formatted
#	not so necessary, as all edits to it occur within function environments


multiGPU	=	is.null(cGPU)
labsGPU		=	labs(caption = cGPU)
if (multiGPU)	labsGPU	=	labs()

levsLOC	=	listLOC
if	(useSHORT	&	!is.null(shortLOC))	levsLOC	=	shortLOC
levsAPI	=	listAPI
if	(useSHORT	&	!is.null(shortAPI))	levsAPI	=	shortAPI

if	(!testAPI)	{
	testAPI		=	(length(unique(results$API)) >= 2)
	overAPI		=	FALSE
}

GROUPS	=	list(GPU = results$GPU, Location = results$Location)
if	(testAPI)	GROUPS	=	list(GPU = results$GPU, API = results$API, Location = results$Location)

diff.CONS	=	function(DATA, DIR = "Forward")	{
	if	(DIR == "Forward")	return(c(diff(DATA), 0))
	if	(DIR == "Backward")	return(c(0, diff(DATA)))
}

if (textDiff	|	graphDiff)	{
	results$MsDifferencePresents		=	unlist(by(results$MsBetweenPresents, GROUPS, diff.CONS))
	results$MsDifferenceDisplayChange	=	unlist(by(results$MsBetweenDisplayChange, GROUPS, diff.CONS))
}

DESC	=	function(ITEM = NULL)	{
	QUA		=	"Review"
	gameQ	=	paste0(game, " - Review")
	if (levels(results$Quality)[1] != "Review")	{
		QUA		=	paste0(levels(results$Quality)[1])
		gameQ	=	paste0(game,	" - ",	QUA, " Quality")
	}

	desc	=	""
	if	(!multiGPU)											desc	=	paste0(desc,	" - ",	cGPU)
	if	((!testAPI	&	all(listAPI != ""))	|	overAPI)	desc	=	paste0(desc,	" - ",	unique(results$API))

	gameGAQF	=	paste0(gameF,	desc,	" - ",	QUA)
	gameGAQ		=	paste0(game,	desc,	" - ",	QUA,	" Quality")

	if	(!is.null(ITEM))	{
		gameGAQF	=	paste0(gameGAQF,	" - ",	ITEM)
		gameGAQ		=	paste0(gameGAQ,		" - ",	ITEM)
	}
	return(c(gameGAQF,	gameGAQ, gameQ))
}
gameGAQF	=	DESC()[1]	;	gameGAQ		=	DESC()[2]	;	gameQ	=	DESC()[3]

INDIV	=	function(TYPE, LIST, useSHORT = useSHORT, gWIDTH = gWIDTH, gHEIGH = gHEIGH)	{
	if	(TYPE != "GPU")	dir.create(paste0("@", TYPE))

	for	(ITEM in LIST)	{
		# TYPE	<<-	TYPE
		# LIST	<<-	LIST
		# ITEM	<<-	ITEM
		#	helpful for troubleshooting
		
		
		message(paste0("\n", ITEM))
		results	=	resultsFull[resultsFull[, TYPE] == ITEM, ]
		if (nrow(results)	==	0)	next
		if (TYPE == "GPU" & length(unique(results$API)) == 1)	next

		if	(TYPE != "GPU")	{
			FOLD	=	paste0("@", TYPE, "/", ITEM)
			dir.create(FOLD)
		}

		gameGAQF	=	DESC()[1]	;	gameGAQ		=	DESC()[2]	;	gameQ	=	DESC()[3]
		if (TYPE == "GPU")	{
			gameGAQF	=	paste0(ITEM, "/", gameGAQF)
		}	else	{
			gameGAQF	=	paste0(FOLD, "/", gameGAQF)
		}

		perGPU	=	FALSE
		source("@Combined - Output.r",	local = TRUE)
	}
}

perGPU	=	TRUE	#	used for creating stats files in each GPU in their folders
if (!multiGPU)	perGPU	=	FALSE
source("@Combined - Output.r")

if	(graphs_all)	{
# INDIV("GPU",		listGPU,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 2)
# INDIV("Location",	listLOC,	useSHORT = FALSE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
# INDIV("API",		listAPI,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
}