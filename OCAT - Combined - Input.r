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

textOUT		=	TRUE
HTMLOUT		=	TRUE
graphs		=	TRUE
graphs_all	=	FALSE
useSHORT	=	TRUE

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

testAPI		=	FALSE
listFPS		=	NULL
#	for adding to the FPS Percentile list
diffLim		=	NULL
QUAN		=	c(0.01, 0.99)
FtimeLimit	=	1000/15
yratesEXT	=	NULL

gWIDTH	=	8
gHEIGH	=	9

if (!graphs){
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

if	(textDiff	|	graphDiff)	{
	DIFF	=	as.data.frame(NULL)
	colIN	=	c("MsBetweenPresents",		"MsBetweenDisplayChange")
	colOUT	=	c("MsDifferencePresents",	"MsDifferenceDisplayChange")

	for (gpu in unique(resultsFull$GPU))		{
	for (qua in unique(resultsFull$Quality))	{
	for (loc in unique(resultsFull$Location))	{
	for (api in unique(resultsFull$API))		{
		if (paste0(unique(resultsFull$API)[1]) == "NA")	{
			temp	=	resultsFull[resultsFull$GPU == gpu & resultsFull$Quality == qua & resultsFull$Location == loc, colIN]
		}	else	{
			temp	=	resultsFull[resultsFull$GPU == gpu & resultsFull$Quality == qua & resultsFull$Location == loc & resultsFull$API == api, colIN]
		}
		tempD	=	rbind(as.data.frame(sapply(temp, diff)), 0)

		if (nrow(tempD) > 1)	{
			DIFF	=	rbind(DIFF, tempD)
		}
	}	}	}	}
	colnames(DIFF)	=	colOUT

	resultsFull	=	cbind(resultsFull, DIFF)
}


resultsFull$GPU		=	ordered(resultsFull$GPU, levels = listGPU)
resultsFull$Quality	=	factor(resultsFull$Quality, levels = listQUA)
if (length(listLOC[1]) != 0) {
	resultsFull$Location	=	ordered(resultsFull$Location, levels = listLOC)
}
resultsFull$API		=	ordered(resultsFull$API, levels = listAPI)

results = resultsFull

reLoc	=	function(DATA, shortLOC = NULL)	{
	if (!is.null(shortLOC)	&	length(unique(DATA$Location)) > 1)	{
		for (i in length(shortLOC):1)	{
			DATA$Location	=	gsub(listLOC[i], shortLOC[i], DATA$Location)
		}
		DATA$Location	=	ordered(DATA$Location, levels = shortLOC)
	}
	return(DATA)
}

reAPI	=	function(DATA, shortAPI = NULL)	{
	if (!is.null(shortAPI)	&	testAPI)	{
		for (i in length(shortAPI):1)	{
			DATA$API	=	gsub(listAPI[i], shortAPI[i], DATA$API, fixed=TRUE)
		}
		DATA$API	=	ordered(DATA$API, levels = shortAPI)
	}
	return(DATA)
}
#	reversed the order for going through the lists to address an issue with names being changed because they are a common substring

multiGPU	=	is.null(cGPU)

overAPI	=	TRUE
if	(!testAPI)	{
	testAPI		=	(length(unique(results$API)) >= 2)
	overAPI		=	FALSE
}

if (levels(results$Quality)[1] != "Review")	{
	QUA	=	paste0(levels(results$Quality)[1], " Quality")
	qua	=	paste0(levels(results$Quality)[1])
}	else	{
	QUA	=	"Review"
	qua	=	"Review"
}

gameQ		=	paste0(game, " - ", QUA)
gameGAQ		=	game
gameGAQF	=	gameF

if	(!multiGPU)	{
	gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
	gameGAQF	=	paste0(gameGAQF, " - ", cGPU)
}

# if	(!testAPI	&	!is.null(listAPI))	{
if	(!testAPI	&	listAPI != ""	|	overAPI)	{
	gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
	gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
}
gameGAQ		=	paste0(gameGAQ, " - ", QUA)
gameGAQF	=	paste0(gameGAQF, " - ", qua)

source("@Combined - Output.r")


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
		
		if	(TYPE != "GPU")	{
			FOLD	=	paste0("@", TYPE, "/", ITEM)
			dir.create(FOLD)
		}

		if (useSHORT	&	!is.null(shortLOC))		levels(results$Location)	=	shortLOC
		if (useSHORT	&	!is.null(shortAPI))		levels(results$API)			=	shortAPI

		gameQ		=	paste0(game, " - ", QUA, " - ", ITEM)
		gameGAQ		=	game
		gameGAQF	=	gameF

		if	(!multiGPU)	{
			gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
			gameGAQF	=	paste0(gameGAQF, " - ", cGPU)
		}
		if	(!testAPI	&	!is.null(listAPI))	{
			gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
			gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
		}
		gameGAQ		=	paste0(gameGAQ, " - ", QUA, " - ", ITEM)
		if (TYPE == "GPU")	{
			gameGAQF	=	paste0(ITEM, "/", gameGAQF, " - ", qua, " - ", ITEM)
		}	else	{
			gameGAQF	=	paste0(FOLD, "/", gameGAQF, " - ", qua, " - ", ITEM)
		}
		
		source("@Combined - Output.r", local = TRUE)
	}
}

if	(graphs_all)	{
textFRAM	=	FALSE
textDISP	=	FALSE
textREND	=	FALSE
textDRIV	=	FALSE
HTMLOUT		=	FALSE
textDiff	=	FALSE
#	because the statistics for the individual runs are not needed; just the graphs
#	also, these files are generated as part of the normal execution of the file

# INDIV("GPU",		listGPU,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 2)
# INDIV("Location",	listLOC,	useSHORT = FALSE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
# INDIV("API",		listAPI,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
}