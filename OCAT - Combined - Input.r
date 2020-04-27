#	this script is for reading in CSV data and then passing the desired information to the Output script
library(readr)
#	loads the readr library, used to read in the CSVs as data frames
library(ggplot2)
#	loads the ggplot2 library, used for creating plots/graphs
library(moments)
#	loads the moments library, used for certain functions involving probability distribution moments

game	=	"!GAME!"
cGPU	=	!GPU!
#	variables to hold the game name and current GPU
#		for multi-GPU scenarios, cGPU will be NULL

gameF	=	gsub(":", "-", game)
#	creates a file-name safe version of the game name, replacing colons with dashes
gameF	=	unlist(strsplit(gameF, split=" [(]"))[1]
#	splits the gameF variable at opening parantheses returns just the beginning portion

theme_set(theme_grey(base_size = 16))
#	sets the base size for features in the ggplots2 theme
#		by raising it to 16, text will be larger
DPI			=	120
#	sets the DPI for saving graphs
ggdevice	=	"png"
#	ggplots2 can be saved as multiple formats, this sets it for PNG
#		can also use PDF, but typically PNGs are desired

#	below are a series of switches for various things
textOUT		=	TRUE
#	controls if any of the text output is produced
HTMLOUT		=	TRUE
#	controls if any of the HTML output is produced
graphs		=	TRUE
#	controls if the faceted graphs are created
graphs_all	=	FALSE
#	controls if the individual graphs are created
useSHORT	=	TRUE
#	controls if the shortened versions of the location and API names should be used

textFRAM	=	TRUE
graphFRAM	=	TRUE
#	controls if the frame time-based outputs should be created

textDISP	=	FALSE
graphDISP	=	FALSE
#	controls if the display time-based outputs should be created

textREND	=	FALSE
graphREND	=	FALSE
#	controls if the render time-based outputs should be created
textDRIV	=	FALSE
graphDRIV	=	FALSE
#	controls if the driver lag-based outputs should be created

textAPI		=	FALSE
textLOC		=	FALSE
#	will generate TXT and HTML, if HTML is enabled, files for each API/Location

textDiff	=	FALSE
graphDiff	=	FALSE
#	controls if the display consecutive difference-based outputs should be created
#	cannot be DIFF because of naming conflict in Output

testAPI		=	FALSE
#	provides an override for if multiple APIs were involved in the test
#	when FALSE it does the usual behavior of checking if multiple APIs are involved
#	when TRUE it will not check the number of APIs and will act as though there are multiple regardless
listFPS		=	NULL
#	for adding to the FPS Percentile list
#		default list is 60, 50, 30, 20, 15
#		NULL will have none added
QUAN		=	c(0.01, 0.99)
#	controls the quantiles used for the QQ line in the QQ plots
FtimeLimit	=	1000/15
#	upper frame time limit for graphs

gWIDTH	=	8
gHEIGH	=	9
#	graph width and height

if (!textOUT)	{
	textFRAM	=	FALSE
	textDISP	=	FALSE
	textREND	=	FALSE
	HTMLOUT		=	FALSE
	textDIFF	=	FALSE
}
#	turns off all of the text outputs, if it is not desired

if (!graphs){
	graphFRAM	=	FALSE
	graphDISP	=	FALSE
	graphREND	=	FALSE
	graphDRIV	=	FALSE
	graphDiff	=	FALSE
}
#	turns off all of the graph outputs, if it is not desired

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#		prevents rplots.pdf from being generated by ggplots2

resultsFull	=	read_csv("@Combined - !QUA!.csv")
#	loads in the CSV to the resultsFull variable

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
#	list of the GPUs

listQUA		=	c(
"!QUA!"
)
#	list of the Qualities, but should always be just one element

listLOC		=	c(
!LOC!
)
#	list of locations for the data

shortLOC	=	c(
!LOCSHO!
)
#	list of the shortened location names for the data

listAPI		=	c(
!API!
)
#	list of the APIs for the data

shortAPI	=	c(
!APISHO!
)
#	list of the shortened API names for the data

if	(textDiff	|	graphDiff)	{
#	the the consecutive difference data is needed for its own graphs, this will generate the necessary data
	DIFF	=	as.data.frame(NULL)
#		creates an empty data frame for the difference data
	colIN	=	c("MsBetweenPresents",		"MsBetweenDisplayChange")
	colOUT	=	c("MsDifferencePresents",	"MsDifferenceDisplayChange")
#		column names for the data, similar to those used by PresentMon and derivatives

	for (gpu in unique(resultsFull$GPU))		{
	for (qua in unique(resultsFull$Quality))	{
	for (loc in unique(resultsFull$Location))	{
	for (api in unique(resultsFull$API))		{
#		series of for loops to work through the combinations of GPU, Quality, Location, and API
#			instead of using the lists provided above, it creates them from the data, getting just the unique values from the data
		if (paste0(unique(resultsFull$API)[1]) == "NA")	{
#			check to ignore API if there were no changes
			temp	=	resultsFull[resultsFull$GPU == gpu & resultsFull$Quality == qua & resultsFull$Location == loc, colIN]
#				creates a temporary data frame containing just the data from the current combination
		}	else	{
			temp	=	resultsFull[resultsFull$GPU == gpu & resultsFull$Quality == qua & resultsFull$Location == loc & resultsFull$API == api, colIN]
#				creates a temporary data frame containing just the data from the current combination
#					this one includes the API
		}
		tempD	=	rbind(as.data.frame(sapply(temp, diff)), 0)
#			uses sapply to use diff on each column in temp, then attaches a 0 on the end of the columns to have the correct length

		if (nrow(tempD) > 1)	{
#			makes sure the temporary data frame with difference data actually holds any data
			DIFF	=	rbind(DIFF, tempD)
#				collects the difference data columns together
		}
	}	}	}	}
	colnames(DIFF)	=	colOUT
#		applies the column names to the difference data
	resultsFull	=	cbind(resultsFull, DIFF)
#		sticks the difference data on the resultsFull frame
}


resultsFull$GPU		=	ordered(resultsFull$GPU, levels = listGPU)
results$Quality	=	factor(resultsFull$Quality, levels = listQUA)
if (length(listLOC[1]) != 0) {
	resultsFull$Location = ordered(resultsFull$Location, levels = listLOC)
}
resultsFull$API		=	ordered(resultsFull$API, levels = listAPI)
#	for proper ordering, values should be made factors and then have their levels ordered
#	the ordered function implies the factors should be ordered, and thus the ordered = TRUE argument is not needed
#		in case list of locations is empty, it will not make the values factors

results = resultsFull
#	with the factor levels set, a copy of resultsFull is saved to results
#		this way a complete and formatted copy of the data is protected and results can be a subset of it

reLoc	=	function(DATA, shortLOC = NULL)	{
#	creates a custom function for replacing the levels for the Location column of the provided DATA
#		to protect against a shortened list not being provided, the argument has a default value of NULL
	if (!is.null(shortLOC)	&	length(unique(DATA$Location)) > 1)	{
#		checks if the shortLOC argument is NULL or not and if there are multiple locations in the data
#			if graphs_all is TRUE, there will be just one location and applying the fixed shortLOC list creates unnecessary facets
		for (i in length(shortLOC):1)	{
#			this loop goes from the length of shortLOC down to 1, instead of counting up to protect against partial pattern matches replacing things that should not be replaced yet
			DATA$Location	=	gsub(listLOC[i], shortLOC[i], DATA$Location)
#				searches for and replaces patterns in strings, which is a bit more reliable for what I am doing
		}
		DATA$Location	=	ordered(DATA$Location, levels = shortLOC)
#			sets the column to be factors and the order for these factors
	}
	return(DATA)
}

reAPI	=	function(DATA, shortAPI = NULL)	{
	if (!is.null(shortAPI)	&	testAPI)	{
#		if testAPI is false, there is no API to test
		for (i in length(shortAPI):1)	{
			DATA$API	=	gsub(listAPI[i], shortAPI[i], DATA$API, fixed=TRUE)
		}
		DATA$API	=	ordered(DATA$API, levels = shortAPI)
	}
	return(DATA)
}
#	refer to comments for reLoc above, as these two functions are equivalent, but for either the API or Location column
#		reversed the order for going through the lists to address an issue with names being changed because they are a common substring


multiGPU	=	is.null(cGPU)
#	checks if the current GPU variable is NULL, and then stores the result of that check to the multiGPU variable

overAPI	=	TRUE
#	overAPI is a variable that stores if testAPI has been overriden
#	it needs a value, so it is set to TRUE here, but is changed to FALSE if the override was not used
if	(!testAPI)	{
#	if testAPI is FALSE, then the following test for number of APIs is done
#	if testAPI is true TRUE, then the script continues with testAPI being TRUE
	testAPI		=	(length(unique(results$API)) >= 2)
#		checks if there are multiple APIs in the data, and therefore if they should be tested
	overAPI		=	FALSE
#		if testAPI was not overridden will, overAPI will be made FALSE
}

if (levels(results$Quality)[1] != "Review")	{
#	checks if the quality is set to be the string Review
	QUA	=	paste0(levels(results$Quality)[1], " Quality")
	qua	=	paste0(levels(results$Quality)[1])
#		pair of strings with and without " Quality" on them used for labelling purposes
}	else	{
	QUA	=	"Review"
	qua	=	"Review"
#		sets the values of the QUA and qua variables for when the situation is a review
}

gameQ		=	paste0(game, " - ", QUA)
#	variable combining the game name and quality
gameGAQ		=	game
gameGAQF	=	gameF
#	creates a couple variables for holding the game name, GPU, API, and Quality, with and without file name formatting

if	(!multiGPU)	{
#		checks if this is not a multi-GPU situation
	gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
	gameGAQF	=	paste0(gameGAQF, " - ", cGPU)
}
#	applies the current GPU to the name variables
if	(!testAPI	&	listAPI != ""	|	overAPI)	{
#	checks that API is being tested and that there are multiple APIs in the data
#	if testAPI was overridden, as overAPI stores, then the API will be added to the names
	gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
	gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
}
#	applies the API to to the name variables

gameGAQ		=	paste0(gameGAQ, " - ", QUA)
gameGAQF	=	paste0(gameGAQF, " - ", qua)
#	sticks the quality at the end of the name variables

source("@Combined - Output.r")
#	reads the Output script and executes it


if	(graphs_all)	{
#	checks if the graphs_all switch is TRUE
textFRAM	=	FALSE
textDISP	=	FALSE
textREND	=	FALSE
textDRIV	=	FALSE
HTMLOUT		=	FALSE
textDiff	=	FALSE
#	because the statistics for the individual runs are not needed; just the graphs

for	(loc in listLOC)	{
#	loop to go through the list of locations

	message(paste0("\n", loc))
#		to identify the location being worked on
	results	=	resultsFull[resultsFull$Location == loc, ]
#		filters the data to just that for the current location
#			would be possible to adapt this for filtering by GPU, but that is not the usual scenario
#			with a GPU version, would need to change the working directory, to place the graphs in the correct folders

	gameQ		=	paste0(game, " - ", QUA, " - ", loc)
	gameGAQ		=	game
	gameGAQF	=	gameF

	if	(!multiGPU)	{
		gameGAQ		=	paste0(gameGAQ, " - ", cGPU)
		gameGAQF	=	paste0(gameGAQF, " - ", cGPU)
	}
	if	(!testAPI	&	listAPI != "")	{
		gameGAQ		=	paste0(gameGAQ, " - ", unique(results$API))
		gameGAQF	=	paste0(gameGAQF, " - ", unique(results$API))
	}
#		resets the name variables for the different locations, exactly as was done before

	source("@Combined - Output.r")
#		reads and executes the Output script
}
}