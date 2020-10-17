#	this script is for reading in CSV data and then passing the desired information to the Output script
library(readr)
#	loads the readr library, used to read in the CSVs as data frames
library(ggplot2)
#	loads the ggplot2 library, used for creating plots/graphs
library(moments)
#	loads the moments library, used for certain functions involving probability distribution moments

game	=	"!GAME!"
# cGPU	=	!GPU!
#	variables to hold the game name and current GPU
#		cGPU is set later based on the data read in, but is here to maintain similarity to Combined - Input.r script
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

COLUMN	=	NULL	;	SUBSET	=	NULL
#	the column and term to filter the CSVs by
#		for example, Quality and High or GPU and RX Vega 64
#	these are changed later if NULL and if it is found we are in the OCAT Data folder

#	below are a series of switches for various things
useSHORT	=	TRUE
#	controls if the shortened versions of the location and API names should be used
# testAPI		=	FALSE
#	controls if there are multiple APIs involved and if they should be tested/compared
#	this is set later based on the data and so is commented out here
overAPI		=	FALSE
#	this is to keep track of overriding testAPI
#		one would want to override testAPI when there is a single API in the sample, but multiple in the population
#	should be FALSE unless needed to keep names correct
listFPS		=	NULL
#	for adding to the FPS Percentile list
#		default list is 60, 50, 30, 20, 15
#		NULL will have none added
diffLim		=	NULL
#	for creating a version of the Consecutive Difference graph with an extended Y (difference) scale
#		it wants a frame time
QUAN		=	c(0.01, 0.99)
#	controls the quantiles used for the QQ line in the QQ plots
FtimeLimit	=	1000/15
#	upper frame time limit for graphs
yratesEXT	=	NULL
#	for adding more frame rates to the list used for scale breaks
#		the rates are converted to times for the scales; rates are just a bit easier to work with
testQUA		=	FALSE
#	used for controlling if the different quality levels are being tested
#		this will rarely occur, so the default should be FALSE

gWIDTH	=	8
gHEIGH	=	9
#	graph width and height
ogHEIGH	=	gHEIGH
#	sometimes the non-MEANS graphs need to be taller, and this will set their height
app.BREAK	=	FALSE
#	switch for if line breaks should be used in the scale labels (including secondary axes)
#		can be changed prior to graphs being created for selective application
facWID		=	25
#	control for width for text wrapping in the Location facet label
#		default value for label_wrap_gen is 25

textOUT		=	TRUE
#	controls if any of the text output is produced
HTMLOUT		=	TRUE
#	controls if any of the HTML output is produced
graphs		=	TRUE
#	controls if the faceted graphs are created
graphs_all	=	FALSE
#	controls if the individual graphs are created

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

if (!textOUT)	{
	textFRAM	=	FALSE
	textDISP	=	FALSE
	textREND	=	FALSE
	HTMLOUT		=	FALSE
	textDiff	=	FALSE
}
#	turns off all of the text outputs, if it is not desired

if (!graphs)	{
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

relPath	=	paste0(unlist(strsplit(getwd(), "OCAT Data"))[1], "OCAT Data")
#	stores the path to the OCAT Data folder, which is where various useful and important files are kept

if	(getwd() == relPath & (is.null(COLUMN) | is.null(SUBSET)))	{
	COLUMN	=	"Quality"
	SUBSET	=	"High"
}
#	checks if relPath we are working in the top OCAT Data folder and if either the COLUMN and SUBSET variables are NULL
#		normally if working from the OCAT Data folder it is to look at the High Quality data, so that is what is set here

txtFIND	=	function(TXT, rel.Path = relPath)	{
	if (file.exists(TXT))	{
		return(readLines(TXT, warn = FALSE))
	}	else	{
		locFILE	=	paste0(rel.Path, "/", TXT)
		if (file.exists(locFILE))	return(readLines(locFILE, warn = FALSE))
	}
	return(NULL)
}
#	function to find a TXT file and return its contents
#	it checks both the current folder and the relative path which should point to the OCAT Data folder
#	if the file does not exist, NULL is returned
#		return exits the function, so if the file exists, its contents are returned instead of the later NULL

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

listQUA		=	txtFIND("Qualities.txt")
#	the Qualities.txt file is looked for in the OCAT Data folder and its contents stored here
#		because this will be the complete list, the factor levels for the Quality column cannot be used to identify the current quality
#		instead, unique should be used, assuming the data is in fact for a single quality

listLOC		=	txtFIND("Locations.txt")
#	the Locations.txt file is looked for in the OCAT Data folder and its contents stored here
shortLOC	=	txtFIND("Locations Short.txt")
#	the Locations Short.txt file is looked for in the OCAT Data folder and its contents stored here
levsLOC		=	listLOC
if	(useSHORT	&	!is.null(shortLOC))	levsLOC	=	shortLOC
#	levsLOC is a list of factor levels for the Location column
#	by default levsLOC should be the list of all locations, unless shortened names should be used
#		in the event there are shortened names, levsLOC will store that information instead and can then be used for changing the levels

listAPI		=	txtFIND("APIs.txt")
#	the APIs.txt file is looked for in the OCAT Data folder and its contents stored here
shortAPI	=	txtFIND("APIs Short.txt")
#	the APIs Short.txt file is looked for in the OCAT Data folder and its contents stored here
levsAPI		=	listAPI
if	(useSHORT	&	!is.null(shortAPI))	levsAPI	=	shortAPI
#	levsAPI is a list of factor levels for the Location column
#	by default levsAPI should be the list of all locations, unless shortened names should be used
#		in the event there are shortened names, levsAPI will store that information instead and can then be used for changing the levels

#	by using the txtFIND function, this information will follow any changes to those TXT files

# csvFIND searches the directory tree from the working directory down for CSV files and builds a list of the OCAT outputs
csvFIND	=	function(DIRECT = getwd())	{
	LIST		=	list.files(recursive = TRUE, pattern = ".csv")
#		creates the list of all CSV files under the working directory
	LIST		=	LIST[grepl("OCAT", LIST) & grepl("csv", LIST) & !startsWith(LIST, "@")]
#		filters the list to only those that are OCAT CSV files, which always start with OCAT, and removes any with @ at the start of the folder path
	LIST.full	=	paste0(getwd(), "/", LIST)
#		adds the working directory path to the front of the CSV file list
	LIST.rel	=	t(data.frame(lapply(LIST.full, strsplit, "OCAT Data/"), row.names = NULL)[2, ])
#		removes the portion of the path information from OCAT Data and above, so we have the GPU, (API) and Quality configuration
	colnames(LIST.rel)	=	NULL
	rownames(LIST.rel)	=	NULL
#		column and row names are removed as they are not desired
	
	return(LIST.rel)
}

# LIST.rel	=	csvFIND()
#	would store the LIST.rel information to an accessible variable, but is not necessary

# reads the CSV path information to determine the configuration
csvCONF	=	function(CSV.list, LOCs = listLOC)	{
	CSV.config	=	t(as.data.frame(sapply(CSV.list, strsplit, "/")))
#		takes the provided CSV.list information (LIST.rel from csvFIND) and splits the folder names to be separate elements ina  lit
	colnames(CSV.config)	=	NULL
	rownames(CSV.config)	=	NULL
#		column and row names are removed
	
	CONFIG	=	data.frame(matrix(ncol = 5, nrow = nrow(CSV.config)))
#		creates an empty data frame to store the configuration and CSV information in
	colnames(CONFIG)	=	c("GPU", "API", "Quality", "Location", "CSV")
#		applies column names to the empty frame

	CONFIG$GPU		=	CSV.config[, 1]
#		the first column in CSV.config will always be the GPU name, as that is the highest directory level (below OCAT Data)
	if	(ncol(CSV.config) == 4)	CONFIG$API		=	CSV.config[, 2]
#		if there are four columns to the CSV.config data, then there are APIs to consider
#		API is the next directory level and thus the second column in CSV.config
	CONFIG$Quality	=	CSV.config[, ncol(CSV.config)-1]
#		Quality is the last directory level and so one up from the file name, the last column in the CSV.config frame
	
	appLOC	=	function(DATA, LOCs)	{
		if (is.null(LOCs))	LOCs	=	paste0("Recording ", 1:length(DATA$Location))
		rep(LOCs, length.out = length(DATA$Location))
	}
#	function to apply the Location name to the data provided to it
#		this custom function is used with the by function below to apply Location names by groups properly, so if there is a single location for some configuration, it does not throw everything off
#		if no list of locations is provided, it generates a simple numbered list

	CONFIG$Location	=	unlist(by(CONFIG, list(GPU = CONFIG$GPU, Quality = CONFIG$Quality), appLOC, LOCs))
#	appLOC and by together apply the Location names, and generate them if necessary, to match the GPU-Quality groups
#		API is not necessary for grouping, unless one API has a different number of recordings to it than the others
	CONFIG$CSV		=	CSV.config[, ncol(CSV.config)]
#		the CSV file names are placed in the CSV column
	
	return(CONFIG)
}

# CSV.config	=	csvCONF(LIST.rel)
CSV.configFull	=	csvCONF(csvFIND())
#	stores the data frame produced by csvCONF to CSV.configFull

# csvFILT filters the CSV.configFull data frame (named CSV.list in-function) to a desired configuration
csvFILT	=	function(CSV.list, COL, SUB)	{
	if (!is.null(COL)	&	!is.null(SUB))	return(CSV.list[CSV.list[, COL] == SUB, ])
#		checks to make sure there is a column and subset provided first, so either are missing the original CSV.list is returned
	return(CSV.list)
}


CSV.config	=	csvFILT(CSV.configFull, COLUMN, SUBSET)
#	stores the filtered CSV.configFull information to CSV.config
#		by acting on the list of CSVs, this work is faster than trying to filter the data once read in
#		if either COLUMN or SUBSET is NULL, no filtering will be done

typeFIND	=	function(DATA, TYPE)	{
	if	(length(unique(DATA[, TYPE])) == 1)	return(unique(DATA[, TYPE]))
	return(NULL)
}
#	checks if there is only one value used for a specified column and returns that
#	if there are multiple values present in that column, such as multiple APIs, then NULL is returned

cGPU	=	typeFIND(CSV.config, "GPU")
#	sets the current GPU to be either the only GPU for the CSVs being read or NULL if there are multiple
multiGPU	=	is.null(cGPU)
#	sets the multiGPU switch based on the cGPU value
labsGPU		=	labs(caption = cGPU)
if (multiGPU)	labsGPU	=	labs()
#	in single-GPU situations, a caption is added to the graphs to identify it
#		a default value is set to apply this caption and then removed in multiGPU situations

testAPI	=	is.null(typeFIND(CSV.config, "API"))
testQUA	=	is.null(typeFIND(CSV.config, "Quality"))
#	checks if there are multiple APIs or Qualities among the CSVs to be read in
#		when typeFIND finds there are multiple of a given type, it returns NULL, hence is.null being used

# read_OCAT is what will actually read in a provided OCAT CSV
read_OCAT	=	function(INFO, GPU = NULL, API = NULL, QUA = NULL, LOC = NULL)	{
	if (is.data.frame(INFO))	{
		GPU		=	INFO$GPU
		API		=	INFO$API
		QUA		=	INFO$Quality
		LOC		=	INFO$Location
		FILE	=	INFO$CSV
	}	else	{FILE	=	INFO}
#	INFO could be either just the CSV file name, and thus the other arguments are required, or a row from CSV.config that contains all the information
#	if INFO is a row from a data frame, which is itself a data frame, the configuration information is pulled from it accordingly
#	if INFO is not a row from a data frame, then it is the file name
	
	filePATH		=	paste(relPath, GPU, QUA, FILE, sep = "/")
	if	(!is.na(API))	filePATH		=	paste(relPath, GPU, API, QUA, FILE, sep = "/")
#		builds the path to a CSV file by properly adding the configuraiton information to relPath
#		if the API information is not missing, then it will be added to the path information
	
	out				=	read_csv(filePATH)[, 1:20]
#		reads in the CSV, storing just the first 20 columns to the out variable
	
	out$GPU			=	GPU
	out$Quality		=	QUA
	out$Location	=	LOC
	out$API			=	""
	if (!is.na(API))	out$API	=	API
#		out gains columns with the appropriate names and has the appropriate values stored to them, which will be the single value repeated to fill the number of rows
#		the API column will be made but be empty unless there is API information to store there
	return(out)
}

# the function to take the CSV information and load in all of the files
csvOCAT	=	function(CSVs)	{
	OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
#		creates an empty data frame with 24 columns
	for (ROW in 1:nrow(CSVs))	{
		OCATcomb	=	rbind(OCATcomb, read_OCAT(CSVs[ROW, ]))
	}
#		for each row in the data frame of CSVs, the CSVs are read in and added to the OCATcomb data frame

	return(OCATcomb)
}

resultsFull	=	csvOCAT(CSV.config)
#	loads in the CSVs to the resultsFull variable

resultsFull$GPU		=	ordered(resultsFull$GPU, levels = listGPU)
resultsFull$Quality	=	ordered(resultsFull$Quality, levels = listQUA)
if (length(listLOC[1]) != 0) {
	resultsFull$Location = ordered(resultsFull$Location, levels = listLOC)
}
resultsFull$API		=	ordered(resultsFull$API, levels = listAPI)
#	for desired ordering, values should be made factors and then have their levels ordered
#	the ordered function implies the factors should be ordered, and thus the ordered = TRUE argument is not needed
#		in case list of locations is empty, it will not make the values factors
#	notice, it is not the levsLOC or levsAPI lists used here, but the full-length versions; this is intentional
#	the original data should use the full-length versions and then it changed only when needed

results = resultsFull
#	protects the original data, after having been formatted
#	not so necessary, as all edits to it occur within function environments

GROUPS	=	list(GPU = results$GPU, API = results$API, Quality = results$Quality, Location = results$Location)
if	(!testAPI)	GROUPS$API		=	NULL
if	(!testQUA)	GROUPS$Quality	=	NULL
#	creates the groups for use with the by and aggregrate functions here and in the Output.r script
#	initially GROUPS contains all of the possible grouping factors, but those not needed are selectively removed
#	a list instead of a vector is used as it allows column names to be set, and these are applied to function outputs.

# diff.CONS for consecutive difference, a modification to the diff function
diff.CONS	=	function(DATA, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(DATA, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(DATA, lag = lag)))
}
#	similar to diff but adds a 0 to the beginning or end to match the length of the input data
#		I prefer Forward, so the sum point to the next value, whereas Backward points to the previous
#	the lag argument is present for consistency with other scripts that do need it

if (textDiff	|	graphDiff)	{
	results$MsDifferencePresents		=	unlist(by(results$MsBetweenPresents, GROUPS, diff.CONS))
	results$MsDifferenceDisplayChange	=	unlist(by(results$MsBetweenDisplayChange, GROUPS, diff.CONS))
}
#	the by function will apply a function to the contents of a data frame in groups, set by the GROUPS variable
#	it is very similar to the aggregate function, but returns a by class value, so unlist is needed to make it usable
#		this method is much faster and more efficient than using loops

# DESC for Description as this function creates the names for graphs and files to clearly identify what they represent
DESC	=	function(ITEM = NULL)	{
	descs	=	list(GPU = unique(as.character(results$GPU)), API = unique(as.character(results$API)), Location = unique(as.character(results$Location)), Quality = unique(as.character(results$Quality)))
#		the list of all descriptors present in the data
#			because the data had these columns set to be factors, it is necessary to convert them to strings
	if	(length(descs$GPU)		> 1)	descs$GPU		=	NULL
	if	(length(descs$API)		> 1)	descs$API		=	NULL
	if	(length(descs$Location)	> 1)	descs$Location	=	NULL
	if	(length(descs$Quality)	> 1)	descs$Quality	=	NULL
#		if there are multiple elements to any descriptor, it will be removed from the list as the naming should only use the unchanging information
	
	gameQ	=	game
	if	(!is.null(descs$Quality))	gameQ	=	paste0(game,	" - ",	descs$Quality,	"Quality")
#		gameQ is to hold the game name and current quality, but only if there is only one quality in the data
#		when there is multiple qualities, then gameQ is just the game name
	
	gameGAQF	=	paste0(gameF,	" - ",	paste0(descs,	collapse = " - ")	)
	gameGAQ		=	paste0(game,	" - ",	paste0(descs,	collapse = " - "),	" Quality")
#		by setting the collapse argument in paste0, the surviving elements of the descs list will be concatenated with " - " separating them, which is my preference
#		gameGAQF is meant for file output names while gameGAQ is not
	
	if	(!is.null(ITEM))	{
		gameGAQF	=	paste0(gameGAQF,	" - ",	ITEM)
		gameGAQ		=	paste0(gameGAQ,		" - ",	ITEM)
	}
#		this needs to work with the INDIV function described next and so needs to current individual ITEM being tested in the names
	return(c(gameGAQF,	gameGAQ, gameQ))
#		the three descriptive names are returned together as a single list
}
gameGAQF	=	DESC()[1]	;	gameGAQ		=	DESC()[2]	;	gameQ	=	DESC()[3]
#	sets the values
#		this does call the function multiple times, but the solution would be an intermediate variable and DESC should be fast enough

# INDIV for individual results as this function is for subsetting the resultsFull data, such as for separating locations
INDIV	=	function(COL, SUBS, useSHORT = useSHORT, gWIDTH = gWIDTH, gHEIGH = gHEIGH)	{
#	COL and SUBS arguments are for setting what column should be subset on and the list of elements to find the subsets of
#	useSHORT, gWIDTH, and gHEIGH all serve the same purpose as described earlier, of enabling using shortened names and the dimensions of the graphs
#		it might be desirable or necessary to change these values, which is why they are set here
	if	(COL != "GPU")	dir.create(paste0("@", COL))
#		when not subsetting by GPU, a folder will be made to hold the outputs
#			@ is placed at the front of the folder name to place it at the top of the directory view
	for	(ITEM in SUBS)	{
		# COL	<<-	COL
		# SUBS	<<-	SUBS
		# ITEM	<<-	ITEM
		#	helpful for troubleshooting as values are available outside the function
		
		
		message(paste0("\n", ITEM))
#			displays a message to identify the current item being used for subsetting
		results	=	resultsFull[resultsFull[, COL] == ITEM, ]
#			creates a subset of the complete results based on the COL (column name) and the current value
#			Output.r accepts 'results' which is why that variable should be used
		if (nrow(results)	==	0)	next
#			makes sure there are data in the subset and if there is not, we skip to the next item in the SUBS list
		if (COL == "GPU" & length(unique(results$API)) == 1)	next
#			prevents single-GPU, single-API subsets from being used, as those would be the same result as working with more specific configurations

		if	(COL != "GPU")	{
			FOLD	=	paste0("@", COL, "/", ITEM)
			dir.create(FOLD)
		}
#			creates a folder for the specific ITEM of the current subset
#			will not do this for GPUs as there are already folders for them

		gameGAQF	=	DESC(ITEM)[1]	;	gameGAQ		=	DESC(ITEM)[2]	;	gameQ	=	DESC(ITEM)[3]
#			assigns new values to these naming variables based on the current subset
		if (COL == "GPU")	{
			gameGAQF	=	paste0(ITEM, "/", gameGAQF)
		}	else	{
			gameGAQF	=	paste0(FOLD, "/", gameGAQF)
		}
#			gameGAQF is for file names, so this adds the appropriate folder information at the front, so the files are placed correctly
#			if...else is necessary here because there is not a default value that can be safely overwitten

		perGPU	=	FALSE
#			to disable the generation of stats files for GPU subsets
#				the Output.r script does this and will throw errors in some situations
		source("@Combined - Output.r",	local = TRUE)
#			runs the Output.r script
#			local = TRUE is necessary so the script will be given the variable values set within the INDIV function and its arguments
#			local = FALSE, the default, uses values from the global environment
	}
}

perGPU	=	TRUE
#	to disable the generation of stats files for GPU subsets
#		the Output.r script does this and will throw errors in some situations
if (!multiGPU)	perGPU	=	FALSE
#	turns off perGPU for single-GPU situations
source("@Combined - Output.r")
#	calls the Output.r script

if	(graphs_all)	{
# INDIV("GPU",		listGPU,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 2)
# INDIV("Location",	listLOC,	useSHORT = FALSE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
# INDIV("API",		listAPI,	useSHORT = TRUE,	gWIDTH = gWIDTH * 1.25,	gHEIGH = gHEIGH * 1)
}
#	examples of INDIV for subsetting by the list of GPUs, Locations, and API