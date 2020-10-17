#	this script is to collect all of the CSVs into one CSV
library(readr)
#	loads the readr library, used to read in the CSVs as data frames

game	=	"!GAME!"
#	will be the name of the game
#		!GAME! is replaced by the Python script
COMPRESS	=	TRUE
#	controls if the output CSV should be compressed

COLUMN	=	!SUBS!	;	SUBSET	=	"!QUA!"
#	the column and term to filter the CSVs by
#		for example, Quality and High or GPU and RX Vega 64
#	these are changed later if either are NULL and if it is found we are in the OCAT Data folder
#	by having SUBSET set to be the quality, then the CSV file names can use SUBSET
#		this makes it easier to build multiple CSVs without needing different files for each
#	the !SUBS! will be replaced by NULL if this is multi-GPU and made "Quality" if single-GPU, as that is the most likely column to subset by

setwd("!PATH!")
#	sets the working directory, allowing paths later to be relative from this directory
#		!PATH! is replaced by the Python script
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

listLOC	=	txtFIND("Locations.txt")
#	the Locations.txt file is looked for in the OCAT Data folder and its contents stored here
# listQUA	=	txtFIND("Qualities.txt")
#	the Qualities.txt file is looked for in the OCAT Data folder and its contents stored here
#	not necessary as it will be read from the directory names

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

OCATcomb	=	csvOCAT(CSV.config)
#	loads in the CSVs to the OCATcomb variable

if	(COMPRESS)	{
	write_csv(OCATcomb, paste0("@Combined - ", SUBSET, ".csv.bz2"))
}	else	{
	write_csv(OCATcomb, paste0("@Combined - ", SUBSET, ".csv"))
}
#	write_csv is a readr function that will write a CSV for you
#		OCATcomb is the data that will be written
#	depending on the value of COMPRESS, a compressed version of the CSV will be saved instead.
#	with SUBSET set to be the Quality, according to the Python script, it is easier to have this one file create CSVs for different qualities