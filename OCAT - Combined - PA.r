#	this script is to collect all of the CSVs into one CSV
library(readr)
#	loads the readr library, used to read in the CSVs as data frames

game = "!GAME!"
#	will be the name of the game
#		!GAME! is replaced by the Python script

setwd("!PATH!")
#	sets the working directory, allowing paths later to be relative from this directory
#		!PATH! is replaced by the Python script

OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
OCATtemp	=	data.frame(matrix(ncol = 24, nrow = 0))
#	create empty data frames with the desired number of columns
#	these frames will then hold the data from the CSVs, allowing them to be bound together
#		OCATcomb is the OCAT combined frame
#		OCATtemp is the OCAT temporary frame

listLOC	=	c(
!LOC!
)
#	an ordered list of the different locations for the recordings
#		!LOC! is replaced by the Python script

READ	=	function(fold="", Quality = "", API="")	{
#	creates a new function called Read with arguments fold, Quality, and API
#		the default values for each argument is "", so an empty string
#	this function will be passed a list of CSVs and then read them into the data frames created above
#	the list of CSVs will be after this, 

	if (API != "") {
		API	=	paste0(API, "/")
	}
#	checks if an API was provided
#	there will be an API folder in the path then, so the / is added so the variable can be used to find the CSV's path

	if (length(listLOC[1]) == 0)	{
		listLOC	=	paste0("Recording ", 1:length(CSV))
	}
#	checks if there are actually Locations set, by testing the length of the first entry in the list
#		if Locations were not provided, then it will create a list of Recording # with a number identifying the recording
#			1:length(CSV) creates a list of numbers from 1 to the length of the CSVs
#			paste and paste0 will place the number list onto the string list
#				paste applies a separation character (default is a space) and while this can be changed, paste0 functions the same but does not apply a separation character

	len	=	min(length(listLOC), length(CSV))	
#	the shortest length between either the number of Locations or the number of CSVs
	for (place in 1:len)	{
#		a for loop that will create the place variable and then iterate it through a list of numbers, starting at 1 and going to the len variable set above
		if (CSV[place] != ".csv")	{
#			checks if the entry in the CSV list is just the extension or if it includes a filename
#				the else statement below will trigger the for loop to go to the next place
			fileLOC	=	paste0(fold, GPU, "/" , API, Quality, "/", CSV[place])
#				temporary variable that can hold a default file location string
#				default value is appropriate if searching down GPU, API, and Quality folder

			if	(grepl(GPU, getwd()) & grepl(Quality, getwd()))		fileLOC	=	paste0(CSV[place])
#				if the GPU and Quality are in the current path string, just the file name is needed
			if	(grepl(GPU, getwd()) & !grepl(Quality, getwd()))	fileLOC	=	paste0(fold, API, Quality, "/",CSV[place])
#				if GPU is in current path string but quality is not, API, Quality and file name are combined for the file location string
#				this is for looking in a single GPU fold (single-GPU, multi-API situation)
		}	else {next}
		OCATtemp	=	read_csv(fileLOC)[, 1:20]
#			reads in the csv fileLOC points to, but only grabs the first 20 columns

		OCATtemp[,21]	=	GPU
		OCATtemp[,22]	=	Quality
		OCATtemp[,23]	=	listLOC[place]
		OCATtemp[,24]	=	gsub("/", "", API)
		#	adds on new columns to the data frame, identifying the GPU, quality configuration, location, and the API
		#		gsub will remove the / that was added to the API earlier
		OCATcomb	=	rbind(OCATcomb, OCATtemp)
		#	rbind is row bind and takes the OCATcomb frame and adds to it the rows of OCATtemp
	}
	return(OCATcomb)
	#	return is the command to specify what the function returns
}

!LONG!
#	!LONG! is replaced by the Python script with the list of CSVs, all properly formatted like what is seen in the example below

#	example CSV list and pass to READ function
#GPU = "RX Vega 64"
#CSV = c(
#"OCAT-Sam2017.exe-2019-07-20T105950",
#"OCAT-Sam2017.exe-2019-07-20T110524",
#"OCAT-Sam2017.exe-2019-07-20T111116"
#)
#CSV = paste0(CSV, ".csv")
#OCATcomb = READ("", "Max", "DirectX 11")
#	it may be worth noting, the file name without the extension is what goes into the list, and then the extension is pasted on

colnames(OCATcomb)[21:24] = c("GPU", "Quality", "Location", "API")
#	sets the column names of the columns added for the GPU, Quality, Location, and API
write_csv(OCATcomb, "@Combined - !QUA!.csv")
#	write_csv is a readr function that will write a CSV for you
#		OCATcomb is the data that will be written
#		"@Combined - !QUA!.csv" is the name of the output CSV
#			!QUA! is replaced by the Python script, to identify what the data is for