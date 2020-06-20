#	this script is to collect all of the CSVs into one CSV
library(readr)
#	loads the readr library, used to read in the CSVs as data frames

game = "!GAME!"
#	will be the name of the game
#		!GAME! is replaced by the Python script
COMPRESS	=	TRUE
#	switch to control if the output CSV should be compressed in the end
#	as this script is taking data from multiple files to place it in another, we are approximately doubling the data footprint, so the default is TRUE

setwd("!PATH!")
#	sets the working directory, allowing paths later to be relative from this directory
#		!PATH! is replaced by the Python script

OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
#	creates an empty data frames with the desired number of columns
#	this frame will then hold the data from the CSVs, allowing them to be bound together
#		OCATcomb is the OCAT combined frame

listLOC	=	c(
!LOC!
)
#	an ordered list of the different locations for the recordings
#		!LOC! is replaced by the Python script

#READ as it reads in the CSV data
READ	=	function(fold="",	CSV,	GPU,	Quality = "",	API="")	{
#	the fold argument is for if there is some additional folder to the usual path, but this is almost never the case
#	CSV will be the list of CSV file names to be loaded, and so must be provided
#		previously it was just taken from the global environment, but I would rather have it provided
#	GPU is the graphics card name that was used when collecting the data
#		previously it was just taken from the global environment, but I would rather have it provided
#	Quality actually should always be set but I left it with an empty string default
#	API might not always be set, so it does have a default value
	if (API != "")	API	=	paste0(API, "/")
#		checks if an API was provided and then adds the "/" because it would be an additional folder layer to the CSV path
	if (length(listLOC[1]) == 0)	listLOC	=	paste0("Recording ", 1:length(CSV))
#		if a list of locations was not provided, this will generate one simply numbering them
	len	=	min(length(listLOC), length(CSV))
#		the shortest length between either the number of Locations or the number of CSVs, but these should always be equal
	out	=	data.frame(matrix(ncol = 24, nrow = 0))
#		creates an empty data frame with the necessary number of columns so the contents of the CSVs looped through below can be combined
	for (place in 1:len)	{
#		a loop to work through the CSV list
		if (CSV[place] != ".csv")	{
#			checks to make sure there was not an empty element to the list the CSV extension was attached to
			fileLOC	=	paste0(fold, GPU, "/" , Quality, "/", CSV[place])
			if	(API != "")	fileLOC	=	paste0(fold, GPU, "/" , API, Quality, "/", CSV[place])
#				creates the fileLOC variable that provides the location of the current CSV
#				it first makes a version without API in the path, but if there is an API to consider, then the version with it is made

			if	(grepl(GPU, getwd()) & grepl(Quality, getwd()))		fileLOC	=	paste0(CSV[place])
#				check to make sure if the GPU and Quality name are in the current working directory, which means we are looking at a specific configuration and so only the file name is needed
			if	(grepl(GPU, getwd()) & !grepl(Quality, getwd()))	fileLOC	=	paste0(fold, API, Quality, "/",CSV[place])
#				checks if the GPU name but not the Quality name are in the working directory, as this means we are looking at all of the GPU data, a less common scenario but one I want support for
		}	else {next}
#			skips to the next iteration of the loop if the original if condition fails
		OCATtemp	=	read_csv(fileLOC)[, 1:20]
#			reads in the csv fileLOC points to OCATtemp, but only grabs the first 20 columns
		
		OCATtemp$GPU		=	GPU
		OCATtemp$Quality	=	Quality
		OCATtemp$Location	=	listLOC[place]
		OCATtemp$API		=	gsub("/", "", API)
#			attaches the desired information to columns with appropriate names
		out	=	rbind(out, OCATtemp)
#			row-bind the temporary data frame to the combined data frame
	}
	return(out)
#		with the loop finished, the combined data frame is returned
}

!LONG!
#	!LONG! is replaced by the Python script with the list of CSVs, all properly formatted like what is seen in the example below

#	example CSV list and pass to READ function
#GPU	=	"RX Vega 64"
#CSV	=	c(
#"OCAT-Sam2017.exe-2019-07-20T105950",
#"OCAT-Sam2017.exe-2019-07-20T110524",
#"OCAT-Sam2017.exe-2019-07-20T111116"
#)
#CSV	=	paste0(CSV, ".csv")
#OCATcomb	=	rbind(OCATcomb, READ("", CSV, GPU, "Max", "DirectX 11"))
#	it may be worth noting, the file name without the extension is what goes into the list, and then the extension is pasted on

if	(COMPRESS)	{
	write_csv(OCATcomb, "@Combined - !QUA!.csv.bz2")
}	else	{
	write_csv(OCATcomb, "@Combined - !QUA!.csv")
}
#	write_csv is a readr function that will write a CSV for you
#		the function supports producing a compressed output, which is controlled here with the COMPRESS switch set earlier
#			bzip2 compression is used as this showed the best compression of the three methods: bzip2; gzip; and lzma
#		OCATcomb is the data that will be written
#		"@Combined - !QUA!.csv" is the name of the output CSV
#			!QUA! is replaced by the Python script, to identify what the data is for