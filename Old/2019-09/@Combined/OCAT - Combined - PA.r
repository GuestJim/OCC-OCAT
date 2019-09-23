library(readr)
#	loads the readr library for reading CSVs

#	this script is to collect all of the CSVs into one CSV.
#		possibly more than one, but definitely need a CSV for the High runs
#	will need to identify the GPU used and the API in some cases

game = "!GAME!"
#	sets the game name to a variable for use in the script
#		all !...! text is replaced by the Python script

setwd("!PATH!")
#	it is necessary to set the working directory in many cases

OCATcomb = data.frame(matrix(ncol = 21, nrow = 0))
OCATtemp = data.frame(matrix(ncol = 21, nrow = 0))
#	creates two empty data frames so a loop can append data to them
#		OCATcomb is OCAT combined
#		OCATtemp is OCAT temporary for holding data for intermediate steps

LOC = c(
!LOC!
)
#	list of locations with !LOC! being repalced by the Python script

READ = function(fold="", Quality = "", API="") {
#	custom function for loading in the CSV data the desired way
	if (API != "") {
		API = paste0(API, "/")
	}
#	if different APIs were used, this is necessary to ensure the correct path is used
	if (length(LOC[1]) == 0)	{
		LOC = paste0(rep("Recording ", length(CSV)), 1:length(CSV))
	}
#	checks if the is a location list, and if not it generates a generic Recording # list
	len = min(length(LOC), length(CSV))
#	sets a variable for length so this loop terminates when desired
	for (place in 1:len) {
		if (CSV[place] != ".csv") {
#			the CSV variable is set before the function is called
#				the list initially only has the filename without CSV extension, and that is pasted on so the check for if there is a list item is if there is more than just .csv
			if (grepl(GPU, getwd()))	{
#				checks if the GPU name is in the working directory, to determine if the path to the CSV is further down or not
				OCATtemp = read_csv(paste0(CSV[place]))[,1:17]
			}	else	{
				OCATtemp = read_csv(paste0(fold, GPU, "/" , API, Quality, "/", CSV[place]))[,1:17]
#					assigns the first 17 columns of the CSV data to the OCATtemp variable and will add the necessary elements if the CSV is in a lower directory
			}
		} else {next}
		OCATtemp[,18] = GPU
		OCATtemp[,19] = Quality
		OCATtemp[,20] = LOC[place]
		OCATtemp[,21] = gsub("/", "", API)
#			assigns the GPU, Quality, Location, and API to the appropriate columns
		OCATcomb = rbind(OCATcomb, OCATtemp)
#			appends the OCATtemp dta to the OCATcomb data
	}
	return(OCATcomb)
#		returns the OCATcomb variable
}

!LONG!
#	this is replaced by the Python script

colnames(OCATcomb)[18:21] = c("GPU", "Quality", "Location", "API")
#	sets the column names for the OCATcomb data frame
write_csv(OCATcomb, "@Combined - !QUA!.csv")
#	writes the OCATcomb data to a CSV