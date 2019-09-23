library(readr)

#	this script is to collect all of the CSVs into one CSV.
#		possibly more than one, but definitely need a CSV for the High runs
#	will need to identify the GPU used and the API in some cases

game = "!GAME!"

setwd("!PATH!")

OCATcomb = data.frame(matrix(ncol = 21, nrow = 0))
OCATtemp = data.frame(matrix(ncol = 21, nrow = 0))

LOC = c(
!LOC!
)

READ = function(fold="", Quality = "", API="") {
	if (API != "") {
		API = paste0(API, "/")
	}
	if (length(LOC[1]) == 0)	{
		LOC = paste0(rep("Recording ", length(CSV)), 1:length(CSV))
	}
	len = min(length(LOC), length(CSV))	
	for (place in 1:len) {
		if (CSV[place] != ".csv") {
			if (grepl(GPU, getwd()))	{
				OCATtemp = read_csv(paste0(CSV[place]))[,1:17]
			}	else	{
				OCATtemp = read_csv(paste0(fold, GPU, "/" , API, Quality, "/", CSV[place]))[,1:17]
			}
		} else {next}
		OCATtemp[,18] = GPU
		OCATtemp[,19] = Quality
		OCATtemp[,20] = LOC[place]
		OCATtemp[,21] = gsub("/", "", API)
		OCATcomb = rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}

!LONG!

colnames(OCATcomb)[18:21] = c("GPU", "Quality", "Location", "API")
write_csv(OCATcomb, "@Combined - !QUA!.csv")