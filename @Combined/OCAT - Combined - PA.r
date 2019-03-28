library(readr)

#	this script is to collect all of the CSVs into one CSV.
#		possibly more than one, but definitely need a CSV for the High runs
#	will need to identify the GPU used and the API in some cases

game = "!GAME!"

setwd("!PATH!")

OCATcomb = data.frame(matrix(ncol = 21, nrow = 0))
OCATtemp = data.frame(matrix(ncol = 21, nrow = 0))

LOC = c(
"",
"",
""
)

READ = function(fold="", Quality = "", API="") {
	if (API != "") {
		API = paste0(API, "/")
	}
	for (place in 1:length(LOC)) {
		if (CSV[place] != ".csv") {
			OCATtemp = read_csv(paste0(fold, GPU, "/" , API, Quality, "/", CSV[place]))[,1:17]
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
write_csv(OCATcomb, "@Combined - High.csv")