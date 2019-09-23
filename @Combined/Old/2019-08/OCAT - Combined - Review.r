library(readr)

#	this script is to collect all of the CSVs into one CSV

game = "!GAME!"

setwd(paste0("!PATH!"))

OCATcomb = data.frame(matrix(ncol = 24, nrow = 0))
OCATtemp = data.frame(matrix(ncol = 24, nrow = 0))

LOC = c(
!LOC!
)

READ = function(fold="", Quality = "", API="") {
	if (API != "") {
		API = paste0(API, "/")
	}
	for (place in 1:length(LOC)) {
		if (CSV[place] != ".csv") {
			OCATtemp = read_csv(paste0(fold, GPU, "/", API, CSV[place]))[,1:20]
		} else {next}
		OCATtemp[,21] = GPU
		OCATtemp[,22] = Quality
		OCATtemp[,23] = LOC[place]
		OCATtemp[,24] = gsub("/", "", API)
		OCATcomb = rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}

READreview = function(fold="", Quality = "Review", API="") {
	if (API != "") {
		API = paste0(API, "/")
	}
	if (fold != "") {
		fold = paste0(fold, "/")
	}
	for (place in 1:length(LOC)) {
		if (CSV[place] != ".csv") {
			OCATtemp = read_csv(paste0(fold, CSV[place]))[,1:20]
		} else {next}
		OCATtemp[,21] = GPU
		OCATtemp[,22] = Quality
		OCATtemp[,23] = LOC[place]
		OCATtemp[,24] = gsub("/", "", API)
		OCATcomb = rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}


GPU = "!GPU!"
CSV = c(
!LIST!
)
CSV = paste0(CSV, ".csv")

OCATcomb = READreview("", "Review")

colnames(OCATcomb)[21:24] = c("GPU", "Quality", "Location", "API")
write_csv(OCATcomb, "@Combined - Review.csv")
