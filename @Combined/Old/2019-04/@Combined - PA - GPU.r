library(readr)

#	this script is to collect all of the CSVs into one CSV.
#		possibly more than one, but definitely need a CSV for the High runs
#	will need to identify the GPU used and the API in some cases

game = "!GAME!"
fold = "!GPU!"
GPUname = "!GPU!"

if (interactive()) {
	setwd(paste0("E:/Users/Jim/My Documents/OCC/@Reviews/@Performance Analyses/", game, " Performance Analysis/OCAT Data/", GPUname))
}

OCATcomb = data.frame(matrix(ncol = 20, nrow = 0))
OCATtemp = data.frame(matrix(ncol = 20, nrow = 0))

LOC = c(
"",
"",
""
)

READ = function(fold="", Quality="", API="") {
	for (place in 1:length(LOC)) {
		if (CSV[place] != ".csv") {
			OCATtemp = read_csv(paste0(fold, CSV[place]))[,1:17]
		} else {next}
		OCATtemp[,18] = GPU
		OCATtemp[,19] = LOC[place]
		OCATtemp[,20] = Quality
		OCATtemp[,21] = API
		OCATcomb = rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}

# OCATcomb = READ("Desktop - ")

GPU = GPUname

CSV = c(
"",
"", 
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")

CSV = c(
"",
"", 
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "Ultra")

CSV = c(
"",
"", 
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "Extreme")


colnames(OCATcomb)[18:21] = c("GPU", "Location", "Quality", "API")
write_csv(OCATcomb, paste0("@Combined - ", GPUname, ".csv"))