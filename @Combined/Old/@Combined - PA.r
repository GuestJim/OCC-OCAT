library(readr)

#	this script is to collect all of the CSVs into one CSV.
#		possibly more than one, but definitely need a CSV for the High runs
#	will need to identify the GPU used and the API in some cases

game = "!GAME!"

setwd(paste0("E:/Users/Jim/My Documents/OCC/@Reviews/@Performance Analyses/", game, " Performance Analysis/OCAT Data"))

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
			OCATtemp = read_csv(paste0(fold, GPU, "/", API, CSV[place]))[,1:17]
		} else {next}
		OCATtemp[,18] = GPU
		OCATtemp[,19] = Quality
		OCATtemp[,20] = LOC[place]
		OCATtemp[,21] = gsub("/", "", API)
		OCATcomb = rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}


GPU = "RX Vega 64"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


GPU = "RX 580"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


GPU = "GTX 770"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


GPU = "GTX 980"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


GPU = "GTX 1070"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


GPU = "GTX 1080"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


GPU = "RTX 2060"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


GPU = "RTX 2080"
CSV = c(
"",
"",
""
)
CSV = paste0(CSV, ".csv")

OCATcomb = READ("", "High")


colnames(OCATcomb)[18:21] = c("GPU", "Quality", "Location", "API")
write_csv(OCATcomb, "@Combined - High.csv")