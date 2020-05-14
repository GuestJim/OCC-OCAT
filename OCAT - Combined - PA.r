#	this script is to collect all of the CSVs into one CSV
library(readr)

game	=	"!GAME!"

setwd("!PATH!")

OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
OCATtemp	=	data.frame(matrix(ncol = 24, nrow = 0))

listLOC	=	c(
!LOC!
)

READ	=	function(fold="", Quality = "", API="")	{
	if (API != "") {
		API	=	paste0(API, "/")
	}
	if (length(listLOC[1]) == 0)	{
		listLOC	=	paste0("Recording ", 1:length(CSV))
	}
	len	=	min(length(listLOC), length(CSV))
	for (place in 1:len)	{
		if (CSV[place] != ".csv")	{
			fileLOC	=	paste0(fold, GPU, "/" , API, Quality, "/", CSV[place])
			
			if	(grepl(GPU, getwd()) & grepl(Quality, getwd()))		fileLOC	=	paste0(CSV[place])
			if	(grepl(GPU, getwd()) & !grepl(Quality, getwd()))	fileLOC	=	paste0(fold, API, Quality, "/",CSV[place])
		}	else {next}
		fileLOC		=	gsub("//", ".", fileLOC)
		OCATtemp	=	read_csv(fileLOC)[, 1:20]
		
		OCATtemp[,21]	=	GPU
		OCATtemp[,22]	=	Quality
		OCATtemp[,23]	=	listLOC[place]
		OCATtemp[,24]	=	gsub("/", "", API)
		OCATcomb	=	rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}

!LONG!

colnames(OCATcomb)[21:24]	=	c("GPU", "Quality", "Location", "API")
write_csv(OCATcomb, "@Combined - !QUA!.csv")