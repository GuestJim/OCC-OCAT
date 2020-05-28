#	this script is to collect all of the CSVs into one CSV
library(readr)

game	=	"!GAME!"

setwd("!PATH!")

OCATcomb	=	data.frame(matrix(ncol = 24, nrow = 0))
OCATtemp	=	data.frame(matrix(ncol = 24, nrow = 0))

listLOC	=	c(
!LOC!
)

READ	=	function(fold="",	CSV,	GPU,	Quality = "",	API="")	{
	if (API != "")	API	=	paste0(API, "/")
	if (length(listLOC[1]) == 0)	{
		listLOC	=	paste0("Recording ", 1:length(CSV))
	}
	len	=	min(length(listLOC), length(CSV))
	for (place in 1:len)	{
		if (CSV[place] != ".csv")	{
			fileLOC	=	paste0(fold, GPU, "/" , Quality, "/", CSV[place])
			if	(API != "")	fileLOC	=	paste0(fold, GPU, "/" , API, Quality, "/", CSV[place])
			
			if	(grepl(GPU, getwd()) & grepl(Quality, getwd()))		fileLOC	=	paste0(CSV[place])
			if	(grepl(GPU, getwd()) & !grepl(Quality, getwd()))	fileLOC	=	paste0(fold, API, Quality, "/",CSV[place])
		}	else {next}
		OCATtemp	=	read_csv(fileLOC)[, 1:20]
		
		OCATtemp$GPU		=	GPU
		OCATtemp$Quality	=	Quality
		OCATtemp$Location	=	listLOC[place]
		OCATtemp$API		=	gsub("/", "", API)
		OCATcomb	=	rbind(OCATcomb, OCATtemp)
	}
	return(OCATcomb)
}

!LONG!

write_csv(OCATcomb, "@Combined - !QUA!.csv")