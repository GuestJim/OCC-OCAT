#	the script for processing the data and producing the text, HTML, and graph outputs
yrates	=	c(c(120, 60, 30, 20, 15, 12, 10), yratesEXT)
#	a list of frame rates desired for scale breaks
#		yratesEXT is set in the Input.r script and allows additional breaks to be added if desired
yrates	=	sort(c(yrates,-yrates))
#	combines the above list with its opposite and sorts the larger list
ytimes	=	sort(1000/yrates)
#	converts the frame rates to frame times
ybreals	=	sort(c(round(ytimes, 2), 0))
#	frame times for use as scale breaks in the graphs
ms2FPS	=	function(DATA, r = 0)	round(1000/DATA, r)
#	function to convert times to rates for use with secondary axes using FPS instead of ms

# function to add breaks to alternating scale labels, allowing them to be horizontally closer
labelBreak	=	function(breaks, SEC = FALSE)	{
#	breaks is the list of breaks for the scale
#	SEC is to identify if this is for a secondary axis, as then the breaks need to be reversed, added in front of the break instead of the back
	if (!app.BREAK)	return(breaks)
#		check so this can be disabled from Input.r
	BREAK	=	c("", "\n")
#		the line break pattern
	if	(is.numeric(breaks)	&	0 %in% breaks)	if	((which(breaks %in% 0) %% 2) == 0)	BREAK	=	rev(BREAK)
#		this finds the 0 in the breaks and determines if it is in an odd or even position
#			first it must confirm the breaks are numbers, not strings, and then that 0 is in the list
#			next it checks if the position of 0 in breaks is odd or even
#			depending on the result, the order of the break pattern is reversed, so the 0 will not be impacted
	if	(!SEC)	return(	paste0(rep(BREAK, length.out = length(breaks)),	breaks)	)
#		if this is not for a secondary axis, the BREAK pattern is repeated as needed and pasted on front of the label
	if	(SEC)	return(	paste0(breaks, rep(BREAK, length.out = length(breaks)))	)
#		if this is for a secondary axis, the BREAK pattern is repeated and pasted after the label
}

# labelRound	=	function(x)	sprintf("%.1f", x)
#	function to round values to one decimal place, and will have zero padding
labelRound	=	function(x)			round(x, 1)
#	function to found values to one decimal place, without zero padding
labelRoundB	=	function(breaks)	labelBreak(labelRound(breaks))
#	applies labelBreak to the labelRound function
ms2FPS.lab	=	function(breaks)	labelBreak(ms2FPS(breaks), SEC = TRUE)
#	applies both the ms2FPS conversion and labelBreak to the breaks provided
#		as ms2FPS will only be for secondary axes, SEC = TRUE
labelBreakS	=	function(breaks)	labelBreak(sort(breaks))
#	breaks are sorted before labelBreak is called
labelBreakQQ=	function(breaks)	labelBreak(paste0(pnorm(breaks) * 100, "%"))
#	version of labelBreak for the QQ plot that converts the breaks to percentiles and attaches "%" to them
#		this will make it easier to use different percentile values, if desired
labelDisp	=	function(breaks)	round(breaks * 60/1000, 1)
#	function for creating the labels for Display Time axes, as I use 60Hz Refresh Cycles Later
labelDispB	=	function(breaks)	labelBreak(round(breaks * 60/1000, 1))
#	applies labelBreak to labelDisp


# custom mean and median function with labels for use with aggregate
meanMS	=	function(DATA)	{
	out			=	c(mean(DATA), median(DATA))
	names(out)	=	c("Mean", "Median")
	return(out)
}

# geometric mean function
meanGEO	=	function(DATA)	{
	out			=	exp(mean(log(DATA)))
	names(out)	=	"ms"
	return(out)
}

# normalized geometric means
normGEO	=	function(DATA)	{
	out			=	DATA / max(DATA) * 100
	names(out)	=	"Normalized (%)"
	return(out)
}
#	this should only be used with special AGGREGATE functions with different GROUPS than normally used
#		for example, just GROUP by GPU to compare them, or by GPU and API to compare them
#	normGEO is for normalizing the performance based on the maximum/longest frame time
#		should be used on the AGGREGATE, not within it, and will require passing the specific column to the function

# custom funtion for finding percentiles for the data and applying clear names to them
percMS	=	function(DATA, listPERC = c(0.1, 1, 99, 99.9))	{
	if	(max(listPERC) > 1)	listPERC = listPERC/100
	out			=	quantile(DATA, listPERC)
	names(out)	=	paste0(listPERC * 100, "%")
	return(out)
}

# custom function for using ECDF on the data for a list of values
ecdfFPS	=	function(DATA, listFPS = NULL, r = 2)	{
	default		=	c(60, 50, 30, 20, 15)
#		the default list of FPS values to check on
	listFPS		=	unique(sort(c(default, listFPS), decreasing = TRUE))
#		listFPS can be set in Input.r and here its values are combined with the default
	out			=	100 * (1 - ecdf(DATA)(1000 / listFPS))
	names(out)	=	paste0(listFPS, " FPS")

	return(round(out, r))
}
#		it is configured with an internal list that the listFPS argument can add to

# produces a list of statistical values, all labeled, but not necessarily used
statMS	=	function(DATA, r = 2)	{
	out			=	c(mean(DATA), sd(DATA), sd(DATA)/mean(DATA) * 100, skewness(DATA), kurtosis(DATA))
	names(out)	=	c("Mean (ms)", "StDev (ms)", "CoV (%)", "Skew", "Kurtosis")
	return(round(out, r))
}

# custom function that finds specific quantile values for use with a custom boxplot
BoxPerc	=	function (DATA)	{
	out			=	quantile(DATA, c(0.001, 0.01, 0.5, 0.99, 0.999))
	names(out)	=	c("ymin", "lower", "middle", "upper", "ymax")
	return(out)
}
#	the quantiles correspond to 0.1%, 1%, 50%, 99%, and 99.9%

# finds the slope of a line between two quantiles for the provided data
qqslope	=	function (DATA, r = 2, quan = QUAN)	{
	y		=	quantile(DATA, quan)
	#x		=	qnorm(quan)
	x		=	100 * quan
#		to make this be in percentile instead of Z-score
#			percentile better aligns with the graph
	slope	=	diff(y)/diff(x)
	return(round(slope, r))
}

# produces a list of values that are desired for use in graphs
statGRAPH	=	function(DATA, ...)	{
	out			=	c(mean(DATA), median(DATA), median(diff(DATA)), qqslope(DATA, ...), quantile(DATA, c(0.1, 1, 99, 99.9)/100))
	names(out)	=	c("Mean", "Median", "DiffMedian", "Slope", "0.1", "1", "99", "99.9")
	return(out)
}
#	DiffMedian can be used in graphDIFF to apply a Median-Median cross on the plots
#		not using it but does not hurt to keep it


# the aggregate function produces a two-level table and this function makes the lower table separate columns
sepCOL	=	function(tab)	{
	out	=	as.data.frame(as.matrix(tab))
#		makes the input first a matrix then a data frame as this is necessary for appropriate behavior
	for (i in grep("x", names(out)))	{
		out[, i]	=	as.numeric(as.character(out[, i]))
	}
#		finds every column with "x" in its name and makes its class numeric
	colnames(out)	=	sub("x.", "", colnames(out))
#		removes the "x." aggregates places from the column names
	return(out)
}

GROUPS	=	list(GPU = results$GPU, API = results$API, Quality = results$Quality, Location = results$Location)
if	(!testAPI)	GROUPS$API		=	NULL
if	(!testQUA)	GROUPS$Quality	=	NULL
#	already in Input script, but keeping here for easy finding
#	creates a complete list of possible groups for the data
#		depending on the switches set in Input, unused groups are removed

if	(textFRAM	|	graphFRAM)	{
	dataMEAN	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, meanMS))
	dataPERC	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, percMS))
	dataECDF	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, ecdfFPS, listFPS))
	dataSTAT	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statMS))
	graphSTATS	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statGRAPH))
#		aggregate functions will find described groups in data and apply a function to the groups
#			sepCOL fixes that the returns will be two-level tables
	graphSTATS$GPU		=	ordered(graphSTATS$GPU,			levels = listGPU)
	graphSTATS$Location	=	ordered(graphSTATS$Location,	levels = listLOC)
	if	(testAPI)	graphSTATS$API		=	ordered(graphSTATS$API,	levels = listAPI)
#		makes the GPU, Location, and API columns ordered factors so the graph facets are in the right order
}
#	below are equivalent to above but for other data columns in results
if	(textDISP	|	graphDISP)	{
	dispMEAN	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, meanMS))
	dispPERC	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, percMS))
	dispECDF	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, ecdfFPS, listFPS))
	dispSTAT	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statMS))
	dispgSTATS	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statGRAPH))

	dispgSTATS$GPU		=	ordered(dispgSTATS$GPU,			levels = listGPU)
	dispgSTATS$Location	=	ordered(dispgSTATS$Location,	levels = listLOC)
	if	(testAPI)	dispgSTATS$API		=	ordered(dispgSTATS$API,	levels = listAPI)
}
if	(textREND	|	graphREND)	{
	rendMEAN	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, meanMS))
	rendPERC	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, percMS))
	rendECDF	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, ecdfFPS, listFPS))
	rendSTAT	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, statMS))
	rendgSTATS	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, statGRAPH))

	rendgSTATS$GPU		=	ordered(rendgSTATS$GPU,			levels = listGPU)
	rendgSTATS$Location	=	ordered(rendgSTATS$Location,	levels = listLOC)
	if	(testAPI)	rendgSTATS$API		=	ordered(rendgSTATS$API,	levels = listAPI)
}
if	(textDRIV	|	graphDRIV)	{
	drivMEAN	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, meanMS))
	drivPERC	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, percMS))
	drivECDF	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, ecdfFPS, listFPS))
	drivSTAT	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, statMS))
	drivgSTATS	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, statGRAPH))

	drivgSTATS$GPU		=	ordered(drivgSTATS$GPU,			levels = listGPU)
	drivgSTATS$Location	=	ordered(drivgSTATS$Location,	levels = listLOC)
	if	(testAPI)	drivgSTATS$API		=	ordered(drivgSTATS$API,	levels = listAPI)
}
#	it is worth noting that using a list when passing the data to aggregate allows you to set the name of the output column
#		aggregate(list(Hello = data, groups, function)) will label the column Hello
#	it is also possible to run the function on more columns by placing them all in a list (not a vector, but a list like GROUPS)

# function to take tables in milliseconds and create a version in FPS bound on top
addFPS	=	function(DATA, r = 2)	{
	lab	=	DATA[1:grep("Location", colnames(DATA))]
	val	=	DATA[-(1:grep("Location", colnames(DATA)))]
#		it is necessary to separate the label columns and value columns

	tFPS	=	cbind(lab, rep("FPS", nrow(DATA)), round(1000/val, r))
#		creates the FPS values and binds them to the labels
	names(tFPS)[ncol(lab) + 1]	=	""
#		removes the column name for the column that identifies the unit
	tMS		=	cbind(lab, rep("ms", nrow(DATA)), round(val, r))
#		recreates the input that uses milliseconds as the unit, and adds a column identifying the unit
	names(tMS)[ncol(lab) + 1]	=	""
#		removes the column name for the column that identifies the unit

	out	=	rbind(tFPS, tMS)
#		row binds the FPS and millisecond data together
#			FPS is on top as it is more likely the unit people will want to read
	return(out)
}

# produces a compact table from the other tables
compTAB	=	function(MEAN, PERC, ECDF)	{
	if	(is.null(listFPS))	{
		listECDF	=	grep("60 FPS", colnames(ECDF))
#			if listFPS is NULL in Input.r, only the 60 FPS ECDF value will be shown
	}	else	{
		begECDF		=	grep(paste0(max(c(listFPS, 60)), " FPS"), colnames(ECDF))
		endECDF		=	grep(paste0(min(c(listFPS, 60)), " FPS"), colnames(ECDF))
#			if listFPS is not NULL, its max and min values, combined with 60, will be searched for in ECDF column names
		listECDF	=	begECDF:endECDF
#			the column indices for the min and max of listFPS are used as the beginning and end of the columns to include
	}

	out	=	cbind(
		addFPS(MEAN),
		addFPS(PERC)[-(1:grep("0.1%", colnames(addFPS(PERC))) - 1)],
		ECDF[listECDF]
	)
#		combines the MEAN (mean and median) and PERC (percentile) columns after they have FPS versions added with the ECDF columns

	return(out)
}

# returns a subset of the data determined by the COL value and its value when evaluated as a variable
subOUT	=	function(DATA, COL = "")	{
	if	(COL == "")	{
		out	=	DATA
#			if no COL column name is provided, the input data is returned
	}	else	{
		SUB	=	eval(parse(text = COL))
#			reads the string value of COL as a variable name and stores that value in SUB
		out	=	DATA[DATA[, COL] == SUB, ]
#			filters the DATA to only those where the values in the COL column match the SUB value
	}
	return(out)
}

# function to create generical named variables for holding the aggregate data above
dataSEL	=	function(datatype, COL = "")	{
	if	(datatype == "MsBetweenPresents"		|	datatype == "Frame Time")	{
		type		<<-	"Frame Time"
		typeSHORT	<<-	"data"
		MEAN		<<-	subOUT(dataMEAN, COL)
		PERC		<<-	subOUT(dataPERC, COL)
		ECDF		<<-	subOUT(dataECDF, COL)
		STAT		<<-	subOUT(dataSTAT, COL)
	}
	if	(datatype == "MsBetweenDisplayChange"	|	datatype == "Display Time")	{
		type		<<-	"Display Time"
		typeSHORT	<<-	"disp"
		MEAN		<<-	subOUT(dispMEAN, COL)
		PERC		<<-	subOUT(dispPERC, COL)
		ECDF		<<-	subOUT(dispECDF, COL)
		STAT		<<-	subOUT(dispSTAT, COL)
	}
	if	(datatype == "MsUntilRenderComplete"	|	datatype == "Render Time")	{
		type		<<-	"Render Time"
		typeSHORT	<<-	"rend"
		MEAN		<<-	subOUT(rendMEAN, COL)
		PERC		<<-	subOUT(rendPERC, COL)
		ECDF		<<-	subOUT(rendECDF, COL)
		STAT		<<-	subOUT(rendSTAT, COL)
	}
	if	(datatype == "MsEstimatedDriverLag"		|	datatype == "Driver Lag")	{
		type		<<-	"Driver Lag"
		typeSHORT	<<-	"driv"
		MEAN		<<-	subOUT(drivMEAN, COL)
		PERC		<<-	subOUT(drivPERC, COL)
		ECDF		<<-	subOUT(drivECDF, COL)
		STAT		<<-	subOUT(drivSTAT, COL)
	}
}
#	by giving it a column name, the data can be filtered, provided the variable for the FOR loop has the same name as the column
#		by using <<- the variables are accessible outside of the function

# function that describes how the statistics should be saved to a TXT file
sinkTXT	=	function(datatype, COL = "")	{
#		the name of the variable for the FOR loop must match the name of the column for this to work
	options(width = 1000)
#		sets the line width with the TXT files, as R will apply its own line breaks when reaching the width

	dataSEL(datatype, COL)
#		sets the generic variables to the correct datatype values

	subSTR	=	""
#		creates the subSTR string to hold the name of the filter
	if	(COL != "")	{
		SUB		=	eval(parse(text = COL))
#			finds the specific value of the filter and stores it to SUB
		subSTR	=	paste0(" - ", SUB, " - ")
#			creates a string from the filter to be applied to the file name

	if	(COL	==	"GPU")	{
		sink(paste0(SUB, "\\", gameGAQF, " ", subSTR, type, ".txt"), split = TRUE)
#			if we are filtering by GPU, this will place the TXT file into the GPU folder
#			creates the TXT file and has the currently selected filter in the file name
	}	else	{
		sink(paste0(gameGAQF, " ", subSTR, type, ".txt"), split = TRUE)
#			creates the TXT file and has the currently selected filter in the file name
	}

	writeLines(gameGAQ)
#		writes the name of the game, GPU, API, and Quality into the file
	writeLines(type)
#		writes the data type into the file
	writeLines("\nMean")
	print(addFPS(MEAN), row.names = FALSE)
#		writes a line break and then the label Mean
#		prints the MEAN data, with the addFPS function applied, to the file, without row names
	writeLines("\nPercentiles")
	print(addFPS(PERC), row.names = FALSE)
#		writes a line break and then the label Percentiles
#		prints the PERC data, with the addFPS function applied, to the file, without row names
	writeLines("\nPercentile of FPS")
	print(ECDF, row.names = FALSE)
#		writes a line break and then the label Percentile of FPS
#		prints the ECDF data to the file, without row names
	writeLines("\nDistribution Stats")
	print(STAT, row.names = FALSE)
#		writes a line break and then the label Distribution Stats
#		prints the STAT data to the file, without row names
sink()
#	closes the TXT file created
}

library(tableHTML)
#	loads the tableHTML library that provides functions for outputing HTML tables
OCCHTML	=	function(DATA)	{
#	custom function for creating the HTML tables with the desired formatting
	tableHTML(DATA, rownames = FALSE, class="OCC") %>%
#		calls the tableHTML function from tableHTML, but it needs some changes
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_header_\\d\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\\d\"', '', replace_all = TRUE)
#		replace_html will replace patterns in the tableHTML code with what is provided
#		%>% is how you chain together multiple replace_html functions
#			\\d indicates to include a single digit so by using two it will also cover when there are more than nine columns in the table
}

# custom function for actually writing the HTML file with the data in it
writeOCC	=	function(DATA, dataNAME, name=gameGAQF, fold = FOLD)	{
	if	(fold != "")	{
		write_tableHTML(OCCHTML(DATA), file = paste0(fold, "\\", name, " - ", dataNAME,".html"))
	}	else	{
		write_tableHTML(OCCHTML(DATA), file = paste0(name, " - ", dataNAME,".html"))
	}
}

# function that describes how the statistics should be written to HTML files, similar to sinkTXT
sinkHTML	=	function(datatype, COL = "")	{
#		by supplying a column name, and using a FOR loop where its variable name is the column name, it can filter the output

	dataSEL(datatype, COL)
#		sets the generic variables to the desired datatype values
	
	SUB		=	""
	if	(COL	!=	"")		SUB		=	paste0(eval(parse(text = COL)), " - ")
#		checks if the COL variable has been given a value and will read what the current value of the FOR variable is
	
	FOLD	=	""
	if	(COL	==	"GPU")	FOLD	=	eval(parse(text = COL))
#		checks if filtering by GPU so the HTML files can be sent to the GPU folders

	writeOCC(addFPS(MEAN),				dataNAME = paste0(SUB, typeSHORT, "MEAN"),	fold = FOLD)
	writeOCC(addFPS(PERC),				dataNAME = paste0(SUB, typeSHORT, "PERC"),	fold = FOLD)
	writeOCC(ECDF,						dataNAME = paste0(SUB, typeSHORT, "ECDF"),	fold = FOLD)
	writeOCC(STAT,						dataNAME = paste0(SUB, typeSHORT, "STAT"),	fold = FOLD)
	writeOCC(compTAB(MEAN, PERC, ECDF),	dataNAME = paste0(SUB, typeSHORT, "COMP"),	fold = FOLD)
}

# calls both sinkTXT and sinkHTML, though checks if their respective outputs are desired
sinkOUT	=	function(datatype)	{

if	(textOUT)	sinkTXT(datatype)
if	(HTMLOUT)	sinkHTML(datatype)
#	first checks if this specific output is desired

for (GPU in listGPU)	{	if	(file.exists(GPU) & perGPU)	{	GPU	<<-	GPU
#	goes through the GPU list
#	checks if GPU folder exists and if the separate GPU files are desired
#		to address an issue with the GPU variable not alway being accessing, <<- is used for global access
	if	(textOUT)	sinkTXT(datatype, "GPU")
	if	(HTMLOUT)	sinkHTML(datatype, "GPU")
}	}

if	(textAPI)			{	for (API in listAPI)	{	API	<<-	API
#	goes through the list of APIs, if textAPI is true
#		<<- is not necessary here, but it does not hurt to have, so for symmetry, it is present
	if	(textOUT)	sinkTXT(datatype, "API")
	if	(HTMLOUT)	sinkHTML(datatype, "API")
}	}
}

# function to simplify calling ggsave by providing most of its arguments
customSave	=	function(type="", plot = last_plot(), device=ggdevice, width=gWIDTH, height=gHEIGH, dpi=DPI)	{
#	the plot argument allows a specific plot to be saved without having to be rendered first in the GUI
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".png"), plot = plot, device=device, width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".pdf"), plot = plot, device=device, width=width, height=height)
	}
#	 can be set to have PNG, PDF, or both PNG and PDF outputs
}

# function that handles all of the graph saving configuration
graphOUT	=	function(datatype, graphtype, OUT = TRUE, SHOW = TRUE, diffLim = NULL, ...)	{
	if	(datatype == "MsBetweenPresents")			dataNAME	=	"Frame Time"
	if	(datatype == "MsBetweenDisplayChange")		dataNAME	=	"Display Time"
	if	(datatype == "MsUntilRenderComplete")		dataNAME	=	"Render Time"
	if	(datatype == "MsEstimatedDriverLag")		dataNAME	=	"Driver Lag"

	if	(substitute(graphtype) == "graphMEANS")		graphNAME	=	"Means"
	if	(substitute(graphtype) == "graphCOURSE")	graphNAME	=	"Course"
	if	(substitute(graphtype) == "graphFREQ")		graphNAME	=	"Freq"
	if	(substitute(graphtype) == "graphQQ")		graphNAME	=	"QQ"
	if	(substitute(graphtype) == "graphDIFF")		graphNAME	=	"Diff"
	
	if	(substitute(graphtype) == "graphMEANSbox")	graphNAME	=	"Means Labeled"
#		modified version of graphMEANS that adds labels for the custom boxplots

	PLOT	=	graphtype(datatype)
#	by default, the PLOT will be whatever the graph type is with the data type
	if	(graphNAME == "Diff" & !is.null(diffLim))	{
#		because the graphDIFF function can accept the diffLim argument, it needs to be checked for and applied
		PLOT	=	graphtype(datatype, diffLim)
		graphNAME	=	paste0(graphNAME, " EXT")
	}
#	to identify whether the diffLim argument has been changed, the graphNAME variable is changed
#	this also makes it possible to have the script automtically create two versions of the graph; one normal and one extended
	
	message(paste0(graphNAME, " - ", dataNAME))
#	the message will indicate what graph is currently being worked on

	if	(OUT)	customSave(paste0("@", graphNAME, " - ", dataNAME), plot = PLOT, ...)
#	saves the selected graph to a file if OUT = TRUE
	if	(SHOW)	PLOT
#	shows the selected graph if SHOW = TRUE
}

# function to reverse the order of levels for a column
levels.rev		=	function(DATA, COL)	{
	DATA	=	as.data.frame(DATA)
#	results is a tibble which does not work when trying to order levels in it, but making it a data frame fixes this
	ordered(DATA[, COL],	levels = rev(levels(DATA[, COL])))
#	returns the specified column with its levels reversed
}

# function to changes levels to use the shortened names
levels.short	=	function(DATA, COL, LIST, LEVS)	{
#	arguments are the input data, the column to work on, the original list of levels, the list of levels with shortend names
	DATA	=	as.data.frame(DATA)
#	results is a tibble which does not work when trying to order levels in it, but making it a data frame fixes this
	DATA[, COL]	=	ordered(DATA[, COL],	levels	=	LIST)
#	applies the original list of levels to the specified data column
	levels(DATA[, COL])	=	LEVS
#	changes the levels to use the shortened names
	return(DATA[, COL])
}

# function to apply levels.short to the appropriate columns
data.short	=	function(DATA)	{
	if	(!is.null(shortLOC))				DATA[, "Location"]	=	levels.short(DATA,	"Location",	listLOC,	levsLOC)
#	checks if a list of shortened location names have been provided then applies levels.short to the correct column
	if	(!is.null(shortAPI)	&	testAPI)	DATA[, "API"]		=	levels.short(DATA,	"API",		listAPI,	levsAPI)
#	checks if a list of shortened API names is present, and if APIs are being compared, then applies levels.short
	return(DATA)
}

# function to reverse the orders of levels
graph.rev	=	function(DATA, rev.LOC = FALSE, rev.API = FALSE)	{
#	arguments are the input data and switches for if the Location and/or API columns should be reversed
	if (rev.LOC)				DATA$Location	=	levels.rev(DATA, "Location")
#	checks if Location levels should be reversed, then applies levels.rev
	if (rev.API	&	testAPI)	DATA$API		=	levels.rev(DATA, "API")
#	checks if API levels should be reversed and if APIs are being compared, then applies levels.rev.
	return(DATA)
}
#	levels.rev, levels.short, data.short, and graph.rev exist so they can be called within the graph functions
#	by using them within the graph functions, the original data (results and various STATS) will not be altered
#		this makes keeping level names and orders between the original data much easier

# FACET will return the appropriate faceting design for each graph
#	different configurations will be selected based on graph type and the switch statuses (testAPI and testQUA)
#		unfortunately I have not found a way to manipulate to the appropriate vars list, so multiple facet_grid defintions are required
FACET = function(graphtype)	{
	if	(any(substitute(graphtype)	==	c("graphMEANS")))	{
		if	(testAPI	&	!testQUA)	return(facet_grid(vars(API),			cols = vars(Location), switch = "y"))		
		if	(!testAPI	&	testQUA)	return(facet_grid(vars(Quality),		cols = vars(Location), switch = "y"))
		if	(testAPI	&	testQUA)	return(facet_grid(vars(API, Quality),	cols = vars(Location), switch = "y"))
		
		return(facet_grid(cols = vars(Location), switch = "y"))
	}
#	the MEANS graph has different design requirements than the other graphs
#	if additional graphs are added with the same requirement, they can be added to the lsit

	if	(any(substitute(graphtype)	==	c("graphCOURSE", "graphFREQ", "graphQQ", "graphDIFF")))	{
		if	(multiGPU)	{
			if	(testAPI	&	!testQUA)	return(facet_grid(rows = vars(Location, API),			cols = vars(GPU), switch = "y"))
			if	(!testAPI	&	testQUA)	return(facet_grid(rows = vars(Location, Quality),		cols = vars(GPU), switch = "y"))
			if	(testAPI	&	testQUA)	return(facet_grid(rows = vars(Location, API, Quality),	cols = vars(GPU), switch = "y"))
		}	else	{
			if	(testAPI	&	!testQUA)	return(facet_grid(rows = vars(API),				cols = vars(Location, GPU), switch = "y"))
			if	(!testAPI	&	testQUA)	return(facet_grid(rows = vars(Quality),			cols = vars(Location, GPU), switch = "y"))
			if	(testAPI	&	testQUA)	return(facet_grid(rows = vars(API, Quality),	cols = vars(Location, GPU), switch = "y"))
		}
		
		return(facet_grid(rows = vars(Location), cols = vars(GPU), switch = "y"))
	}
}
#	functions end when they reach return, so there is no issue with using multiple
#		in fact, this provides an advantage by allowing a final facet design to be placed last, without needing an additional if checks
#		switch = "y" is so the facet labels or on the left side, instead of the right


# by using custom graph functions, it is easier to ensure consistent design across data types
#	the main difference between datatypes are the scales and what data is called for, and it is possible to control these with variables
#		get(datatype) is a way to effectively get the desired datatype in the ggplot2 aesthetics
#	spacing between facet panels can be set with  theme(panel.spacing.x = unit(1, "lines"))

# creates a function for the MEANS graph
graphMEANS	=	function(datatype)	{
#	graph of the arithmetic means, median, 0.1%, 1%, 99%, and 99.9% valuesfor the data, by GPU

#	below set the Y scales
#		it shows time and therefore is continuous
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
#				MsBetweenPresents is the frame time with units of milliseconds
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
#				creates a second, duplicate axis that shows frame rate with ms2FPS converting the ms breaks to FPS labels
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
#				for MsBetweenDisplayChange, the display time, I prefer to use the number of refresh cycles for the scale
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelDisp,
#				ybreaks will be converted as per labelDisp set earlier
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis()
#				a duplicate of the scale is applied to the opposite side of the graph
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
#		checks if the datatype is "MsUntilRenderComplete" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
#				MsUntilRenderComplete is the render time
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
#				creates a second, duplicate axis that shows frame rate with ms2FPS converting the ms breaks to FPS labels
		)
	}
	if	(datatype == "MsEstimatedDriverLag")	{
#		checks if the datatype is "MsEstimatedDriverLag" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Estimated Driver Lag (ms)",
#				MsEstimatedDriverLag is the estimated driver lag
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis()
#				a duplicate of the scale is applied to the opposite side of the graph
	}

	# if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	# if (useSHORT)	STATS	=	data.short(STATS)	; STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)
#	though not appropriate for all graphs (hence the commenting) these lines apply levels.rev and levels.short as desired
#		rev.LOC and rev.API must be set prior to the graphs being called

	ggplot(data = results, aes(x = GPU, y = get(datatype))) +
#		initiates the creation of a plot using ggplot2
#			by using the + symbol, layers are added to the graph (layers do go in order)
#		the data the plot will pull from is set to be results
	ggtitle(gameQ, subtitle = paste0(datatype, " - Means, Medians, and Percentiles")) + labsGPU +
#		sets the title of the graph, as well as the subtitle
#			labsGPU will add the current GPU caption or not depending on what it is set to
	geom_hline(yintercept = 1000/60, color = "red") +
#		a horizontal line, colored red, where 60 FPS falls
	# geom_boxplot(outlier.alpha = 0) +
	#	standard boxplot without outlier dots shown, but is not used
	stat_summary(fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
#		a layer that summarizes the data provided with the function provided (BoxPerc) and draws it as as geometry provided (boxplot)
	geom_bar(aes(fill = GPU), stat = "summary", fun.y = "mean") + scale_fill_hue() +
#		a bar graph layer where the height of the bar is the mean of the data
#			the fill for these bars will follow the default scale_fill_hue values
	stat_summary(fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
#		identical to the layer before, but this time with reduced opacity so the bar plot can be seen underneath
	# geom_boxplot(alpha = 0.50, outlier.alpha = 0.1) +
	#	standard boxplot with reduced opacity, but is not used
	FACET(graphMEANS) +
	scale_x_discrete(labels = labelBreak) +
#		the X axis is the GPUs, a discrete scale, and labelBreakF will add line breaks to every other label, to avoid overlap
	scale_Y + coord_cartesian(ylim = c(0, FtimeLimit)) +
#		applies the Y scale set earlier in the function
#		the limits for the Y-axis are set with coord_cartesian, which effectively crops the graph
#			coord_cartsian can be better than limits set inside the scale, as those will remove values outside those limits
	guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
#		sets the guide to be based on the fill scale (GPU), to have one row, and to be on the bottom of the graph
}

# function to create labels for the values in graphMEANS
boxLABS		=	function(datatype)	{
	if	(datatype == "MsBetweenPresents")		STATS	=	graphSTATS
	if	(datatype == "MsBetweenDisplayChange")	STATS	=	dispgSTATS
	if	(datatype == "MsUntilRenderComplete")	STATS	=	rendgSTATS
	if	(datatype == "MsEstimatedDriverLag")	STATS	=	drivgSTATS
#	checks the datatype and sets the appropriate statistics to STATS

	ALPHA	=	0.65
#	controls the opacity for the label box fills

	nudOUT	=	0.50
	nudIN	=	0.40
	nudMED	=	0.55
#	amount of nudging for the outer values (0.1% and 99.9%), inner values (1% and 99%), and median value

	list(
#	opens a list for holding each geom_label layer
#		later layers in this list will appear on top of earlier ones, so the more significant values are last
		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "99.9"],	label = round(STATS[, "99.9"], 2)),		alpha = ALPHA,
											vjust = 0,	nudge_y = nudOUT),
		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "0.1"],	label = round(STATS[, "0.1"], 2)),		alpha = ALPHA,
											vjust = 1,	nudge_y = -nudOUT),
		#	0.1% and 99.9%
#		with STATS as the data, it uses the appropriate columns for the Y values
#			quotes are necessary as the column names are numbers
#			vjust and nudge_y are used together so the labels will, hopefully, not overlap

		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "99"],		label = round(STATS[, "99"], 2)),		alpha = ALPHA,
			hjust = 1,	nudge_x = nudIN,	vjust = 0),
		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "1"],		label = round(STATS[, "1"], 2)),		alpha = ALPHA,
			hjust = 1,	nudge_x = nudIN,	vjust = 1),
		#	1% and 99%
#		with STATS as the data, it uses the appropriate columns for the Y values
#			quotes are necessary as the column names are numbers
#			hjust, nudge_x, vjust and nudge_y are used together so the labels will, hopefully, not overlap

		geom_label(data = STATS,	aes(x = GPU, y = STATS[, "Median"],	label = round(STATS[, "Median"], 2)),	alpha = ALPHA,
			hjust = 1,	nudge_x = nudMED),
		geom_text(data = STATS,		aes(x = GPU, y = Mean, 				label = round(Mean, 2)),
			hjust = 0,	nudge_x = -0.55,	vjust = 0)
		#	median and mean
#		with STATS as the data, it uses the appropriate columns for the Y values
#			hjust, nudge_x, vjust and nudge_y are used together so the labels will, hopefully, not overlap
		)
}

# adds the custom box plot labels to graphMEANS
graphMEANSbox	=	function(datatype)	graphMEANS(datatype) + boxLABS(datatype)
#		ggplot2 allows elements to be added together with the + symbol


# creates a function for the COURSE graph
graphCOURSE	=	function(datatype)	{
#		graph of measurements over the length of the test

#	below set the Y scales
#		it shows time and therefore is continuous
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
#				MsBetweenPresents is the frame time with units of milliseconds
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
#				creates a second, duplicate axis that shows frame rate with ms2FPS converting the ms breaks to FPS labels
		)
	}
	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
#				for MsBetweenDisplayChange, the display time, I prefer to use the number of refresh cycles for the scale
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelDisp,
#				ybreaks will be converted as per labelDisp set earlier
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis()
#				a duplicate of the scale is applied to the opposite side of the graph
		)
	}
	if	(datatype == "MsUntilRenderComplete")	{
#		checks if the datatype is "MsUntilRenderComplete" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
#				MsUntilRenderComplete is the render time
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
#				creates a second, duplicate axis that shows frame rate with ms2FPS converting the ms breaks to FPS labels
		)
	}
	if	(datatype == "MsEstimatedDriverLag")	{
#		checks if the datatype is "MsEstimatedDriverLag" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Estimated Driver Lag (ms)",
#				MsEstimatedDriverLag is the estimated driver lag
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis()
#				a duplicate of the scale is applied to the opposite side of the graph
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	# if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)
#	though not appropriate for all graphs (hence the commenting) these lines apply levels.rev and levels.short as desired
#		rev.LOC and rev.API must be set prior to the graphs being called

	ALPHA	=	0.05
	if	(length(unique(results$Location)) == 1)	ALPHA	=	1
#	sets a default transparency value for the points
#		being mostly transparent allows data density to be interpreted by the darkness of the plot
#	if there are multiple locations in the data though, the points will be opaque

	ggplot(data = results, aes(x = TimeInSeconds, y = get(datatype))) +
#		initiates the creation of the graph
#		the data the graph should use is identified, as are the X and Y aesthetics
	ggtitle(gameQ, subtitle = paste0(datatype, " - Course")) + labsGPU +
#		sets the title and subtitle of the graph, and applies the labsGPU variable
	geom_hline(yintercept = 1000/60, color = "red") +
#		a red horizontal line at 60 FPS
	geom_point(alpha = ALPHA) +
#		places points at the X and Y coordinates set by the aesthetics earlier
#		the alpha/transparency value is set based on the earlier switch
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
#		adds a smooth line to indicate the trend of the performance
#			the method used is the generalized additive module
#			the formula shown is the default for gam, but helps protect against certain issues
	FACET(graphCOURSE) + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=max(results$TimeInSeconds), by=60), labels = labelBreakN, expand=c(0.02, 0)) +
#		sets the X scale
#			name is given
#			breaks are from 0 to the greatest integer from the TimeInSeconds measurement
#			labelBreakN is used to prevent overlap
#			the padding along the scale is set
	scale_Y + coord_cartesian(ylim = c(0, FtimeLimit))
#		applies the Y scale set earlier
#		the limits for the Y-axis are set with coord_cartesian, which effectively crops the graph
#			coord_cartsian can be better than limits set inside the scale, as those will remove values outside those limits
}

# creates a function for the FREQuency graph
graphFREQ	=	function(datatype)	{
#	graph of the frequency of certain measurements to appear in the data
#		this is effectively showing the distribution of the data

#	below set the X scales
#		it shows time and therefore is continuous
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the X scale and the STATS used for parts of the graph
		STATS	=	graphSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
#				MsBetweenPresents is the frame time with units of milliseconds
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelRoundB,
#				ybreaks will be rounded for the labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			expand	=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS.lab
			)
#				creates a second, duplicate axis that shows frame rate with ms2FPS converting the ms breaks to FPS labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
		)
	}
	
	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the X scale and the STATS used for parts of the graph
		STATS	=	dispgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
#				MsBetweenDisplayChange is the disply time and I prefer units of Refresh Cycles Later
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelDispB,
#				ybreaks converted as labelDisp describes
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			expand	=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Display Time (ms)",
				labels	=	ybreaks
			)
#				creates a second, duplicate axis that shows display time in milliseconds and not refresh cycles
		)
	}

	if	(datatype == "MsUntilRenderComplete")	{
#		checks if the datatype is "MsUntilRenderComplete" and will set the X scale and the STATS used for parts of the graph
		STATS	=	rendgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
#				MsUntilRenderComplete is the render time with units of milliseconds
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelRoundB,
#				ybreaks will be rounded for the labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			expand	=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS.lab
			)
#				creates a second, duplicate axis that shows render rate with ms2FPS converting the ms breaks to FPS labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
		)
	}

	if	(datatype == "MsEstimatedDriverLag")	{
		STATS	=	drivgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Estimated Driver Lag (ms)",
#				MsEstimatedDriverLag is the estimated driver lag with units of milliseconds
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelRoundB,
#				ybreaks will be rounded for the labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			expand	=	c(0.02, 0),
#				the amount of expansion along the axis
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)
#	though not appropriate for all graphs these lines apply levels.rev and levels.short as desired
#		rev.LOC and rev.API must be set prior to the graphs being called

	ggplot(data = results, aes(get(datatype))) +
#		initiates the graph	with the data being results and the aesthetics to be the datatype
	ggtitle(gameQ, subtitle=paste0(datatype, " - Frequency Plot")) + labsGPU +
#		sets the title and subtitle of the graph, and applies the labsGPU variable
	geom_vline(xintercept = 1000/60, color = "red") +
#		red vertical at 60 FPS
	geom_freqpoly(binwidth=0.03, size=0) +
#		the frequency plot with the aesthetics set earlier, a binwidth of 0.03, and line size of 0, for thin lines
		geom_vline(data = STATS, aes(xintercept = Mean), color = "darkgreen") +
		geom_vline(data = STATS, aes(xintercept = Median), color = "darkcyan", linetype="dotted") +
#			using the STATS provided, vertical lines are added for the Mean and Median, with different colors and line types
	FACET(graphFREQ) + 
	scale_X + coord_cartesian(xlim = c(0, FtimeLimit)) +
#		applies the X scale set earlier
#		the limits for the X-axis are set with coord_cartesian, which effectively crops the graph
#			coord_cartsian can be better than limits set inside the scale, as those will remove values outside those limits
	scale_y_continuous(name="Count", expand=c(0.02, 0))
#		sets the Y scale to have the name Count and the padding for this scale
}

# creates a function for the Quantile graph
graphQQ	=	function(datatype, PERCS = c(.001, .01, .5, .99, .999))	{
#		graph of the frame times against their quantile distribution
#		the PERCS argument allows one to change the percentiles wanted on the X axis
#			the default values are for 0.1%, 1%, 50%, 99%, and 99.9%
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the Y scale
		STATS	=	graphSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
#				MsBetweenPresents is the frame time with units of milliseconds
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
#				creates a second, duplicate axis that shows frame rate with ms2FPS converting the ms breaks to FPS labels
		)
	}

	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the Y scale
		STATS	=	dispgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
#				for MsBetweenDisplayChange, the display time, I prefer to use the number of refresh cycles for the scale
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelDisp,
#				ybreaks will be converted as per labelDisp set earlier
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis()
#				a duplicate of the scale is applied to the opposite side of the graph
		)
	}
	
	if	(datatype == "MsUntilRenderComplete")	{
#		checks if the datatype is "MsUntilRenderComplete" and will set the Y scale
		STATS	=	rendgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
#				MsUntilRenderComplete is the render time
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
#				creates a second, duplicate axis that shows frame rate with ms2FPS converting the ms breaks to FPS labels
		)
	}

	if	(datatype == "MsEstimatedDriverLag")	{
		STATS	=	drivgSTATS
		scale_Y	=	scale_y_continuous(
			name		=	"Estimated Driver Lag (ms)",
#				MsEstimatedDriverLag is the estimated driver lag
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			expand		=	c(0.02, 0),
#				the amount of expansion along the axis
			sec.axis	=	dup_axis()
#				a duplicate of the scale is applied to the opposite side of the graph
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
	if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)
#	though not appropriate for all graphs these lines apply levels.rev and levels.short as desired
#		rev.LOC and rev.API must be set prior to the graphs being called

#	sec.axis	=	sec_axis(~.,
#		breaks	=	STATS[c("0.1", "1", "Median", "99", "99.9")],
#		labels	=	paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
#	)
#		this can be used to add a secondary axis that shows the values for the percentiles
#			it needs to be put in place after STATS is assigned, else it throws an error
	


	ggplot(data = STATS, aes(ymin = -Inf, xmin = -Inf)) +
#		initiates the graph and sets certain things for the rectangles below
#			-Inf will set the minimums to whatever the minimum is for the appropriate scale
	ggtitle(gameQ, subtitle = paste0(datatype, " - QQ Distribution")) + labsGPU +
#		sets the title and subtitle of the graph, and applies the labsGPU variable
	geom_hline(yintercept = 1000/60, color	=	"red") +
#		a red horizontal line at 60 FPS
		geom_rect(aes(ymax = get("0.1"),	xmax = qnorm(.001)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = get("1"),		xmax = qnorm(.010)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = get("Median"),	xmax = qnorm(.500)), alpha=0.1, fill=c("blue"), color = "grey") +
		geom_rect(aes(ymax = get("99"),		xmax = qnorm(.990)), alpha=0.1, fill=c("red"), color = "grey") +
		geom_rect(aes(ymax = get("99.9"),	xmax = qnorm(.999)), alpha=0.1, fill=c("red"), color = "grey") +
#			rectangles that go from the lower-left corner to the 0.1%, 1%, 50% (median), 99%, and 99.9% percentiles
#			by using colors and transparency, we can see how these rectangles overlap
#				the Y values are gotten from the STATS data where they were previously calculated
#				the X values are gotten using the qnorm function to get a Z-score
#				the rectangles have a grey border
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", size = 1.1, linetype = "dotted") +
#		a line is created from the data connecting the quantiles stored in QUAN and set in the Input script
#			the line is green and dotted
	stat_qq(data = results, aes(sample=get(datatype))) +
#		the quantile plot from the data
	stat_qq_line(data = results, aes(sample=get(datatype)), line.p = QUAN, color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") +
#		identical to the layer before, but this time with reduced opacity so the QQ plot can be seen underneath
	geom_label(data = STATS, aes(x = Inf, y = -Inf, label = paste0("Slope: ", Slope)), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") +
#		adds a label to the plot to show the Slope of the qq line
#			the label is placed in the lower-right using Inf, but right and bottom aligned so it does not go outside the graph
	FACET(graphQQ) + 
	scale_Y + coord_cartesian(ylim = c(0, FtimeLimit)) +
#		applies the Y scale set earlier
#		coord_cartesian can crop the plots to specific values
#			just setting limits sometimes causes issues, as values beyond the limits are dropped, while this just does not show them
	scale_x_continuous(name = "Percentile", breaks = qnorm(PERCS), labels = labelBreakQQ, minor_breaks = NULL, expand = c(0.02, 0))
#		sets the X scale to show the desired percentiles
#			qnorm is used to get the correct positions of the breaks, as the axis is actually in Z-score
#			the labelBreakQQ function adds line breaks, converts to percentages, and adds % symboles
#			the minor breaks between the major breaks are disabled
#			the graphs padding of the scale is set
}

# creates a function for the Consecutive DIFFerence graph
graphDIFF	=	function(datatype, diffLim = 1000/50)	{
#	graph of the frame times and the consecutive frame time difference
#		the limits for the difference scale can be set when calling the function

#	the X axis is the measured time
#	the Y axis is the consecutive difference
#		both must be continuous scales
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the X and Y scales
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
#				MsBetweenPresents is the frame time with units of milliseconds
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelRoundB,
#				ybreaks will be rounded for the labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			limits	=	c(0, FtimeLimit),
#				sets the same limit to the time scale used by other maps
			expand	=	c(0.02, 0)
#				the amount of expansion along the axis
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Frame Time Difference (ms)",
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			limits	=	c(-diffLim, diffLim),
#				applies symmetric limits to the difference scale
			expand		=	c(0, 0),
#				the amount of expansion along the axis
		)
	}

	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the Y scale
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelDispB,
#				ybreaks converted as labelDisp describes
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			limits	=	c(0, FtimeLimit),
#				sets the same limit to the time scale used by other maps
			expand	=	c(0.02, 0)
#				the amount of expansion along the axis
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Display Time Difference (ms)",
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			limits	=	c(-diffLim, diffLim),
#				applies symmetric limits to the difference scale
			expand		=	c(0, 0),
#				the amount of expansion along the axis
		)
	}

	if	(datatype == "MsUntilRenderComplete")	{
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelRoundB,
#				ybreaks will be rounded for the labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			limits	=	c(0, FtimeLimit),
#				sets the same limit to the time scale used by other maps
			expand	=	c(0.02, 0)
#				the amount of expansion along the axis
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Render Time Difference (ms)",
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			limits	=	c(-diffLim, diffLim),
#				applies symmetric limits to the difference scale
			expand		=	c(0, 0),
#				the amount of expansion along the axis
		)
	}

	if	(datatype == "MsEstimatedDriverLag")	{
		scale_X	=	scale_x_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels	=	labelRoundB,
#				ybreaks will be rounded for the labels
#				being an X scale, line breaks will be applied to the labels if app.BREAK = TRUE
			limits	=	c(0, FtimeLimit),
#				sets the same limit to the time scale used by other maps
			expand	=	c(0.02, 0)
#				the amount of expansion along the axis
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Lag Difference (ms)",
			breaks		=	ybreaks,
#				ybreaks was set earlier and will be common across time-based scales
			labels		=	labelRound,
#				ybreaks will be rounded for the labels
			limits	=	c(-diffLim, diffLim),
#				applies symmetric limits to the difference scale
			expand		=	c(0, 0),
#				the amount of expansion along the axis
		)
	}

	if (useSHORT)	results	=	data.short(results)
	results	=	graph.rev(results,	rev.LOC,	rev.API)
#	if (useSHORT)	STATS	=	data.short(STATS)	;	STATS	=	graph.rev(STATS,	rev.LOC,	rev.API)
#	though not appropriate for all graphs these lines apply levels.rev and levels.short as desired
#		rev.LOC and rev.API must be set prior to the graphs being called

	ggplot(data = results, aes(x = get(datatype), y = diff.CONS(get(datatype))) ) +
#		initiates the graph with the data being results
#			we can provide as the Y value the datatype with the diff.CONS function applied to it
#			by default the direction will be Forward
	ggtitle(gameQ, subtitle=paste0(datatype, " Consecutive Differences")) + labsGPU +
#		sets the title and subtitle of the graph, and applies the labsGPU variable
	geom_point(alpha = 0.1) +
#		places points at the X and Y coordinates set by the aesthetics earlier
#		the alpha/transparency value is set to 0.1 so the darkness indicates density
	stat_density_2d(geom = "polygon", aes(fill = stat(nlevel)), show.legend = FALSE) + scale_fill_viridis_c() +
#		adds a density plot with the geometry of a polygon
#			the color of the polygon will be based on the density level
#			a legend will not be shown
#			the viridis scale is used to set the fill colors
	# stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
	#	identical to the above, but the alpha value is also based on the density level
	FACET(graphDIFF) + 
	scale_X + 
	scale_Y
#		applies the X and Y scales set earlier
#		coord_cartesian cannot be used as it breaks the heat maps in some situations
}

#	text outputs
if	(textFRAM)	sinkOUT("MsBetweenPresents")
if	(textDISP)	sinkOUT("MsBetweenDisplayChange")
if	(textREND)	sinkOUT("MsUntilRenderComplete")
if	(textDRIV)	sinkOUT("MsEstimatedDriverLag")
#	checks if the statistics for these data types are desired
message("")
#	the print commands above will show their outputs in the console window, so this places an empty line between that and what comes next

rev.LOC	=	FALSE	;	rev.API	=	TRUE
#	sets that the location and API factor levels should or should not be reversed

#Means
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphMEANS)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphMEANS)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphMEANS)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphMEANS)
#	checks if the data type should have a graph made, then does so

#Means with Boxplot Lables
#				graphOUT("MsBetweenPresents",		graphMEANSbox)
#	will save graphMEANS with the custom boxplot labels
#	here as example but commented out as I do not need it
#		there is no switch to control it but I indented so it aligns with other graphOUT calls

rev.LOC	=	TRUE	;	rev.API	=	TRUE
#	sets that the location and API factor levels should or should not be reversed

#Course
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphCOURSE)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphCOURSE)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphCOURSE)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphCOURSE)
#	checks if the data type should have a graph made, then does so

#Frequency
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphFREQ)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphFREQ)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphFREQ)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphFREQ)
#	checks if the data type should have a graph made, then does so

#QQ
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphQQ)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphQQ)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphQQ)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphQQ)
#	checks if the data type should have a graph made, then does so

#Difference
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphDIFF)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphDIFF)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphDIFF)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphDIFF)
#	checks if the data type should have a graph made, then does so

#Difference - Extended
if (!is.null(diffLim))	{
	if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphDIFF,	diffLim = diffLim)
	if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphDIFF,	diffLim = diffLim)
	if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphDIFF,	diffLim = diffLim)
	if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphDIFF,	diffLim = diffLim)
}
#	checks if a custom diffLim value was set and then calls graphOUT to make the DIFF EXT graphs