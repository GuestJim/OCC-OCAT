#	the script for processing the data and producing the text, HTML, and graph outputs
yrates	=	c(120, 60, 30, 20, 15, 12, 10)
#	a list of frame rates desired for scale breaks
yrates	=	sort(c(yrates,-yrates))
#	combines the above list with its opposite and sorts the larger list
ytimes	=	1000/yrates
#	converts the frame rates to frame times
ms2FPS	=	function(DATA, r = 0)	round(1000/DATA, r)
#	function to convert times to rates for use with secondary axes using FPS instead of ms

# labelRound	=	function(x)	sprintf("%.1f", x)
#	function to round values to one decimal place, and will have zero padding
labelRound	=	function(x)			round(x, 1)
#	function to found values to one decimal place, without zero padding
labelBreakF	=	function(breaks)	paste0(rep(c("", "\n"), length.out = length(breaks)), breaks)
labelBreakN	=	function(breaks)	paste0(rep(c("", "\n"), length.out = length(breaks)), sort(breaks))
#	functions to place line breaks on alternating labels on graphs
#		labelBreakF is for factors when we do not want sorting done
#		labelBreakN is for numbers when we do want sorting
labelBreakQQ=	function(breaks)	paste0(rep(c("", "\n"),	length.out = length(breaks)),	pnorm(breaks) * 100, "%")
#	function to add line breaks and convert Z scores to percentiles, and attach a % simple after the value, for the QQ plots
#		this will make it easier to use different percentile values, if desired
labelDisp	=	function(breaks)	round(breaks * 60/1000, 1)
#	function to convert frame times to display cycles, assuming 60 Hz rate, and rounds to one decimal place

BoxPerc	=	function (DATA)	{
	out			=	quantile(DATA, c(0.001, 0.01, 0.5, 0.99, 0.999))
	names(out)	=	c("ymin", "lower", "middle", "upper", "ymax")
	return(out)
}
#	custom function that finds specific quantile values for use with a custom boxplot
#		the quantiles correspond to 0.1%, 1%, 50%, 99%, and 99.9%

meanMS	=	function(DATA)	{
	out			=	c(mean(DATA), median(DATA))
	names(out)	=	c("Mean", "Median")
	return(out)
}
#	custom function to return both the arithmetic mean and the median of the data

meanGEO	=	function(DATA)	{
	out			=	exp(mean(log(DATA)))
	names(out)	=	"ms"
	return(out)
}
#	custom function for finding the geometric mean of the provided data

normGEO	=	function(DATA)	{
	out			=	DATA / max(DATA) * 100
	names(out)	=	"Normalized (%)"
	return(out)
}
#	this should only be used with special AGGREGATE functions with different GROUPS than normally used
#		for example, just GROUP by GPU to compare them, or by GPU and API to compare them
#	normGEO is for normalizing the performance based on the maximum/longest frame time
#		should be used on the AGGREGATE, not within it, and will require passing the specific column to the function

percMS	=	function(DATA, listPERC = c(0.1, 1, 99, 99.9))	{
	if	(max(listPERC) > 1)	listPERC = listPERC/100
	out			=	quantile(DATA, listPERC)
	names(out)	=	paste0(listPERC * 100, "%")
	return(out)
}
#	custom funtion for finding percentiles for the data and applying clear names to them

ecdfFPS	=	function(DATA, listFPS = NULL, r = 2)	{
	default		=	c(60, 50, 30, 20, 15)
	listFPS		=	unique(sort(c(default, listFPS), decreasing = TRUE))
	out			=	100 * (1 - ecdf(DATA)(1000 / listFPS))
	names(out)	=	paste0(listFPS, " FPS")

	return(round(out, r))
}
#	custom function for using ECDF on the data for a list of values
#		it is configured with an internal list that the listFPS argument can add to

statMS	=	function(DATA, r = 2)	{
	out			=	c(mean(DATA), sd(DATA), sd(DATA)/mean(DATA) * 100, skewness(DATA), kurtosis(DATA))
	names(out)	=	c("Mean (ms)", "StDev (ms)", "CoV (%)", "Skew", "Kurtosis")
	return(round(out, r))
}
#	produces a list of statistical values, all labeled, but not necessarily used in articles

qqslope	=	function (DATA, r = 2, quan = QUAN)	{
	y		=	quantile(DATA, quan)
	x		=	qnorm(quan)
	x		=	100 * quan
	#	to make this be in percentile instead of Z-score
	#		percentile better aligns with the graph
	slope	=	diff(y)/diff(x)
	return(round(slope, r))
}
#	finds the slope of a line between two quantiles for the provided data

statGRAPH	=	function(DATA, r = 2, quan = QUAN)	{
	out	=	c(mean(DATA), median(DATA), qqslope(DATA, quan = quan), quantile(DATA, c(0.1, 1, 99, 99.9)/100))
	names(out)	=	c("Mean", "Median", "Slope", "0.1", "1", "99", "99.9")
	return(out)
}
#	produces a list of values that are desired for use in graphs

sepCOL	=	function(tab)	{
	out	=	as.data.frame(as.matrix(tab))
	for (i in grep("x", names(out)))	{
		out[, i]	=	as.numeric(as.character(out[, i]))
	}
	colnames(out)	=	sub("x.", "", colnames(out))
	return(out)
}
#	the aggregate function produces a two-level table and this function will make it one level to make working with it easier

addFPS	=	function(DATA, r = 2)	{
	lab	=	DATA[1:grep("Location", colnames(DATA))]
	val	=	DATA[-(1:grep("Location", colnames(DATA)))]

	tFPS	=	cbind(lab, rep("FPS", nrow(DATA)), round(1000/val, r))
	names(tFPS)[ncol(lab) + 1]	=	""
	tMS		=	cbind(lab, rep("ms", nrow(DATA)), round(val, r))
	names(tMS)[ncol(lab) + 1]	=	""

	out	=	rbind(tFPS, tMS)
	return(out)
}
#	custom function for taking tables that are in milliseconds and adding FPS to them

compTAB	=	function(MEAN, PERC, ECDF)	{
	if	(is.null(listFPS))	{
		listECDF	=	grep("60 FPS", colnames(ECDF))
	}	else	{
		begECDF		=	grep(paste0(max(c(listFPS, 60)), " FPS"), colnames(ECDF))
		endECDF		=	grep(paste0(min(c(listFPS, 60)), " FPS"), colnames(ECDF))

		listECDF	=	begECDF:endECDF
	}

	out	=	cbind(
		addFPS(MEAN),
		addFPS(PERC)[-(1:grep("0.1%", colnames(addFPS(PERC))) - 1)],
		ECDF[listECDF]
	)

	return(out)
}
#	produces the compact table from other tables
#		normally the ECDF data stops at the 60 FPS value, but endECDF allows the function to go farther in the list

customSave	=	function(type="", plot = last_plot(), device=ggdevice, width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if	(device	==	"png")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".png"), plot = plot, device=device, width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".pdf"), plot = plot, device=device, width=width, height=height)
	}
}
#	function to simplify calling ggsave by already configuring most of its arguments
#		it will work for producing PNG or PDF outputs and with a recording variable no longer used
#	it is possible to directly save a plot without having to render it first by using the plot argument
#		customSave("@Means - Frame Time", plot = graphMEANS("MsBetweenPresents"))

if	(testAPI)	{
	GROUPS	=	list(GPU = results$GPU, API = results$API, Location = results$Location)
}	else	{
	GROUPS	=	list(GPU = results$GPU, Location = results$Location)
}
#	creates the groups for use with the aggregate functions below
#	by using an actual list, not just a vector, the column names for the groups can be set here

levsLOC	=	listLOC
if	(useSHORT	&	!is.null(shortLOC))	levsLOC	=	shortLOC
#	sets a list of levels for Location so the below better works with shortened names

if	(textFRAM	|	graphFRAM)	{
	dataMEAN	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, meanMS))
	dataPERC	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, percMS))
	dataECDF	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, ecdfFPS, listFPS))
	dataSTAT	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statMS))
	graphSTATS	=	sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statGRAPH))
#		aggregte functions are fun, with sepCOL run on the output and that result saved to a variable
	graphSTATS$GPU		=	ordered(graphSTATS$GPU,			levels = listGPU)
	graphSTATS$Location	=	ordered(graphSTATS$Location,	levels = levsLOC)
#		makes the GPU and Location columns ordered factors so the graph facets are in the right order
}
#	below are equivalent to above but for other data columns in results
if	(textDISP	|	graphDISP)	{
	dispMEAN	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, meanMS))
	dispPERC	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, percMS))
	dispECDF	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, ecdfFPS, listFPS))
	dispSTAT	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statMS))
	dispgSTATS	=	sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statGRAPH))

	dispgSTATS$GPU		=	ordered(dispgSTATS$GPU,			levels = listGPU)
	dispgSTATS$Location	=	ordered(dispgSTATS$Location,	levels = levsLOC)
}
if	(textREND	|	graphREND)	{
	rendMEAN	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, meanMS))
	rendPERC	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, percMS))
	rendECDF	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, ecdfFPS, listFPS))
	rendSTAT	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, statMS))
	rendgSTATS	=	sepCOL(aggregate(results$MsUntilRenderComplete, GROUPS, statGRAPH))

	rendgSTATS$GPU		=	ordered(rendgSTATS$GPU,			levels = listGPU)
	rendgSTATS$Location	=	ordered(rendgSTATS$Location,	levels = levsLOC)
}
if	(textDRIV	|	graphDRIV)	{
	drivMEAN	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, meanMS))
	drivPERC	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, percMS))
	drivECDF	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, ecdfFPS, listFPS))
	drivSTAT	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, statMS))
	drivgSTATS	=	sepCOL(aggregate(results$MsEstimatedDriverLag, GROUPS, statGRAPH))

	drivgSTATS$GPU		=	ordered(drivgSTATS$GPU,			levels = listGPU)
	drivgSTATS$Location	=	ordered(drivgSTATS$Location,	levels = levsLOC)
}

#	it is worth noting that using a list when passing the data to aggregate allows you to set the name of the output column
#		aggregate(list(Hello = data, groups, function)) will label the column Hello
#	it is also possible to run the function on more columns by placing them all in a list (not a vector, but a list like GROUPS)

subOUT	=	function(DATA, COL = "")	{
	if	(COL == "")	{
		out	=	DATA
	}	else	{
		SUB	=	eval(parse(text = COL))
		out	=	DATA[DATA[, COL] == SUB, ]
	}
	return(out)
}
#	takes a DATA input and the name of the column to used for filtering
#	it is very important with for loops that the variable name matches the column name
#		if COL is an empty string, then the DATA is just returned as is
#		if COL is not an empty string, then DATA will be filtered by that column
#		DATA[DATA[, COL] == SUB, ] will filter to just those rows where the value in the column is the same as the value of the SUB variable provided

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
		MEAN		<<-	subOUT(rendMEAN, COL)
		PERC		<<-	subOUT(rendPERC, COL)
		ECDF		<<-	subOUT(rendECDF, COL)
		STAT		<<-	subOUT(rendSTAT, COL)
	}
}
#	this function creates generically named variables for holding the AGGEGRATE data for frame, display, and render time, depending on what is desired
#	by giving it a column name, the data can be filtered, provided the variable for the FOR loop has the same name as the column
#		by using <<- the variables are accessible outside of the function

sinkTXT	=	function(datatype, COL = "")	{
#	instead of having multiple similar blocks of code for creating TXT files with data in them, this function handles it
#		its arguments are the datatype and the name of the column the filtering is based on
#		the name of the variable for the FOR loop must match the name of the column for this to work
	options(width = 1000)
#		sets the line width with the TXT files, as R will apply its own line breaks when reaching the width

	dataSEL(datatype, COL)
#		sets the generic variables to the desired values

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

writeOCC	=	function(DATA, dataNAME, name=gameGAQF, fold = FOLD)	{
	if	(fold != "")	{
		write_tableHTML(OCCHTML(DATA), file = paste0(fold, "\\", name, " - ", dataNAME,".html"))
	}	else	{
		write_tableHTML(OCCHTML(DATA), file = paste0(name, " - ", dataNAME,".html"))
	}
}
#	custom function for actually writing the HTML file with the data in it

sinkHTML	=	function(datatype, COL = "")	{
#	similar to sinkTXT this is a custom function for creating the HTML files
#		by supplying a column name, and using a FOR loop where its variable name is the column name, it can filter the output

	dataSEL(datatype, COL)
#		sets the generic variables to the desired values
	
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

sinkOUT	=	function(datatype)	{
#	calls both sinkTXT and sinkHTML when their output is desired, simplifying the output code significantly
#	can be passed any of the data types the script has be prepared for
#		MsBetweenPresents, MsBetweenDisplayChange, MsUntilRenderComplete
#	more can be added via additional aggregate functions and additions to dataSEL
if	(textOUT)	sinkTXT(datatype)
if	(HTMLOUT)	sinkHTML(datatype)
#	first checks if this specific output is desired

for (GPU in listGPU)	{	if	(file.exists(GPU))	{	GPU	<<-	GPU
	if	(textOUT)	sinkTXT(datatype, "GPU")
	if	(HTMLOUT)	sinkHTML(datatype, "GPU")
}	}
#	goes through the GPU list, checks if the folder for it exists, and creates the outputs
#		to address an issue with the GPU variable not alway being accessing, <<- is used for global access

if	(textAPI)			{	for (API in listAPI)	{	API	<<-	API
	if	(textOUT)	sinkTXT(datatype, "API")
	if	(HTMLOUT)	sinkHTML(datatype, "API")
}	}
#	goes through the list of APIs, if textAPI is true
#		<<- is not necessary here, but it does not hurt to have, so for symmetry, it is present
}

if	(textFRAM)	sinkOUT("MsBetweenPresents")
if	(textDISP)	sinkOUT("MsBetweenDisplayChange")
if	(textREND)	sinkOUT("MsUntilRenderComplete")
if	(textDRIV)	sinkOUT("MsEstimatedDriverLag")
#	calls for the creation of the desired TXT and HTML files for the data types
message("")
#	the print commands above will show their outputs in the console window, so this places an empty line between that and what comes next


if	(multiGPU)	{
	labsGPU	=	labs()
}	else	{
	labsGPU	=	labs(caption = cGPU)
}
#	creates a variable to add a caption to the graphs for what the current GPU is, for the single-GPU situation
#		for multi-GPU, there will be no such caption

#To create graphs, I have changed to a modular function-based solution, as this makes it easier to ensure consistency across data types, is cleaner, and lets me mess with the order of things later, which is important
#	the main difference between datatypes are the scales and what data is called for, and it is possible to control these with variables
#		get(datatype) is a way to effectively get the desired datatype in the ggplot2 aesthetics
#	spacing between facet panels can be set with  theme(panel.spacing.x = unit(1, "lines"))
graphMEANS	=	function(datatype)	{
#	creates a function for the MEANS graph
#		graph of the arithmetic means, median, 0.1%, 1%, 99%, and 99.9% valuesfor the data, by GPU
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Frame time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer
	}
	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
			breaks		=	c(0, round(ytimes, 2)),
			labels		=	labelDisp,
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Display time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#			labels will be the values shown at the breaks and here will be converted to something more reasonable for display time
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second, duplicate axis is made and placed on the opposite side of the graph
	}
	if	(datatype == "MsUntilRenderComplete")	{
#		checks if the datatype is "MsUntilRenderComplete" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Render time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Estimated Driver Lag (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Estimate Driver Lag
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second, duplicate axis is made and placed on the opposite side of the graph
	}
	if	(testAPI)	{
		FACET	=	facet_grid(rows = vars(API), cols = vars(Location), switch = "y")
	}	else	{
		FACET	=	facet_grid(cols = vars(Location), switch = "y")
	}
#		check to change the facet grouping to avoiding adding API group when there are no APIs to test
#		tells ggplot2 to create a faceted graph
#			the facet rows follow the API value
#			the facet columns follow the Location value
#			switches the Y labels for the facets to be on the left side, instead of the right

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
	FACET +
	scale_x_discrete(labels = labelBreakF) +
#		the X axis is the GPUs, a discrete scale, and labelBreakF will add line breaks to every other label, to avoid overlap
	scale_Y +
#		applies the Y scale set earlier in the function
	guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
#		sets the guide to be based on the fill scale (GPU), to have one row, and to be on the bottom of the graph
}


graphCOURSE	=	function(datatype)	{
#	creates a function for the COURSE graph
#		graph of measurements over the length of the test
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the Y scale
		scale_Y	=	scale_y_continuous(
			name		=	"Frame Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Frame Time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Refresh Cycles Later (1/60 s)",
			breaks		=	c(0, round(ytimes, 2)),
			labels		=	labelDisp,
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis()
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Refresh Cycles Later
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#			labels will be the values shown at the breaks and here will be converted to something more reasonable for display time
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second, duplicate axis is made and placed on the opposite side of the graph
	}
	if	(datatype == "MsUntilRenderComplete")	{
		scale_Y	=	scale_y_continuous(
			name		=	"Render Time (ms)",
			breaks		=	c(0, round(ytimes, 2)),
			limits		=	c(0, FtimeLimit),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Render time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer
	}
	if	(datatype == "MsEstimatedDriverLag")	{
	scale_Y	=	scale_y_continuous(
		name		=	"Estimated Driver Lag (ms)",
		breaks		=	c(0, round(ytimes, 2)),
		limits		=	c(0, FtimeLimit),
		expand		=	c(0.02, 0),
		sec.axis	=	dup_axis()
	)
#			for this data type the appropriate name is Estimated Driver Lag
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second, duplicate axis is made and placed on the opposite side of the graph

	if	(testAPI)	{
		FACET	=	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y")
	}	else	{
		FACET	=	facet_grid(rows = vars(Location), cols = vars(GPU), switch = "y")
	}
#		check to change the facet grouping to avoiding adding API group when there are no APIs to test
#		tells ggplot2 to create a faceted graph
#			the facet rows follow the API value
#			the facet columns follow the Location value
#			switches the Y labels for the facets to be on the left side, instead of the right
	
	if	(length(unique(results$Location)) == 1)	{
		ALPHA	=	1
	}	else	{
		ALPHA	=	0.05
	}
#		checks if there are multiple locations in results and will alter the transparency accordingly
#			for the single-location graphs, the points are fully opaque
#			for the multi-location graphs, the graphs are mostly transparent, to suggest density

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
	FACET + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=max(results$TimeInSeconds), by=60), labels = labelBreakN, expand=c(0.02, 0)) +
#		sets the X scale
#			name is given
#			breaks are from 0 to the greatest integer from the TimeInSeconds measurement
#			labelBreakN is used to prevent overlap
#			the padding along the scale is set
	scale_Y +
#		applies the Y scale set earlier
	guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")
#		sets the guide to be based on the color scale, to have one row, and to be on the bottom of the graph
#			no color scale is set, so this does nothing
}


graphDIFF	=	function(datatype, diffLim = 1000/50)	{
#	creates a function for the Consecutive DIFFerence graph
#		graph of the frame times and the consecutive frame time difference
#	the limits for the difference scale can be set when calling the function
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the X and Y scales
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Frame Time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Frame Time Difference (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Consecutive Frame Time Difference
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are from -1000/50 (-20) to 1000/50 (20)
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#				the paddng is disabled

	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the Y scale
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelDisp,
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Refresh Cycles Later
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Display Time Difference (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Consecutive Display Time Difference
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are from -1000/50 (-20) to 1000/50 (20)
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#				the paddng is disabled

	if	(datatype == "MsUntilRenderComplete")	{
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Render Time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Render Time Difference (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Consecutive Render Time Difference
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are from -1000/50 (-20) to 1000/50 (20)
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#				the paddng is disabled

	if	(datatype == "MsEstimatedDriverLag")	{
		scale_X	=	scale_x_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Lag Difference (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			limits	=	c(-diffLim, diffLim),
			expand	=	c(0, 0)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Estimated Driver Lag
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks are inherited from the breaks, unless specified
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
	if	(testAPI)	{
		FACET	=	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y")
	}	else	{
		FACET	=	facet_grid(rows = vars(Location), cols = vars(GPU), switch = "y")
	}
#		check to change the facet grouping to avoiding adding API group when there are no APIs to test
#		tells ggplot2 to create a faceted graph
#			the facet rows follow the API value
#			the facet columns follow the Location value
#			switches the Y labels for the facets to be on the left side, instead of the right

	temp	=	eval(parse(text = paste0("results$", datatype)))
#		to get the difference data, it is necessary to work around some issues with getting columns from results
#			this creates the code to call the desired column from results as a string
#			the string is then parsed and evaluated as code, saving the column to the temp variable
	#	the [1,] is needed because it otherwise just gets the list of row names

	ggplot(data = results, aes(x = get(datatype), y = rbind(c(diff(temp), 0))[1,]) ) +
#		initiates the graph with the data being results but the aesthetics coming from the temp variable
#			for the differences to match the length of temp, a value must be added to the list
#			placing 0 at the end means the Y value for the point will indicate where the next point will be (X+Y)
#			to address an issue with the row names being grabbed, [1,] is used to grab just the data
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
	FACET + 
	scale_X +
	scale_Y
#		applies the X and Y scales set earlier
}

graphFREQ	=	function(datatype)	{
#	creates a function for the FREQuency graph
#		graph of the frequency of certain measurements to appear in the data
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the X scale and the STATS used for parts of the graph
		STATS	=	graphSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			limits	=	c(0,  FtimeLimit),
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Frame Time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks will be rounded according to the labelRound function
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer
	
	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the X scale and the STATS used for parts of the graph
		STATS	=	dispgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelDisp,
			limits	=	c(0, FtimeLimit),
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Display Time (ms)",
				labels	=	c(0, round(ytimes, 2))
			)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Refresh Cycles Later
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#			labels will be the values shown at the breaks and here will be converted to something more reasonable for display time
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses time as the unit instead, and while cycles later I feel is appropriate, some viewers may appreciate this

	if	(datatype == "MsUntilRenderComplete")	{
#		checks if the datatype is "MsUntilRenderComplete" and will set the X scale and the STATS used for parts of the graph
		STATS	=	rendgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			limits	=	c(0,  FtimeLimit),
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Render Time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks will be rounded according to the labelRound function
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer

	if	(datatype == "MsEstimatedDriverLag")	{
		STATS	=	drivgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_X	=	scale_x_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			limits	=	c(0,  FtimeLimit),
			expand	=	c(0.02, 0)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Estimated Driver Lag
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#			labels will be the values shown at the breaks and here will be converted to something more reasonable for display time
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
	
	STATS$GPU	=	factor(STATS$GPU, levels = listGPU, ordered = TRUE)
#		applies the desired order to the GPU factors in the table
	if	(testAPI)	{
		FACET	=	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y")
	}	else	{
		FACET	=	facet_grid(rows = vars(Location), cols = vars(GPU), switch = "y")
	}
#		check to change the facet grouping to avoiding adding API group when there are no APIs to test
#		tells ggplot2 to create a faceted graph
#			the facet rows follow the API value
#			the facet columns follow the Location value
#			switches the Y labels for the facets to be on the left side, instead of the right

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
	FACET + 
	scale_X +
#		applies the X scale set earlier
	scale_y_continuous(name="Count", expand=c(0.02, 0))
#		sets the Y scale to have the name Count and the padding for this scale
}

graphQQ	=	function(datatype, PERCS, PERCS = c(.001, .01, .5, .99, .999))	{
#	creates a function for the Quantile graph
#		graph of the frame times against their quantile distribution
#		the PERCS argument allows one to change the percentiles wanted on the X axis
#			the default values are for 0.1%, 1%, 50%, 99%, and 99.9%
	if	(datatype == "MsBetweenPresents")	{
#		checks if the datatype is "MsBetweenPresents" and will set the Y scale
		STATS	=	graphSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_Y	=	scale_y_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frame Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Frame Time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks will be rounded according to the labelRound function
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer

	if	(datatype == "MsBetweenDisplayChange")	{
#		checks if the datatype is "MsBetweenDisplayChange" and will set the Y scale
		STATS	=	dispgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_Y	=	scale_y_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelDisp,
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Display Time (FPS)",
				labels	=	c(0, round(ytimes, 2))
			)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Refresh Cycles Later
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#			labels will be the values shown at the breaks and here will be converted to something more reasonable for display time
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses time as the unit instead, and while cycles later I feel is appropriate, some viewers may appreciate this
	
	if	(datatype == "MsUntilRenderComplete")	{
#		checks if the datatype is "MsUntilRenderComplete" and will set the Y scale
		STATS	=	rendgSTATS
#			creates a generic STATS variable for holding the graph stats
		scale_Y	=	scale_y_continuous(
			name	=	"Render Time (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Render Rate (FPS)",
				labels	=	ms2FPS
			)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Render Time
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks will be rounded according to the labelRound function
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#			a second axis is made and placed on the opposite side of the graph
#				this second axis uses FPS as the unit instead, so both units are presented to the viewer

	if	(datatype == "MsEstimatedDriverLag")	{
		STATS	=	drivgSTATS
		scale_Y	=	scale_y_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	c(0, round(ytimes, 2)),
			labels	=	labelRound,
			expand	=	c(0.02, 0)
		)
	}
#			a continuous scale is used because the measured times are on a continuous range, and not discrete values
#			for this data type the appropriate name is Estimated Driver Lag
#			breaks, where major guide lines are drawn, will be on the list of common times set at the beginning of the file
#				labels for the breaks will be rounded according to the labelRound function
#			the limits for the scale are 0 to the FtimeLimit set in the Input script
#			how the graph pads out from the scale
#				the first value is a coefficient and the second is additive
#	sec.axis	=	sec_axis(~.,
#		breaks	=	STATS[c("0.1", "1", "Median", "99", "99.9")],
#		labels	=	paste0(round(STATS[c("0.1", "1", "Median", "99", "99.9")], 2), c(" (0.1%)", " (1%)", " (50%)", " (99%)", " (99.9%)"))
#	)
#		this can be used to add a secondary axis that shows the values for the percentiles
#			it needs to be put in place after STATS is assigned, else it throws an error
	
	STATS$GPU	=	factor(STATS$GPU, levels = listGPU, ordered = TRUE)
#		makes the GPU column of STATS ordered factors based on listGPU
#			this is necessary for the faceting to work correctly
	if	(testAPI)	{
		FACET	=	facet_grid(rows = vars(Location, API), cols = vars(GPU), switch = "y")
	}	else	{
		FACET	=	facet_grid(rows = vars(Location), cols = vars(GPU), switch = "y")
	}
#		check to change the facet grouping to avoiding adding API group when there are no APIs to test
#		tells ggplot2 to create a faceted graph
#			the facet rows follow the API value
#			the facet columns follow the Location value
#			switches the Y labels for the facets to be on the left side, instead of the right

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
	FACET + 
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


graphOUT	=	function(datatype, graphtype, OUT = TRUE, diffLim = NULL, ...)	{
	if	(datatype == "MsBetweenPresents")			dataNAME	=	"Frame Time"
	if	(datatype == "MsBetweenDisplayChange")		dataNAME	=	"Display Time"
	if	(datatype == "MsUntilRenderComplete")		dataNAME	=	"Render Time"
	if	(datatype == "MsEstimatedDriverLag")		dataNAME	=	"Driver Lag"

	if	(substitute(graphtype) == "graphMEANS")		graphNAME	=	"Means"
	if	(substitute(graphtype) == "graphCOURSE")	graphNAME	=	"Course"
	if	(substitute(graphtype) == "graphFREQ")		graphNAME	=	"Freq"
	if	(substitute(graphtype) == "graphQQ")		graphNAME	=	"QQ"
	if	(substitute(graphtype) == "graphDIFF")		graphNAME	=	"Diff"

	#ARGS	=	list(...)
	#	how to get the miscellaneous arguments into a variable

	message(paste0(graphNAME, " - ", dataNAME))

	if	(graphNAME == "Diff" & !is.null(diffLim))	{
	#	because the graphDIFF function can accept the diffLim argument, it needs to be checked for and applied
		PLOT	=	graphtype(datatype, diffLim)
		dataNAME	=	paste0(dataNAME, " EXT")
		#	to identify whether the diffLim argument has been changed, the dataNAME variable is changed
		#	this also makes it possible to have the script automtically create two versions of the graph; one normal and one extended
	}	else	{
		PLOT	=	graphtype(datatype)
	}

	if	(OUT)	customSave(paste0("@", graphNAME, " - ", dataNAME), plot = PLOT, ...)
	PLOT	#shows the current graph, but must be after customSave
}
#	can be given the data type and graph function to have it create the graph
#		by setting OUT to false, it will not save the graph, but will still render it in the R GUI
#		it does check for and will apply the diffLim argument for the graphDIFF function
#	a message will be shown to identify the current graph being worked on

results$API	=	factor(results$API, levels = rev(listAPI))
#	my desired order for the APIs differs from R, so I reverse the order

#Means
if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphMEANS)
if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphMEANS)
if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphMEANS)
if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphMEANS)
#	checks if the data type should have a graph made, then does so


if	(useSHORT)	{
results							=	reLoc(results, shortLOC);		results		=	reAPI(results, shortAPI)

if	(graphFRAM)	{	graphSTATS	=	reLoc(graphSTATS, shortLOC);	graphSTATS	=	reAPI(graphSTATS, shortAPI)	}
if	(graphDISP)	{	dispgSTATS	=	reLoc(dispgSTATS, shortLOC);	dispgSTATS	=	reAPI(dispgSTATS, shortAPI)	}
if	(graphREND)	{	rendgSTATS	=	reLoc(rendgSTATS, shortLOC);	rendgSTATS	=	reAPI(rendgSTATS, shortAPI)	}
if	(graphDRIV)	{	drivgSTATS	=	reLoc(drivgSTATS, shortLOC);	drivgSTATS	=	reAPI(drivgSTATS, shortAPI)	}
#	the semi-colon allows these commands to occupy a single line, but {} are necessary for the if statements to work as desired
}
#	checks if the shortened versions of the Location and API names should be applied
#		typically the MEANS graph has enough room to use the longer names, hence this happening after that

results$Location						=	factor(results$Location,	levels = rev(levels(results$Location)))
if	(graphFRAM)		graphSTATS$Location	=	factor(graphSTATS$Location, levels = rev(levels(graphSTATS$Location)))
if	(graphDISP)		dispgSTATS$Location	=	factor(dispgSTATS$Location, levels = rev(levels(dispgSTATS$Location)))
if	(graphREND)		rendgSTATS$Location	=	factor(rendgSTATS$Location, levels = rev(levels(rendgSTATS$Location)))
if	(graphDRIV)		drivgSTATS$Location	=	factor(drivgSTATS$Location, levels = rev(levels(drivgSTATS$Location)))
#	reverses the levels so they go in the order I want

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
#	formalized extended DIFF graphs
if (!is.null(diffLim))	{
	if	(graphFRAM)	graphOUT("MsBetweenPresents",		graphDIFF,	diffLim = diffLim)
	if	(graphDISP)	graphOUT("MsBetweenDisplayChange",	graphDIFF,	diffLim = diffLim)
	if	(graphREND)	graphOUT("MsUntilRenderComplete",	graphDIFF,	diffLim = diffLim)
	if	(graphDRIV)	graphOUT("MsEstimatedDriverLag",	graphDIFF,	diffLim = diffLim)
} 