ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)
#	list of frame rates and their opposites for use as axes labels

#labelRound = function(x)	sprintf("%.1f", x)
labelRound = function(x)	round(x, 1)
labelBreak = function(input)	paste0(rep(c("", "\n"), length.out = length(input)), input)
labelDisp = function(breaks)	round(breaks * 60/1000, 1)
#	functions for generating formatting graph labels based on the graph breaks, instead of manually setting labels
#		labelRound will round the values to 1 digit after the decimal point
#		labelBreak will add an alternating line break to the breaks, for when the labels would run into each other horizontally
#		labelDisp is to convert the values from milliseconds to Refresh Cycles, for use with display time as opposed to frame time

FtimeLimit = 1000/15
#	a limit on frame time for constraining graph axes

BoxPerc = function (DATA) {
	out = quantile(DATA, c(0.001, 0.01, 0.5, 0.99, 0.999))
	names(out) = c("ymin", "lower", "middle", "upper", "ymax")
	return(out)
}
#	by using this with stat_summary I can have custom quantiles for the boxplot

meanFPS = function(x, r = 2) {
	out = c(1000/mean(x), mean(x))
	names(out) = c("FPS", "ms")
	return(round(out, r))
}
#	custom function that will provide the mean (average) for the data in both FPS and ms units, and labels the units

percFPS = function(x, listPERC = c(0.1, 1, 99, 99.9), r = 2) {
	if (max(listPERC) > 1) listPERC = listPERC/100
	out = c()
	for (i in listPERC) {
		temp = c(1000/quantile(x, i), quantile(x, i))
		names(temp) = paste0(i * 100, c("% (FPS)", "% (ms)"))
		out = append(out, temp)
	}
	return(round(out, r))
}
#	function to provide the 0.1%, 1%, 99%, and 99.9% percentil values for the data, in both FPS and ms units, and mark the units

ecdfFPS = function(x, listFPS=NULL, r = 2) {
	listFPS = unique(sort(append(c(60, 50, 30, 20, 15), listFPS), decreasing = TRUE))
	out = 100*(1-ecdf(x)(1000/listFPS))
	names(out) = paste0(listFPS, " FPS")
	return(round(out, r))
}
#	function to use the ECDF function that will find the corresponding percentile for a given value
#		the default list of frame rates to find is 60, 50, 30, 20, and 15 but I have it set up such that more can be added, and the list will be sorted and duplicates removed

sepCOL = function(tab, name = c("GPU", "Location", "V")) {
	names(tab) = name
	out = as.data.frame(as.matrix(tab))
	colnames(out) = gsub("V.", "", colnames(out))
	return(out)
}
#	a function that will take the multi-level lists the aggregate function will generate and convert them into a data frame with separate columns
#		the V. is placed by the aggregate function but this function will remove it

nameFIND = function (FRAME, name)	{
	grep(name, names(FRAME), value=TRUE)
}
#	finds and returns the column numbers for a frame that contain a certain name in them

nameSEARCH = function(FRAME, name)	{
	if (is.vector(name)){
		out = numeric(0)
		for (i in name)	{
			print(i)
			out = append(out, which(colnames(FRAME)==i))
		}
	}	else	{
		out = which(colnames(FRAME)==name)
	}
	return(out)
}
#	this returns the columns with 'name' in its column name

findSEARCH = function(FRAME, term)	{
	name = grep(term, names(FRAME), value=TRUE)
	if (is.vector(name)){
		out = numeric(0)
		for (i in name)	{
			out = append(out, which(colnames(FRAME)==i))
		}
	}	else	{
		out = which(colnames(FRAME)==name)
	}
	return(out)
}
#	combination of the above two functions

compMEAN = function(FRAME)	{
	temp1 = cbind(FRAME[1:findSEARCH(FRAME, "Average")], rep("FPS", nrow(FRAME)), FRAME[findSEARCH(FRAME, "FPS")])
	temp2 = cbind(FRAME[1:findSEARCH(FRAME, "Average")], rep("ms", nrow(FRAME)), FRAME[findSEARCH(FRAME, "ms")])
	colnames(temp1)[findSEARCH(FRAME, "Average"):length(temp1)] = c("Location", "", "Average")
	colnames(temp2)[findSEARCH(FRAME, "Average"):length(temp1)] = c("Location", "", "Average")

	return(rbind(temp1, temp2))
}
#	compact mean (average) function that will take a frame containing FPS and ms columns and produce a frame with FPS rows followed by ms rows
#		by using the findSEARCH function, it is able to identify the columns specific strings are in
#		this allows it to work with data where there are different APIs to consider

compPERC = function(FRAME, listPERC = c(0.1, 1, 99, 99.9))	{
	temp1 = cbind(FRAME[1:2], rep("FPS", nrow(FRAME)), FRAME[findSEARCH(FRAME, "FPS")])
	temp2 = cbind(FRAME[1:2], rep("ms", nrow(FRAME)), FRAME[findSEARCH(FRAME, "ms")])
	colnames(temp1) = c("GPU", "Location", "", paste0(listPERC, "%"))
	colnames(temp2) = c("GPU", "Location", "", paste0(listPERC, "%"))

	return(rbind(temp1, temp2))
}
#	compact percentile function that will take a frame containing FPS and ms columns and produce a frame with FPS rows followed by ms rows

compTAB = function(MEAN, PERC, ECDF, endECDF = NULL)	{
	if (is.null(endECDF) && !is.null(listFPS))	{
		endECDF = nameSEARCH(ECDF, paste0(min(listFPS), " FPS"))
	}	else if	(is.null(endECDF) && is.null(listFPS)) {
		endECDF = nameSEARCH(ECDF, "60 FPS")
	}
	out = cbind(compMEAN(MEAN), compPERC(PERC)[-(1:nameSEARCH(PERC, "0.1% (FPS)")-1)], ECDF[nameSEARCH(ECDF, "60 FPS"):endECDF])
	colnames(out)[findSEARCH(out, "Var")] = ""
	return(out)
}
#	compact table function for creating a single table holding the mean, percentile, and ECDF data in it
#	be default it will only get the 60 FPS ECDF value, but by passing a different endECDF value, it will include more
#		it will now check the listFPS variable set in the Input file for adding to the list of FPS targets to include
#			when additional values are present in listFPS that are less than 60, this will automatically add them to the table
#		by using the findSEARCH function, it is able to identify the columns specific strings are in
#		this allows it to work with data where there are different APIs to consider


customSave = function(type="", device=ggdevice, width=16, height=9, dpi=DPI) {
	if (exists("recording")) {
		if (device=="png") {
			ggsave(filename=paste0(gameFqua, " - ", recording, " - ", type, ".png"), device=device, width=width, height=height, scale=scale, dpi=dpi)
		}	else if (device=="pdf")	{
			ggsave(filename=paste0(gameFqua, " - ", recording, " - ", type, ".pdf"), device=device, width=width, height=height, scale=scale)
		}
	}	else {
		if (device=="png") {
			ggsave(filename=paste0(gameFqua, " - ", type, ".png"), device=device, width=width, height=height, dpi=dpi)
		}	else if (device=="pdf")	{
			ggsave(filename=paste0(gameFqua, " - ", type, ".pdf"), device=device, width=width, height=height)
		}
	}
}
#	custom function for saving graphs that automates the sizing and naming, significantly cleaning up the command to save the graphs

if (length(levels(results$API)) >= 2) {
	dataMEAN = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$API, results$Location), meanFPS), c("GPU", "API", "Average (Frame Time)", "V"))
	dataPERC = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$API, results$Location), percFPS), c("GPU", "API", "Percentile (Frame Time)", "V"))
	dataECDF = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$API, results$Location), ecdfFPS, listFPS), c("GPU", "API", "FPS Percentile (Frame Time)", "V"))
	if (textDISP){
		dispMEAN = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$API, results$Location), meanFPS), c("GPU", "API", "Average (Display Time)", "V"))
		dispPERC = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$API, results$Location), percFPS), c("GPU", "API", "Percentile (Display Time)", "V"))
		dispECDF = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$API, results$Location), ecdfFPS, listFPS), c("GPU", "API", "FPS Percentile (Display Time)", "V"))
	}
} else {
	dataMEAN = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), meanFPS), c("GPU", "Average (Frame Time)", "V"))
	dataPERC = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile (Frame Time)", "V"))
	dataECDF = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), ecdfFPS, listFPS), c("GPU", "FPS Percentile (Frame Time)", "V"))
	if (textDISP){
		dispMEAN = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), meanFPS), c("GPU", "Average (Display Time)", "V"))
		dispPERC = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile (Display Time)", "V"))
		dispECDF = sepCOL(aggregate(results$MsBetweenDisplayChange,  list(results$GPU, results$Location), ecdfFPS, listFPS), c("GPU", "FPS Percentile (Display Time)", "V"))
	}
}
#	the aggregate function will take data and create internal subsets to work with, based on provided lists
#	these subsets then have functions run on them, specifically the functions created above, generating the mean, percentile, and ECDF statistics for the separate locations and GPUs

if (textFRAM){
#	checks if a text file of the frame time statistics should be generated
options(width = 1000)
#	sets the character width allowed for the text file, and is set high so it does not line break
sink(paste0(game, " - ", QUA, " Frame Data.txt"), split = TRUE)
#	opens the file to create using the provided name
writeLines(paste0(gameGPU, " - ", QUA))
#	the name of the game, GPU, and quality configuratino are all written in the file.
writeLines("Frame Time")
writeLines("\nMean")
print(dataMEAN, row.names = FALSE)
writeLines("\nPercentiles")
print(dataPERC, row.names = FALSE)
writeLines("\nPercentile of FPS")
print(dataECDF, row.names = FALSE)
#	with labels, the aggregate data above is written to the files here
sink()
#	closes the text file, saving the data

for (GPU in listGPU) {	if (file.exists(GPU))	{
	options(width = 1000)
	sink(paste0(GPU, "\\", game, " - ", GPU, " - ", QUA, " Frame Data.txt"), split = TRUE)
		writeLines(paste0(gameGPU, " - ", QUA))
		writeLines("Frame Time")
		writeLines("\nMean")
		print(dataMEAN[dataMEAN$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dataPERC[dataPERC$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dataECDF[dataECDF$GPU==GPU,], row.names = FALSE)
	sink()
	}	}

if	(textLOC)	{
for (Location in listLOC) {
	options(width = 1000)
	sink(paste0(game, " - ", Location, " - ", QUA, " Frame Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Frame Time")
		writeLines("\nMean")
		print(dataMEAN[dataMEAN[nameSEARCH(dataMEAN, "FPS")-1]==Location,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(compPERC[compPERC[nameSEARCH(compPERC, "0.1% (FPS)")-1]==Location,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dataECDF[dataECDF[nameSEARCH(dataECDF, "60 FPS")-1]==Location,], row.names = FALSE)
	sink()
#	will create separate files for each location if desired
#		to cover when Location is not the name of the column, and when an API column is also present, nameSEARCH is used to find the first column with data, and then go back one, as this is always the Location column
}
#	will run through the different GPUs tested, if there is a sub-folder for them, and generates similar files to the above but only for the specific GPU
#		actually this likely is not necessary here, as this is the output file for data concerning a single GPU, but there is no harm in leaving it

if (textDISP){
	options(width = 1000)
	sink(paste0(game, " - ", QUA, " Display Data.txt"), split = TRUE)
	writeLines(paste0(gameGPU, " - ", QUA))
	writeLines("Display Time")
	writeLines("\nMean")
	print(dispMEAN, row.names = FALSE)
	writeLines("\nPercentiles")
	print(dispPERC, row.names = FALSE)
	writeLines("\nPercentile of FPS")
	print(dispECDF, row.names = FALSE)
	sink()

	for (GPU in listGPU) {	if (file.exists(GPU))	{
	options(width = 1000)
	sink(paste0(GPU, "\\", game, " - ", GPU, " - ", QUA, " Display Data.txt"), split = TRUE)
		writeLines(paste0(gameGPU, " - ", QUA))
		writeLines("Display Time")
		writeLines("\nMean")
		print(dispMEAN[dispMEAN$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dispPERC[dispPERC$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dispECDF[dispECDF$GPU==GPU,], row.names = FALSE)
	sink()
	}	}

	if (textLOC)	{
	for (Location in textLOC) {
		options(width = 1000)
		sink(paste0(game, " - ", Location, " - ", QUA, " Frame Data.txt"), split = TRUE)
			writeLines(game)
			writeLines("Frame Time")
			writeLines("\nMean")
			print(dispMEAN[dispMEAN[nameSEARCH(dispMEAN, "FPS")-1]==Location,], row.names = FALSE)
			writeLines("\nPercentiles")
			print(compPERC[compPERC[nameSEARCH(compPERC, "0.1% (FPS)")-1]==Location,], row.names = FALSE)
			writeLines("\nPercentile of FPS")
			print(dispECDF[dispECDF[nameSEARCH(dispECDF, "60 FPS")-1]==Location,], row.names = FALSE)
		sink()
}
#	precisely the same as above, but for display time data
message("")
#	this will create a blank line in the console window when running the script

library(tableHTML)
#	loads the tableHTML library, for writing tables to an HTML file
OCCHTML = function(tab) {
	tableHTML(tab[-1], rownames = FALSE, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}
#	custom function that will produce an HTML version of the provided table, but strip out the undesired code and have it approximately match my desired formatting for OCC articles

writeOCC = function(type, typeName=substitute(type), name=gameFQUA)	{
	write_tableHTML(OCCHTML(type), file = paste0(name, " - ", typeName,".html"))
}
#	function to take a table, create the OCCHTML table, and then actually write the HTML to a file with the desired name

writeGPU = function(type, GPU, typeName=substitute(type), name=gameF)	{
	write_tableHTML(OCCHTML(type[type$GPU==GPU,]), file = paste0(GPU, "\\", name, " - ", GPU, " - ", QUA, " - ", typeName,".html"))
}
#	similar to the above but is for generating HTML tables for each GPU
#		actually this likely is not necessary here, as this is the output file for data concerning a single GPU, but there is no harm in leaving it

writeSUB = function(type, SUB, typeName=substitute(type), name=gameF)	{
	COL = deparse(substitute(SUB))
	write_tableHTML(OCCHTML(type[type[,COL]==SUB,]), file = paste0(name, " - ", SUB, " - ", QUA, " - ", typeName,".html"))
}
#	similar to above but for generating HTML tables of certain subsets
if (HTMLOUT){
writeOCC(dataMEAN)
writeOCC(dataPERC)
writeOCC(dataECDF)
writeOCC(compTAB(dataMEAN, dataPERC, dataECDF), typeName = "dataCOMP")

if	(textLOC){
	for	(Location in textLOC)	{
	writeSUB(dataMEAN, Location)
	writeSUB(dataPERC, Location)
	writeSUB(dataECDF, Location)
	writeSUB(compTAB(dataMEAN, dataPERC, dataECDF), Location, typeName = "dataCOMP")
	}	}
#	will generate individual files for each location

for (GPU in listGPU) {	if (file.exists(GPU))	{
	writeGPU(dataMEAN, GPU)
	writeGPU(dataPERC, GPU)
	writeGPU(dataECDF, GPU)
	writeGPU(compTAB(dataMEAN, dataPERC, dataECDF), GPU, typeName = "dataCOMP")
	}	}

if (textDISP){
writeOCC(dispMEAN)
writeOCC(dispPERC)
writeOCC(dispECDF)
writeOCC(compTAB(dispMEAN, dispPERC, dispECDF), typeName = "dispCOMP")

if	(textLOC){
	for	(Location in textLOC)	{
	writeSUB(dispMEAN, Location)
	writeSUB(dispPERC, Location)
	writeSUB(dispECDF, Location)
	writeSUB(compTAB(dispMEAN, dispPERC, dispECDF), Location, typeName = "dataCOMP")
	}	}
#	will generate individual files for each location

for (GPU in listGPU) {	if (file.exists(GPU))	{
	writeGPU(dispMEAN, GPU)
	writeGPU(dispPERC, GPU)
	writeGPU(dispECDF, GPU)
	writeGPU(compTAB(dispMEAN, dispPERC, dispECDF), GPU, typeName = "dataCOMP")
	}	}
}
}
#	the commands to actually generate the HTML tables, if desired
#		the generated tables are of the mean, percentile, and ECDF statistics as well as the compact table described earlier

#
#	below are all of the graphs, which I do not typically comment individually
#	before each graph is a message command, so it can be seen what graph is being generated from the console window
#	there are checks for if the frame time or display time graphs are wanted
#

if (graphFRAM) {
#Averages - Frame Time
message("Averages - Frame Time")

results$Location = factor(results$Location, levels = listLOC)
results$API = factor(results$API, levels = rev(listAPI))
#	reverses the levels so they go in the order I want

ggplot(data = results) +
ggtitle(gameQUA, subtitle = "Averages, Medians, and Percentiles (MsBetweenPresent)") + labs(caption = cGPU) +
geom_hline(yintercept = 1000/60, color = "red") +
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), outlier.alpha = 0) +
stat_summary(aes(x = Location, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
geom_bar(aes(x = Location, y = MsBetweenPresents, fill = Location), stat = "summary", fun.y = "mean") +
stat_summary(aes(x = Location, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), alpha = 0.50, outlier.alpha = 0.1) +
facet_grid(rows = vars(GPU), cols = vars(API), switch = "y") +
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + scale_x_discrete(labels = labelBreak) +
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "none")

customSave("@Averages - Frame Time", width = 8)

results$Location = factor(results$Location, levels = rev(listLOC))
#	reverses the levels so they go in the order I want


#Course - Frame Time
message("Course - Frame Time")

ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenPresents)) +
ggtitle(gameQUA, subtitle = "MsBetweenPresent") + labs(caption = cGPU) +
geom_hline(yintercept = 1000/60, color = "red") +
geom_point(alpha = 0.05) +
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) +
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) +
guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

customSave("@Course - Frame Time", width = 8)


#Diff - Frame Time
message("Diff - Frame Time")

ggplot(data=results, aes(x=results$MsBetweenPresents, y=rbind(c(diff(results$MsBetweenPresents), 0))[1,])) +
ggtitle(gameQUA, subtitle="MsBetweenPresent consecutive differences") + labs(caption = cGPU) +
geom_point(alpha = 0.1) +
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
# geom_point(x=median(results$MsBetweenPresents), y=median(diff(results$MsBetweenPresents)), color = "magenta", shape ="x") +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) +
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

customSave("@Diff - Frame Time", width = 8)

#Frequency - Frame Time
message("Frequency - Frame Time")

ggplot(results, aes(MsBetweenPresents)) +
ggtitle(paste0(gameQUA), subtitle="MsBetweenPresents - Frequency Plot") + labs(caption = cGPU) +
geom_vline(xintercept = 1000/60, color = "red") +
geom_freqpoly(binwidth=0.03, size=0) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=FtimeLimit, by=1000/60), labels=labelRound, limits = c(0, FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) +
expand_limits(x=c(1000/60, 1000/30)) +
scale_y_continuous(name="Count", expand=c(0.02, 0))

customSave("@Freq - Frame Time", width = 8)


#QQ - Frame Time
message("QQ - Frame Time")

ggplot(results, aes(sample=MsBetweenPresents)) +
ggtitle(paste0(gameQUA), subtitle="MsBetweenPresents - QQ Distribution") + labs(caption = cGPU) +
geom_hline(yintercept = 1000/60, color = "red") +
geom_point(stat="qq") +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(0, FtimeLimit), expand=c(0.02, 0)) +
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50 (Median)", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

customSave("@QQ - Frame Time", width = 8)
}


if (graphDISP) {
#Averages - Display Time
message("Averages - Display Time")

results$Location = factor(results$Location, levels = listLOC)

ggplot(data = results) +
ggtitle(gameQUA, subtitle = "Averages, Medians, and Percentiles (MsBetweenDisplayChange)") + labs(caption = cGPU) +
geom_hline(yintercept = 1000/60, color = "red") +
# geom_boxplot(aes(x = GPU, y = MsBetweenDisplayChange), outlier.alpha = 0) +
stat_summary(aes(x = Location, y = MsBetweenDisplayChange), fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
geom_bar(aes(x = Location, y = MsBetweenDisplayChange, fill = Location), stat = "summary", fun.y = "mean") +
stat_summary(aes(x = Location, y = MsBetweenDisplayChange), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
# geom_boxplot(aes(x = GPU, y = MsBetweenDisplayChange), alpha = 0.50, outlier.alpha = 0.1) +
facet_grid(rows = vars(GPU), cols = vars(API), switch = "y") +
scale_y_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + scale_x_discrete(labels = labelBreak) +
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "none")

customSave("@Averages - Display Time", width = 8)

results$Location = factor(results$Location, levels = rev(listLOC))


#Course - Display Time
message("Course - Display Time")

ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenDisplayChange)) +
ggtitle(gameQUA, subtitle = "MsBetweenDisplayChange") + labs(caption = cGPU) +
geom_hline(yintercept = 1000/60, color = "red") +
geom_point(alpha = 0.05) +
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) +
scale_y_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) +
guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

customSave("@Course - Display Time", width = 8)


#Diff - Display Time
message("Diff - Display Time")

ggplot(data=results, aes(x=results$MsBetweenDisplayChange, y=rbind(c(diff(results$MsBetweenDisplayChange), 0))[1,])) +
ggtitle(gameQUA, subtitle="MsBetweenDisplayChange consecutive differences") + labs(caption = cGPU) +
geom_point(alpha = 0.1) +
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
# geom_point(x=median(results$MsBetweenDisplayChange), y=median(diff(results$MsBetweenDisplayChange)), color = "magenta", shape ="x") +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) +
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

customSave("@Diff - Display Time", width = 8)


#Frequency - Display Time
message("Frequency - Display Time")

ggplot(results, aes(MsBetweenDisplayChange)) +
ggtitle(paste0(gameQUA), subtitle="MsBetweenDisplayChange - Frequency Plot") + labs(caption = cGPU) +
geom_vline(xintercept = 1000/60, color = "red") +
geom_freqpoly(binwidth=0.03, size=0) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Refresh Cycles Later (1/60 s)", breaks=seq(from=0, to=FtimeLimit, by=1000/60), labels=labelDisp, limits = c(0, FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) +
expand_limits(x=c(1000/60, 1000/30)) +
scale_y_continuous(name="Count", expand=c(0.02, 0))

customSave("@Freq - Display Time", width = 8)


#QQ - Display Time
message("QQ - Display Time")

ggplot(results, aes(sample=MsBetweenDisplayChange)) +
ggtitle(paste0(gameQUA), subtitle="MsBetweenDisplayChange - QQ Distribution") + labs(caption = cGPU) +
geom_hline(yintercept = 1000/60, color = "red") +
geom_point(stat="qq") +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_y_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels=labelDisp, limits=c(0, FtimeLimit), expand=c(0.02, 0)) +
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50 (Median)", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

customSave("@QQ - Display Time", width = 8)
}