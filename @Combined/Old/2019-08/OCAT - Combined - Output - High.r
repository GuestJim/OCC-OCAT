ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)

# labelRound = function(x)	sprintf("%.1f", x)
labelRound = function(x)	round(x, 1)
labelBreak = function(input)	paste0(rep(c("", "\n"), length.out = length(input)), input)
labelDisp = function(breaks)	round(breaks * 60/1000, 1)

FtimeLimit = 1000/15

BoxPerc = function (DATA) {
	out = quantile(DATA, c(0.001, 0.01, 0.5, 0.99, 0.999))
	names(out) = c("ymin", "lower", "middle", "upper", "ymax")
	return(out)
}
#	by using this with stat_summary I can have custom quantiles for the boxplot

meanMS = function(x) {
	out = c(mean(x), median(x))
	names(out) = c("Mean", "Median")
	return(out)
}

# meanMS = function(x) {
	# out = as.list(mean(x))
	# names(out) <- "Mean"
	# return(out)
# }
#	if I use this, then I must use
#		aggregate(list(Mean = results$MsBetweenPresents), GROUPS, meanMS)
#	to get the proper naming

meanGEO = function(x) {
	out = c(1000/exp(mean(log(x))), exp(mean(log(x))))
	names(out) = c("FPS", "ms")
	return(round(out, r))
}

percMS = function(x, listPERC = c(0.1, 1, 99, 99.9)) {
	if (max(listPERC) > 1) listPERC = listPERC/100
	out = c()
	for (i in listPERC) {
		temp = quantile(x, i)
		names(temp) = paste0(i * 100, "%")
		out = append(out, temp)
	}
	
	return(out)
}

ecdfFPS = function(x, listFPS=NULL, r = 2) {
	listFPS = unique(sort(append(c(60, 50, 30, 20, 15), listFPS), decreasing = TRUE))
	out = 100 * (1 - ecdf(x)(1000 / listFPS))
	names(out) = paste0(listFPS, " FPS")
	
	return(round(out, r))
}

statMS	=	function(x, r = 2)	{
	out	=	c(mean(x), sd(x), sd(x)/mean(x) * 100, skewness(x), kurtosis(x))
	names(out)	=	c("Mean (ms)", "StDev (ms)", "CoV (%)", "Skew", "Kurtosis")
	return(round(out, r))
}

qqslope = function (data, r = 2, quan = c(0.01, 0.99))	{
	y = quantile(data, quan)
	x = qnorm(quan)
	slope = diff(y)/diff(x)
	return(round(slope, r))
}

statGRAPH = function(x, r = 2, quan = c(0.01, 0.99))	{
	out = c(mean(x), median(x), qqslope(x, quan = quan))
	for (i in c(0.1, 1, 99, 99.9)/100) {
		temp = c(quantile(x, i))
		names(temp) = paste0(i * 100)
		out = append(out, temp)
	}
	names(out) = c("Mean", "Median", "Slope", "0.1", "1", "99", "99.9")
	return(out)
}

sepCOL = function(tab) {
	out = as.data.frame(as.matrix(tab))
	for (i in grep("x", names(out)))	{
		out[, i] = as.numeric(as.character(out[, i]))
	}
	colnames(out) = sub("x.", "", colnames(out))
	return(out)
}

addFPS	=	function(x, r = 2){
	lab = x[1:grep("Location", colnames(x))]
	val = x[-(1:grep("Location", colnames(x)))]
	
	tFPS = cbind(lab, rep("FPS", nrow(x)), round(1000/val, 2))
	names(tFPS)[ncol(lab) + 1] = ""
	tMS = cbind(lab, rep("ms", nrow(x)), round(val, 2))
	names(tMS)[ncol(lab) + 1] = ""
	
	out = rbind(tFPS, tMS)	
	return(out)
}

compTAB = function(MEAN, PERC, ECDF, endECDF = NULL)	{
	if (is.null(endECDF) && !is.null(listFPS))	{
		endECDF = grep(paste0(min(listFPS), " FPS"), colnames(ECDF))
	}	else if	(is.null(endECDF) && is.null(listFPS)) {
		endECDF = grep("60 FPS", colnames(ECDF))
	}
			
	out = cbind(
		addFPS(MEAN), 
		addFPS(PERC)[-(1:grep("0.1%", colnames(addFPS(PERC))) - 1)],
		ECDF[grep("60 FPS", colnames(ECDF)):endECDF]
	)
	
	colnames(out)[grep("Var", colnames(out))] = ""
	return(out)
}

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

if (testAPI) {
	GROUPS	=	list(GPU = results$GPU, API = results$API, Location = results$Location)
	# namesF	=	c("GPU", "API", "Location", "_")
	# namesD	=	c("GPU", "API", "Location", "_")
}	else	{
	GROUPS	=	list(GPU = results$GPU, Location = results$Location)
	# namesF	=	c("GPU", "Location", "_")
	# namesD	=	c("GPU", "Location", "_")
}

dataMEAN = sepCOL(aggregate(results$MsBetweenPresents, GROUPS, meanMS))
dataPERC = sepCOL(aggregate(results$MsBetweenPresents, GROUPS, percMS))
dataECDF = sepCOL(aggregate(results$MsBetweenPresents, GROUPS, ecdfFPS, listFPS))
dataSTAT = sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statMS))
graphSTATS = sepCOL(aggregate(results$MsBetweenPresents, GROUPS, statGRAPH))
if (textDISP){
	dispMEAN = sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, meanMS))
	dispPERC = sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, percMS))
	dispECDF = sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, ecdfFPS, listFPS))
	dispSTAT = sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statMS))
	dispgSTATS = sepCOL(aggregate(results$MsBetweenDisplayChange, GROUPS, statGRAPH))
}
#	it is worth noting that using a list when passing the data to aggregate allows you to set the name of the output column
#		aggregate(list(Hello = data, groups, function)) will label the column Hello

if (textFRAM)	{
options(width = 1000)
sink(paste0(game, " - ", QUA, " Frame Data.txt"), split = TRUE)
writeLines(game)
writeLines("Frame Time")
writeLines("\nMean")
print(dataMEAN, row.names = FALSE)
writeLines("\nPercentiles")
print(dataPERC, row.names = FALSE)
writeLines("\nPercentile of FPS")
print(dataECDF, row.names = FALSE)
writeLines("\nDistribution Stats")
print(dataSTAT, row.names = FALSE)
sink()

for (GPU in listGPU) {	if (file.exists(GPU))	{
	options(width = 1000)
	sink(paste0(GPU, "\\", game, " - ", GPU, " - ", QUA, " Frame Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Frame Time")
		writeLines("\nMean")
		print(dataMEAN[dataMEAN$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dataPERC[dataPERC$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dataECDF[dataECDF$GPU==GPU,], row.names = FALSE)
	sink()
	}	}

if (textAPI)	{
for (API in listAPI) {
	options(width = 1000)
	sink(paste0(game, " - ", API, " - ", QUA, " Frame Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Frame Time")
		writeLines("\nMean")
		print(dataMEAN[dataMEAN$API==API,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dataPERC[dataPERC$API==API,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dataECDF[dataECDF$API==API,], row.names = FALSE)
		writeLines("\nDistribution Stats")
		print(dataSTAT[dataSTAT$API==API,], row.names = FALSE)
	sink()
}	}	}

if (textDISP){
	options(width = 1000)
	sink(paste0(game, " - ", QUA, " Display Data.txt"), split = TRUE)
	writeLines(game)
	writeLines("Display Time")
	writeLines("\nMean")
	print(dispMEAN, row.names = FALSE)
	writeLines("\nPercentiles")
	print(dispPERC, row.names = FALSE)
	writeLines("\nPercentile of FPS")
	print(dispECDF, row.names = FALSE)
	writeLines("\nDistribution Stats")
	print(dispSTAT, row.names = FALSE)
	sink()

	for (GPU in listGPU) {	if (file.exists(GPU))	{
	options(width = 1000)
	sink(paste0(GPU, "\\", game, " - ", GPU, " - ", QUA, " Display Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Display Time")
		writeLines("\nMean")
		print(dispMEAN[dispMEAN$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dispPERC[dispPERC$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dispECDF[dispECDF$GPU==GPU,], row.names = FALSE)
		writeLines("\nDistribution Stats")
		print(dispSTAT[dispSTAT$GPU==GPU,], row.names = FALSE)
	sink()
	}	}

	if (textAPI)	{
	for (API in listAPI) {
		options(width = 1000)
		sink(paste0(game, " - ", API, " - ", QUA, " Frame Data.txt"), split = TRUE)
			writeLines(game)
			writeLines("Frame Time")
			writeLines("\nMean")
			print(dispMEAN[dispMEAN$API==API,], row.names = FALSE)
			writeLines("\nPercentiles")
			print(dispPERC[dispPERC$API==API,], row.names = FALSE)
			writeLines("\nPercentile of FPS")
			print(dispECDF[dispECDF$API==API,], row.names = FALSE)
			writeLines("\nDistribution Stats")
			print(dispSTAT[dispSTAT$API==API,], row.names = FALSE)
		sink()
}	}	}
message("")

library(tableHTML)
OCCHTML = function(tab) {
	tableHTML(tab, rownames = FALSE, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}

writeOCC = function(type, typeName=substitute(type), name=paste0(game, " - ", QUA))	{
	write_tableHTML(OCCHTML(type), file = paste0(name, " - ", typeName,".html"))
}

writeGPU = function(type, GPU, typeName=substitute(type), name=gameF)	{
	write_tableHTML(OCCHTML(type[type$GPU==GPU,]), file = paste0(GPU, "\\", name, " - ", GPU, " - ", QUA, " - ", typeName,".html"))
}

writeSUB = function(type, SUB, typeName=substitute(type), name=gameF)	{
	COL = deparse(substitute(SUB))
	write_tableHTML(OCCHTML(type[type[,COL]==SUB,]), file = paste0(name, " - ", SUB, " - ", QUA, " - ", typeName,".html"))
}

if (HTMLOUT){
writeOCC(dataMEAN)
writeOCC(dataPERC)
writeOCC(dataECDF)
writeOCC(dataSTAT)
writeOCC(compTAB(dataMEAN, dataPERC, dataECDF), typeName = "dataCOMP")

if	(textAPI)	{
	for	(API in listAPI)	{
	writeSUB(dataMEAN, API)
	writeSUB(dataPERC, API)
	writeSUB(dataECDF, API)
	writeSUB(dataSTAT, API)
	writeSUB(compTAB(dataMEAN, dataPERC, dataECDF), API, typeName = "dataCOMP")
	}	}
	
for (GPU in listGPU) {	if (file.exists(GPU))	{
	writeGPU(dataMEAN, GPU)
	writeGPU(dataPERC, GPU)
	writeGPU(dataECDF, GPU)
	writeGPU(dataSTAT, GPU)
	writeGPU(compTAB(dataMEAN, dataPERC, dataECDF), GPU, typeName = "dataCOMP")
	}	}


if (textDISP){
writeOCC(dispMEAN)
writeOCC(dispPERC)
writeOCC(dispECDF)
writeOCC(dispSTAT)
writeOCC(compTAB(dispMEAN, dispPERC, dispECDF), typeName = "dispCOMP")

if	(textAPI){
	for	(API in listAPI)	{
	writeSUB(dispMEAN, API)
	writeSUB(dispPERC, API)
	writeSUB(dispECDF, API)
	writeSUB(dispSTAT, API)
	writeSUB(compTAB(dispMEAN, dispPERC, dispECDF), API, typeName = "dispCOMP")
	}	}

for (GPU in listGPU) {	if (file.exists(GPU))	{
	writeGPU(dispMEAN, GPU)
	writeGPU(dispPERC, GPU)
	writeGPU(dispECDF, GPU)
	writeGPU(dispSTAT, GPU)
	writeGPU(compTAB(dispMEAN, dispPERC, dispECDF), GPU, typeName = "dispCOMP")
	}	}
}
}

results$API = factor(results$API, levels = rev(listAPI))
if (graphFRAM) {
#Averages - Frame Time
message("Averages - Frame Time")

# results$Location = factor(results$Location, levels = levels(results$Location))
# results$API = factor(results$API, levels = rev(listAPI))

ggplot(data = results) + ggtitle(gameQUA, subtitle = "Averages, Medians, and Percentiles (MsBetweenPresent)") +
geom_hline(yintercept = 1000/60, color = "red") +
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), outlier.alpha = 0) +
stat_summary(aes(x = GPU, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
geom_bar(aes(x = GPU, y = MsBetweenPresents, fill = GPU), stat = "summary", fun.y = "mean") + scale_fill_hue() + 
stat_summary(aes(x = GPU, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), alpha = 0.50, outlier.alpha = 0.1) +
facet_grid(rows = vars(API), cols = vars(Location), switch = "y") +
scale_x_discrete(labels = labelBreak) +
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) +
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

results = reLoc(results, shortLOC)
results = reAPI(results, shortAPI)
graphSTATS = reLoc(graphSTATS, shortLOC)
graphSTATS = reAPI(graphSTATS, shortAPI)

results$Location = factor(results$Location, levels = rev(levels(results$Location)))
graphSTATS$Location = factor(graphSTATS$Location, levels = rev(levels(graphSTATS$Location)))
#	reverses the levels so they go in the order I want

customSave("@Averages - Frame Time")


#Course - Frame Time
message("Course - Frame Time")

ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenPresents)) +
ggtitle(gameQUA, subtitle = "MsBetweenPresent") +
geom_hline(yintercept = 1000/60, color = "red") +
geom_point(alpha = 0.05) +
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), labels = labelBreak, expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) +
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(0, 66.67), labels = labelBreak, expand=c(0.02, 0), sec.axis = dup_axis()) +
guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

customSave("@Course - Frame Time")


#Diff - Frame Time
message("Diff - Frame Time")

ggplot(data=results, aes(x=results$MsBetweenPresents, y=rbind(c(diff(results$MsBetweenPresents), 0))[1,])) +
ggtitle(gameQUA, subtitle="MsBetweenPresent consecutive differences") +
geom_point(alpha = 0.1) +
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
# geom_point(x=median(results$MsBetweenPresents), y=median(diff(results$MsBetweenPresents)), color = "magenta", shape ="x") +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels = labelBreak, limits=c(0, 66.67), expand=c(0.02, 0)) +
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

customSave("@Diff - Frame Time")

#Frequency - Frame Time
message("Frequency - Frame Time")

ggplot(results, aes(MsBetweenPresents)) +
ggtitle(paste0(gameQUA, " Frequency Plot of Frame Times"), subtitle="MsBetweenPresents") +
geom_vline(xintercept = 1000/60, color = "red") +
geom_freqpoly(binwidth=0.03, size=0) +
	geom_vline(data = graphSTATS, aes(xintercept = Mean), color = "darkgreen") + 
	geom_vline(data = graphSTATS, aes(xintercept = Median), color = "darkcyan", linetype="dotted") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=FtimeLimit, by=1000/60), labels=labelRound, limits = c(0,  FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) +
expand_limits(x=c(1000/60, 1000/30)) +
scale_y_continuous(name="Count", expand=c(0.02, 0)) + theme(panel.spacing.x = unit(1, "lines"))

customSave("@Freq - Frame Time")


#QQ - Frame Time
message("QQ - Frame Time")

ggplot() + 
ggtitle(paste0(gameQUA, " QQ Distribution"), subtitle="MsBetweenPresents") +
geom_hline(yintercept = 1000/60, color = "red") +
	geom_rect(data = graphSTATS, aes(ymin=-Inf, ymax=graphSTATS[, c("0.1")],	xmin=-Inf, xmax=qnorm(c(.001))), alpha=0.1, fill=c("blue"), color = "grey") +
	geom_rect(data = graphSTATS, aes(ymin=-Inf, ymax=graphSTATS[, c("1")],	xmin=-Inf, xmax=qnorm(c(.010))), alpha=0.1, fill=c("blue"), color = "grey") +
	geom_rect(data = graphSTATS, aes(ymin=-Inf, ymax=graphSTATS[, c("Median")],	xmin=-Inf, xmax=qnorm(c(.500))), alpha=0.1, fill=c("blue"), color = "grey") +
	geom_rect(data = graphSTATS, aes(ymin=-Inf, ymax=graphSTATS[, c("99")],	xmin=-Inf, xmax=qnorm(c(.990))), alpha=0.1, fill=c("red"), color = "grey") +
	geom_rect(data = graphSTATS, aes(ymin=-Inf, ymax=graphSTATS[, c("99.9")],	xmin=-Inf, xmax=qnorm(c(.999))), alpha=0.1, fill=c("red"), color = "grey") +
stat_qq_line(data = results, aes(sample=MsBetweenPresents), line.p = c(0.1, 0.99), color = "green", size = 1.1, linetype = "dotted") + 
stat_qq(data = results, aes(sample=MsBetweenPresents)) +
stat_qq_line(data = results, aes(sample=MsBetweenPresents), line.p = c(0.1, 0.99), color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") + 
geom_label(data = graphSTATS, aes(x = Inf, y = -Inf, label = paste0("Slope: ", Slope), fontface = "bold"), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(0,  FtimeLimit), expand=c(0.02, 0)) +
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

customSave("@QQ - Frame Time")
}


if (graphDISP) {
results = reLoc(results, listLOC)

#Averages - Display Time
message("Averages - Display Time")
results$Location = factor(results$Location, levels = levels(results$Location))

ggplot(data = results) + ggtitle(gameQUA, subtitle = "Averages, Medians, and Percentiles (MsBetweenDisplayChange)") +
geom_hline(yintercept = 1000/60, color = "red") +
# geom_boxplot(aes(x = GPU, y = MsBetweenDisplayChange), outlier.alpha = 0) +
stat_summary(aes(x = GPU, y = MsBetweenDisplayChange), fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
geom_bar(aes(x = GPU, y = MsBetweenDisplayChange, fill = GPU), stat = "summary", fun.y = "mean") + scale_fill_hue() + 
stat_summary(aes(x = GPU, y = MsBetweenDisplayChange), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
# geom_boxplot(aes(x = GPU, y = MsBetweenDisplayChange), alpha = 0.50, outlier.alpha = 0.1) +
facet_grid(rows = vars(API), cols = vars(Location), switch = "y") +
scale_x_discrete(labels = labelBreak) +
scale_y_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + scale_x_discrete(labels = labelBreak) +
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

results = reLoc(results, shortLOC)
results = reAPI(results, shortAPI)
dispgSTATS = reLoc(dispgSTATS, shortLOC)
dispgSTATS = reAPI(dispgSTATS, shortAPI)

results$Location = factor(results$Location, levels = rev(levels(results$Location)))
dispgSTATS$Location = factor(dispgSTATS$Location, levels = rev(levels(dispgSTATS$Location)))

customSave("@Averages - Display Time")


#Course - Display Time
message("Course - Display Time")

ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenDisplayChange)) +
ggtitle(gameQUA, subtitle = "MsBetweenDisplayChange") +
geom_hline(yintercept = 1000/60, color = "red") +
geom_point(alpha = 0.05) +
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), labels = labelBreak, expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) +
scale_y_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) +
guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

customSave("@Course - Display Time")


#Diff - Display Time
message("Diff - Display Time")

ggplot(data=results, aes(x=results$MsBetweenDisplayChange, y=rbind(c(diff(results$MsBetweenDisplayChange), 0))[1,])) +
ggtitle(gameQUA, subtitle="MsBetweenDisplayChange consecutive differences") +
geom_point(alpha = 0.1) +
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
# geom_point(x=median(results$MsBetweenDisplayChange), y=median(diff(results$MsBetweenDisplayChange)), color = "magenta", shape ="x") +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0)) +
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

customSave("@Diff - Display Time")


#Frequency - Display Time
message("Diff - Display Time")

ggplot(results, aes(MsBetweenDisplayChange)) +
ggtitle(paste0(gameQUA, " Frequency Plot of Frame Times"), subtitle="MsBetweenDisplayChange") +
geom_vline(xintercept = 1000/60, color = "red") +
geom_freqpoly(binwidth=0.03, size=0) +
	geom_vline(data = dispgSTATS, aes(xintercept = Mean), color = "darkgreen") + 
	geom_vline(data = dispgSTATS, aes(xintercept = Median), color = "darkcyan", linetype="dotted") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=FtimeLimit, by=1000/60), labels=labelRound, limits = c(0,  FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) +
expand_limits(x=c(1000/60, 1000/30)) +
scale_y_continuous(name="Count", expand=c(0.02, 0)) + theme(panel.spacing.x = unit(1, "lines"))

customSave("@Freq - Display Time")


#QQ - Display Time
message("QQ - Display Time")

ggplot() + 
ggtitle(paste0(gameQUA, " QQ Distribution"), subtitle="MsBetweenDisplayChange") +
geom_hline(yintercept = 1000/60, color = "red") +
	geom_rect(data = dispgSTATS, aes(ymin=-Inf, ymax=dispgSTATS[, c("0.1")],	xmin=-Inf, xmax=qnorm(c(.001))), alpha=0.1, fill=c("blue"), color = "grey") +
	geom_rect(data = dispgSTATS, aes(ymin=-Inf, ymax=dispgSTATS[, c("1")],	xmin=-Inf, xmax=qnorm(c(.010))), alpha=0.1, fill=c("blue"), color = "grey") +
	geom_rect(data = dispgSTATS, aes(ymin=-Inf, ymax=dispgSTATS[, c("Median")],	xmin=-Inf, xmax=qnorm(c(.500))), alpha=0.1, fill=c("blue"), color = "grey") +
	geom_rect(data = dispgSTATS, aes(ymin=-Inf, ymax=dispgSTATS[, c("99")],	xmin=-Inf, xmax=qnorm(c(.990))), alpha=0.1, fill=c("red"), color = "grey") +
	geom_rect(data = dispgSTATS, aes(ymin=-Inf, ymax=dispgSTATS[, c("99.9")],	xmin=-Inf, xmax=qnorm(c(.999))), alpha=0.1, fill=c("red"), color = "grey") +
stat_qq(data = results, aes(sample=MsBetweenDisplayChange)) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(0,  FtimeLimit), expand=c(0.02, 0)) +
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

customSave("@QQ - Display Time")
}

if (graphDIFF){
#Course - Difference Frame
message("Course - Difference Frame")

ggplot(data = results, aes(x = TimeInSeconds, y = MsDifferencePresents)) +
ggtitle(gameQUA, subtitle="MsBetweenPresents consecutive differences") + labs(caption = cGPU) +
geom_point(alpha = 0.05) +
geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0)) +
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0))

customSave("@Course - Difference Frame")

#Frequency - Frame Time
message("Frequency - Difference Frame")

ggplot(results, aes(MsDifferencePresents)) +
ggtitle(paste0(gameQUA, " Frequency Plot of Frame Time Differences"), subtitle="MsBetweenPresents") +
geom_vline(xintercept = 0, color = "red") +
geom_freqpoly(binwidth=0.03, size=0) +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
scale_x_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0.02, 0), sec.axis = dup_axis()) +
expand_limits(x=c(-1000/50, 1000/50)) +
scale_y_continuous(name="Count", expand=c(0.02, 0)) + theme(panel.spacing.x = unit(1, "lines"))

customSave("@Freq - Difference Frame")

#QQ - Frame Time
message("QQ - Frame Time")

ggplot(results, aes(sample=MsDifferencePresents)) +
ggtitle(paste0(gameQUA, " QQ Distribution of Frame Time Differences"), subtitle="MsBetweenPresents") +
geom_hline(yintercept = 0, color = "red") +
geom_point(stat="qq") +
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") +
# scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(0,  FtimeLimit), expand=c(0.02, 0)) +
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(-1000/50, 1000/50), expand=c(0.02, 0)) +
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

customSave("@QQ - Difference Frame")
}