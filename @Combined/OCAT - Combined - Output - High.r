ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)

# labelRound = function(x) sprintf("%.1f", x)
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

#saves file with the desired statistics
#	checks if API is a column or not and currently assumes RTX Off is the meaning of an empty cell
#	should still work if not using RTX, but there cannot be empty cells then

meanFPS = function(x, r = 2) {
	out = c(1000/mean(x), mean(x))
	names(out) = c("FPS", "ms")
	return(round(out, r))
}

percFPS = function(x, listPERC = c(0.1, 1, 99, 99.9), r = 2) {
	if (max(listPERC) > 1) listPERC = listPERC/100
	out = c()
	for (i in listPERC) {
		temp = c(1000/quantile(x, i), quantile(x, i))
		names(temp) = paste0(i * 100, c("% (ms)", "% (FPS)"))
		out = append(out, temp)
	}
	return(round(out, r))
}

ecdfFPS = function(x, listFPS=c(60, 50, 30, 20, 15), r = 2) {
	out = 100*(1-ecdf(x)(1000/listFPS))
	names(out) = paste0(listFPS, " FPS")
	return(round(out, r))
}

sepCOL = function(tab, name = c("GPU", "Location", "V")) {
	names(tab) = name
	out = as.data.frame(as.matrix(tab))
	colnames(out) = gsub("V.", "", colnames(out))
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

if (length(levels(results$API)) >= 2) {
	dataMEAN = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "API", "V"))
	dataPERC = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "API", "V"))
	dataECDF = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "API", "V"))
	if (textDISP){
		dispMEAN = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "API", "V"))
		dispPERC = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "API", "V"))
		dispECDF = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "API", "V"))	
	}
} else {
	dataMEAN = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "V"))
	dataPERC = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "V"))
	dataECDF = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "V"))
	if (textDISP){
		dispMEAN = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "V"))
		dispPERC = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "V"))
		dispECDF = sepCOL(aggregate(results$MsBetweenDisplayChange,  list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "V"))
	}
}

if (textFRAM){
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
}

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
	sink()
	}	}
}
message("")

library(tableHTML)
OCCHTML = function(tab) {
	tableHTML(tab[-1], rownames = FALSE, class="OCC") %>% 
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}

writeOCC = function(type, typeName=substitute(type), name=gameFQUA)	{
	write_tableHTML(OCCHTML(type), file = paste0(name, " - ", typeName,".html"))
}

writeGPU = function(type, GPU, typeName=substitute(type), name=gameF)	{
	write_tableHTML(OCCHTML(type[type$GPU==GPU,]), file = paste0(GPU, "\\", name, " - ", GPU, " - ", QUA, " - ", typeName,".html"))
}

if (HTMLOUT){
writeOCC(dataMEAN)
writeOCC(dataPERC)
writeOCC(dataECDF)

for (GPU in listGPU) {	if (file.exists(GPU))	{
	writeGPU(dataMEAN, GPU)
	writeGPU(dataPERC, GPU)
	writeGPU(dataECDF, GPU)
	}	}
	
if (textDISP){
writeOCC(dispMEAN)
writeOCC(dispPERC)
writeOCC(dispECDF)

for (GPU in listGPU) {	if (file.exists(GPU))	{
	writeGPU(dispMEAN, GPU)
	writeGPU(dispPERC, GPU)
	writeGPU(dispECDF, GPU)
	}	}
}
}


if (graphFRAM) {
#Averages - Frame Time
message("Averages - Frame Time")

results$Location = factor(results$Location, levels = listLOC)

ggplot(data = results) + ggtitle(gameQUA, subtitle = "Averages, Medians, and Percentiles (MsBetweenPresent)") + 
geom_hline(yintercept = 1000/60, color = "red") + 
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), outlier.alpha = 0) + 
stat_summary(aes(x = GPU, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", width = 0.6) + 
geom_bar(aes(x = GPU, y = MsBetweenPresents, fill = GPU), stat = "summary", fun.y = "mean") + 
stat_summary(aes(x = GPU, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) + 
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), alpha = 0.50, outlier.alpha = 0.1) + 
facet_grid(rows = vars(API), cols = vars(Location), switch = "y") + 
scale_x_discrete(labels = labelBreak) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + 
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

results$Location = factor(results$Location, levels = rev(listLOC))
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
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
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
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=FtimeLimit, by=1000/60), labels=labelRound, limits = c(0,  FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) + 
expand_limits(x=c(1000/60, 1000/30)) + 
scale_y_continuous(name="Count", expand=c(0.02, 0)) + theme(panel.spacing.x = unit(1, "lines"))

customSave("@Freq - Frame Time")


#QQ - Frame Time
message("QQ - Frame Time")

ggplot(results, aes(sample=MsBetweenPresents)) + 
ggtitle(paste0(gameQUA, " QQ Distribution"), subtitle="MsBetweenPresents") + 
geom_hline(yintercept = 1000/60, color = "red") + 
geom_point(stat="qq") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(0,  FtimeLimit), expand=c(0.02, 0)) + 
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

customSave("@QQ - Frame Time")
}


if (graphDISP) {
#Averages - Display Time
message("Averages - Display Time")
results$Location = factor(results$Location, levels = listLOC)

ggplot(data = results) + ggtitle(gameQUA, subtitle = "Averages, Medians, and Percentiles (MsBetweenDisplayChange)") + 
geom_hline(yintercept = 1000/60, color = "red") + 
# geom_boxplot(aes(x = GPU, y = MsBetweenDisplayChange), outlier.alpha = 0) + 
stat_summary(aes(x = GPU, y = MsBetweenDisplayChange), fun.data = BoxPerc, geom = "boxplot", width = 0.6) + 
geom_bar(aes(x = GPU, y = MsBetweenDisplayChange, fill = GPU), stat = "summary", fun.y = "mean") + 
stat_summary(aes(x = GPU, y = MsBetweenDisplayChange), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) + 
# geom_boxplot(aes(x = GPU, y = MsBetweenDisplayChange), alpha = 0.50, outlier.alpha = 0.1) + 
facet_grid(rows = vars(API), cols = vars(Location), switch = "y") + 
scale_x_discrete(labels = labelBreak) + 
scale_y_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + scale_x_discrete(labels = labelBreak) + 
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

results$Location = factor(results$Location, levels = rev(listLOC))

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
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
# geom_point(x=median(results$MsBetweenDisplayChange), y=median(diff(results$MsBetweenDisplayChange)), color = "magenta", shape ="x") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels = labelDisp, limits=c(0, 66.67), expand=c(0.02, 0)) + 
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

customSave("@Diff - Display Time")


#Frequency - Display Time
message("Diff - Display Time")

ggplot(results, aes(MsBetweenDisplayChange)) + 
ggtitle(paste0(gameQUA, " Frequency Plot of Display Times"), subtitle="MsBetweenDisplayChange") + 
geom_vline(xintercept = 1000/60, color = "red") + 
geom_freqpoly(binwidth=0.03, size=0) + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Refresh Cycles Later (1/60 s)", breaks=seq(from=0, to=FtimeLimit, by=1000/60), labels=labelDisp, limits = c(0,  FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) + 
expand_limits(x=c(1000/60, 1000/30)) + 
scale_y_continuous(name="Count", expand=c(0.02, 0))

customSave("@Freq - Display Time")


#QQ - Display Time
message("QQ - Display Time")

ggplot(results, aes(sample=MsBetweenDisplayChange)) + 
ggtitle(paste0(gameQUA, " QQ Distribution"), subtitle="MsBetweenDisplayChange") + 
geom_hline(yintercept = 1000/60, color = "red") + 
geom_point(stat="qq") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_y_continuous(name="Refresh Cycles Later (1/60 s)", breaks=c(0, round(1000/ytimes, 2)), labels=labelDisp, limits=c(0,  FtimeLimit), expand=c(0.02, 0)) + 
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

customSave("@QQ - Display Time")
}