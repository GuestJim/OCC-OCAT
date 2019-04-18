library(readr)
library(ggplot2)

game = "!GAME!"

showDisplay = FALSE
theme_set(theme_grey(base_size = 16))
DPI = 120

if (interactive()) {
	setwd("!PATH!")
} else {
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#	prevents rplots.pdf from being generated

results <- read_csv("@Combined - High.csv", col_types = "?????????????????ffff")

listGPU = c(
"RX 580",
"RX Vega 64",
"GTX 770",
"GTX 980",
"GTX 1070",
"GTX 1080",
"RTX 2060",
"RTX 2080"
)

listQUA = c(
"High"
)

listLOC = c(
""
)

listAPI = c(
!API!
)

results$GPU = factor(results$GPU, levels = listGPU)
results$Quality = factor(results$Quality, levels = listQUA)
results$Location = factor(results$Location, levels = listLOC)
results$API = factor(results$API, levels = listAPI, ordered = TRUE)

ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)

labelRound = function(x) sprintf("%.1f", x)
labelBreak = function(input) {
	return(paste0(rep(c("", "\n"), length.out = length(input)), input))
}

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


if (length(levels(results$API)) >= 2) {
	dataMEAN = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "API", "V"))
	dataPERC = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "API", "V"))
	dataECDF = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "API", "V"))
	# names(dataMEAN) = c("GPU", "Location", "API", "")
	# names(dataPERC) = c("GPU", "Location", "API", "")
	# names(dataECDF) = c("GPU", "Location", "API", "")
	if (showDisplay){
		dispMEAN = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "API", "V"))
		dispPERC = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "API", "V"))
		dispECDF = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "API", "V"))	
	}
} else {
	dataMEAN = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "V"))
	dataPERC = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "V"))
	dataECDF = sepCOL(aggregate(results$MsBetweenPresents, list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "V"))
	# names(dataMEAN) = c("GPU", "Location", "")
	# names(dataPERC) = c("GPU", "Location", "")
	# names(dataECDF) = c("GPU", "Location", "")
	if (showDisplay){
		dispMEAN = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), meanFPS), c("GPU", "Average", "V"))
		dispPERC = sepCOL(aggregate(results$MsBetweenDisplayChange, list(results$GPU, results$Location), percFPS), c("GPU", "Percentile", "V"))
		dispECDF = sepCOL(aggregate(results$MsBetweenDisplayChange,  list(results$GPU, results$Location), ecdfFPS), c("GPU", "FPS Percentile", "V"))
	}
}

options(width = 1000)
sink(paste0(game, " - ", QUA, " Data.txt"), split = TRUE)
writeLines(game)
writeLines("Frame Time")
writeLines("\nMean")
print(dataMEAN, row.names = FALSE)
writeLines("\nPercentiles")
print(dataPERC, row.names = FALSE)
writeLines("\nPercentile of FPS")
print(dataECDF, row.names = FALSE)
sink()

for (GPU in listGPU) {
	options(width = 1000)
	sink(paste0(GPU, "\\", game, " - ", GPU, " - ", QUA, " Data.txt"), split = TRUE)
		writeLines(game)
		writeLines("Frame Time")
		writeLines("\nMean")
		print(dataMEAN[dataMEAN$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentiles")
		print(dataPERC[dataPERC$GPU==GPU,], row.names = FALSE)
		writeLines("\nPercentile of FPS")
		print(dataECDF[dataECDF$GPU==GPU,], row.names = FALSE)
	sink()
}

if (showDisplay){
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
	
	for (GPU in listGPU) {
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
	}
}

library(tableHTML)
OCCHTML = function(tab) {
	tableHTML(tab[-1], rownames = FALSE, class="OCC") %>% 
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}

write_tableHTML(OCCHTML(dataMEAN), file = paste0(game, " - dataMEAN.html"))
write_tableHTML(OCCHTML(dataPERC), file = paste0(game, " - dataPERC.html"))
write_tableHTML(OCCHTML(dataECDF), file = paste0(game, " - dataECDF.html"))

for (GPU in listGPU) {
	write_tableHTML(OCCHTML(dataMEAN[dataMEAN$GPU==GPU,]), file = paste0(GPU, "\\", game, " - ", GPU, " - dataMEAN.html"))
	write_tableHTML(OCCHTML(dataPERC[dataPERC$GPU==GPU,]), file = paste0(GPU, "\\", game, " - ", GPU, " - dataPERC.html"))
	write_tableHTML(OCCHTML(dataECDF[dataECDF$GPU==GPU,]), file = paste0(GPU, "\\", game, " - ", GPU, " - dataECDF.html"))
}

if (showDisplay){
	write_tableHTML(OCCHTML(dispMEAN), file = paste0(game, " - dispMEAN.html"))
	write_tableHTML(OCCHTML(dispPERC), file = paste0(game, " - dispPERC.html"))
	write_tableHTML(OCCHTML(dispECDF), file = paste0(game, " - dispECDF.html"))
	
	for (GPU in listGPU) {
		write_tableHTML(OCCHTML(dispMEAN[dispMEAN$GPU==GPU,]), file = paste0(GPU, "\\", game, " - ", GPU, " - dispMEAN.html"))
		write_tableHTML(OCCHTML(dispPERC[dispPERC$GPU==GPU,]), file = paste0(GPU, "\\", game, " - ", GPU, " - dispPERC.html"))
		write_tableHTML(OCCHTML(dispECDF[dispECDF$GPU==GPU,]), file = paste0(GPU, "\\", game, " - ", GPU, " - dispECDF.html"))
	}
}

#Averages
ggplot(data = results) + ggtitle(paste0(game, " - High Quality"), subtitle = "Averages, Medians, and Percentiles") + 
geom_hline(yintercept = 1000/60, color = "red") + 
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), outlier.alpha = 0) + 
stat_summary(aes(x = GPU, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", width = 0.6) + 
geom_bar(aes(x = GPU, y = MsBetweenPresents, fill = GPU), stat = "summary", fun.y = "mean") + 
stat_summary(aes(x = GPU, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) + 
# geom_boxplot(aes(x = GPU, y = MsBetweenPresents), alpha = 0.50, outlier.alpha = 0.1) + 
facet_grid(rows = vars(API), cols = vars(Location), switch = "y") + 
scale_x_discrete(labels = labelBreak) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + 
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

ggsave(paste0(game, " - Averages.png"), width = 16, height = 9, dpi = DPI)

#Course - Frame Time
ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenPresents)) + 
ggtitle(paste0(game, " - High Quality"), subtitle = "MsBetweenPresent") + 
geom_hline(yintercept = 1000/60, color = "red") + 
geom_point(alpha = 0.05) + 
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), labels = labelBreak, expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + 
guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

ggsave(paste0(game, " - Course Frame Time.png"), width = 16, height = 9, dpi = DPI)

#Course - Display Time
if (showDisplay) {
ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenDisplayChange)) + 
ggtitle(paste0(game, " - High Quality"), subtitle = "MsBetweenDisplayChange") + 
geom_hline(yintercept = 1000/60, color = "red") + 
geom_point(alpha = 0.05) + 
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), labels = labelBreak, expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + 
guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

ggsave(paste0(game, " - Course Display Time.png"), width = 16, height = 9, dpi = DPI)
}


#Plotting Frame Time and Diff
ggplot(data=results, aes(x=results$MsBetweenPresents, y=rbind(c(diff(results$MsBetweenPresents), 0))[1,])) + 
ggtitle(paste0(game, " - High Quality"), subtitle="MsBetweenPresent consecutive differences") + 
geom_point(alpha = 0.1) + 
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
# geom_point(x=median(results$MsBetweenPresents), y=median(diff(results$MsBetweenPresents)), color = "magenta", shape ="x") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels = labelBreak, limits=c(NA,66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + 
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

ggsave(paste0(game, " - Diff Frame Time.png"), width = 16, height = 9, dpi = DPI)


#Plotting Display Time and Diff
if (showDisplay) {
ggplot(data=results, aes(x=results$MsBetweenDisplayChange, y=rbind(c(diff(results$MsBetweenDisplayChange), 0))[1,])) + 
ggtitle(paste0(game, " - High Quality"), subtitle="MsBetweenDisplayChange consecutive differences") + 
geom_point(alpha = 0.1) + 
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
# geom_point(x=median(results$MsBetweenDisplayChange), y=median(diff(results$MsBetweenDisplayChange)), color = "magenta", shape ="x") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels = labelBreak, limits=c(NA,66.67), expand=c(0.02, 0), sec.axis = dup_axis()) + 
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

ggsave(paste0(game, " - Diff Display Time.png"), width = 16, height = 9, dpi = DPI)
}


#Frequency Plot - Frame Time
ggplot(results, aes(MsBetweenPresents)) + 
ggtitle(paste0(game, " - High Quality"), subtitle="Frequency Plot of MsBetweenPresents") + 
geom_vline(xintercept = 1000/60, color = "red") + 
geom_freqpoly(binwidth=0.03, size=0) + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=FtimeLimit, by=1000/60), labels=labelRound, limits = c(NA, FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) + 
expand_limits(x=c(1000/60, 1000/30)) + 
scale_y_continuous(name="Count", expand=c(0.02, 0))

ggsave(paste0(game, " - Freq Frame.png"), width = 16, height = 9, dpi = DPI)


#QQ Plot - Frame Time
ggplot(results, aes(sample=MsBetweenPresents)) + 
ggtitle(paste0(game, " - High Quality"), subtitle="QQ Plot of MsBetweenPresents") + 
geom_hline(yintercept = 1000/60, color = "red") + 
geom_point(stat="qq") + 
facet_grid(cols = vars(GPU), rows = vars(Location, API), switch = "y") + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(NA, FtimeLimit), expand=c(0.02, 0)) + 
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "\n1", "50 (Median)", "\n99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

ggsave(paste0(game, " - QQ Frame.png"), width = 16, height = 9, dpi = DPI)
