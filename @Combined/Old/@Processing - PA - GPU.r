library(readr)
library(ggplot2)

game = "!GAME!"
fold = "!GPU!"
GPUname = "!GPU!"

showDisplay = FALSE
theme_set(theme_grey(base_size = 16))
DPI = 120

if (interactive()) {
	setwd(paste0("E:/Users/Jim/My Documents/OCC/@Reviews/@Performance Analyses/", game, " Performance Analysis/OCAT Data/", fold))
} else {
	pdf(NULL)
}
#	checks if the script is being run in the GUI or not
#	prevents rplots.pdf from being generated

results <- read_csv(paste0("@Combined - ", GPUname, ".csv"), col_types = "?????????????????ffff")

listLOC = c(
"",
"",
""
)

listQuality = c(
"",
"",
""
)

listAPI = c(
"",
)

results$GPU = factor(results$GPU, levels = GPUname)
results$Location = factor(results$Location, levels = listLOC)
results$Quality = factor(results$Quality, levels = listQuality, ordered = TRUE)
results$API = factor(results$API, levels = listAPI, ordered = TRUE)

ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)

labelRound = function(x) sprintf("%.2f", x)
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

if ("API" %in% colnames(results)) {
	results$APIf = sapply(results$API, as.character)
	results$APIf[is.na(results$APIf)] = "RTX Off"
	results$APIf = factor(results$APIf, levels = c("RTX Off", "RTX - High", "RTX - High DLSS", "RTX - Ultra"), ordered = TRUE)

	dataMEAN = aggregate(results$MsBetweenPresents, list(results$Location, results$Quality, results$APIf), meanBOT)
	dataPERC = aggregate(results$MsBetweenPresents, list(results$Location, results$Quality, results$APIf), percFPS)
	dataECDF = aggregate(results$MsBetweenPresents, list(results$Location, results$Quality, results$APIf), ecdfFPS)
	names(dataMEAN) = c("Location", "Quality", "API", "")
	names(dataPERC) = c("Location", "Quality", "API", "")
	names(dataECDF) = c("Location", "Quality", "API", "")
} else {
	dataMEAN = aggregate(results$MsBetweenPresents, list(results$Location, results$Quality), meanBOT)
	dataPERC = aggregate(results$MsBetweenPresents, list(results$Location, results$Quality), percFPS)
	dataECDF = aggregate(results$MsBetweenPresents, list(results$Location, results$Quality), ecdfFPS)
	names(dataMEAN) = c("Location", "Quality", "")
	names(dataPERC) = c("Location", "Quality", "")
	names(dataECDF) = c("Location", "Quality", "")
}

options(width = 1000)
sink(paste0(GPUname, " Data.txt"), split = TRUE)
writeLines(GPUname)
writeLines("\nMean")
print(dataMEAN)
writeLines("\nPercentiles")
print(dataPERC)
writeLines("\nPercentile of FPS")
print(dataECDF)
sink()


#Averages
ggplot(data = results) + ggtitle(paste0(game, " - ", GPUname), subtitle = "Averages, Medians, and Percentiles") + 
geom_hline(yintercept = 1000/60, color = "red") + 
stat_summary(aes(x = Quality, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", width = 0.6) + 
geom_bar(aes(x = Quality, y = MsBetweenPresents, fill = Quality), stat = "summary", fun.y = "mean") + 
stat_summary(aes(x = Quality, y = MsBetweenPresents), fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) + 
facet_grid(cols = vars(Location), rows = vars(API), switch = "y") + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA, FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) + 
guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

ggsave(paste0(game, " - ", GPUname, " - Averages.png"), width = 16, height = 12, dpi = DPI)

#Course - Frame Time
ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenPresents)) + 
ggtitle(paste0(game, " - ", GPUname), subtitle = "MsBetweenPresent") + 
geom_hline(yintercept = 1000/60, color = "red") + 
geom_point(alpha = 0.05) + 
geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
facet_grid(cols = vars(Location), rows = vars(API, Quality), switch = "y") + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA, FtimeLimit), expand=c(0.02, 0)) + 
guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

ggsave(paste0(game, " - ", GPUname, " - Course Frame Time.png"), width = 9, height = 16, dpi = DPI)

#Course - Display Time
if (showDisplay) {
	ggplot(data = results, aes(x = TimeInSeconds, y = MsBetweenDisplayChange)) + 
	ggtitle(paste0(game, " - ", GPUname), subtitle = "MsBetweenDisplayChange") + 
	geom_hline(yintercept = 1000/60, color = "red") + 
	geom_point(alpha = 0.05) + 
	geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")) + 
	facet_grid(cols = vars(Location), rows = vars(API, Quality), switch = "y") + 
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(results$TimeInSeconds), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
	scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA, FtimeLimit), expand=c(0.02, 0)) + 
	guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")

	ggsave(paste0(game, " - ", GPUname, " - Course Display Time.png"), width = 9, height = 16, dpi = DPI)
}

#Plotting Frame Time and Diff
ggplot(data=results, aes(x=results$MsBetweenPresents, y=rbind(c(diff(results$MsBetweenPresents), 0))[1,])) + 
ggtitle(paste0(game, " - ", GPUname), subtitle="MsBetweenPresent consecutive differences") + 
geom_point(alpha = 0.1) + 
stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
facet_grid(cols = vars(Location), rows = vars(API, Quality), switch = "y") + 
scale_x_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA, FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) + 
scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

ggsave(paste0(game, " - ", GPUname, " - Diff Frame Time.png"), width = 9, height = 16, dpi = DPI)


#Plotting Display Time and Diff
if (showDisplay) {
	ggplot(data=results, aes(x=results$MsBetweenDisplayChange, y=rbind(c(diff(results$MsBetweenDisplayChange), 0))[1,])) + 
	ggtitle(paste0(game, " - ", GPUname), subtitle="MsBetweenDisplayChange consecutive differences") + 
	geom_point(alpha = 0.1) + 
	stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() + 
	facet_grid(cols = vars(Location), rows = vars(API, Quality), switch = "y") + 
	scale_x_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA, FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) + 
	scale_y_continuous(name="Consecutive Frame Time Difference (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(-1000/50, 1000/50), expand=c(0, 0))

	ggsave(paste0(game, " - ", GPUname, " - Diff Display Time.png"), width = 9, height = 16, dpi = DPI)
}

#Frequency Plot - Frame Time
ggplot(results, aes(MsBetweenPresents)) + 
ggtitle(paste0(game, " - ", GPUname), subtitle="Frequency of Frame Times (MsBetweenPresents)") + 
geom_vline(xintercept = 1000/60, color = "red") + 
geom_freqpoly(binwidth=0.03, size=0) + 
facet_grid(cols = vars(Location), rows = vars(API, Quality), switch = "y") + 
scale_x_continuous(name="Frame Time (ms)", breaks=seq(from=0, to=FtimeLimit, by=1000/120), labels=labelRound, limits = c(NA, FtimeLimit), expand=c(0.02, 0), sec.axis = dup_axis()) + 
expand_limits(x=c(1000/60, 1000/30)) + 
scale_y_continuous(name="Count", expand=c(0.02, 0))

ggsave(paste0(game, " - ", GPUname, " - Freq Frame.png"), width = 9, height = 16, dpi = DPI)

#QQ Plot - Frame Time
ggplot(results, aes(sample=MsBetweenPresents)) + 
ggtitle(paste0(game, " - ", GPUname), subtitle="QQ Distribution (MsBetweenPresents)") + 
geom_hline(yintercept = 1000/60, color = "red") + 
geom_point(stat="qq") +
facet_grid(cols = vars(Location), rows = vars(API, Quality), switch = "y") + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), labels=labelRound, limits=c(NA, FtimeLimit), expand=c(0.02, 0)) + 
scale_x_continuous(name="Percentile", breaks=qnorm(c(.001, .01, .5, .99, .999)), labels=c("0.1", "1", "50 (Median)", "99", "99.9"), minor_breaks=NULL, expand=c(0.02, 0))

ggsave(paste0(game, " - ", GPUname, " - QQ Frame.png"), width = 9, height = 16, dpi = DPI)