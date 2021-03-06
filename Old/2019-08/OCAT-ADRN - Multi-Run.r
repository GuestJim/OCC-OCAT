library(readr)
library(ggplot2)
library(gridExtra)

if (interactive()) { 
	setwd("!PATH!")
} else {
	pdf(NULL) #prevents rplots.pdf from being generated
}

game = character(0)
game = "Combined Results - !FILE!"

gameF = gsub(":", "-", game)

titled = FALSE
graphs = FALSE

if(titled) {
	recording = "!NAME!"
	recordnam = paste(" - (", recording, ")",sep="")
	setname = paste(" - \n", game, recordnam, sep = "")
} else {
	recording = character(0)
	setname = character(0)
}

DPI = 120
ggscale = 1

theme_set(theme_grey(base_size = 16))

fpslim = 200
fpsstep = 10

FRTC = seq(from = 60, to = fpslim + fpsstep / 2, by = fpsstep)

RUNlist = c(as.character(FRTC), "Default")

yclk = seq(from = 400, to = 1600, by = 300)

yfps = c(seq(from = 30, to = 240, by = 30))
xfps = c(30, FRTC, seq(from = max(FRTC), to = 300, by = 30))

pstates = c(852, 991, 1084, 1138, 1200, 1401, 1536, 1630)
plabels = c("P0 - 852", "P1 - 991", "P2 - 1084", "P3 - 1138", "P4 - 1200", "P5 - 1401", "P6 - 1536", "P7 - 1630")

mstates = c(167, 500, 800, 945)
mlabels = c("P0 - 167", "P1 - 500", "P2 - 800", "P3 - 945")

ytimes = c(120, 60, 30, 20, 15, 12, 10)
ytimes = c(ytimes,-ytimes)

labelRound = function(x) sprintf("%.2f", x)

results = data.frame(matrix(ncol = 6, nrow = 0))

for (run in RUNlist) {
	comreadOCAT = paste("OCAT", run, " <- read_csv(\"", run, " - OCAT.csv\")", sep = "")
	comreadADRN = paste("ADRN", run, " <- read_csv(\"", run, " - Adrenalin.csv\")", sep = "")
	if (file.exists(paste(run, " - OCAT.csv", sep = ""))) {
		eval(parse(text = comreadOCAT))
		eval(parse(text = comreadADRN))
	} else {
		comreadOCAT = paste("OCAT", run, " = data.frame(matrix(ncol = 18, nrow = 0))", sep = "")
		comreadADRN = paste("ADRN", run, " = data.frame(matrix(ncol = 18, nrow = 0))", sep = "")
		eval(parse(text = comreadOCAT))
		eval(parse(text = comreadADRN))
	}

	comreadRESULTS = paste("colnames(ADRN", run, ") = gsub(' ', '', colnames(ADRN", run, "))", sep = "")
	eval(parse(text = comreadRESULTS))
	#	handles the ADRN column names having spaces
	
	comreadRESULTS = paste("results = rbind(results, c('", run, "', sum(ADRN", run, "$GPUPWR), mean(ADRN", run, "$GPUPWR), mean(ADRN", run, "$GPUSCLK), mean(ADRN", run, "$GPUFAN), 1000/mean(OCAT", run, "$MsBetweenPresents)), stringsAsFactors = FALSE)", sep = "")	
	eval(parse(text = comreadRESULTS))
}
colnames(results) = c("RUN", "PWRSUM", "PWRAVG", "GPUCLK", "GPUFAN", "FPSAVG")
results[,] = lapply(results, as.character)
results[, 2:6] = lapply(results[, 2:6], as.numeric)
results[,2:6] = round(results[, 2:6], 3)
write_csv(results, path = "@Combined.csv")
#	reads in all of the OCAT CSVs
#	also generates the data for @Combined.csv

OCAT = data.frame(matrix(ncol = 17, nrow = 0))
ADRN = data.frame(matrix(ncol = 9, nrow = 0))

for (run in RUNlist) {
	if (file.exists(paste(run, " - OCAT.csv", sep = ""))) {	
		commandOCAT = paste("OCAT", run, "$RUN <- rep('", run, "', nrow(OCAT", run, "))", sep = "")
		commandADRN = paste("ADRN", run, "$RUN <- rep('", run, "', nrow(ADRN", run, "))", sep = "")
		eval(parse(text = commandOCAT))
		eval(parse(text = commandADRN))
		#	adds the RUN column
		
		commandADRN = paste("ADRN", run, "$TIME <- seq(from = 1, to = nrow(ADRN", run, "))", sep = "")
		eval(parse(text = commandADRN))
		#	adds the TIME column	
		
		commandOCAT = paste("OCAT = rbind(OCAT, OCAT", run, ")", sep = "")
		commandADRN = paste("ADRN = rbind(ADRN, ADRN", run, ")", sep = "")
		eval(parse(text = commandOCAT))
		eval(parse(text = commandADRN))
		#	Creates the single OCAT and ADRN frames, based on RUNlist
	} else {
		comreadOCAT = paste("OCAT", run, " = data.frame(matrix(ncol = 17, nrow = 0))", sep = "")
		comreadADRN = paste("ADRN", run, " = data.frame(matrix(ncol = 17, nrow = 0))", sep = "")
		eval(parse(text = comreadOCAT))
		eval(parse(text = comreadADRN))
	}
}

for (run in RUNlist){
	OCAT[nrow(OCAT) + 1, ] <- NA
	OCAT$RUN[nrow(OCAT)] <- run
	ADRN[nrow(ADRN) + 1, ] <- NA
	ADRN$RUN[nrow(ADRN)] <- run
}
#	adds empty rows for all runs

OCAT$RUN = factor(OCAT$RUN, levels = RUNlist)
ADRN$RUN = factor(ADRN$RUN, levels = RUNlist)
#	sets grouping levels for use later

colnames(ADRN) = gsub(" ", "", colnames(ADRN))
#	Removes spaces from column names

FRTCOnly = results
FRTCOnly$RUN = as.character(FRTCOnly$RUN)
FRTCOnly[FRTCOnly == "Default"] <- fpslim + fpsstep
FRTCOnly$RUN = as.numeric(FRTCOnly$RUN)
#	replaces Default with a step up, allowing it to be graphed

#FRTCOnly = results[!(results$RUN == "Default"), ]
#FRTCOnly$RUN = as.numeric(as.character(FRTCOnly$RUN))
#	removes the Default run

pshape = 10
psize = 5
#	size and shape for the average points in some plots

#Clock Statistics, seeing how often it was at specific values
if (TRUE) {
runs = vector("character")

HBM167 = vector("double")
HBM500 = vector("double")
HBM800 = vector("double")
HBM945 = vector("double")

P0 = vector("double")
P1 = vector("double")
P2 = vector("double")
P3 = vector("double")
P4 = vector("double")
P5 = vector("double")
P6 = vector("double")
P7 = vector("double")

for (run in RUNlist) {
	if (file.exists(paste(run, " - OCAT.csv", sep = ""))) {
		runs = c(runs, run)
		
		HBM167 = c(HBM167, ecdf(ADRN[ADRN$RUN == run, ]$GPUMCLK)(167))
		HBM500 = c(HBM500, ecdf(ADRN[ADRN$RUN == run, ]$GPUMCLK)(500))
		HBM800 = c(HBM800, ecdf(ADRN[ADRN$RUN == run, ]$GPUMCLK)(800))
		HBM945 = c(HBM945, ecdf(ADRN[ADRN$RUN == run, ]$GPUMCLK)(945))
		
		P0 = c(P0, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(852))
		P1 = c(P1, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(991))
		P2 = c(P2, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(1084))
		P3 = c(P3, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(1138))
		P4 = c(P4, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(1200))
		P5 = c(P5, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(1401))
		P6 = c(P6, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(1536))
		P7 = c(P7, ecdf(ADRN[ADRN$RUN == run, ]$GPUSCLK)(1630))
	}
}

HBM0 = rep(0, length(runs))
#	to keep the HBM167 row
HBM = data.frame(HBM0, HBM167, HBM500, HBM800, HBM945, row.names = runs)
#	because these speeds are set, I do not need to worry about the domain
colnames(HBM) = c(0, "P0 - 167", "P1 - 500", "P2 - 800", "P3 - 945")
HBM = t(HBM * 100)

P11 = rep(1, length(runs))
#	to keep the P0 row
GPU = data.frame(P0, P1, P2, P3, P4, P5, P6, P7, P11, row.names = runs)
#	I want the P11 there so it is the P-state value and above for the domain
colnames(GPU) = c(0, plabels)
GPU = t(GPU * 100)

options(width = 10000)
#	to prevent line wrapping

sink("Clocks.txt", split=TRUE)
writeLines("GPU - Time Spent at P-States")
print(diff(GPU))
writeLines("\n")
writeLines("HBM - Time Spent at P-States")
print(diff(HBM))
sink()
}

#png("Clocks.png", width = 800, height = 400)
pdf("Clocks.pdf", width = 10.5, height = 5.5)
grid.arrange(
	tableGrob(settext),
	tableGrob("GPU - Percentage Spent at P-states"),
	tableGrob(round(diff(GPU), 2)),
	tableGrob("HBM - Percentage Spent at P-states"),
	tableGrob(round(diff(HBM), 2)),
	ncol = 1, heights = c(0.5, 0.5, 3, 0.5, 2)
)
dev.off()

options(error=expression(NULL))

pdf(NULL) #prevents rplots.pdf from being generated

#Power Average
if (TRUE){
scaleFP = 1
ggplot(na.rm = TRUE) + 
ggtitle(paste("Comparison of ASIC Average Power", setname, sep = ""), subtitle="Watts")+
scale_fill_gradient2("Power (W)", low="blue", mid = "green", midpoint = 225,  high="red", limits = c(50, 400), breaks = c(75,150,225,300,375)) + 
geom_boxplot(data = OCAT, outlier.shape = NA, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFP)) + 
geom_col(data = results, aes(x = RUN, y = PWRAVG, fill=PWRAVG)) + 
geom_boxplot(data = OCAT, outlier.shape = NA, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFP), alpha = 0.25) + 
#geom_boxplot(data = OCAT, outlier.size = 0.25, outlier.alpha = .2, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFP)) + 
geom_label(data = results, aes(label = PWRAVG, x = RUN, y = 0), vjust=-0.25) + 
scale_y_continuous(name="Average ASIC Power (W)", breaks=c(75,150,225,300,375), limits = c(NA,400), expand = c(0.02, 0)
, sec.axis = sec_axis(~., name = "Frame Rate (FPS)", breaks = (yfps * scaleFP), labels = yfps)) + 
scale_x_discrete(name = "Frame Rate Target", limits = RUNlist) + 
geom_point(data = results, aes(x = RUN, y = FPSAVG * scaleFP), color = "black", shape = pshape, size = psize) +
theme(legend.position = c(1, 0.9))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Power.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Power.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#QQ Plots of ASIC Power
if (TRUE){
ggplot(na.rm = TRUE, data = ADRN) + 
ggtitle(paste("QQ Plot of ASIC Power", setname, sep = "")) + 
stat_qq(aes(sample = GPUPWR, group = RUN, color = RUN)) + 
scale_y_continuous(name="ASIC Power (W)", breaks = c(0, 75, 150, 225, 300), limits = c(0, 225)) + 
scale_x_continuous(name="Percentile", breaks=qnorm(c(.01, .25, .5, .75, .99)), labels=c("1", "25", "50", "75", "99"), minor_breaks=NULL, expand=c(0.02, 0))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - QQ Power.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - QQ Power.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Power through Course
if (TRUE){
ggplot(na.rm = TRUE) + 
ggtitle(paste("ASIC Power Usage Through Course", setname, sep = ""), subtitle="Watts") +
scale_fill_gradient2(name = "Power (W)", low="blue", mid = "green", midpoint = 225,  high="red", limits = c(50, 400), breaks = c(75,150,225,300,375)) + 
geom_col(data = ADRN, aes(x = TIME, y = GPUPWR, group=RUN, fill = GPUPWR)) + 
facet_grid(RUN ~., as.table = FALSE, drop = FALSE) + 
scale_x_continuous(name = "Time", breaks = seq(from = 0, to = 300, by = 60), expand=c(0.02, 0)) + 
scale_y_continuous(name="ASIC Power (W)", breaks=c(75,150,225,300,375), limits = c(NA,250), expand = c(0.01, 0), minor_breaks = NULL) + 
theme(panel.spacing=unit(.1, "lines"), panel.border = element_rect(color = "blue", fill = NA, size = 1), strip.background = element_rect(color = "black", size = 1))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Power.pdf", sep=""), device="pdf", width=8, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Power.png", sep=""), device="png", width=8, height=9, dpi=DPI, scale = ggscale)
	}
}

#Clock Speed Average
if (TRUE){
scaleFC = 5

ggplot(na.rm = TRUE) + 
ggtitle(paste("Comparison of Clock Speed", setname, sep = ""), subtitle="Average MHz") +
scale_fill_gradient2(name = "MHz", low = "blue", mid = "green", midpoint = 1200,  high="red", limits = c(800, 1600), breaks = yclk) + 
geom_hline(yintercept = pstates, color = "blue") + 
geom_label(aes(label = plabels, x = "P-States", y = pstates), hjust = 0.5, size = 2.75) + 
#geom_boxplot(data = OCAT, outlier.shape = NA, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFC)) + 
geom_col(data = results, aes(x = RUN, y = GPUCLK, fill=GPUCLK)) + 
geom_boxplot(data = ADRN, outlier.shape = NA, aes(x = RUN, y = GPUSCLK), fill = NA) + 
geom_boxplot(data = OCAT, outlier.shape = NA, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFC), alpha = 0.25) + 
#geom_boxplot(data = OCAT, outlier.size = 0.25, outlier.alpha = .2, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFC)) + theme(legend.position = c(1, 0.9))
geom_label(data = results, aes(label = GPUCLK, x = RUN, y = 0), vjust=-0.25) + 
scale_y_continuous(name="Average GPU Clock Speed (MHz)", breaks = yclk, labels = yclk, limits=c(NA,1900), expand=c(0.02, 0)
, sec.axis = sec_axis(~., name = "Frame Rate (FPS)", breaks = (yfps * scaleFC), labels = yfps)) + 
scale_x_discrete(name = "Frame Rate Target", limits = c("P-States", RUNlist)) + 
geom_point(data = results, aes(x = RUN, y = FPSAVG * scaleFC), color = "black", shape = pshape, size = psize) +
theme(legend.position = c(1, 0.9))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Clock Speed.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Clock Speed.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#QQ Plots of Core Clock speed
if (TRUE){
ggplot(na.rm = TRUE, data = ADRN) + 
ggtitle(paste("QQ Plot of Clock Speeds", setname, sep = "")) + 
stat_qq(aes(sample = GPUSCLK, group = RUN, color = RUN)) + 
scale_y_continuous(name="GPU Clock Speed (MHz)", breaks = pstates, labels = plabels, limits=c(NA,1900), expand=c(0.02, 0), minor_breaks = NULL) + coord_cartesian(ylim = c(800, 1700)) + 
scale_x_continuous(name="Percentile", breaks=qnorm(c(.01, .25, .5, .75, .99)), labels=c("1", "25", "50 (Median)", "75", "99"), minor_breaks=NULL, expand=c(0.02, 0))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - QQ Clock.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - QQ Clock.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Clock through Course
if (TRUE){
ggplot(na.rm = TRUE) + 
ggtitle(paste("Clock Speed Through Course", setname, sep = ""), subtitle="MHz") +
scale_fill_gradient2(name = "MHz", low = "blue", mid = "green", midpoint = 1200,  high="red", limits = c(800, 1600), breaks = pstates) + 
geom_hline(yintercept = pstates, color = "blue", alpha = 0.3) + 
geom_col(data = ADRN, aes(x = TIME, y = GPUSCLK, group=RUN, fill = GPUSCLK)) + 
facet_grid(RUN ~., as.table = FALSE, drop = FALSE) + 
scale_x_continuous(name = "Time", breaks = seq(from = 0, to = 300, by = 60), expand=c(0.01, 0)) + 
scale_y_continuous(name="GPU Clock Speed (MHz)", breaks = yclk, limits=c(NA,1900), expand=c(0.02, 0), minor_breaks = NULL) + coord_cartesian(ylim = c(800, 1700)) + 
theme(panel.spacing=unit(.1, "lines"), panel.border = element_rect(color = "blue", fill = NA, size = 1), strip.background = element_rect(color = "black", size = 1)) + 
theme(legend.key.height = unit(2, "line"))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Clock.pdf", sep=""), device="pdf", width=8, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Clock.png", sep=""), device="png", width=8, height=9, dpi=DPI, scale = ggscale)
	}
}

#Memory through Course
if (TRUE){
ggplot(na.rm = TRUE) + 
ggtitle(paste("HBM Through Course", setname, sep = ""), subtitle="MHz") +
scale_fill_gradientn("HBM MHz", colors = c("blue", "yellow", "red"), breaks = c(500, 800, 945), guide = "legend") + 
geom_col(data = ADRN, aes(x = TIME, y = GPUMCLK, group=RUN, fill = GPUMCLK)) + 
facet_grid(RUN ~., as.table = FALSE, drop = FALSE) + 
scale_x_continuous(name = "Time", breaks = seq(from = 0, to = 300, by = 60), expand=c(0.01, 0)) + 
scale_y_continuous(name="HBM Speed (MHz)", breaks = mstates, labels=mlabels, limits=c(0, 965), expand=c(0.02, 0)) + coord_cartesian(ylim = c(400, 965)) + 
theme(panel.spacing=unit(.1, "lines"), panel.border = element_rect(color = "blue", fill = NA, size = 1), strip.background = element_rect(color = "black", size = 1)) + 
theme(legend.key.height = unit(2, "line"))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - HBM.pdf", sep=""), device="pdf", width=8, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - HBM.png", sep=""), device="png", width=8, height=9, dpi=DPI, scale = ggscale)
	}
}

#Clock vs Power - Averages
if (TRUE){
lifts = c(0, 10, 20, 30, 40, 0, 0, 10, 0, 0, 0, 10, 20, 30, 40, 50)

ggplot(na.rm = TRUE) + 
ggtitle(paste("Clock Speed vs Power (Averages)", setname, sep = ""), subtitle = "MHz vs W") + 
scale_fill_gradient2("Power (W)", low="blue", mid = "green", midpoint = 225,  high="red", limits = c(50, 400), breaks = c(75,150,225,300,375)) +  
scale_color_gradient2("Power (W)", low="blue", mid = "green", midpoint = 225,  high="red", limits = c(50, 400), breaks = c(75,150,225,300,375)) + 
geom_vline(xintercept = pstates) + 
geom_col(data = results, aes(y = PWRAVG, x = GPUCLK, fill = PWRAVG), width = 2) + 
geom_line(data = results, aes(y = PWRAVG, x = GPUCLK, color = PWRAVG), linejoin = "round", size = 2) + 
geom_label(data = results, aes(label = RUN, x = GPUCLK, y = lifts)) + 
scale_y_continuous(name="Average ASIC Power (W)", breaks=c(75,150,225,300,375), limits = c(NA,250), expand = c(0.02, 0)) + 
scale_x_continuous(name = "GPU Clock (MHz)", breaks = seq(from = 1000, to = 1600, by = 50), limits = c(1050, 1700), sec.axis = sec_axis(~., breaks = pstates, labels = plabels)) + 
theme(legend.position = c(0.97, 0.9))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - CLK vs PWR Averages.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - CLK vs PWR Averages.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Clock vs Power
if (TRUE){
ggplot(data = ADRN, aes(x = GPUSCLK, y = GPUPWR, group = RUN, color = RUN, fill = RUN)) + 
ggtitle(paste("Clock vs Power", setname, sep = ""), subtitle = "MHz vs W") + 
scale_y_continuous(name="ASIC Power (W)", breaks=c(0, 75, 150, 225, 300, 375), limits = c(0, 250), expand = c(0.02, 0)) + 
scale_x_continuous(name = "GPU Clock (MHz)", breaks = seq(from = 1000, to = 1600, by = 50), limits = c(1050, 1700), sec.axis = sec_axis(~., breaks = pstates, labels = plabels)) + 
geom_vline(xintercept = pstates) + 
geom_rug() + 
geom_hex(binwidth = c(3.457778, 1) * 3) + 
geom_smooth(aes(group = NA), method = 'glm', formula = y ~ poly(x ^ 1, 5, raw = TRUE))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - CLK vs PWR.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - CLK vs PWR.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Fan Speed Average
if (TRUE){
scaleFF = 7.5

ggplot(na.rm = TRUE) + 
ggtitle(paste("Comparison of Fan Speed", setname, sep = ""), subtitle="Average RPM") +
scale_fill_gradient2("RPM", low="blue", mid = "green", midpoint = 1400,  high="red", limits = c(300, 2500), breaks = c(400,900,1400,1900,2400)) + 
geom_boxplot(data = OCAT, outlier.shape = NA, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFF)) + 
geom_col(data = results, aes(x = RUN, y = GPUFAN, fill=GPUFAN)) + 
geom_boxplot(data = OCAT, outlier.shape = NA, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFF), alpha = 0.25) + 
#geom_boxplot(data = OCAT, outlier.size = 0.25, outlier.alpha = .2, aes(x = RUN, y = 1000 / MsBetweenPresents * scaleFF)) + theme(legend.position = c(1, 0.9))
geom_label(data = results, aes(label = GPUFAN, x = RUN, y = 0), vjust=-0.25) + 
scale_y_continuous(name="Rotations Per Minute (RPM)", breaks=c(400,900,1400,1900,2400), limits=c(NA,3000), expand=c(0.02, 0)
, sec.axis = sec_axis(~., name = "Frame Rate (FPS)", breaks = (yfps * scaleFF), labels = yfps)) + 
scale_x_discrete(name = "Frame Rate Target", limits = RUNlist) + 
geom_point(data = results, aes(x = RUN, y = FPSAVG * scaleFF), color = "black", shape = pshape, size = psize) +
theme(legend.position = c(1, 0.9))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Fan.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Fan.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Fan through Course
if (TRUE){
ggplot(na.rm = TRUE) + 
ggtitle(paste("Fan Speed Through Course", setname, sep = ""), subtitle="RPM") +
scale_fill_gradient2(name = "RPM", low="blue", mid = "green", midpoint = 1400,  high="red", limits = c(300, 3000), breaks = c(400, 900, 1400, 1900, 2400, 2900)) + 
geom_col(data = ADRN, aes(x = TIME, y = GPUFAN, group=RUN, fill = GPUFAN)) + 
facet_grid(RUN ~., as.table = FALSE, drop = FALSE) + 
scale_x_continuous(name = "Time", breaks = seq(from = 0, to = 300, by = 60), expand=c(0.02, 0), minor_breaks = NULL) + 
scale_y_continuous(name="Rotations Per Minute (RPM)", breaks=c(400,1400,2400), limits=c(NA,3000), expand=c(0.01, 0)) + 
theme(panel.spacing=unit(.1, "lines"), panel.border = element_rect(color = "blue", fill = NA, size = 1), strip.background = element_rect(color = "black", size = 1))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Fan.pdf", sep=""), device="pdf", width=8, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Fan.png", sep=""), device="png", width=8, height=9, dpi=DPI, scale = ggscale)
	}
}

#Frame Rate Distributions
if (TRUE){
ggplot(OCAT, na.rm = TRUE) + 
ggtitle(paste("Frequency Plot of Frame Rates", setname, sep = ""), subtitle="1000 * MsBetweenPresents^-1") +
geom_freqpoly(aes(1000 / MsBetweenPresents), binwidth=1) + 
scale_x_continuous(name="Frame Rate (FPS)", breaks = xfps, expand=c(0.02, 0), limits = c(30, 300)) + expand_limits(x=c(30, 60)) + scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Frame Rate Distributions - Grouped
if (TRUE){
ggplot(OCAT, na.rm = TRUE) + 
ggtitle(paste("Frequency Plot of Frame Rates", setname, sep = ""), subtitle="1000 * MsBetweenPresents^-1") +
geom_freqpoly(aes(1000 / MsBetweenPresents, group = RUN, color = RUN), binwidth=1, size=1) + 
scale_x_continuous(name="Frame Rate (FPS)", breaks = xfps, expand=c(0.02, 0), limits = c(30, 300)) + expand_limits(x=c(30, 60)) + scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Grouped.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Grouped.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#FPS Distribution with Clock
if (TRUE){
scaleFD = 5

ggplot(na.rm = TRUE) + 
ggtitle(paste("Clock Speed and Net Frame Rate Distribution", setname, sep = "")) +
scale_fill_gradient2(name = "MHz", low = "blue", mid = "green", midpoint = 1200,  high="red", limits = c(800, 1600), breaks = yclk) + 
geom_hline(yintercept = pstates, color = "blue") + 
geom_col(data = FRTCOnly, aes(x = RUN, y = GPUCLK, fill=GPUCLK)) + 
geom_freqpoly(data = OCAT, aes(1000 / MsBetweenPresents, y = ..count.. / 10), binwidth=1) + 
#	cannot use variable within aes, so must manually put in scaleFD value
geom_label(data = FRTCOnly, aes(label = GPUCLK, x = RUN, y = 0), vjust=-0.25) + 
scale_x_continuous(name="Frame Rate (FPS)", breaks = FRTCOnly$RUN, labels = RUNlist, expand=c(0.02, 0), limits = c(50, max(FRTCOnly$RUN) + fpsstep / 2)) + 
expand_limits(x=c(30, 60)) + 
scale_y_continuous(name="Average GPU Clock Speed (MHz)", breaks = yclk, labels = yclk, expand=c(0.02, 0), sec.axis = sec_axis(~., name = "P-States", breaks = pstates, labels = plabels)) + 
theme(legend.position = c(0.97, 0.9))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency Clock.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency Clock.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Frame Time Courses Faceted
if (TRUE){
ggplot(na.rm = TRUE) + 
ggtitle(paste("Frame Times Through Course", setname, sep = ""), subtitle="MsBetweenPresents") +
geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenPresents), color="black", alpha = 0.10) + 
scale_y_continuous(name="Frame Time (ms)", breaks=c(0, round(1000/ytimes, 2)), limits=c(NA,1000/40), expand=c(0.02, 0), minor_breaks = NULL) + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds, na.rm = TRUE), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
#geom_hline(yintercept = c(quantile(OCAT$MsBetweenPresents, c(.001, .01, .99, 0.999))), color="red") + 
facet_grid(RUN ~ ., as.table = FALSE, drop = FALSE) + 
theme(panel.spacing=unit(.1, "lines"), panel.border = element_rect(color = "blue", fill = NA, size = 1), strip.background = element_rect(color = "black", size = 1))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Frames.pdf", sep=""), device="pdf", width=8, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Frames.png", sep=""), device="png", width=8, height=9, dpi=DPI, scale = ggscale)
	}
}

#Display Rate Distributions
if (TRUE){
ggplot(OCAT, na.rm = TRUE) + 
ggtitle(paste("Frequency Plot of Display Rates", setname, sep = ""), subtitle="MsBetweenDisplayChange") +
geom_freqpoly(aes(MsBetweenDisplayChange * 60 / 1000), binwidth=0.003, size=1) + 
scale_x_continuous(name="Refresh Cycles Later (1 / 60 Hz)", breaks=seq(from=0, to=5, by=1), minor_breaks=NULL, limits=c(0, 1.1), expand=c(0.02, 0)) + expand_limits(x=c(0, 2)) + scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency - Display.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency - Display.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Display Rate Distributions - Grouped
if (TRUE){
ggplot(OCAT, na.rm = TRUE) + 
ggtitle(paste("Frequency Plot of Display Rates", setname, sep = ""), subtitle="MsBetweenDisplayChange") +
geom_freqpoly(aes(MsBetweenDisplayChange * 60 / 1000, group = RUN, color = RUN), binwidth=0.003, size=1) + 
scale_x_continuous(name="Refresh Cycles Later (1 / 60 Hz)", breaks=seq(from=0, to=5, by=1), minor_breaks=NULL, limits=c(0, 1.1), expand=c(0.02, 0)) + expand_limits(x=c(0, 2)) + scale_y_continuous(name="Count", expand=c(0.02, 0))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Grouped - Display.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Grouped - Display.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}

#Display Time Courses Faceted
if (TRUE){
ggplot(na.rm = TRUE) + 
ggtitle(paste("Display Times Through Course", setname, sep = ""), subtitle="MsBetweenDisplayChange") +
geom_point(data = OCAT, aes(x = TimeInSeconds, y = MsBetweenDisplayChange), color="black", alpha = 0.10) + 
scale_y_continuous(name="Cycles Later (1 / 60 Hz)", breaks=c(0, 1000/60, 1000/30), labels=c(0, 1, 2), limits=c(NA,1000/31), expand=c(0.02, 0), minor_breaks = NULL) + 
scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=signif(max(OCAT$TimeInSeconds, na.rm = TRUE), digits=1), by=60), expand=c(0.02, 0)) + expand_limits(y=c(0, 1000/30)) + 
#geom_hline(yintercept = c(quantile(OCAT$MsBetweenDisplayChange, c(.001, .01, .99, 0.999))), color="red") + 
facet_grid(RUN ~ ., as.table = FALSE, drop = FALSE) + 
theme(panel.spacing=unit(.1, "lines"), panel.border = element_rect(color = "blue", fill = NA, size = 1), strip.background = element_rect(color = "black", size = 1))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Display.pdf", sep=""), device="pdf", width=8, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - Course Facet - Display.png", sep=""), device="png", width=8, height=9, dpi=DPI, scale = ggscale)
	}
}

#Display Distribution with Clock
if (TRUE){
scaleFD = 20

ggplot(na.rm = TRUE) + 
ggtitle(paste("Clock Speed and Net Display Rate Distribution", setname, sep = "")) +
scale_fill_gradient2(name = "MHz", low = "blue", mid = "green", midpoint = 1200,  high="red", limits = c(800, 1600), breaks = yclk) + 
geom_hline(yintercept = pstates, color = "blue") + 
geom_col(data = FRTCOnly, aes(x = RUN, y = GPUCLK, fill=GPUCLK)) + 
geom_freqpoly(data = OCAT, aes(1000 / MsBetweenDisplayChange, y = ..count.. / 20), binwidth=1) + 
#	cannot use variable within aes, so must manually put in scaleFD value
geom_label(data = FRTCOnly, aes(label = GPUCLK, x = RUN, y = 0), vjust=-0.25) + 
scale_x_continuous(name="Frame Rate (FPS)", breaks = FRTCOnly$RUN, labels = RUNlist, expand=c(0.02, 0), limits = c(50, max(FRTCOnly$RUN) + fpsstep / 2)) + 
expand_limits(x=c(30, 60)) + 
scale_y_continuous(name="Average GPU Clock Speed (MHz)", breaks = yclk, labels = yclk, expand=c(0.02, 0), sec.axis = sec_axis(~., name = "P-States", breaks = pstates, labels = plabels)) + 
theme(legend.position = c(0.97, 0.9))

if (pdf) {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency Clock - Display.pdf", sep=""), device="pdf", width=16, height=9, dpi=DPI, scale = ggscale)
	} else {
		ggsave(filename=paste(gameF, " - ", setname, " - FPS Frequency Clock - Display.png", sep=""), device="png", width=16, height=9, dpi=DPI, scale = ggscale)
	}
}