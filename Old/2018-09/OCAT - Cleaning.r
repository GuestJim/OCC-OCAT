library(readr)
setwd("!PATH!")
results <- read_csv("!FILEX!")

ColClear = c("Motherboard","OS","Processor","System RAM","Base Driver Version","Driver Package","GPU #","GPU","GPU Core Clock (MHz)","GPU Memory Clock (MHz)","GPU Memory (MB)","ProcessID","SwapChainAddress","SyncInterval","PresentFlags","WasBatched","DwmNotified")

results = results[ , -which(names(results) %in% ColClear)]

write_csv(results, "!FILEX!")