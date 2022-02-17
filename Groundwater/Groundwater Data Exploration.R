library(ggplot2)
library(dplyr)
setwd("/Users/shaelanoble/Documents/Desktop/Documents/UC Davis/EPM Program/ESP 106/Final Project")
kern_casgem <- read.csv("Kern_CASGEM_WellElevationData.csv")
th <- theme_bw(legend.position=none)
a <- ggplot(kern_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode)) + geom_line() + theme(legend.position = "none") + ylim(0, 300)
a
test <- subset(kern_casgem, MasterSiteCode == "344779N1192479W001")
test$RPtoWS <- test$RPtoWS*(-1)
test$Date <- as.Date(test$Date, format = "%m/%d/%Y")
b <- ggplot(test, aes(x=Date, y=RPtoWS, col=MasterSiteCode, group = 1)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
b