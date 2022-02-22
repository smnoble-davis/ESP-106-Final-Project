library(ggplot2)
library(dplyr)
setwd("/Users/shaelanoble/Documents/GitHub/ESP-106-Final-Project/Groundwater")
kern_casgem <- read.csv("Kern_CASGEM_WellElevationData.csv")
th <- theme_bw(legend.position="none")
a <- ggplot(kern_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode)) + geom_line() + theme(legend.position = "none") + ylim(0, 300)
a
test <- subset(kern_casgem, MasterSiteCode == "344779N1192479W001")
test$RPtoWS <- test$RPtoWS*(-1)
test$Date <- as.Date(test$Date, format = "%m/%d/%Y")
b <- ggplot(test, aes(x=Date, y=RPtoWS, col=MasterSiteCode, group = 1)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
b


unique(kern_casgem$MasterSiteCode)

test2 <- subset(kern_casgem, MasterSiteCode == "348205N1182465W001")
new <- rbind(test, test2)
c <- ggplot(new, aes(x=Date, y=RPtoWS, col=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
c

well_locations <- read.csv("well_locations.csv")

outliers <- kern_casgem[which(kern_casgem$RPtoWS>0,),]
Voluntary_only <- kern_casgem[which(kern_casgem$VoluntaryorCASGEM=="Voluntary",),]
CASGEM_only <- subset(kern_casgem, VoluntaryorCasgem == "CASGEM")
  
  kern_casgem[which(kern_casgem$VoluntaryorCASGEM=="CASGEM",),]



CASGEM_plot <- ggplot(CASGEM_only, aes(x=Date, y=RPtoWS, col=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
CASGEM_plot

Voluntary_plot <- ggplot(Voluntary_only, aes(x=Date, y=RPtoWS, col=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
Voluntary_plot

CASGEM_outliers <- CASGEM_only[which(CASGEM_only$RPtoWS>0,),]
CASGEM_outliers <- subset(CASGEM_only, RPtoWS>0)
CASGEM_only <- CASGEM_only$RPtoWS[c(365, 1621)] * (-1) ## doesn't work
CASGEM_only[365, 9] ## this value should be 285.7 but it is showing up as -209
CASGEM_only["365", 9]
CASGEM_only["365", 9] = -285.7
CASGEM_only["1621", 9] = -1

