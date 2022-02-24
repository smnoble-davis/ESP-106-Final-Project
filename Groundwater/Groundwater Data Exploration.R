library(ggplot2)
library(dplyr)
setwd("/Users/shaelanoble/Documents/GitHub/ESP-106-Final-Project/Groundwater")
kern_casgem <- read.csv("Kern_CASGEM_WellElevationData.csv")
kern_casgem$Date <- as.Date(kern_casgem$Date, format = "%m/%d/%Y")
kern_casgem$RPtoWS <- kern_casgem$RPtoWS*(-1)
outliers <- kern_casgem[which(kern_casgem$RPtoWS>0,),]
## changing positive values to negative values
kern_casgem["365", 9] = -285.7
kern_casgem["1621", 9] = -1
kern_casgem["2026", 9] = -301.9
kern_casgem["2279", 9] = -104.7
## plot all measurements
a <- ggplot(kern_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
a
## lat/long coordinates
well_locations <- read.csv("well_locations.csv")
## merge
colnames(well_locations)[2] <- 'MasterSiteCode'
kern_casgem_loc <- merge(kern_casgem, well_locations, by = "MasterSiteCode")
## trying to do spatial stuff
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
CA_counties <- counties(state = "California", cb=TRUE)
b <- ggplot()
b <- b + geom_sf(data = CA_counties, color="black", fill="white", size=0.25)
b
## plot only kern county shapefile
kern_county <- subset(CA_counties, NAME == "Kern")
c <- ggplot()
c <- c + geom_sf(data = kern_county, color="black", fill="white", size=0.25)
c
## convert casgem data to sf object?
df <- st_as_sf(x = kern_casgem_loc, coords = c("LONGITUDE", "LATITUDE"))
st_crs(df) <- st_crs(kern_county)
## plot points on shapefile
c <- ggplot()
c <- c + geom_sf(data = kern_county, color="black", fill="white", size=0.25) + geom_sf(data = df)
c
##reference height for each well
max_height <- aggregate(RPtoWS ~ MasterSiteCode, kern_casgem, function(x) max(x))

colnames(max_height) <- c("MasterSiteCode", "m")
df2 <- merge(kern_casgem, max_height, by= "MasterSiteCode")

df2 <- df2 %>% 
  group_by(MasterSiteCode) %>%
  mutate(depth_std = RPtoWS - m) %>%
  ungroup()

##wells plotted by reference height
d <- ggplot(df2, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=depth_std))
d

##trying to find outliers in individual wells
df3 <- kern_casgem %>% 
  group_by(MasterSiteCode) %>%
  summarize(sd = sd(RPtoWS, na.rm=TRUE), mean = mean(RPtoWS, na.rm=TRUE), min = min(RPtoWS, na.rm=TRUE), max = max(RPtoWS, na.rm=TRUE), n=n()) %>%
  ungroup()

##plotting individual wells
e <- ggplot(subset(kern_casgem, MasterSiteCode=="356684N1193954W002"), aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
e

data_check <- subset(kern_casgem, MasterSiteCode=="356684N1193954W002")


select
filter
summarize
arrange

library(data.table)
kern_casgem_dt <- as.data.table(kern_casgem)
maxvalues <- kern_casgem_dt[kern_casgem_dt[, .I[RPtoWS== max(RPtoWS)], by=MasterSiteCode]$V1]

max(kern_casgem$MasterSiteCode==344779N1192479W001)


th <- theme_bw(legend.position="none")
a <- ggplot(kern_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode)) + geom_line() + theme(legend.position = "none") 
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

