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

##reference height for each well with outliers removed
max_height2 <- aggregate(RPtoWS ~ MasterSiteCode, df1, function(x) max(x))

colnames(max_height2) <- c("MasterSiteCode", "m")
nooutliers_standardized <- merge(kern_casgem, max_height, by= "MasterSiteCode")

nooutliers_standardized <- nooutliers_standardized %>% 
  group_by(MasterSiteCode) %>%
  mutate(depth_std = RPtoWS - m) %>%
  ungroup()

##wells plotted by reference height
d <- ggplot(df2, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=depth_std))
d

##trying to find outliers in individual wells
df3 <- kern_casgem %>% 
  group_by(MasterSiteCode) %>%
  summarize(sd = sd(RPtoWS, na.rm=TRUE), quantile = quantile(RPtoWS, probs=0.99, na.rm=TRUE), mean = mean(RPtoWS, na.rm=TRUE), min = min(RPtoWS, na.rm=TRUE), max = max(RPtoWS, na.rm=TRUE), n=n()) %>%
  ungroup()

g <- ggplot(df2, aes(depth_std)) + geom_histogram() + theme(legend.position = "none")
g

kern_casgem <-merge(kern_casgem, df3, by = "MasterSiteCode")

df1 = kern_casgem %>%
  group_by(MasterSiteCode) %>%
  filter(!((RPtoWS - mean(RPtoWS, na.rm=TRUE)) > 3*sd(RPtoWS, na.rm=TRUE)))

##trying boxplot for outliers
boxplot(kern_casgem$RPtoWS~kern_casgem$MasterSiteCode)

##plot without outliers
withoutoutliers <- ggplot(df1, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
withoutoutliers



##not working
df4 <- kern_casgem %>% 
  group_by(MasterSiteCode) %>%
  filter(RPtoWS > quantile(RPtoWS, probs=0.99, na.rm = TRUE)) %>%
  ungroup()

##plotting individual wells
e <- ggplot(subset(kern_casgem, MasterSiteCode=="356684N1193954W002"), aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS))
e

data_check <- subset(kern_casgem, MasterSiteCode=="356684N1193954W002")

##aggregating measurements up by year, finding avg depth_std by year

df2$Year <- as.numeric(format(df2$Date, "%Y"))
avgperyear <- aggregate(df2$depth_std, by=list(year=df2$Year), FUN=mean, na.rm=TRUE)

## plotting change in average standardized depth to groundwater across time for Kern county
f <- ggplot(avgperyear, aes(x=year, y=x)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=year, y=x))
f

## plotting the above but with outliers removed
nooutliers_standardized$Year <- as.numeric(format(nooutliers_standardized$Date, "%Y"))
avgperyear2 <- aggregate(nooutliers_standardized$depth_std, by=list(year=nooutliers_standardized$Year), FUN=mean, na.rm=TRUE)
h <- ggplot(avgperyear2, aes(x=year, y=x)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=year, y=x))
h

##attempt to plot well locations with cropscape
library(cdlTools)
crop2015 <- getCDL("California", 2015, location = NULL)
##the above does not work, had to download files from the website. Imported tif file has a raster object.
library(raster)
kern_crop_2015 <- "CDL_2015_06029.tif"
kern_crop_2015=raster(kern_crop_2015)
##plotting raster layer onto plot with well points
##convert raster layer to data frame?
kern_crop_2015pt <- rasterToPoints(kern_crop_2015, spatial = TRUE)
kern_crop_2015df <- as.data.frame(kern_crop_2015pt)

ggplot() +
  geom_raster(data = kern_crop_2015df , aes(x = x, y = y, fill = CDL_2015_06029)) + geom_sf(data = df)
  ggtitle("raster")

##reprojecting raster data to match kern county shape file?
  kern_crop_2015rp <- projectRaster(kern_crop_2015,
                                crs = crs(kern_county))
  plot(kern_crop_2015rp)
  plot(kern_county, add=TRUE)
  

plot(kern_crop_2015)
plot(df$geometry, add = TRUE, pch = 20, col= "purple")

kern_crop_map <- ggplot() + geom_raster(data = kern_crop_2015)

kern_crop_map <- ggplot()
kern_crop_map <- kern_crop_map + geom_sf(data = kern_county, color="black", fill="white", size=0.25) + geom_sf(data = df) + geom_raster(data = kern_crop_2015df)
kern_crop_map





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

