# reading in csv file for all 5 counties
library(ggplot2)
library(dplyr)
library(gridExtra)
setwd("/Users/shaelanoble/Documents/GitHub/ESP-106-Final-Project/Groundwater")

kern_casgem <- read.csv("Kern_CASGEM_WellElevationData.csv")
fresno_casgem <- read.csv("Fresno_CASGEM_WellElevationData.csv")
merced_casgem <- read.csv("Merced_CASGEM_WellElevationData.csv")
monterey_casgem <- read.csv("Monterey_CASGEM_WellElevationData.csv")
tulare_casgem <- read.csv("Tulare_CASGEM_WellElevationData.csv")

# adding column to all dataframes specifying the county
kern_casgem$County <- "Kern"
fresno_casgem$County <- "Fresno"
merced_casgem$County <- "Merced"
monterey_casgem$County <- "Monterey"
tulare_casgem$County <- "Tulare"

# turning date column into date format
kern_casgem$Date <- as.Date(kern_casgem$Date, format = "%m/%d/%Y")
fresno_casgem$Date <- as.Date(fresno_casgem$Date, format = "%m/%d/%Y")
merced_casgem$Date <- as.Date(merced_casgem$Date, format = "%m/%d/%Y")
monterey_casgem$Date <- as.Date(monterey_casgem$Date, format = "%m/%d/%Y")
tulare_casgem$Date <- as.Date(tulare_casgem$Date, format = "%m/%d/%Y")

# changing RPtoWS to (-) as it represents depth to groundwater
kern_casgem$RPtoWS <- kern_casgem$RPtoWS*(-1)
fresno_casgem$RPtoWS <- fresno_casgem$RPtoWS*(-1)
merced_casgem$RPtoWS <- merced_casgem$RPtoWS*(-1)
monterey_casgem$RPtoWS <- monterey_casgem$RPtoWS*(-1)
tulare_casgem$RPtoWS <- tulare_casgem$RPtoWS*(-1)

# identifying incorrectly entered measurements (only found in kern - 4 measurements and monterey - 1 measurement)
kern_positive <- kern_casgem[which(kern_casgem$RPtoWS>0,),]
monterey_positive <- monterey_casgem[which(monterey_casgem$RPtoWS>0,),]

# changing (+) measurements to (-) measurements
kern_casgem["365", 9] = -285.7
kern_casgem["1621", 9] = -1
kern_casgem["2026", 9] = -301.9
kern_casgem["2279", 9] = -104.7

monterey_casgem["654", 9] = -4

# exploratory plots of all 5 counties 
kern <- ggplot(kern_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Kern County") + ylab("Depth to Groundwater (ft)")
kern
fresno <- ggplot(fresno_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Fresno County") + ylab("Depth to Groundwater (ft)")
fresno
merced <- ggplot(merced_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Merced County") + ylab("Depth to Groundwater (ft)")
merced
monterey <- ggplot(monterey_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Monterey County") + ylab("Depth to Groundwater (ft)")
monterey
tulare <- ggplot(tulare_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Tulare County") + ylab("Depth to Groundwater (ft)")
tulare

# code to show all 5 plots at the same time
grid.arrange(kern, fresno, merced, monterey, tulare)

##dealing with outliers
#create a plot showing a boxplot distribution for each well
boxplot(kern_casgem$RPtoWS~kern_casgem$MasterSiteCode)
boxplot(fresno_casgem$RPtoWS~fresno_casgem$MasterSiteCode)
boxplot(merced_casgem$RPtoWS~merced_casgem$MasterSiteCode)
boxplot(monterey_casgem$RPtoWS~monterey_casgem$MasterSiteCode)
boxplot(tulare_casgem$RPtoWS~tulare_casgem$MasterSiteCode)

#remove outliers >2.5 sd away from the mean
kern_elim = kern_casgem %>%
  group_by(MasterSiteCode) %>%
  filter(!((RPtoWS - mean(RPtoWS, na.rm=TRUE)) > 2.5*sd(RPtoWS, na.rm=TRUE)))
fresno_elim = fresno_casgem %>%
  group_by(MasterSiteCode) %>%
  filter(!((RPtoWS - mean(RPtoWS, na.rm=TRUE)) > 2.5*sd(RPtoWS, na.rm=TRUE)))
merced_elim = merced_casgem %>%
  group_by(MasterSiteCode) %>%
  filter(!((RPtoWS - mean(RPtoWS, na.rm=TRUE)) > 2.5*sd(RPtoWS, na.rm=TRUE)))
monterey_elim = monterey_casgem %>%
  group_by(MasterSiteCode) %>%
  filter(!((RPtoWS - mean(RPtoWS, na.rm=TRUE)) > 2.5*sd(RPtoWS, na.rm=TRUE)))
tulare_elim = tulare_casgem %>%
  group_by(MasterSiteCode) %>%
  filter(!((RPtoWS - mean(RPtoWS, na.rm=TRUE)) > 2.5*sd(RPtoWS, na.rm=TRUE)))

#compare eliminated data boxplot to raw data boxplot to see if outliers were removed (some were?)
boxplot(kern_elim$RPtoWS~kern_elim$MasterSiteCode)
boxplot(fresno_elim$RPtoWS~fresno_elim$MasterSiteCode)
boxplot(merced_elim$RPtoWS~merced_elim$MasterSiteCode)
boxplot(monterey_elim$RPtoWS~monterey_elim$MasterSiteCode)
boxplot(tulare_elim$RPtoWS~tulare_elim$MasterSiteCode)

# plots for all 5 counties with outliers removed
kern_e <- ggplot(kern_elim, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Kern County")
kern_e
fresno_e <- ggplot(fresno_elim, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Fresno County")
fresno_e
merced_e <- ggplot(merced_elim, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Merced County")
merced_e
monterey_e <- ggplot(monterey_elim, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Monterey County")
monterey_e
tulare_e <- ggplot(tulare_elim, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("Tulare County")
tulare_e

# code to show all 5 plots at the same time
grid.arrange(kern_e, fresno_e, merced_e, monterey_e, tulare_e)

#finding reference heights for wells in all 5 counties
kern_height <- aggregate(RPtoWS ~ MasterSiteCode, kern_elim, function(x) max(x))
fresno_height <- aggregate(RPtoWS ~ MasterSiteCode, fresno_elim, function(x) max(x))
merced_height <- aggregate(RPtoWS ~ MasterSiteCode, merced_elim, function(x) max(x))
monterey_height <- aggregate(RPtoWS ~ MasterSiteCode, monterey_elim, function(x) max(x))
tulare_height <- aggregate(RPtoWS ~ MasterSiteCode, tulare_elim, function(x) max(x))

colnames(kern_height) <- c("MasterSiteCode", "max_h")
kern_elim <- merge(kern_elim, kern_height, by= "MasterSiteCode")
colnames(fresno_height) <- c("MasterSiteCode", "max_h")
fresno_elim <- merge(fresno_elim, fresno_height, by= "MasterSiteCode")
colnames(merced_height) <- c("MasterSiteCode", "max_h")
merced_elim <- merge(merced_elim, merced_height, by= "MasterSiteCode")
colnames(monterey_height) <- c("MasterSiteCode", "max_h")
monterey_elim <- merge(monterey_elim, monterey_height, by= "MasterSiteCode")
colnames(tulare_height) <- c("MasterSiteCode", "max_h")
tulare_elim <- merge(tulare_elim, tulare_height, by= "MasterSiteCode")

kern_elim <- kern_elim %>% 
  group_by(MasterSiteCode) %>%
  mutate(depth_std = RPtoWS - max_h) %>%
  ungroup()
fresno_elim <- fresno_elim %>% 
  group_by(MasterSiteCode) %>%
  mutate(depth_std = RPtoWS - max_h) %>%
  ungroup()
merced_elim <- merced_elim %>% 
  group_by(MasterSiteCode) %>%
  mutate(depth_std = RPtoWS - max_h) %>%
  ungroup()
monterey_elim <- monterey_elim %>% 
  group_by(MasterSiteCode) %>%
  mutate(depth_std = RPtoWS - max_h) %>%
  ungroup()
tulare_elim <- tulare_elim %>% 
  group_by(MasterSiteCode) %>%
  mutate(depth_std = RPtoWS - max_h) %>%
  ungroup()

#plotting based on standardized depth
kern_std <- ggplot(kern_elim, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=depth_std)) + ggtitle("Kern County")
kern_std
fresno_std <- ggplot(fresno_elim, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=depth_std)) + ggtitle("Fresno County")
fresno_std
merced_std <- ggplot(merced_elim, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=depth_std)) + ggtitle("Merced County")
merced_std
monterey_std <- ggplot(monterey_elim, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=depth_std)) + ggtitle("Monterey County")
monterey_std
tulare_std <- ggplot(tulare_elim, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=Date, y=depth_std)) + ggtitle("Tulare County")
tulare_std

##aggregating measurements up by year
#adding a year column
kern_elim$Year <- as.numeric(format(kern_elim$Date, "%Y"))
fresno_elim$Year <- as.numeric(format(fresno_elim$Date, "%Y"))
merced_elim$Year <- as.numeric(format(merced_elim$Date, "%Y"))
monterey_elim$Year <- as.numeric(format(monterey_elim$Date, "%Y"))
tulare_elim$Year <- as.numeric(format(tulare_elim$Date, "%Y"))

#creating new dataframe for avg depth per year 
kern_apy <- aggregate(kern_elim$depth_std, by=list(year=kern_elim$Year), FUN=mean, na.rm=TRUE)
fresno_apy <- aggregate(fresno_elim$depth_std, by=list(year=fresno_elim$Year), FUN=mean, na.rm=TRUE)
merced_apy <- aggregate(merced_elim$depth_std, by=list(year=merced_elim$Year), FUN=mean, na.rm=TRUE)
monterey_apy <- aggregate(monterey_elim$depth_std, by=list(year=monterey_elim$Year), FUN=mean, na.rm=TRUE)
tulare_apy <- aggregate(tulare_elim$depth_std, by=list(year=tulare_elim$Year), FUN=mean, na.rm=TRUE)

#adding county column to apy data frams
kern_apy$County <-"Kern"
fresno_apy$County <-"Fresno"
merced_apy$County <-"Merced"
monterey_apy$County <-"Monterey"
tulare_apy$County <-"Tulare"

# plotting change in average standardized depth to groundwater across time for each county
agg_kern <- ggplot(kern_apy, aes(x=year, y=x)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=year, y=x)) + ggtitle("Kern County") + xlab("Year") + ylab("Average Depth to Groundwater")
agg_kern
agg_fresno <- ggplot(fresno_apy, aes(x=year, y=x)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=year, y=x)) + ggtitle("Fresno County") + xlab("Year") + ylab("Average Depth to Groundwater")
agg_fresno
agg_merced <- ggplot(merced_apy, aes(x=year, y=x)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=year, y=x)) + ggtitle("Merced County") + xlab("Year") + ylab("Average Depth to Groundwater")
agg_merced
agg_monterey <- ggplot(monterey_apy, aes(x=year, y=x)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=year, y=x)) + ggtitle("Monterey County") + xlab("Year") + ylab("Average Depth to Groundwater")
agg_monterey
agg_tulare <- ggplot(tulare_apy, aes(x=year, y=x)) + geom_point() + theme(legend.position = "none") + geom_line(aes(x=year, y=x)) + ggtitle("Tulare County") + xlab("Year") + ylab("Average Depth to Groundwater")
agg_tulare

# code to show all 5 plots at the same time
grid.arrange(agg_kern, agg_fresno, agg_merced, agg_monterey, agg_tulare)

# combining all dataframes into one single df
casgem_all <- do.call("rbind", list(kern_casgem, fresno_casgem, merced_casgem, monterey_casgem, tulare_casgem))

## combine apy dataframes and run boxplot
apy_all <- do.call("rbind", list(kern_apy, fresno_apy, merced_apy, monterey_apy, tulare_apy))
colnames(apy_all) <- c("Year", "Avg_Depth", "County")
apy_box <- ggplot(apy_all, aes(x=County, y=Avg_Depth)) + geom_boxplot()

# save apy_all dataframe as an rfile (can load in later)
save(apy_all, file="apy_all.RData")

#loading in agricultural data and saved groundwater data
load("TotalProduction.RData")
load("5Counties_AllCrops.RData")
load("apy_all.RData")

#merge county_yr_total and apy_all
merged_data <- merge(county_yr_total, apy_all, by = c("County", "Year"))

#adding in a column that codes for drought based on emergency drought proclamation status
merged_data <- merged_data %>%
  mutate(drought=ifelse(Year==2009 | between(Year, 2012,2016),1,0))

#converting the year column to a date 
merged_data$Year <- ymd(merged_data$Year, truncated=2)

#run linear regression model
merged_data$drought <- as.factor(merged_data$drought)
model <- lm(Value_sum~Avg_Depth + drought, data=merged_data)

#run multivariate regression model
model2 <- lm(Value_sum~Avg_Depth*drought,data=merged_data)

#running regression for each county (assuming drought and groundwater depth are correlated?)
linear_models <- merged_data %>%
  group_split(County) %>%
  map(function(d) lm(Value_sum~Avg_Depth*drought,data=d))

#plot multivariate regression
explore2 <- ggplot(merged_data, aes(x=Avg_Depth, y=Value_sum, color=factor(drought))) + geom_point() + geom_smooth(method = "lm") +facet_wrap(~County)
explore3 <- ggplot(merged_data, aes(x=Avg_Depth, y=Value_sum, color=factor(drought)), label=scales::comma) + geom_point() + geom_smooth(method = "lm") + xlab("Depth to Groundwater (ft)") + ylab("Gross Production Value ($1000)") + th + scale_y_continuous(label=scales::comma) + scale_color_discrete(name = "Year", labels = c("Normal", "Drought"))
explore4 <- ggplot(merged_data, aes(x=Avg_Depth, y=Value_sum, color=factor(drought)), label=scales::comma) + geom_point() + geom_smooth(method = "lm") + xlab("Depth to Groundwater (ft)") + ylab("Gross Production Value ($1000)") + th + scale_y_continuous(label=scales::comma) + scale_color_discrete(name = "Year", labels = c("Normal", "Drought"))+facet_wrap(~County, scales = "free")

