#AUTHORS: SHAELA NOBLE & CHRISTINA HARRINGTON

#Load in the the packages we'll be using for wrangling and plotting
library(viridis)
library(tidyverse)
library(stringr)
library(corrplot)
library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

##SECTION 1: WRANGLING/CLEANING AGRICULTURAL DATA
#read in ag crop production data files 2009-2020

setwd("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production")

#For all data sets for years 2009-2020, write subset function so we only see data for the 5 counties of interest: Fresno, Kern, Monterey, Merced and Tulare.
#Remove crops that aren't produce or feed (e.g. hay) by finding all crops that contain the words: apiary, cattle, fish, hogs, chickens, eggs, livestock, manure, milk, poultry, sheep, turkeys, wool, flowers, forest products, nursery products, pasture, silage, ostrich,birds,goats (str_detect from stringr package)
#Notice that some data sets have a space at the end of the County name and crop names, while some data sets do not.Remove the trailing white space from those vectors to avoid double counting of crops when you combine all the data frames later.

#2009 DATA
ag2009=read.csv("2009cropyear.csv")
ag2009$Crop.Name=trimws(ag2009$Crop.Name,which="r")
ag2009$County=trimws(ag2009$County,which="r")

ag2009_cntcrops=subset(ag2009,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))

#2010 DATA
ag2010=read.csv("2010cropyear.csv")   
ag2010$Crop.Name=trimws(ag2010$Crop.Name,which="r")
ag2010$County=trimws(ag2010$County,which="r")

ag2010_cntcrops=subset(ag2010,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))


#2011 DATA
ag2011=read.csv("2011cropyear.csv")
ag2011$Crop.Name=trimws(ag2011$Crop.Name,which="r")
ag2011$County=trimws(ag2011$County,which="r")

ag2011_cntcrops=subset(ag2011,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))


#2012 DATA
ag2012=read.csv("2012cropyear.csv")
ag2012$Crop.Name=trimws(ag2012$Crop.Name,which="r")
ag2012$County=trimws(ag2012$County,which="r")

ag2012_cntcrops=subset(ag2012,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))


#2013 DATA
ag2013=read.csv("2013cropyear.csv")
ag2013_cntcrops=subset(ag2013,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))
#2014 DATA
ag2014=read.csv("2014cropyear.csv")
ag2014_cntcrops=subset(ag2014,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))

#2015 DATA
ag2015=read.csv("2015cropyear.csv")
ag2015_cntcrops=subset(ag2015,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))
#2016 DATA
ag2016=read.csv("2016cropyear.csv")
ag2016_cntcrops=subset(ag2016,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS "))

#2017 DATA
ag2017=read.csv("2017cropyear.csv")
ag2017$Crop.Name=trimws(ag2017$Crop.Name,which="r")
ag2017$County=trimws(ag2017$County,which="r")

ag2017_cntcrops=subset(ag2017,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))


#2018 DATA
ag2018err=read.csv("2018cactbsErrata.csv")
ag2018err$Crop.Name=trimws(ag2018err$Crop.Name,which="r")
ag2018err$County=trimws(ag2018err$County,which="r")

ag2018_cntcrops=subset(ag2018err,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))


#2019 DATA
ag2019=read.csv("2019cropyear.csv")
ag2019$Crop.Name=trimws(ag2019$Crop.Name,which="r")
ag2019$County=trimws(ag2019$County,which="r")

ag2019_cntcrops=subset(ag2019,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))

#2020 DATA
ag2020=read.csv("2020cropyear.csv")
ag2020$Crop.Name=trimws(ag2020$Crop.Name,which="r")
ag2020$County=trimws(ag2020$County,which="r")

ag2020_cntcrops=subset(ag2020,County%in%c("Fresno","Kern","Monterey","Merced","Tulare") &
                         FALSE == str_detect(Crop.Name,"APIARY") &
                         FALSE == str_detect(Crop.Name,"CATTLE") &
                         FALSE == str_detect(Crop.Name,"FISH") &
                         FALSE == str_detect(Crop.Name,"HOGS") &
                         FALSE == str_detect(Crop.Name,"CHICKENS") &
                         FALSE == str_detect(Crop.Name,"EGGS") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                         FALSE == str_detect(Crop.Name,"MANURE") &
                         FALSE == str_detect(Crop.Name,"MILK") &
                         FALSE == str_detect(Crop.Name,"POULTRY") &
                         FALSE == str_detect(Crop.Name,"SHEEP") &
                         FALSE == str_detect(Crop.Name,"TURKEYS") &
                         FALSE == str_detect(Crop.Name,"WOOL") &
                         FALSE == str_detect(Crop.Name,"SILAGE") &
                         FALSE == str_detect(Crop.Name,"NURSERY") &
                         FALSE == str_detect(Crop.Name,"FOREST") &
                         FALSE == str_detect(Crop.Name,"FLOWERS") &
                         FALSE == str_detect(Crop.Name,"PASTURE") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS") &
                         FALSE == str_detect(Crop.Name,"BIRDS") &
                         FALSE == str_detect(Crop.Name,"GOATS") &
                         FALSE == str_detect(Crop.Name,"OSTRICH") &
                         FALSE == str_detect(Crop.Name,"BIOMASS"))

#Change the 2020 data set column names so it matches the column names of the other data sets, otherwise you won't be able to combine the data frame with the others.
ag2020_cntcrops=rename(ag2020_cntcrops,Yield=Yield..Unit.Acre.,Price.P.U=Price..Dollars.Unit.,Value=Value..Dollars.)


#Now combine all data frames so you see all counties for every year 2009-2020.Some years have differing numbers of observations, so you'll want to use the bind_row function.

allag=bind_rows(ag2009_cntcrops,ag2010_cntcrops,ag2011_cntcrops,ag2012_cntcrops,ag2013_cntcrops,ag2014_cntcrops,ag2015_cntcrops,ag2016_cntcrops,ag2017_cntcrops,ag2018_cntcrops,ag2019_cntcrops,ag2020_cntcrops)


#This isn't the final data frame we're interested in, but create some exploratory data plots & stats of the new data frame. 

yield_summary=print(summary(allag$Yield))
prod_summary=print(summary(allag$Production))
value_summary=print(summary(allag$Value))
harvest_summary=print(summary(allag$Harvested.Acres))

#We're interested in the total value of production for each county in each year. 
#Create data frame that has columns Year, County, Total Value only. We need to sum the value of all crops for each county per year.
#The total production values will be high (in the billions), so far clarity on the graph, divide the value by 1000.

county_yr_total=allag %>%
  group_by(County,Year) %>%
  summarize(Value_sum = sum(Value)/1000)

#Produce exploratory plots showing the distribution of production value over the years and how much each county contributes to overall production.
YrCounty_Valuesummary=print(summary(county_yr_total$Value_sum))

YrCountyBoxplot=print(
  ggplot(county_yr_total,aes(y=Value_sum,fill=County))+
    geom_boxplot()+
    scale_y_continuous(name="Gross Production Value ($1000)",labels=scales::comma)+
    scale_fill_viridis(option="C", discrete=TRUE)+
    ggtitle("Gross Crop Production Value (2009-2020)")+
    theme_bw()+
    theme(plot.title = element_text(face="bold"),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank())
)


YrCountyBar=print(
  ggplot(county_yr_total,aes(Year,Value_sum,fill=County))+
    geom_bar(position="stack",stat="identity")+
    scale_x_continuous(name="Production Year",breaks=c(2009:2020))+
    scale_y_continuous(name="Gross Production Value ($1000)",labels=scales::comma)+
    scale_fill_viridis(option="C", discrete=TRUE)+
    ggtitle("Counties' Share of Gross Crop Production Value")+
    theme_bw()+
    theme(plot.title = element_text(face="bold"))
)

#Plot all the counties' production values over time
YrCountyLine=print(
  ggplot(county_yr_total,aes(Year,Value_sum,group=County,color=County))+
    ggtitle("Gross Crop Production Value (2009-2020)")+
    scale_color_viridis(option="C", discrete=TRUE)+
    geom_line()+
    geom_point()+
    scale_x_continuous(name="Production Year",breaks=c(2009:2020))+
    scale_y_continuous(name="Gross Production Value ($1000)",labels=scales::comma)+
    theme_bw()+
    theme(plot.title = element_text(face="bold")))


#Plot each county separately, to see their production values over time more clearly
EachCounty=
  ggplot(county_yr_total,aes(Year,Value_sum))+
  facet_wrap(~County)+  
  ggtitle("Gross Crop Production Value (2009-2020)")+
  geom_line()+
  geom_point()+
  scale_x_continuous(name="Production Year",breaks=c(2009:2020))+
  scale_y_continuous(name="Gross Production Value ($1000)",labels=scales::comma)+
  theme_bw()+
  theme(plot.title = element_text(face="bold"))

#Save the 2 compiled agriculture data frames as objects so you can call them later and use in other R scripts
save(county_yr_total,file="TotalProduction.Rdata")
save(allag,file="5Counties_AllCrops.Rdata")



##SECTION 2: WRANGLING/CLEANING GROUNDWATER DATA

#Set working directory and read in .csv files for all 5 counties

setwd("/Users/shaelanoble/Documents/GitHub/ESP-106-Final-Project/Groundwater")

kern_casgem <- read.csv("Kern_CASGEM_WellElevationData.csv")
fresno_casgem <- read.csv("Fresno_CASGEM_WellElevationData.csv")
merced_casgem <- read.csv("Merced_CASGEM_WellElevationData.csv")
monterey_casgem <- read.csv("Monterey_CASGEM_WellElevationData.csv")
tulare_casgem <- read.csv("Tulare_CASGEM_WellElevationData.csv")

#Add a column to all data frames for the identity of the county
kern_casgem$County <- "Kern"
fresno_casgem$County <- "Fresno"
merced_casgem$County <- "Merced"
monterey_casgem$County <- "Monterey"
tulare_casgem$County <- "Tulare"

#Convert the date column to a date format
kern_casgem$Date <- as.Date(kern_casgem$Date, format = "%m/%d/%Y")
fresno_casgem$Date <- as.Date(fresno_casgem$Date, format = "%m/%d/%Y")
merced_casgem$Date <- as.Date(merced_casgem$Date, format = "%m/%d/%Y")
monterey_casgem$Date <- as.Date(monterey_casgem$Date, format = "%m/%d/%Y")
tulare_casgem$Date <- as.Date(tulare_casgem$Date, format = "%m/%d/%Y")

#Change RPtoWS values to (-) as they represent depth to groundwater and will be visualized as negative values
kern_casgem$RPtoWS <- kern_casgem$RPtoWS*(-1)
fresno_casgem$RPtoWS <- fresno_casgem$RPtoWS*(-1)
merced_casgem$RPtoWS <- merced_casgem$RPtoWS*(-1)
monterey_casgem$RPtoWS <- monterey_casgem$RPtoWS*(-1)
tulare_casgem$RPtoWS <- tulare_casgem$RPtoWS*(-1)

#Identify incorrectly entered measurements (only found in Kern - 4 measurements and Monterey - 1 measurement). Measurements were likely entered incorrectly as all measurements should have initially been entered as a positive value. 
kern_positive <- kern_casgem[which(kern_casgem$RPtoWS>0,),]
monterey_positive <- monterey_casgem[which(monterey_casgem$RPtoWS>0,),]

#Change (+) measurements to (-) measurements
kern_casgem["365", 9] = -285.7
kern_casgem["1621", 9] = -1
kern_casgem["2026", 9] = -301.9
kern_casgem["2279", 9] = -104.7
monterey_casgem["654", 9] = -4

#Exploratory plots of all 5 counties 
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

#Remove outliers >2.5 standard deviations away from the mean within each set of measurements from a well
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

#Plots for all 5 counties with outliers removed
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

#It is difficult to see how the groundwater table is changing across the counties since each well is hydrologically different with a different standard depth to water. 
#By setting the maximum height observed within each well as the reference height of "0", we can see how the groundwater table is changing within the wells in reference to each other.
#Find reference heights for wells in all 5 counties
kern_height <- aggregate(RPtoWS ~ MasterSiteCode, kern_elim, function(x) max(x))
fresno_height <- aggregate(RPtoWS ~ MasterSiteCode, fresno_elim, function(x) max(x))
merced_height <- aggregate(RPtoWS ~ MasterSiteCode, merced_elim, function(x) max(x))
monterey_height <- aggregate(RPtoWS ~ MasterSiteCode, monterey_elim, function(x) max(x))
tulare_height <- aggregate(RPtoWS ~ MasterSiteCode, tulare_elim, function(x) max(x))

#Rename columns
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

#Standardize all measurements within a well to the reference height
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

#Plot the standardized data
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

#The agricultural data is in a County-Year format therefore we need to aggregate the groundwater measurements up to the year level 
#Aggregate measurements up by year - add a year column
kern_elim$Year <- as.numeric(format(kern_elim$Date, "%Y"))
fresno_elim$Year <- as.numeric(format(fresno_elim$Date, "%Y"))
merced_elim$Year <- as.numeric(format(merced_elim$Date, "%Y"))
monterey_elim$Year <- as.numeric(format(monterey_elim$Date, "%Y"))
tulare_elim$Year <- as.numeric(format(tulare_elim$Date, "%Y"))

#Create a new data frame for average depth per year 
kern_apy <- aggregate(kern_elim$depth_std, by=list(year=kern_elim$Year), FUN=mean, na.rm=TRUE)
fresno_apy <- aggregate(fresno_elim$depth_std, by=list(year=fresno_elim$Year), FUN=mean, na.rm=TRUE)
merced_apy <- aggregate(merced_elim$depth_std, by=list(year=merced_elim$Year), FUN=mean, na.rm=TRUE)
monterey_apy <- aggregate(monterey_elim$depth_std, by=list(year=monterey_elim$Year), FUN=mean, na.rm=TRUE)
tulare_apy <- aggregate(tulare_elim$depth_std, by=list(year=tulare_elim$Year), FUN=mean, na.rm=TRUE)

#Add "county" column to apy data frames
kern_apy$County <-"Kern"
fresno_apy$County <-"Fresno"
merced_apy$County <-"Merced"
monterey_apy$County <-"Monterey"
tulare_apy$County <-"Tulare"

#Plot change in average standardized depth to groundwater across time for each county
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

#Code for Figure 1 showing data cleaning and aggregation process for Kern County
th <- theme_bw()
kern <- ggplot(kern_casgem, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + th + guides(col=FALSE) + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("(A) Kern County Well Measurements") + ylab("Depth to Groundwater (ft)") + xlab("Year")
kern
kern_e <- ggplot(kern_elim, aes(x=Date, y=RPtoWS, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + th + guides(col=FALSE) + geom_line(aes(x=Date, y=RPtoWS)) + ggtitle("(B) Kern County Well Measurements (Outliers Removed)") + ylab("Depth to Groundwater (ft)") + xlab("Year")
kern_e
kern_std <- ggplot(kern_elim, aes(x=Date, y=depth_std, col=MasterSiteCode,group=MasterSiteCode)) + geom_point() + th + guides(col=FALSE) + geom_line(aes(x=Date, y=depth_std)) + ggtitle("(C) Kern County Well Measurements (Standardized)") + ylab("Depth to Groundwater (ft)") + xlab("Year")
kern_std
agg_kern <- ggplot(kern_apy, aes(x=year, y=x)) + geom_point() + th + guides(col=FALSE) + geom_line(aes(x=year, y=x)) + ggtitle("(D) Kern County") + xlab("Year") + ylab("Average Depth to Groundwater (ft)")
agg_kern
grid.arrange(kern, kern_e, kern_std, agg_kern)

#Combine apy data frames and rename columns
apy_all <- do.call("rbind", list(kern_apy, fresno_apy, merced_apy, monterey_apy, tulare_apy))
colnames(apy_all) <- c("Year", "Avg_Depth", "County")

#Save apy_all dataframe as an rfile to load in later
save(apy_all, file="apy_all.RData")

##SECTION 3: MERGING & ANALYZING DATA

#Load in the cleaned/merged data frame objects for groundwater and crop production

load("Groundwater/apy_all.RData")
load("Ag Production/TotalProduction.Rdata")

#merge county_yr_total and apy_all
merged_data <- merge(county_yr_total, apy_all, by = c("County", "Year"))

#add in a column that codes for drought based on emergency drought proclamation status
merged_data <- merged_data %>%
  mutate(drought=ifelse(Year==2009 | between(Year, 2012,2016),1,0))

#Convert the year column to a date format 
merged_data$Year <- ymd(merged_data$Year, truncated=2)

#Convert drought data to factor format
merged_data$drought <- as.factor(merged_data$drought)

#(Code for Figure 4) Boxplots showing the association between groundwater depth and drought across counties and boxplot showing the association between agricultural production and drought across counties.
th <- theme_bw()

drought_bp_county <- ggplot(merged_data, aes(x=drought, y=Value_sum, fill=County)) + 
  geom_boxplot() + th + scale_y_continuous(labels = comma) + scale_x_discrete(labels = c("Non-drought", "Drought")) + xlab("Year") + ylab("Gross Production Value ($1000)") +scale_fill_viridis(option="C", discrete = TRUE) + ggtitle("(B) Agricultural Production in Non-Drought and Drought Years")
drought_bp_county
depthvdrought_county <- ggplot(merged_data, aes(x=drought, y=Avg_Depth, fill=County)) + 
  geom_boxplot() + th + scale_y_continuous(labels = comma) + scale_x_discrete(labels = c("Non-drought", "Drought")) + xlab("Year") + ylab("Depth to Groundwater (ft)") +scale_fill_viridis(option="C", discrete = TRUE) + ggtitle("(A) Depth to Groundwater in Non-Drought and Drought Years")
depthvdrought_county 
grid.arrange(depthvdrought_county, drought_bp_county, ncol=2)

##Investigating variable separately
#Regression for groundwater depth and drought across all data
groundvdrought <- lm(Avg_Depth~drought, data=merged_data)
summary(groundvdrought)

#Regressions for groundwater depth and drought per county
groundvdrought_models <- merged_data %>%
  group_split(County) %>%
  map(function(d) lm(Avg_Depth~drought,data=d))

#Regression for agricultural production and drought across all data
agvdrought <- lm(Value_sum~drought, data=merged_data)
summary(agvdrought)

#Regressions for agricultural production and drought per county
agvdrought_models <- merged_data %>%
  group_split(County) %>%
  map(function(d) lm(Value_sum~drought,data=d))

#Run linear regression model for all variables (non-interacting)
model <- lm(Value_sum~Avg_Depth*drought, data=merged_data)

#Run regression for each county (non-interacting variables)
linear_models <- merged_data %>%
  group_split(County) %>%
  map(function(d) lm(Value_sum~Avg_Depth*drought,data=d))

#Plot for figure 5 - multivariate regression across all data
regression_all <- ggplot(merged_data, aes(x=Avg_Depth, y=Value_sum, color=factor(drought)), label=scales::comma) + geom_point() + geom_smooth(method = "lm") + xlab("Depth to Groundwater (ft)") + ylab("Gross Production Value ($1000)") + th + scale_y_continuous(label=scales::comma) + scale_color_discrete(type=c("#5ab4ac", "#d8b365"), name = "Year", labels = c("Non-Drought", "Drought")) + ggtitle("Relationship Between Agricultural Production and Groundwater Depth")

#Plot for figure 6 - multivariate regression per county
regression_county <- ggplot(merged_data, aes(x=Avg_Depth, y=Value_sum, color=factor(drought)), label=scales::comma) + geom_point() + geom_smooth(method = "lm") + xlab("Depth to Groundwater (ft)") + ylab("Gross Production Value ($1000)") + th + scale_y_continuous(label=scales::comma) + scale_color_discrete(type=c("#5ab4ac", "#d8b365"), name = "Year", labels = c("Non-Drought", "Drought"))+facet_wrap(~County, scales = "free")


#Run a correlation of groundwater depth, production value, and drought for all 5 counties aggregated and plot it.
#To run the correlation, all variables must be numeric, so you'll have to remove the County and Year columns.The drought variable is currently a factor, so convert to numeric.

merged_data$drought=as.numeric(merged_data$drought)


cor_counties=subset(merged_data,select=-c(County,Year))
colnames(cor_counties)=c("Mean Depth","Prod. Value","Drought")
cor_countiesfinal=print(cor(cor_counties))
counties_corplot=corrplot(cor_countiesfinal,tl.col="black",tl.cex=1)
title("Counties Aggregated")

#Run a correlation for each of the 5 counties and plot them.

#Kern Correlations

kern_df=subset(merged_data,County=="Kern")
kern_df=subset(kern_df,select=-c(County,Year))
colnames(kern_df)=c("Mean Depth","Prod. Value","Drought")
kern_cor=print(cor(kern_df))
kern_plot=corrplot(kern_cor,tl.col="black",tl.cex=1)
title("Kern County")

#Fresno Correlations

fresno_df=subset(merged_data,County=="Fresno")
fresno_df=subset(fresno_df,select=-c(County,Year))
colnames(fresno_df)=c("Mean Depth","Prod. Value","Drought")
fresno_cor=print(cor(fresno_df))
fres_plot=corrplot(fresno_cor,tl.col="black",tl.cex=1)
title("Fresno County")

#Merced Correlations

merced_df=subset(merged_data,County=="Merced")
merced_df=subset(merced_df,select=-c(County,Year))
colnames(merced_df)=c("Mean Depth","Prod. Value","Drought")
merced_cor=print(cor(merced_df))
mer_plot=corrplot(merced_cor,tl.col="black",tl.cex=1)
title("Merced County")


#Tulare Correlations

tulare_df=subset(merged_data,County=="Tulare")
tulare_df=subset(tulare_df,select=-c(County,Year))
colnames(tulare_df)=c("Mean Depth","Prod. Value","Drought")
tulare_cor=print(cor(tulare_df))
tul_plot=corrplot(tulare_cor,tl.col="black",tl.cex=1)
title("Tulare County")

#Monterey Correlations

monterey_df=subset(merged_data,County=="Monterey")
monterey_df=subset(monterey_df,select=-c(County,Year))
colnames(monterey_df)=c("Mean Depth","Prod. Value","Drought")
monterey_cor=print(cor(monterey_df))
mont_plot=corrplot(monterey_cor,tl.col="black",tl.cex=1)
title("Monterey County")





