#read in ag crop production data files 2009-2020
library(tidyverse)
library(stringr)
library(viridis)
setwd("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production")

#Set Directory so that if you gave one folder with all your data, someone could read in each data set by only chaning the set directory at the top.

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
#Use 'group_by' and 'summarize' functions in dplyr

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


#Save the 2 compiled ag dataframes as objects so you can call them later and use in other R scripts
save(county_yr_total,file="TotalProduction.Rdata")
save(allag,file="5Counties_AllCrops.Rdata")

