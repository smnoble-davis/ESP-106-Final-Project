#read in ag crop production data files 2009-2020
library(tidyverse)
library("stringr")
library("ggplot2")
install.packages("purrr")
library("purrr")
install.packages("dplyr")
library("dplyr")

#2009 DATA
ag2009=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2009cropyear.csv")

#see how there's a "state totals" and "sum of others" rows at the end of every crop section.
#notice units are in tons or US$
#write subset function to remove rows that have value 'sum of others' and 'state totals' in the County column
#Remove non-crops: apiary, cattle, fish, hogs, chickens, eggs, livestock, manure, milk, poultry, sheep, turkeys, wool, flowers, forest products, nursery products, pasture, silage, ostrich,birds,goats (str_detect from stringr package)
#sum total $ production for all crops
#Be sure to remove 'na' values when summing/plotting - unless situation where you'd want to show? 

ag2009_cntcrops=subset(ag2009,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
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
                        FALSE == str_detect(Crop.Name,"BIOMASS "))

ggplot(ag2009_cntcrops,aes(County,Value))+
  geom_bar(stat="sum")


#2010 DATA
ag2010=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2010cropyear.csv")   
ag2010_cntcrops=subset(ag2010,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
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

#2011 DATA

ag2011=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2011cropyear.csv")
ag2011_cntcrops=subset(ag2011,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
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

#2012 DATA
ag2012=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2012cropyear.csv")
ag2012_cntcrops=subset(ag2012,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
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
                         

#2013 DATA
ag2013=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2013cropyear.csv")
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
                         FALSE == str_detect(Crop.Name,"BIOMASS "))
#2014 DATA
ag2014=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2014cropyear.csv")
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
                         FALSE == str_detect(Crop.Name,"BIOMASS "))

#2015 DATA
ag2015=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2015cropyear.csv")
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
                         FALSE == str_detect(Crop.Name,"BIOMASS "))
#2016 DATA
ag2016=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2016cropyear.csv")
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
ag2017=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2017cropyear.csv")
ag2017_cntcrops=subset(ag2017,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
                         FALSE == str_detect(Crop.Name,"APIARY ") &
                         FALSE == str_detect(Crop.Name,"CATTLE ") &
                         FALSE == str_detect(Crop.Name,"FISH ") &
                         FALSE == str_detect(Crop.Name,"HOGS ") &
                         FALSE == str_detect(Crop.Name,"CHICKENS ") &
                         FALSE == str_detect(Crop.Name,"EGGS ") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK ") &
                         FALSE == str_detect(Crop.Name,"MANURE ") &
                         FALSE == str_detect(Crop.Name,"MILK ") &
                         FALSE == str_detect(Crop.Name,"POULTRY ") &
                         FALSE == str_detect(Crop.Name,"SHEEP ") &
                         FALSE == str_detect(Crop.Name,"TURKEYS ") &
                         FALSE == str_detect(Crop.Name,"WOOL ") &
                         FALSE == str_detect(Crop.Name,"SILAGE ") &
                         FALSE == str_detect(Crop.Name,"NURSERY ") &
                         FALSE == str_detect(Crop.Name,"FOREST ") &
                         FALSE == str_detect(Crop.Name,"FLOWERS ") &
                         FALSE == str_detect(Crop.Name,"PASTURE ") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS ") &
                         FALSE == str_detect(Crop.Name,"BIRDS ") &
                         FALSE == str_detect(Crop.Name,"GOATS ") &
                         FALSE == str_detect(Crop.Name,"OSTRICH ") &
                         FALSE == str_detect(Crop.Name,"BIOMASS "))
#2018 DATA
ag2018err=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2018cactbsErrata.csv")
ag2018_cntcrops=subset(ag2018err,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
                         FALSE == str_detect(Crop.Name,"APIARY ") &
                         FALSE == str_detect(Crop.Name,"CATTLE ") &
                         FALSE == str_detect(Crop.Name,"FISH ") &
                         FALSE == str_detect(Crop.Name,"HOGS ") &
                         FALSE == str_detect(Crop.Name,"CHICKENS ") &
                         FALSE == str_detect(Crop.Name,"EGGS ") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK ") &
                         FALSE == str_detect(Crop.Name,"MANURE ") &
                         FALSE == str_detect(Crop.Name,"MILK ") &
                         FALSE == str_detect(Crop.Name,"POULTRY ") &
                         FALSE == str_detect(Crop.Name,"SHEEP ") &
                         FALSE == str_detect(Crop.Name,"TURKEYS ") &
                         FALSE == str_detect(Crop.Name,"WOOL ") &
                         FALSE == str_detect(Crop.Name,"SILAGE ") &
                         FALSE == str_detect(Crop.Name,"NURSERY ") &
                         FALSE == str_detect(Crop.Name,"FOREST ") &
                         FALSE == str_detect(Crop.Name,"FLOWERS ") &
                         FALSE == str_detect(Crop.Name,"PASTURE ") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS ") &
                         FALSE == str_detect(Crop.Name,"BIRDS ") &
                         FALSE == str_detect(Crop.Name,"GOATS ") &
                         FALSE == str_detect(Crop.Name,"OSTRICH ") &
                         FALSE == str_detect(Crop.Name,"BIOMASS "))

#2019 DATA
ag2019=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2019cropyear.csv")
ag2019_cntcrops=subset(ag2019,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
                         FALSE == str_detect(Crop.Name,"APIARY ") &
                         FALSE == str_detect(Crop.Name,"CATTLE ") &
                         FALSE == str_detect(Crop.Name,"FISH ") &
                         FALSE == str_detect(Crop.Name,"HOGS ") &
                         FALSE == str_detect(Crop.Name,"CHICKENS ") &
                         FALSE == str_detect(Crop.Name,"EGGS ") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK ") &
                         FALSE == str_detect(Crop.Name,"MANURE ") &
                         FALSE == str_detect(Crop.Name,"MILK ") &
                         FALSE == str_detect(Crop.Name,"POULTRY ") &
                         FALSE == str_detect(Crop.Name,"SHEEP ") &
                         FALSE == str_detect(Crop.Name,"TURKEYS ") &
                         FALSE == str_detect(Crop.Name,"WOOL ") &
                         FALSE == str_detect(Crop.Name,"SILAGE ") &
                         FALSE == str_detect(Crop.Name,"NURSERY ") &
                         FALSE == str_detect(Crop.Name,"FOREST ") &
                         FALSE == str_detect(Crop.Name,"FLOWERS ") &
                         FALSE == str_detect(Crop.Name,"PASTURE ") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS ") &
                         FALSE == str_detect(Crop.Name,"BIRDS ") &
                         FALSE == str_detect(Crop.Name,"GOATS ") &
                         FALSE == str_detect(Crop.Name,"OSTRICH ") &
                         FALSE == str_detect(Crop.Name,"BIOMASS "))
#2020 DATA
ag2020=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2020cropyear.csv")
ag2020_cntcrops=subset(ag2020,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
                         FALSE == str_detect(Crop.Name,"APIARY ") &
                         FALSE == str_detect(Crop.Name,"CATTLE ") &
                         FALSE == str_detect(Crop.Name,"FISH ") &
                         FALSE == str_detect(Crop.Name,"HOGS ") &
                         FALSE == str_detect(Crop.Name,"CHICKENS ") &
                         FALSE == str_detect(Crop.Name,"EGGS ") &
                         FALSE == str_detect(Crop.Name,"LIVESTOCK ") &
                         FALSE == str_detect(Crop.Name,"MANURE ") &
                         FALSE == str_detect(Crop.Name,"MILK ") &
                         FALSE == str_detect(Crop.Name,"POULTRY ") &
                         FALSE == str_detect(Crop.Name,"SHEEP ") &
                         FALSE == str_detect(Crop.Name,"TURKEYS ") &
                         FALSE == str_detect(Crop.Name,"WOOL ") &
                         FALSE == str_detect(Crop.Name,"SILAGE ") &
                         FALSE == str_detect(Crop.Name,"NURSERY ") &
                         FALSE == str_detect(Crop.Name,"FOREST ") &
                         FALSE == str_detect(Crop.Name,"FLOWERS ") &
                         FALSE == str_detect(Crop.Name,"PASTURE ") &
                         FALSE == str_detect(Crop.Name,"CHRISTMAS ") &
                         FALSE == str_detect(Crop.Name,"BIRDS ") &
                         FALSE == str_detect(Crop.Name,"GOATS ") &
                         FALSE == str_detect(Crop.Name,"OSTRICH ") &
                         FALSE == str_detect(Crop.Name,"BIOMASS "))
ag2020_cntcrops=rename(ag2020_cntcrops,Yield=Yield..Unit.Acre.,Price.P.U=Price..Dollars.Unit.,Value=Value..Dollars.)


#Now combine all data frames.Some years have differing numbers of observations, so you'll want to use the binc_row function.


allag=bind_rows(ag2009_cntcrops,ag2010_cntcrops,ag2011_cntcrops,ag2011_cntcrops,ag2012_cntcrops,ag2013_cntcrops,ag2014_cntcrops,ag2015_cntcrops,ag2016_cntcrops,ag2017_cntcrops,ag2018_cntcrops,ag2019_cntcrops,ag2020_cntcrops)

#Create some exploratory data plots & stats

yield_summary=print(summary(allag$Yield))
prod_summary=print(summary(allag$Production))
value_summary=print(summary(allag$Value))
harvest_summary=print(summary(allag$Harvested.Acres))

#Create data frame that has columns Year, County, Total Value. Need to sum the value of all crops for each county. 
#What is the sum of all value in the county of Fresno for each year (2009-2020)?
#use 'group_by' function? How to group by County and Year?
#NEED TO RENAME VARIABLES TO REMOVE SPACE E.G. COUNTY NAME

newdataframe <- allag %>%
  group_by(County&Year) %>%
  summarize(sum = sum(Value))

group_by(Year) %>%







