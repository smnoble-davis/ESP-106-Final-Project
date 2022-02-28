#read in ag crop production data files 2009-2020
library(tidyverse)
library("stringr")

#Set Directory so that if you gave one folder with all your data, someone could read in each data set by only chaning the set directory at the top.

#For all data sets for years 2009-2020, write subset function so we only see data for the 5 counties of interest: Fresno, Kern, Monterey, Merced and Tulare.
#Remove crops that aren't produce or feed (e.g. hay) by finding all crops that contain the words: apiary, cattle, fish, hogs, chickens, eggs, livestock, manure, milk, poultry, sheep, turkeys, wool, flowers, forest products, nursery products, pasture, silage, ostrich,birds,goats (str_detect from stringr package)
#Notice that some data sets have a space at the end of the County name and crop names, while some data sets do not. 

#2009 DATA
ag2009=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2009cropyear.csv")

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
ag2009_cntcrops$County[ag2009_cntcrops$County == "Kern "]="Kern"
ag2009_cntcrops$County[ag2009_cntcrops$County == "Fresno "]="Fresno"
ag2009_cntcrops$County[ag2009_cntcrops$County == "Tulare "]="Tulare"
ag2009_cntcrops$County[ag2009_cntcrops$County == "Monterey "]="Monterey"
ag2009_cntcrops$County[ag2009_cntcrops$County == "Merced "]="Merced"


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
ag2010_cntcrops$County[ag2010_cntcrops$County == "Kern "]="Kern"
ag2010_cntcrops$County[ag2010_cntcrops$County == "Fresno "]="Fresno"
ag2010_cntcrops$County[ag2010_cntcrops$County == "Tulare "]="Tulare"
ag2010_cntcrops$County[ag2010_cntcrops$County == "Monterey "]="Monterey"
ag2010_cntcrops$County[ag2010_cntcrops$County == "Merced "]="Merced"



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
ag2011_cntcrops$County[ag2011_cntcrops$County == "Kern "]="Kern"
ag2011_cntcrops$County[ag2011_cntcrops$County == "Fresno "]="Fresno"
ag2011_cntcrops$County[ag2011_cntcrops$County == "Tulare "]="Tulare"
ag2011_cntcrops$County[ag2011_cntcrops$County == "Monterey "]="Monterey"
ag2011_cntcrops$County[ag2011_cntcrops$County == "Merced "]="Merced"


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
ag2012_cntcrops$County[ag2012_cntcrops$County == "Kern "]="Kern"
ag2012_cntcrops$County[ag2012_cntcrops$County == "Fresno "]="Fresno"
ag2012_cntcrops$County[ag2012_cntcrops$County == "Tulare "]="Tulare"
ag2012_cntcrops$County[ag2012_cntcrops$County == "Monterey "]="Monterey"
ag2012_cntcrops$County[ag2012_cntcrops$County == "Merced "]="Merced"         

              

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
ag2017_cntcrops$County[ag2017_cntcrops$County == "Kern "]="Kern"
ag2017_cntcrops$County[ag2017_cntcrops$County == "Fresno "]="Fresno"
ag2017_cntcrops$County[ag2017_cntcrops$County == "Tulare "]="Tulare"
ag2017_cntcrops$County[ag2017_cntcrops$County == "Monterey "]="Monterey"
ag2017_cntcrops$County[ag2017_cntcrops$County == "Merced "]="Merced"


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

ag2018_cntcrops$County[ag2018_cntcrops$County == "Kern "]="Kern"
ag2018_cntcrops$County[ag2018_cntcrops$County == "Fresno "]="Fresno"
ag2018_cntcrops$County[ag2018_cntcrops$County == "Tulare "]="Tulare"
ag2018_cntcrops$County[ag2018_cntcrops$County == "Monterey "]="Monterey"
ag2018_cntcrops$County[ag2018_cntcrops$County == "Merced "]="Merced"



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
ag2019_cntcrops$County[ag2019_cntcrops$County == "Kern "]="Kern"
ag2019_cntcrops$County[ag2019_cntcrops$County == "Fresno "]="Fresno"
ag2019_cntcrops$County[ag2019_cntcrops$County == "Tulare "]="Tulare"
ag2019_cntcrops$County[ag2019_cntcrops$County == "Monterey "]="Monterey"
ag2019_cntcrops$County[ag2019_cntcrops$County == "Merced "]="Merced"


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
ag2020_cntcrops$County[ag2020_cntcrops$County == "Kern "]="Kern"
ag2020_cntcrops$County[ag2020_cntcrops$County == "Fresno "]="Fresno"
ag2020_cntcrops$County[ag2020_cntcrops$County == "Tulare "]="Tulare"
ag2020_cntcrops$County[ag2020_cntcrops$County == "Monterey "]="Monterey"
ag2020_cntcrops$County[ag2020_cntcrops$County == "Merced "]="Merced"

#Change the 2020 data set column names so it matches the column names of the other data sets
ag2020_cntcrops=rename(ag2020_cntcrops,Yield=Yield..Unit.Acre.,Price.P.U=Price..Dollars.Unit.,Value=Value..Dollars.)


#Now combine all data frames so you see all counties for every year 2009-2020.Some years have differing numbers of observations, so you'll want to use the bind_row function.

allag=bind_rows(ag2009_cntcrops,ag2010_cntcrops,ag2011_cntcrops,ag2011_cntcrops,ag2012_cntcrops,ag2013_cntcrops,ag2014_cntcrops,ag2015_cntcrops,ag2016_cntcrops,ag2017_cntcrops,ag2018_cntcrops,ag2019_cntcrops,ag2020_cntcrops)


#Create some exploratory data plots & stats of the new data frame

yield_summary=print(summary(allag$Yield))
prod_summary=print(summary(allag$Production))
value_summary=print(summary(allag$Value))
harvest_summary=print(summary(allag$Harvested.Acres))

#We're interested in the total value of production for each county each year. Create data frame that has columns Year, County, Total Value only. Need to sum the value of all crops for each county per year. 
#Use 'group_by' and 'summarize' functions in dplyr

county_yr_total=allag %>%
  group_by(County,Year) %>%
  summarize(Value_sum = sum(Value))

#Produce exploratory data for new dataframe
YrCounty_Valuesummary=print(summary(county_yr_total$Value_sum))

YrCountyBoxplot=print(
  ggplot(county_yr_total,aes(County,Value_sum))+
  geom_boxplot()+
  ylab("Total Production Value from 2009-2020 (US$)"))

#Per the boxplot, even though there are seemingly outliers in the data, we won't remove these values because we're not assuming this is due to miscalculation.
#These varying values are likely due to changes in environmental conditions.

#Plot the county production values over time
YrCountyLine=print(
  ggplot(county_yr_total,aes(Year,Value_sum,group=County,color=County))+
  geom_line()+
  scale_x_continuous(name="Production Year",breaks=c(2009:2020))+
  scale_y_continuous(name="Total Production Value (US$)"))

#NEED TO DO
#Per the rubric, best if you're not hard coding file path names, so set Directory so that if you gave one folder with all your data, someone could read in each data set by only chaning the set directory at the top.
#Double check County ag reports to see if the county production value is way off or not...
#use "trimws" function to remove space after County and Crop Name to avoid double counting...





