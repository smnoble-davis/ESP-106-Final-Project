#read in 2009 ag crop production data file
library(tidyverse)
library("stringr")

ag2009=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2009cropyear.csv")

#see how there's a "state totals" and "sum of others" rows at the end of every crop section.
#notice units are in tons or US$
#write subset function to remove rows that have value 'sum of others' and 'state totals' in the County column
#Remove non-crops: apiary, cattle, fish, hogs, chickens, eggs, livestock, manure, milk, poultry, sheep, turkeys, wool, flowers, forest products, nursery products, pasture, silage (str_detect from stringr package)
#sum total $ production for all crops
#Be sure to remove 'na' values when summing/plotting - unless situation where you'd want to show? 



ag2009_cntcrops=subset(ag2009,County%in%c("Fresno ","Kern ","Monterey ","Merced ","Tulare ") &
                      FALSE == str_detect(Crop.Name,"APIARY") &
                      FALSE == str_detect(Crop.Name,"CATTLE") &
                      FALSE == str_detect(Crop.Name,"FISH") &
                      FALSE == str_detect(Crop.Name,"HOGS") &
                      FALSE == str_detect(Crop.Name,"CHICKENS") &
                        FALSE == str_detect(Crop.Name,"EGG") &
                        FALSE == str_detect(Crop.Name,"LIVESTOCK") &
                        FALSE == str_detect(Crop.Name,"MANURE") &
                        FALSE == str_detect(Crop.Name,"MILK") &
                        FALSE == str_detect(Crop.Name,"POULTRY") &
                        FALSE == str_detect(Crop.Name,"SHEEP") &
                        FALSE == str_detect(Crop.Name,"TURKEY") &
                        FALSE == str_detect(Crop.Name,"WOOL") &
                        FALSE == str_detect(Crop.Name,"SILAGE") &
                        FALSE == str_detect(Crop.Name,"NURSERY") &
                        FALSE == str_detect(Crop.Name,"FOREST") &
                        FALSE == str_detect(Crop.Name,"FLOWER") &
                        FALSE == str_detect(Crop.Name,"PASTURE") &
                        FALSE == str_detect(Crop.Name,"CHRISTMAS"))
         

