#read in 2009 ag crop production data file
install.packages("tidyverse")
library(tidyverse)

ag2009=read.csv("/Users/christinaharrington/Desktop/ESP-106-Final-Project/Ag Production/2009cropyear.csv")

#see how there's a "state totals" and "sum of others" rows at the end of every crop section.
#notice units are in tons or US$
#Maybe you could subset these rows to exlude those extraneous roww and then create sum for all crops
#Should we remove crops that don't typically use water eg. apiary products??
#Be sure to remove 'na' values when summing/plotting - unless situation where you'd want to show? 

ag2009_rev=subset(ag2009,subset=!(which(ag2009$County=="Sum of Others" & ag2009$County=="State Totals")))