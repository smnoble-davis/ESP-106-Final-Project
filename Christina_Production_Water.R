install.packages("corrplot")
library(corrplot)
library(lubridate)
library(tidyverse)

setwd("/Users/christinaharrington/Desktop/ESP-106-Final-Project")
load("Groundwater/apy_all.RData")
load("Ag Production/TotalProduction.Rdata")


#Merge the data frames
water_prod=merge(apy_all,county_yr_total,by=c("County","Year"))

#Drought = 2009, 2012-2016
prod_drought=water_prod %>%
  mutate(drought=ifelse(Year==2009| between(Year,2012,2016),1,0))

#change the drought variable from a numeric to a factor so it plots correctly
prod_drought$drought=as_factor(prod_drought$drought)

#Change the year to class 'date' using lubridate(). Add a column coding whether the year is a drought year (1) or not (0)
prod_drought$Year=ymd(prod_drought$Year,truncated=2)


#Run a correlation of groundwater depth, production value, and drought for all 5 counties aggregated. The drought variable is currently a factor, so convert to numeric.
prod_drought$drought=as.numeric(prod_drought$drought)

cor_counties=subset(prod_drought,select=-c(County,Year))
colnames(cor_counties)=c("Mean Depth","Prod. Value","Drought")
cor_countiesfinal=print(cor(cor_counties))
counties_corplot=corrplot(cor_countiesfinal,tl.col="black",tl.cex=1)
                          title("Counties Aggregated")

#Run a correlation for each of the 5 counties and plot them.

#Kern Correlations

kern_df=subset(prod_drought,County=="Kern")
kern_df=subset(kern_df,select=-c(County,Year))
colnames(kern_df)=c("Mean Depth","Prod. Value","Drought")
kern_cor=print(cor(kern_df))
kern_plot=corrplot(kern_cor,tl.col="black",tl.cex=1)
                   title("Kern County")

#Fresno Correlations

fresno_df=subset(prod_drought,County=="Fresno")
fresno_df=subset(fresno_df,select=-c(County,Year))
colnames(fresno_df)=c("Mean Depth","Prod. Value","Drought")
fresno_cor=print(cor(fresno_df))
fres_plot=corrplot(fresno_cor,tl.col="black",tl.cex=1)
         title("Fresno County")

#Merced Correlations

merced_df=subset(prod_drought,County=="Merced")
merced_df=subset(merced_df,select=-c(County,Year))
colnames(merced_df)=c("Mean Depth","Prod. Value","Drought")
merced_cor=print(cor(merced_df))
mer_plot=corrplot(merced_cor,tl.col="black",tl.cex=1)
         title("Merced County")


#Tulare Correlations

tulare_df=subset(prod_drought,County=="Tulare")
tulare_df=subset(tulare_df,select=-c(County,Year))
colnames(tulare_df)=c("Mean Depth","Prod. Value","Drought")
tulare_cor=print(cor(tulare_df))
tul_plot=corrplot(tulare_cor,tl.col="black",tl.cex=1)
         title("Tulare County")

#Monterey Correlations

monterey_df=subset(prod_drought,County=="Monterey")
monterey_df=subset(monterey_df,select=-c(County,Year))
colnames(monterey_df)=c("Mean Depth","Prod. Value","Drought")
monterey_cor=print(cor(monterey_df))
mont_plot=corrplot(monterey_cor,tl.col="black",tl.cex=1)
         title("Monterey County")
