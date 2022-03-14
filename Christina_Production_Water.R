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


#Run a correlation of groundwater depth, production value, and drought for all 5 counties
cor_all=print(cor(x=prod_drought$Avg_Depth,y=prod_drought$Value_sum))

prod_drought$drought=as.numeric(prod_drought$drought)
cor_counties=subset(prod_drought,select=-c(County,Year))
cor_countiesfinal=print(cor(cor_counties))

#The coefficient -0.35 indicates there's a negative relationship between groundwater depth and production value, but it's not a very strong relationship.
#This suggests that across all five counties, as the groundwater depth lowers, gross production value increases.

#Run a correlation for each of the 5 counties (Christina)

#Kern Correlations

kern_df=subset(prod_drought,County=="Kern")
kern_df=subset(kern_df,select=-c(County,Year))
kern_cor=print(cor(kern_df))

#Fresno Correlations

fresno_df=subset(prod_drought,County=="Fresno")
fresno_df=subset(fresno_df,select=-c(County,Year))
fresno_cor=print(cor(fresno_df))

#Merced Correlations

merced_df=subset(prod_drought,County=="Merced")
merced_df=subset(merced_df,select=-c(County,Year))
merced_cor=print(cor(merced_df))


#Tulare Correlations

tulare_df=subset(prod_drought,County=="Tulare")
tulare_df=subset(tulare_df,select=-c(County,Year))
tulare_cor=print(cor(tulare_df))

#Monterey Correlations

monterey_df=subset(prod_drought,County=="Monterey")
monterey_df=subset(monterey_df,select=-c(County,Year))
monterey_cor=print(cor(monterey_df))


################WHY NOT WORKING??####################

numeric_df=prod_drought%>% 
  mutate(drought = as.numeric(drought))%>%
  group_by(County)%>%
  select(Avg_Depth:drought)%>%
  summarise()


cor_all2=as_tibble(prod_drought) %>% 
  mutate(drought = as.numeric(drought)) %>% 
  group_by(County) %>%
  select(cor_all2,Avg_Depth:drought) %>% 
  summarize()%>% 
  ungroup



#how good was my model...use glance() - look for R2,adjusted R2,p values, F statistic
  


  
  
#Drought vs non-drought plots by county have fitted lines don't show interaction effects
#more valuable to report on the coefficient and standard error...avoid p values cuz 'trash stats for trash people'
#standard errors +/- 2 standard deviations
#coefficient values should be interpreted in light of the unit of the predictors and the range they spann

#Don't need to report on the coefficient values in the final paper but we do care about their sign and relative magnitude 
#R2 is correlated with correlation coefficients
