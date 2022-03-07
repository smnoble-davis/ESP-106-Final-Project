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

#change the drought variable from a numeric to a factor
prod_drought$drought=as_factor(prod_drought$drought)

#Change the year to class 'date' using lubridate(). Add a column coding whether the year is a drought year (1) or not (0)
prod_drought$Year=ymd(prod_drought$Year,truncated=2)


#Plot the production value and groundwater levels aggregated for all 5 counties and group by year.
allcounties=print(
  ggplot(prod_drought,aes(Avg_Depth,Value_sum,group=Year,color=Year))+
  geom_point()+
  xlab("Mean Change in Depth (m)")+
  scale_y_continuous(name="Gross Production Value ($1000)",labels=scales::comma)+
  ggtitle("Change in Production Value and Groundwater Levels Per Year")+
  theme(plot.title = element_text(face="bold"))
)

LM=print(
  ggplot(prod_drought,aes(Avg_Depth,Value_sum))+
    geom_point()+
    geom_smooth(method="lm")+
    xlab("Mean Change in Depth (m)")+
    scale_y_continuous(name="Gross Production Value ($1000)",labels=scales::comma)+
    ggtitle("Change in Production Value and Groundwater Levels for All Counties (2009-2020)")+
    theme(plot.title = element_text(face="bold"))
)


#Run a correlation of groundwater depth and production value for all 5 counties (Christina)
cor_all=print(cor(x=prod_drought$Avg_Depth,y=prod_drought$Value_sum))

#The coefficient -0.35 indicates there's a negative relationship between groundwater depth and production value, but it's not a very strong relationship.
#This suggests that across all five counties, as the groundwater depth lowers, gross production value increases.

#Plot each county's total value and groundwater levels.
#Consider adding lines between the points esp Kern
bycounty=print(
  ggplot(prod_drought,aes(Avg_Depth,Value_sum,group=drought,color=drought))+
  facet_wrap(~County)+
  geom_point()+
  xlab("Mean Change in Depth (m)")+
  scale_y_continuous(name="Gross Production Value ($1000)",labels=scales::comma)+
  ggtitle("Change in Production Value and Groundwater Levels")+
  theme(plot.title = element_text(face="bold"))
)

#Run a correlation for each of the 5 counties (Christina)
kern=prod_drought %>%
  filter(County=="Kern")

cor_kern=print(cor(x=kern$Avg_Depth,y=kern$Value_sum))

#The coefficient -0.59 indicates there's a stronger negative relationship between groundwater depth and production value in Kern County.

fresno=prod_drought %>%
  filter(County=="Fresno")
cor_fresno=print(cor(x=fresno$Avg_Depth,y=fresno$Value_sum))

#The coefficient -0.46 indicates there's a negative relationship, but it's weaker than Kern County and stronger than the counties aggregated.

merced=prod_drought %>%
  filter(County=="Merced")
cor_merced=print(cor(x=merced$Avg_Depth,y=merced$Value_sum))

#The coefficient -0.42 indicates there's a negative relationship, but it's weaker than Kern County, stronger than the counties aggregated, and about the same as Fresno County.

tulare=prod_drought %>%
  filter(County=="Tulare")
cor_tulare=print(cor(x=tulare$Avg_Depth,y=tulare$Value_sum))

#The coefficient .06 indicates there's a very weak positive relationship where as groundwater levels rise, production also increases.

monterey=prod_drought %>%
  filter(County=="Monterey")
cor_monterey=print(cor(x=monterey$Avg_Depth,y=monterey$Value_sum))

#The coefficient -.08 indicates there's a very weak negative relationship where as groundwater levels lower, production increases.


#Correlation between value and drought?
#Create boxplots for drought and non-drought



test=subset(prod_drought,select=-c(County,Year))




#Run LM for each county and interpret (Shaela)


#RUNNING A CORRELATION WITH DROUGHT VARIABLE?
#VISUALIZING THE MULTIVARIATE REGRESSION, POSSIBLE?
#SHOULD WE RUN A CORRELATION/LM FOR DROUGHT VS GROUNDWATER LEVEL? POSSIBLE SINCE ONE IS A CATEGORICAL/FACTOR VARIABLE?
  
  
#NOTES FROM MEETING WITH EVAN
#Our hypothesis is as groundwater drops, ag production value drops - depth should be on x axis
  #add column with code for whtehr it's drought or non-drought (using mutate function)
#How values change in drought vs wet periods - mutate(drought=ifelse(year>2011 & year<2015,1,0))
  #multiple regression: change in depth (ind), drought vs wet year (ind),((y=a+b1x1+b2x2))
  #interaction effects: y=a+b1x1+b2x2+b3x1*x2 (ie does drought have impact on value and ) --> lm(formula=y~x1*x2)
  #y=a+b1x1

#sometimes aggregating data can reverse the dirction - "Simpsons Paradox"
  #include in discussion: discuss alternatve explanations/hypotheses
  

#GOING ABOVE & BEYOND (not needed for this project but if we wanted to continue this in future): mixed effects models / heirarchaal models - lme4 package - if there are few data points (maybe sampling error)
  
  
  
