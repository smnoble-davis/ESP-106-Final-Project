install.packages("corrplot")
library(corrplot)
library(lubridate)
setwd("/Users/christinaharrington/Desktop/ESP-106-Final-Project")
load("Groundwater/apy_all.RData")
load("Ag Production/TotalProduction.Rdata")


#Merge the dataframes
water_prod=merge(apy_all,county_yr_total,by=c("County","Year"))

#Change the year to class 'date' using lubridate()



#Plot each county's total value and groundwater levels. clean up plot so years appear more discrete.
ggplot(water_prod,aes(Value_sum,Avg_Depth,group=Year,color=Year))+
  facet_wrap(~County)+
  geom_point()

#Run a correlation for each county (Christina)
cor(x=water_prod$Avg_Depth,y=water_prod$Value_sum)

#Run LM for each county and interpret (Shaela)


#Our hypothesis is as groundwater drops, ag production value drops
