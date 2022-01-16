#=============================================================================================================
# ISM646/ IAF605   Visualizations of the Great Recession December 2007 - June 2009
# Assignment 2       
# Due Date       4/6/2021    
#
#  Note to Professor Dr. Singh and TA:  The following Script also uses API Key 
#                                       from the American Community Survey website
#                                       for Geological Data.  For following key used for code is:
#       
#                                  census_api_key('84b9a04931a2937de9e353dfac85129ca4c477a0', install=TRUE)
#
#
#==============================================================================================================


library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
install.packages("broom")
library(readxl)
install.packages("stringi")
library(stringi)
install.packages("tidycensus")
library(tidycensus)
install.packages("tmap")
library(tmap)
library(tmaptools)
library(sf)
library(png)

#Set Home Directory

setwd("C:/Dev/ISM 646/Assignment2/")
census_api_key('84b9a04931a2937de9e353dfac85129ca4c477a0', install=TRUE)



# Import from the EXCEL spreadsheets and explore it.  Load ACS data through US Census API

ZORI_res <- read_excel("data/Condo_Market_Zillow.xlsx")
 
ZORI <- read_excel("data/Zip_ZORI_AllHomesPlusMultifamily_Smoothed.xls")


Subprime_Credit <- 
  read_excel("data/GeoFRED_Equifax_Subprime_Credit_Population_by_County_Percent.xls", skip = 1)

SNAP_Recipients <-
GeoFRED_SNAP_Benefits_Recipients_by_County_Persons <- read_excel("data/GeoFRED_SNAP_Benefits_Recipients_by_County_Persons.xls", skip = 1)
 
ACS_Data <- read_excel("data/2000 to 2017 Data.xls")
 

Poverty <-  read_excel("data/GeoFRED_Estimated_Percent_of_People_of_All_Ages_in_Poverty_by_County_Percent.xls", skip = 1)
 


ACS_2010 <- get_acs("state",  year=2010, variables="S1702_C02_001", output="tidy", geometry=TRUE) %>%
  select(-moe)

ACS_2011 <- get_acs("state", variables="S1702_C02_001", year=2011, output="tidy", geometry=TRUE) 
 
ACS_2012 <- get_acs("state", variables="S1702_C02_001", year=2012, output="tidy", geometry=TRUE) %>%
  select(-moe)
  
ACS_2013 <- get_acs("state", variables="S1702_C02_001", year=2013, output="tidy", geometry=TRUE) %>%
  select(-moe)

ACS_2014 <- get_acs("state", variables="S1702_C02_001", year=2014, output="tidy", geometry=TRUE) %>%
  select(-moe)

ACS_2015 <- get_acs("state", variables="S1702_C02_001", year=2015, output="tidy", geometry=TRUE) %>%
  select(-moe)

ACS_2016 <- get_acs("state", variables="S1702_C02_001", year=2016, output="tidy", geometry=TRUE) %>%
  select(-moe)

ACS_2017 <- get_acs("state", variables="S1702_C02_001", year=2017, output="tidy", geometry=TRUE) %>%
  select(-moe)



ACS_S1702_C01_001_Family <- get_acs("county", variables="S1702_C01_001", output="tidy", geometry=TRUE) %>%
  select(-moe)
  
ACS_S1702_C01_013_Household_Work <-  get_acs("county", variables="S1702_C01_013", output="tidy", geometry=TRUE) %>%
  select(-moe)

ACS_household_work <-  get_acs("county", variables="S1702_C01_013", output="tidy", geometry=TRUE) %>%
  select(-moe)

 ACS_Education <- get_acs("county", variables= c(nohighschool = "S1702_C01_018", 
                                                  highschool = "S1702_C01_019",
                                                 somecollege = "S1702_C01_020", 
                                                 collegeplus = "S1702_C01_021"),
                         output="tidy", geometry=TRUE) %>%
  select(-moe)


ACS_Poverty <- get_acs("county", variables="S1702_C02_001", output="tidy", geometry=TRUE) %>%
  select(-moe)
 

  

#========= Clean and Pivot Data =============================

SNAP_df <- SNAP_Recipients[-c(1,3)]
Subprime_df <- Subprime_Credit[-c(1,3)]
Poverty_df <- Poverty[-c(1,3)]
ZORI_df <- ZORI[-c(1,2,3)]

SNAP_df2 <- SNAP_df %>%
    pivot_longer(!"Region Name", names_to="Year", values_to="Number")

Subprime_df2 <- Subprime_df %>% 
    pivot_longer(!"Region Name", names_to="Year-Quarter",values_to="Percent")

Poverty_df2 <- Poverty_df %>%
    pivot_longer(!"Region Name", names_to="Year", values_to="Percent")

ZORI_df2 <- ZORI_df %>% 
    pivot_longer(!"MsaName", names_to="Year-Quarter", values_to="Value")

#====================== Remove Nulls =============================

Subprime_df2  <- Subprime_df2 %>%
  na.omit()

SNAP_df2 <- SNAP_df2 %>%
  na.omit()
  
Poverty_df2 <- Poverty_df2 %>%
  na.omit()

ACS_Data_df <- ACS_Data %>% 
   #spread(State, `House Delinquency Rate`) %>%
   filter(`Home Values` >= 0) %>%
   na.omit()

ZORI_df2 <- ZORI_df2 %>%
  na.omit()

#head(ACS_Data) 
#head(ACS_Data_df,10)
#head(Poverty_df2,10)
#head(Subprime_df2,10)
#head(SNAP_df2,10)

#=======================================================================

#========== Group and Organize Data ====================================


ACS_geo_2010 <- ACS_2010 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate))


ACS_geo_2011 <- ACS_2011 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate)) 

ACS_geo_2012 <- ACS_2012 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate)) 

ACS_geo_2013 <- ACS_2013 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate)) 

ACS_geo_2014 <- ACS_2014 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate)) 

ACS_geo_2015 <- ACS_2015 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate)) 

ACS_geo_2016 <- ACS_2016 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate))

ACS_geo_2017 <- ACS_2017 %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(variable=='S1702_C02_001') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate))

ACS_Household_Work_guilford <- ACS_S1702_C01_013_Household_Work %>%
  select ('GEOID','NAME','estimate','geometry') %>%
  filter(str_detect(NAME, ", North Carolina"))
 

ACS_SSI <- ACS_S1702_C01_016_SSI %>%
  select ('GEOID','NAME','estimate','geometry') %>%
  filter(str_detect(NAME, ", North Carolina"))

ACS_Education_NC <- ACS_Education %>%
   select('GEOID','NAME','estimate','variable','geometry') %>%
   filter(str_detect(NAME, ", North Carolina"))


ACS_poverty_NC <- ACS_Poverty %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(str_detect(NAME, ", North Carolina"))
 
ACS_household_work_nc <- ACS_household_work %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  filter(str_detect(NAME, ", North Carolina"))


head(ACS_Education_NC)
head(ACS)

write.csv(ACS,'data/ACS.csv',row.names=TRUE)
 
head(ACS_Household_Work_guilford)

monthyear=("2021-01-01")

substr(monthyear,1,8)

SNAP_NC <- SNAP_df2 %>%
  select('Region Name','Year','Number') %>%
  filter(str_detect(`Region Name`,", NC")) %>%
  select('Year','Number') %>%
  group_by(`Year`)


SNAP_MA <- SNAP_df2 %>%
  select('Region Name','Year','Number') %>%
  filter(str_detect(`Region Name`,", MA")) %>%
  select('Year','Number') %>%
  group_by(`Year`)

SNAP_FL <- SNAP_df2 %>%
  select('Region Name','Year','Number') %>%
  filter(str_detect(`Region Name`,", FL")) %>%
  select('Year','Number') %>%
  group_by(`Year`)

SNAP_CA <- SNAP_df2 %>%
  select('Region Name','Year','Number') %>%
  filter(str_detect(`Region Name`,", CA")) %>%
  select('Year','Number') %>%
  group_by(`Year`)

Subprime_CA <- Subprime_df2 %>%
  select('Region Name','Year-Quarter','Percent') %>%
  filter(str_detect(`Region Name`,", CA")) %>% 
  select('Year-Quarter','Percent') %>%
  group_by(`Year-Quarter`)

Subprime_MA <- Subprime_df2 %>%
  select('Region Name','Year-Quarter','Percent') %>%
  filter(str_detect(`Region Name`,", MA")) %>% 
  select('Year-Quarter','Percent') %>%
  group_by(`Year-Quarter`)

Subprime_NC <- Subprime_df2 %>%
  select('Region Name','Year-Quarter','Percent') %>%
  filter(str_detect(`Region Name`,", NC")) %>% 
  select('Year-Quarter','Percent') %>%
  group_by(`Year-Quarter`)

Subprime_FL <- Subprime_df2 %>%
  select('Region Name','Year-Quarter','Percent') %>%
  filter(str_detect(`Region Name`,", FL")) %>% 
  select('Year-Quarter','Percent') %>%
  group_by(`Year-Quarter`)

Poverty_NC <- Poverty_df2 %>%
  select('Region Name','Year','Percent') %>%
  filter(str_detect(`Region Name`,", NC")) %>%
  select('Year','Percent') %>%
  group_by(`Year`)

Poverty_MA <- Poverty_df2 %>%
  select('Region Name','Year','Percent') %>%
  filter(str_detect(`Region Name`,", MA")) %>%
  select('Year','Percent') %>%
  group_by(`Year`)

Poverty_FL <- Poverty_df2 %>%
  select('Region Name','Year','Percent') %>%
  filter(str_detect(`Region Name`,", FL")) %>%
  select('Year','Percent') %>%
  group_by(`Year`)

Poverty_CA <- Poverty_df2 %>%
  select('Region Name','Year','Percent') %>%
  filter(str_detect(`Region Name`,", CA")) %>%
  select('Year','Percent') %>%
  group_by(`Year`)


ACS_Data_Housing <- ACS_Data %>%
  select('Home Values','Household Income','Bankruptcies','Percent Homeownership','Percent People in Poverty','State','Year') %>%
  filter(State %in% c("North Carolina","Massachusetts","Florida","California")) %>%
  group_by(`Year`)

ACS_Data_Stock_Market <- ACS_Data %>%
  select('Home Values','GDP','Dow 30 Closing Price','State','Year') %>%
  group_by(`Year`)
 
ZORI_df2 <- ZORI_df2 %>%
  filter(str_detect(`MsaName`,", NC") | str_detect(`MsaName`,", MA") | str_detect(`MsaName`,", FL") | str_detect(`MsaName`,", CA") ) %>%
  group_by(`Year-Quarter`)


#ACS_Data_mean <- ACS_Data %>% summarise(`Home Values` = mean(`Home Values`))  

Subprime_NC <- Subprime_NC %>%  summarise(Percent =  mean(Percent))

Subprime_MA <- Subprime_MA %>%  summarise(Percent =  mean(Percent))

Subprime_CA <- Subprime_CA %>%  summarise(Percent =  mean(Percent))

Subprime_FL <- Subprime_FL %>%  summarise(Percent =  mean(Percent))


SNAP_NC <- SNAP_NC %>% summarise(Number = mean(Number))

SNAP_MA <- SNAP_MA %>% summarise(Number = mean(Number))

SNAP_CA <- SNAP_CA %>% summarise(Number = mean(Number))

SNAP_FL <- SNAP_FL %>% summarise(Number = mean(Number))

Poverty_NC <- Poverty_NC %>% summarise(Percent = mean(Percent))

Poverty_MA <- Poverty_MA %>% summarise(Percent = mean(Percent))

Poverty_CA <- Poverty_CA %>% summarise(Percent = mean(Percent))

Poverty_FL <- Poverty_FL %>% summarise(Percent = mean(Percent))


ZORI_df3 <- ZORI_df2


ZORI_df3 <- ZORI_df3 %>%
   mutate(MsaName=str_trim(str_sub(MsaName,nchar(MsaName)-2))) %>%
            group_by(`Year-Quarter`,`MsaName`)        

  
ZORI_mean <- ZORI_df3 %>%  summarise(Value =  mean(Value))

 
 #============================ Geographic Mapping ACS Data (requires ACS API Key) ==========================================


png(file="images/ACS_geo_2010.png")
tm_shape(ACS_geo_2010) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2010 Post-Recession", asp=1)
dev.off()

png(file="images/ACS_geo_2011.png")
tm_shape(ACS_geo_2011) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2011 Post-Recession", asp=1)
dev.off()

png(file="images/ACS_geo_2012.png")
tm_shape(ACS_geo_2012) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2012 Post-Recession", asp=1)
dev.off()

png(file="images/ACS_geo_2013.png")
tm_shape(ACS_geo_2013) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2013 Post-Recession", asp=1)
dev.off()

png(file="images/ACS_geo_2014.png")
tm_shape(ACS_geo_2014) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2014 Post-Recession", asp=1)
dev.off()

png(file="images/ACS_geo_2015.png")
tm_shape(ACS_geo_2015) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2015 Post-Recession", asp=1)
dev.off()

png(file="images/ACS_geo_2016.png")
tm_shape(ACS_geo_2016) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2016 Post-Recession", asp=1)
dev.off()

png(file="images/ACS_geo_2017.png")
tm_shape(ACS_geo_2017) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="% Poverty in U.S. Year: 2017 Post-Recession", asp=1)
dev.off()

 
tm_shape(ACS_Household_Work_guilford) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="Number of Households that work", asp=1)
 
tm_shape(ACS_SSI) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="Supplemental Security Income (SSI) and/or cash public assistance income in the past 12 months", asp=1)

tm_shape(ACS_poverty_NC) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="All families Percent below poverty level", asp=1)

tm_shape(ACS_poverty_NC) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="All families Percent below poverty level", asp=1)

tm_shape(ACS_household_work_nc) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="Number of Householders where all Memories Worked", asp=1)


#=========================== Graphs ============================================

#ggplot(Subprime_NC, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
#  geom_point(size=3) + 
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People Receiving Subprime Loans in NC") +
#  theme(legend.position = "none") +
#  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))

#ggplot(Subprime_MA, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
#  geom_point(size=3) + 
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People Receiving Subprime Loans in Massachusetts") +
#  theme(legend.position = "none") +
#  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))

#ggplot(Subprime_CA, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
#  geom_point(size=3) + 
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People Receiving Subprime Loans in California") +
#  theme(legend.position = "none") +
#  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))

#ggplot(Subprime_FL, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
#  geom_point(size=3) + 
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People Receiving Subprime Loans in Florida") +
#  theme(legend.position = "none") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))



#ggplot(SNAP_NC, aes(x = Year, y = Number, color = Number)) + 
#  geom_point(size=3) + geom_line() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Number of people receiving SNAP assistance in North Carolina")

#ggplot(SNAP_MA, aes(x = Year, y = Number, color = Number)) + 
#  geom_point(size=3) + geom_line() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Number of people receiving SNAP assistance in Massachusetts")
   
#ggplot(SNAP_CA, aes(x = Year, y = Number, color = Number)) + 
#  geom_point(size=3) + geom_line() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Number of people receiving SNAP assistance in California")

#ggplot(SNAP_FL, aes(x = Year, y = Number, color = Number)) + 
#  geom_point(size=3) + geom_line() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Number of people receiving SNAP assistance in Florida")


#ggplot(Poverty_NC, aes(x = Year, y = Percent, color=Percent)) + 
#  geom_point() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People living in Poverty in NC")
#
#ggplot(Poverty_MA, aes(x = Year, y = Percent, color=Percent)) + 
#  geom_point() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People living in Poverty in Massachusetts")

#ggplot(Poverty_CA, aes(x = Year, y = Percent, color=Percent)) + 
#  geom_point() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People living in Poverty in California")

#ggplot(Poverty_FL, aes(x = Year, y = Percent, color=Percent)) + 
#  geom_point() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Percent People living in Poverty in Florida")

#ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Home Values`, group=as.factor(`State`), color=as.factor(`State`))) +
#   geom_line() + geom_point() +
#  ylab("Home Values") +
#  labs("States")
  

#ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Bankruptcies`, group=as.factor(`State`), color=as.factor(`State`))) +
#  geom_line() + geom_point() +
#  ylab("Bankruptcies") +
#  labs("States")

#ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Percent People in Poverty`, group=as.factor(`State`), color=as.factor(`State`))) +
#  geom_line() + geom_point() +
#  ylab("Percent People in Poverty") +
#  labs("States")

#ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Household Income`, group=as.factor(`State`), color=as.factor(`State`))) +
#  geom_line() + geom_point() +
#  ylab("Household Income") +
#  labs("States")

#ggplot(data=ACS_Data_Stock_Market, aes(x=GDP, y=`Dow 30 Closing Price`, color=Year)) + 
#  geom_point() + 
#  labs(title = "Stock Market and GDP (2000-2017)", x="GDP", y="Dow 30 Closing Price")




#ggplot(ZORI_mean, aes(x= `Year-Quarter`, y = Value, color=MsaName)) + 
#  geom_point(size=2) +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Zillow Home Values") +
#  labs(title="Housing Market Recovery 2014-2017", color="State")
#  scale_x_discrete(limit = c("2014-01","2015-01","2016-01","2017-01","2018-01"))

#  ggplot(ZORI_res, aes(x= `Date`, y = Florida)) + 
#    geom_line() +
#    geom_smooth(method="lm", se=FALSE) +
#    ylab("Zillow Home Values 2007-2009 Florida") +
#    labs(title="Housing Market during recession 2007-2009", color="State")
#  scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
#)
  

#ggplot(ZORI_res, aes(x= `Date`, y = NorthCarolina)) + 
#  geom_line() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Zillow Home Values 2007-2009 North Carolina") +
#  labs(title="Housing Market during recession 2007-2009", color="State")
#scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
#)

#ggplot(ZORI_res, aes(x= `Date`, y = California)) + 
#  geom_line() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Zillow Home Values 2007-2009 California") +
#  labs(title="Housing Market during recession 2007-2009", color="State")
#scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
#)

#ggplot(ZORI_res, aes(x= `Date`, y = Massachusetts)) + 
#  geom_line() +
#  geom_smooth(method="lm", se=FALSE) +
#  ylab("Zillow Home Values 2007-2009 Massachusetts") +
#  labs(title="Housing Market during recession 2007-2009", color="State")
#scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
#)




#===============================================================================


