#===================================================================================
# ISM646/ IAF605   Visualizations of the Great Recession December 2007 - June 2009
# Assignment 2       
# Due Date       4/6/2021    
#===================================================================================


library(tidyverse)
library(tidycensus)
library(tidyr)
library(ggplot2)
library(dplyr)
library(mapview)
library(sf)
install.packages("broom")
library(readxl)
install.packages("stringi")
library(stringi)
install.packages("tidycensus")
library(tidycensus)
install.packages("tmap")
library(tmap)
library(tmaptools)
library(png)
install.packages("imager")
library(imager)
install.packages("rgdal")
install.packages("rgeos")
install.packages("maptools")
install.packages("igraph")
library(rgdal)
library(rgeos)
library(maps)
library(MASS)
library(sf)
library(maps)
library(igraph)


#Set Home Directory

setwd("C:/Dev/Datasets/")


# Import from the EXCEL spreadsheets and explore it.  Load ACS data through US Census API

#ZORI_res <- read_excel("Condo_Market_Zillow.xlsx")
 
#ZORI <- read_excel("Zip_ZORI_AllHomesPlusMultifamily_Smoothed.xls")

census_api_key('84b9a04931a2937de9e353dfac85129ca4c477a0', install=TRUE)

variables <- load_variables(2019, "acs5", cache=TRUE)

summary(variables)


var1 <-  dplyr::filter(variables, grepl("INCOME",concept)) %>%
         group_by("label") 
var_label <-  unique(var1["label"])
var_concept <- unique(var1["concept"])var_list <- split(var_concept, seq(nrow(var_concept)))
var_string <- toString(var_concept)
writeLines(var_string)

  write.csv(var1, "census_data_income.csv")

vData <-  dplyr::filter(variables, grepl("B19001",name))
write.csv(vData,"census_data.csv")        


#var1 <-  variables %>%
#  select('name','label','concept') %>%
 

house_income_all <- get_acs(geography = "state",
              variables = c(TOTAL = "B19001_001",
                            LESS_10K = "B19001_002",
                            B10K_15K = "B19001_003",
                            B15K_20K = "B19001_004",
                            B20K_25K = "B19001_005",
                            B25K_30K = "B19001_006",
                            B30K_35K = "B19001_007",
                            B35K_40K = "B19001_008",
                            B40K_45K = "B19001_009",
                            B45K_50K = "B19001_010",
                            B50K_60K = "B19001_011",
                            B60K_75K = "B19001_012",
                            B75K_100K = "B19001_013",
                            B100K_125K = "B19001_014",
                            B125K_150K = "B19001_015",
                            B150K_200K = "B19001_016",
                            B200K_MORE = "B19001_017"),
              output = "tidy",
              geometry = TRUE,
              year = 2019) #%>%
       ## select(-moe)

house_income_white <- get_acs(geography = "state",
                            variables = c(TOTAL = "B19001_001",
                                          LESS_10K = "B19001A_002",
                                          B10K_15K = "B19001A_003",
                                          B15K_20K = "B19001A_004",
                                          B20K_25K = "B19001A_005",
                                          B25K_30K = "B19001A_006",
                                          B30K_35K = "B19001A_007",
                                          B35K_40K = "B19001A_008",
                                          B40K_45K = "B19001A_009",
                                          B45K_50K = "B19001A_010",
                                          B50K_60K = "B19001A_011",
                                          B60K_75K = "B19001A_012",
                                          B75K_100K = "B19001A_013",
                                          B100K_125K = "B19001A_014",
                                          B125K_150K = "B19001A_015",
                                          B150K_200K = "B19001A_016",
                                          B200K_MORE = "B19001A_017"),
                            output = "tidy",
                            geometry = TRUE,
                            year = 2019) #%>%
 # select(-moe)

house_income_black <- get_acs(geography = "state",
                              variables = c(TOTAL = "B19001_001",
                                            LESS_10K = "B19001B_002",
                                            B10K_15K = "B19001B_003",
                                            B15K_20K = "B19001B_004",
                                            B20K_25K = "B19001B_005",
                                            B25K_30K = "B19001B_006",
                                            B30K_35K = "B19001B_007",
                                            B35K_40K = "B19001B_008",
                                            B40K_45K = "B19001B_009",
                                            B45K_50K = "B19001B_010",
                                            B50K_60K = "B19001B_011",
                                            B60K_75K = "B19001B_012",
                                            B75K_100K = "B19001B_013",
                                            B100K_125K = "B19001B_014",
                                            B125K_150K = "B19001B_015",
                                            B150K_200K = "B19001B_016",
                                            B200K_MORE = "B19001B_017"),
                              output = "tidy",
                              geometry = TRUE,
                              year = 2019) # %>%
  #select(-moe)

house_income_native <- get_acs(geography = "state",
                              variables = c(TOTAL = "B19001_001",
                                            LESS_10K = "B19001C_002",
                                            B10K_15K = "B19001C_003",
                                            B15K_20K = "B19001C_004",
                                            B20K_25K = "B19001C_005",
                                            B25K_30K = "B19001C_006",
                                            B30K_35K = "B19001C_007",
                                            B35K_40K = "B19001C_008",
                                            B40K_45K = "B19001C_009",
                                            B45K_50K = "B19001C_010",
                                            B50K_60K = "B19001C_011",
                                            B60K_75K = "B19001C_012",
                                            B75K_100K = "B19001C_013",
                                            B100K_125K = "B19001C_014",
                                            B125K_150K = "B19001C_015",
                                            B150K_200K = "B19001C_016",
                                            B200K_MORE = "B19001C_017"),
                              output = "tidy",
                              geometry = TRUE,
                              year = 2019) # %>%
  # select(-moe)

house_income_asian <- get_acs(geography = "state",
                               variables = c(TOTAL = "B19001_001",
                                             LESS_10K = "B19001D_002",
                                             B10K_15K = "B19001D_003",
                                             B15K_20K = "B19001D_004",
                                             B20K_25K = "B19001D_005",
                                             B25K_30K = "B19001D_006",
                                             B30K_35K = "B19001D_007",
                                             B35K_40K = "B19001D_008",
                                             B40K_45K = "B19001D_009",
                                             B45K_50K = "B19001D_010",
                                             B50K_60K = "B19001D_011",
                                             B60K_75K = "B19001D_012",
                                             B75K_100K = "B19001D_013",
                                             B100K_125K = "B19001D_014",
                                             B125K_150K = "B19001D_015",
                                             B150K_200K = "B19001D_016",
                                             B200K_MORE = "B19001D_017"),
                               output = "tidy",
                               geometry = TRUE,
                               year = 2019) #%>%
   # select(-moe)

house_income_hi_pac <- get_acs(geography = "state",
                              variables = c(TOTAL = "B19001_001",
                                            LESS_10K = "B19001E_002",
                                            B10K_15K = "B19001E_003",
                                            B15K_20K = "B19001E_004",
                                            B20K_25K = "B19001E_005",
                                            B25K_30K = "B19001E_006",
                                            B30K_35K = "B19001E_007",
                                            B35K_40K = "B19001E_008",
                                            B40K_45K = "B19001E_009",
                                            B45K_50K = "B19001E_010",
                                            B50K_60K = "B19001E_011",
                                            B60K_75K = "B19001E_012",
                                            B75K_100K = "B19001E_013",
                                            B100K_125K = "B19001E_014",
                                            B125K_150K = "B19001E_015",
                                            B150K_200K = "B19001E_016",
                                            B200K_MORE = "B19001E_017"),
                              output = "tidy",
                              geometry = TRUE,
                              year = 2019) ##%>%
  ##select(-moe)

house_income_other <- get_acs(geography = "state",
                               variables = c(TOTAL = "B19001_001",
                                             LESS_10K = "B19001F_002",
                                             B10K_15K = "B19001F_003",
                                             B15K_20K = "B19001F_004",
                                             B20K_25K = "B19001F_005",
                                             B25K_30K = "B19001F_006",
                                             B30K_35K = "B19001F_007",
                                             B35K_40K = "B19001F_008",
                                             B40K_45K = "B19001F_009",
                                             B45K_50K = "B19001F_010",
                                             B50K_60K = "B19001F_011",
                                             B60K_75K = "B19001F_012",
                                             B75K_100K = "B19001F_013",
                                             B100K_125K = "B19001F_014",
                                             B125K_150K = "B19001F_015",
                                             B150K_200K = "B19001F_016",
                                             B200K_MORE = "B19001F_017"),
                               output = "tidy",
                               geometry = TRUE,
                               year = 2019) 

house_income_hispanic <- get_acs(geography = "state",
                              variables = c(TOTAL = "B19001_001",
                                            LESS_10K = "B19001I_002",
                                            B10K_15K = "B19001I_003",
                                            B15K_20K = "B19001I_004",
                                            B20K_25K = "B19001I_005",
                                            B25K_30K = "B19001I_006",
                                            B30K_35K = "B19001I_007",
                                            B35K_40K = "B19001I_008",
                                            B40K_45K = "B19001I_009",
                                            B45K_50K = "B19001I_010",
                                            B50K_60K = "B19001I_011",
                                            B60K_75K = "B19001I_012",
                                            B75K_100K = "B19001I_013",
                                            B100K_125K = "B19001I_014",
                                            B125K_150K = "B19001I_015",
                                            B150K_200K = "B19001I_016",
                                            B200K_MORE = "B19001I_017"),
                              output = "tidy",
                              geometry = TRUE,
                              year = 2019)  

 

  
#========= Clean and Pivot Data =============================

#SNAP_df <- SNAP_Recipients[-c(1,3)]
#Subprime_df <- Subprime_Credit[-c(1,3)]
#Poverty_df <- Poverty[-c(1,3)]
#ZORI_df <- ZORI[-c(1,2,3)]

house_income_hispanic <- house_income_hispanic[-c(1,2,4,5,6)]
Hispanic <- house_income_hispanic %>%
    pivot_longer(!"variable", names_to="income range", values_to="total")

 
ZORI_df2 <- ZORI_df %>% 
    pivot_longer(!"MsaName", names_to="Year-Quarter", values_to="Value")

#====================== Remove Nulls =============================

house_income_hispanic  <- house_income_hispanic %>%
  na.omit()

write.csv(house_income_hispanic,"house_income_hispanic.csv")        

#ACS_Data_df <- ACS_Data %>% 
#    filter(`Home Values` >= 0) %>%
#   na.omit()



#========================= Tableau Visualization Testing ===============================

usa_pop <- read.csv(header=TRUE, file="nst-est2019-popchg2010_2019.csv")
summary(usa_pop)
usa_pop_estimate <- usa_pop %>%
                   select(NAME, POPESTIMATE2019) 

house_income_all <- get_acs(geography = "state",
                            variables = c(TOTAL = "B19001_001",
                                          LESS_10K = "B19001_002",
                                          B10K_15K = "B19001_003",
                                          B15K_20K = "B19001_004",
                                          B20K_25K = "B19001_005",
                                          B25K_30K = "B19001_006",
                                          B30K_35K = "B19001_007",
                                          B35K_40K = "B19001_008",
                                          B40K_45K = "B19001_009",
                                          B45K_50K = "B19001_010",
                                          B50K_60K = "B19001_011",
                                          B60K_75K = "B19001_012",
                                          B75K_100K = "B19001_013",
                                          B100K_125K = "B19001_014",
                                          B125K_150K = "B19001_015",
                                          B150K_200K = "B19001_016",
                                          B200K_MORE = "B19001_017"),
                            output = "tidy",
                            geometry = TRUE,
                            year = 2019) %>%
  select(-moe)

house_income_pivot <- house_income_all[-c(1,2,4,5,6)]
summary(house_income_pivot)
summary(house_income_all)
summary(usa_pop_estimate)


house_income_merge <- merge(usa_pop_estimate, house_income_all, by = "NAME")

house_income_total <- house_income_merge %>%
                      filter(variable=="TOTAL") %>%
                      select(NAME, estimate)

house_income_merge <- merge(house_income_merge, house_income_total, by = "NAME")

house_income_percent <- house_income_merge %>%
                        mutate(estimate = ifelse(variable=="TOTAL",(estimate.x/POPESTIMATE2019)*100,(estimate.x/estimate.y)*100)) %>%
                        select(NAME, GEOID, variable, estimate, moe, geometry)

# usa_pop_percentage %>%
#  augment(newdata = wine_test) %>%
#  mutate(residual = quality - .fitted) %>%
#  mutate(sq_residual = residual^2) %>%
#  summarize(mse = mean(sq_residual)) %>%
#  summarize(rmse = sqrt(mse))



summary(house_income_all)
house_income_all
st_write(house_income_percent, "House_income2.shp")

house_income_2019 <- house_income_all %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate))

house_income_2019

tm_shape(house_income_2019) + tm_polygons("estimate")
dev.off()

jpeg(file="housing_2019.jpg")
tm_shape(house_income_2019, raster.warp = TRUE) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="Housing values 2019", asp=1)
dev.off()





#==================================================================================================

hispanic_2019 <- house_income_hispanic %>%
  select('GEOID','NAME','variable','estimate','geometry') %>%
  group_by(GEOID, NAME) %>%
  summarize(estimate = sum(estimate))



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

 
 #===============================================================================

#=========================== Graphs ============================================

ggplot(Subprime_NC, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
  geom_point(size=3) + 
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People Receiving Subprime Loans in NC") +
  theme(legend.position = "none") +
  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))

ggplot(Subprime_MA, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
  geom_point(size=3) + 
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People Receiving Subprime Loans in Massachusetts") +
  theme(legend.position = "none") +
  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))

ggplot(Subprime_CA, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
  geom_point(size=3) + 
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People Receiving Subprime Loans in California") +
  theme(legend.position = "none") +
  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))

ggplot(Subprime_FL, aes(x = `Year-Quarter`, y = Percent, color=`Percent`)) + 
  geom_point(size=3) + 
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People Receiving Subprime Loans in Florida") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(limit = c("2005 Q1","2005 Q2","2005 Q3","2005 Q4","2006 Q1","2006 Q2","2006 Q3","2006 Q4","2007 Q1","2007 Q2","2007 Q3","2007 Q4","2008 Q1","2008 Q2","2008 Q3","2008 Q5","2009 Q1","2009 Q2","2009 Q3","2009 Q4","2010 Q1"))



ggplot(SNAP_NC, aes(x = Year, y = Number, color = Number)) + 
  geom_point(size=3) + geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Number of people receiving SNAP assistance in North Carolina")

ggplot(SNAP_MA, aes(x = Year, y = Number, color = Number)) + 
  geom_point(size=3) + geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Number of people receiving SNAP assistance in Massachusetts")
   
ggplot(SNAP_CA, aes(x = Year, y = Number, color = Number)) + 
  geom_point(size=3) + geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Number of people receiving SNAP assistance in California")

ggplot(SNAP_FL, aes(x = Year, y = Number, color = Number)) + 
  geom_point(size=3) + geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Number of people receiving SNAP assistance in Florida")


ggplot(Poverty_NC, aes(x = Year, y = Percent, color=Percent)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People living in Poverty in NC")

ggplot(Poverty_MA, aes(x = Year, y = Percent, color=Percent)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People living in Poverty in Massachusetts")

ggplot(Poverty_CA, aes(x = Year, y = Percent, color=Percent)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People living in Poverty in California")

ggplot(Poverty_FL, aes(x = Year, y = Percent, color=Percent)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Percent People living in Poverty in Florida")

ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Home Values`, group=as.factor(`State`), color=as.factor(`State`))) +
   geom_line() + geom_point() +
  ylab("Home Values") +
  labs("States")
  

ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Bankruptcies`, group=as.factor(`State`), color=as.factor(`State`))) +
  geom_line() + geom_point() +
  ylab("Bankruptcies") +
  labs("States")

ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Percent People in Poverty`, group=as.factor(`State`), color=as.factor(`State`))) +
  geom_line() + geom_point() +
  ylab("Percent People in Poverty") +
  labs("States")

ggplot(data=ACS_Data_Housing, aes(x=Year, y=`Household Income`, group=as.factor(`State`), color=as.factor(`State`))) +
  geom_line() + geom_point() +
  ylab("Household Income") +
  labs("States")

ggplot(data=ACS_Data_Stock_Market, aes(x=GDP, y=`Dow 30 Closing Price`, color=Year)) + 
  geom_point() + 
  labs(title = "Stock Market and GDP (2000-2017)", x="GDP", y="Dow 30 Closing Price")




ggplot(ZORI_mean, aes(x= `Year-Quarter`, y = Value, color=MsaName)) + 
  geom_point(size=2) +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Zillow Home Values") +
  labs(title="Housing Market Recovery 2014-2017", color="State")
  scale_x_discrete(limit = c("2014-01","2015-01","2016-01","2017-01","2018-01"))

  ggplot(ZORI_res, aes(x= `Date`, y = Florida)) + 
    geom_line() +
    geom_smooth(method="lm", se=FALSE) +
    ylab("Zillow Home Values 2007-2009 Florida") +
    labs(title="Housing Market during recession 2007-2009", color="State")
  scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
)
  

ggplot(ZORI_res, aes(x= `Date`, y = NorthCarolina)) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Zillow Home Values 2007-2009 North Carolina") +
  labs(title="Housing Market during recession 2007-2009", color="State")
scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
)

ggplot(ZORI_res, aes(x= `Date`, y = California)) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Zillow Home Values 2007-2009 California") +
  labs(title="Housing Market during recession 2007-2009", color="State")
scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
)

ggplot(ZORI_res, aes(x= `Date`, y = Massachusetts)) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Zillow Home Values 2007-2009 Massachusetts") +
  labs(title="Housing Market during recession 2007-2009", color="State")
scale_x_discrete(limit = c("1/31/2007","2/28/2007","3/31/2007","4/30/2007","5/31/2007","6/30/2007","8/31/2007","7/31/2007","10/31/2007","9/30/2007","12/31/2007","11/30/2007","1/31/2008","2/29/2008","3/31/2008","4/30/2008","5/31/2008","6/30/2008","7/31/2008","8/31/2008","9/30/2008","10/31/2008","11/30/2008","12/31/2008","1/31/2009","2/28/2009","4/30/2009","3/31/2009","5/31/2009","6/30/2009","7/31/2009","8/31/2009","9/30/2009","10/31/2009","11/30/2009")
)


jpeg(file="ACS_geo_2010.jpg")
tm_shape(ACS_geo_2010) + tm_polygons("estimate") + tm_layout(title.position=c("left","top"), title="Poverty Levels in U.S. Post-Recessions", asp=1)
dev.off()

plot(load.image("ACS_geo_2010.jpg"), axes=FALSE)
dev.off()

tm_shape(house_income_hispanic) + tm_polygons("estimate")
dev.off()

writeOGR(house_income_hispanic, dsn="C:/Dev/Datasets/",layer="map", drive = "ESRI Shapefile")

tm_shape(ACS_geo_2012) + tm_polygons("estimate")

tm_shape(ACS_geo_2013) + tm_polygons("estimate")

tm_shape(ACS_geo_2014) + tm_polygons("estimate")

tm_shape(ACS_geo_2015) + tm_polygons("estimate")

tm_shape(ACS_geo_2016) + tm_polygons("estimate")

tm_shape(ACS_geo_2017) + tm_polygons("estimate")



#===============================================================================


#===================================================================
