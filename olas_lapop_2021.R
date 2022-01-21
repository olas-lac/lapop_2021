# This script prepares the lapop water data set for inclusion in the OLAS database. 
# For dimensions we will have country, scope, quintile, gender
   # iso	
   # scope	
   # quintile	
   # gender
   # year

# It will also include average income per capita, max income per capita, average 
# income per household and max income per household for creation of graphics
   # ave_income_pc	
   # max_income_pc	
   # ave_hh_income	
   # max_hh_income	

# The water access related variables included in the household suvey data set will also
# be included here: 

  #  access_water_piped_house	
  #  access_water_piped_plot	
  #  access_water_other_min	
  #  access_water_other_max	
  #  access_water_daily	
  #  access_water_piped_house_daily	
  #  access_water_piped_plot_daily	
  #  access_water	

# The sanitaiton access related variables included in the household suvey data set will also
# be included here:
  
  #  access_san_exclusive	
  #  access_sewer	
  #  access_septic	
  #  access_latrine_min	
  #  access_latrine_max	
  #  access_sewer_exclusive	
  #  access_septic_exclusive	
  #  access_latrine_exclusive_min	
  #  access_latrine_exclusive_max	
  #  hygiene_defecation	
  #  access_latrines_unimproved_min	
  #  access_latrines_unimproved_exclusive_min	

# The following additional variables will be added for sanitation: 
  # sewage_treated : Sewage goes to a treatment plant
  # sewage_contaminates : Sewage goes into environment/contaminates water body or soil

# Water cost information will also be added:
  # mwe_poi_pc : Monthly water expenditure per hh member as a percent of household income per capita.  

rm(list=ls())

# Install the following packages if not already installed: 
#install.packages("haven")
#install.packages("survey")
#install.packages("sjmisc")
#install.packages("sjlabelled")
#install.packages("tidyr")
#install.packages("dyplr")
#install.packages("ggplot2")
library(haven)
library(survey)
library(sjmisc)
library(sjlabelled)
library(tidyr)
library(dplyr)
library(ggplot2)


load("lapop_water.rda")

# NOTE for lapop cleaning: many monthly expenditure on bottled water = 977777, need to take out

# We correct the dummy variables for the svyby function, replacing NAs with 0 for the calculations. The original NA
# values are important to keep in the lapop data set. For cleanliness we work with a data set dedicated to processing.
lapop_processing <- lapop

lapop_processing$pipedmain_dw <- ifelse(is.na(lapop_processing$pipedmain_dw),0, lapop_processing$pipedmain_dw)
lapop_processing$pipedplot_dw <-ifelse(is.na(lapop_processing$pipedplot_dw),0, lapop_processing$pipedplot_dw)
lapop_processing$bottled <- ifelse(is.na(lapop_processing$bottled),0, lapop_processing$bottled)
lapop_processing$bottled_quality
#lapop_processing$psc1t1 ## add a field about psc1t1 weather tap water is treated by family
lapop_processing$psc9n_insufficient






lapop_2021<- svydesign(ids=~upm, strata=~strata, weights = ~weight1500, nest=TRUE, data=lapop_processing)


#wateraccess<-svytable(~iso3+pipedmain_dw, design=lapop_2021)
#piped_main_dw <- svymean(~iso3 + pipedmain_dw, design = lapop_2021)

b<-svyby(~pipedmain_dw+pipedplot_dw+piped_dw, ~iso3, design = lapop_2021, FUN =  svymean,keep.var = TRUE, na.rm.all= FALSE)

#  access_water_piped_house	
#  access_water_piped_plot	
#  access_water_other_min	
#  access_water_other_max	
#  access_water_daily	
#  access_water_piped_house_daily	
#  access_water_piped_plot_daily

# The water access related variables included in the household suvey data set will also
# be included here: 

#  access_water_piped_house	
#  access_water_piped_plot	
#  access_water_other_min	
#  access_water_other_max	
#  access_water_daily	
#  access_water_piped_house_daily	
#  access_water_piped_plot_daily	
#  access_water	


