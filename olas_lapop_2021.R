# This script prepares the lapop water data set for inclusion in the OLAS database. 
rm(list=ls())

library(haven)
library(survey)
library(sjmisc)
library(sjlabelled)
library(tidyr)
library(dplyr)
library(ggplot2)
library(foreign)
library(stringr)
library(plyr)
#statadata <- read.dta("Merged-LAPOP-AmericasBarometer-2021-v1.1-IDB_water.dta")
load("input_data/lapop_water.rda")
mean(is.na(lapop$idnum))
mean(is.na(lapop$uniq_id))
mean(is.na(lapop$q10newt))
mean(is.na(lapop$pais))

 lapop_not_na <- lapop[!is.na(lapop$psc1n),]
 
 

# year
 # lapop$year_survey <- format(lapop$fecha,"%Y")
  lapop$year_survey <- lapop$wave
  
  
  # For dimensions we will have country, scope, quintile, gender
         # iso	
      
         # scope	
      
      lapop$scope <- ifelse(lapop$urban == 1, "urban", "rural")
      mean(is.na(lapop$scope))
#      View(lapop[is.na(lapop$scope),])


      lapop$scope <- ifelse(is.na(lapop$scope), "no scope data", lapop$scope)
      
      
         # quintile	
      
      lapop$quintile <- as.numeric(str_sub(lapop$q10newt, start=-1))
      hist(lapop$quintile)
      lapop$quintile <- ifelse(is.na(lapop$quintile), "no income data",lapop$quintile)
      
      missing_income <- lapop %>%
        group_by(iso3) %>%
        summarise(incomena = mean(is.na(quintile)))
      
      
         # gender
      
      lapop$gender <- ifelse(lapop$female ==1, "female", "male")
      mean(is.na(lapop$gender))
      

# The water access related variables included in the household survey data set will also
# be included here: 
     
      
      lapop$access_water_piped_house <- ifelse(lapop$psc1n ==1, 1,0)      
      lapop$access_water_piped_plot <- ifelse(lapop$psc1n == 2, 1,0)
      lapop$water_trucked <- ifelse(lapop$psc1n ==13,1,0)
      
      # psc1n (main drinking water source), psc2n (main source for other purposes), psc2f1 (if other water sources are piped into house or plot)
      # psc1n are counted if piped to house, plot, trucked water or rainwater harvesting
      
      lapop$water_on_premises <- ifelse(lapop$psc1n %in% c(1,2,10,13), 1,
                              ifelse(lapop$psc1n == lapop$psc2n & lapop$psc1n %in% c(4,5,6,7,8,14) & lapop$psc2f1 %in% c(2,3),1,
                                     ifelse(is.na(lapop$psc1n),NA, 0)))
      
      
      # should not just be sum of piped to plot, piped to house and trucked, which is what the below does. 
      #data_on_prem <- select(lapop,access_water_piped_house,access_water_piped_plot, water_trucked)
      #data_on_prem$prem<- +(rowSums(data_on_prem, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_on_prem)) == 0) > 0)
      #lapop$water_on_premises <-data_on_prem$prem
      
      
      ## testing by taking out is na clause 
      
      lapop$water_distr <- ifelse(lapop$piped_dw ==1 | lapop$piped_ou ==1,1,0)
      lapop$consume_distr <- ifelse(lapop$psc1n %in% c(1,2),1,
                                   ifelse(is.na(lapop$psc1n), NA,0))
      lapop$consume_other <- ifelse(lapop$psc1n %in% c(3,4,5,6,8,10,11,13,15),1,
                                   ifelse(is.na(lapop$psc1n),NA,0))
      
 
      
      lapop$consume_bottled <- ifelse(lapop$psc1n %in% c(11,15),1,
                                    ifelse(is.na(lapop$psc1n),NA,0))
      lapop$consume_unimproved <- ifelse(lapop$psc1n %in% c(7,14,15),1,0)
      

      improved_w_access <- select(lapop,consume_distr,consume_other)
      improved_w_access$improved<- +(rowSums(improved_w_access, na.rm = TRUE) * NA ^ (rowSums(!is.na(improved_w_access)) == 0) > 0)
      lapop$improved_w_access <-improved_w_access$improved
      
      
      test<-lapop %>%
        group_by(iso3) %>%
        summarise(consume_distr= weighted.mean(consume_distr, w=weight1500, na.rm =T),
                  consume_other= weighted.mean(consume_other, w=weight1500, na.rm =T),
                  improved_w_access= weighted.mean(improved_w_access, w=weight1500, na.rm =T))
      
      lapop$water_daily <- ifelse(lapop$psc9n == 2,1,
                                  ifelse(is.na(lapop$psc9n), NA,
                                         ifelse(lapop$psc9n ==1,0,0)))

      
      lapop$water_dist_daily <- ifelse((lapop$psc1n %in% c(1,2) | lapop$psc2n %in% c(1,2)) & lapop$psc9n == 2,1,
                                       ifelse(is.na(lapop$psc9n), NA,
                                              ifelse((lapop$psc1n %in% c(1,2) | lapop$psc2n %in% c(1,2)) & lapop$psc9n ==1,0,NA))) ## asked to everyone... adjusted to just be the family has sufficient water
      
      lapop$access_water_piped_house_daily <- ifelse(lapop$access_water_piped_house ==1 & lapop$water_dist_daily ==1, 1,
                                                            ifelse(lapop$access_water_piped_house == 0 | lapop$water_dist_daily == 0, 0, NA))
      

      
      lapop$access_water_piped_plot_daily <- ifelse(lapop$access_water_piped_plot ==1 & lapop$water_dist_daily ==1, 1,
                                                    ifelse(lapop$access_water_piped_plot == 0 | lapop$water_dist_daily == 0, 0, NA ))

      lapop$w_treatment <- ifelse(lapop$psc1t1 == 1, 0,
                                  ifelse(is.na(lapop$psc1t1) & lapop$psc1n == 11,0,
                                         ifelse(lapop$psc1t1 %in% c(2,3,4,5,6,7,77), 1, NA)))
      
      
      lapop$w_dist_treatment <- ifelse(!lapop$psc1n %in% c(1,2), NA, 
                                  ifelse(lapop$psc1n %in% c(1,2) & lapop$psc1t1 == 1, 0,
                                         ifelse(lapop$psc1n %in% c(1,2) & lapop$psc1t1 %in% c(2,3,4,5,6,7,77), 1, NA)))
      
      lapop$treated_why_simplified <- ifelse(lapop$psc1t3 %in% c(1),1,
                                             ifelse(lapop$psc1t3 %in% c(2),2,
                                                    ifelse(lapop$psc1t3 %in% c(3),3,
                                                           ifelse(lapop$psc1t3 %in% c(4,5,6),4,
                                                                  ifelse(lapop$psc1t3 %in% c(7),5,
                                                                         ifelse(lapop$psc1t3 %in% c(8),6,
                                                                                ifelse(lapop$psc1t3 %in% c(77),77, NA)))))))
      
                                  
      lapop$reason_w_treatment_taste <- ifelse(lapop$treated_why_simplified %in% c(1),1,0)
      lapop$reason_w_treatment_color <- ifelse(lapop$treated_why_simplified %in% c(2),1,0)
      lapop$reason_w_treatment_quality <- ifelse(lapop$treated_why_simplified %in% c(3,4),1,0)
      lapop$reason_w_treatment_continuity <- ifelse(lapop$treated_why_simplified %in% c(5),1,0)

      
      #lapop$bottled_color
      #lapop$bottled_continuity
      #lapop$bottled_quality
      #lapop$bottled_custom
      #lapop$bottled_taste
      
      #lapop$piped_ou
      lapop$other_ou <- ifelse(lapop$psc2n %in% c(4,5,6,8,10,11,13,15),1,0)
      lapop$unimproved_ou <- ifelse(lapop$psc2n %in% c(7,14),1,0)
      
      lapop$we_piped_water <- lapop$psc2r1_usd
      lapop$we_bottled <- lapop$psc1c4_usd
      lapop$we_trucked <- lapop$psc2c1_usd
      #lapop$twe_usd 
      #lapop$twe_usd_pc
      lapop$satisfied_service <- ifelse(lapop$sd5new2 %in% c(1,2),1,
                                        ifelse(is.na(lapop$sd5new2), NA, 0))
      lapop$unsatisfied_service <- ifelse(lapop$sd5new2 %in% c(3,4),1,
                                          ifelse(is.na(lapop$sd5new2), NA,0))


# The sanitation access related variables included in the household survey data set will also
# be included here:
      lapop$access_san_exclusive <-  lapop$psc12n_exclusive
   #   lapop$access_sewer <- lapop$psc11n_t_sewer
      
     lapop$access_sewer <- ifelse(lapop$psc11n %in% c(1),1,
                               ifelse(is.na(lapop$psc11n),NA, 0))
     # lapop$access_septic <- lapop$psc11n_t_septic
      
      lapop$access_septic <- ifelse(lapop$psc11n %in% c(2),1,
                                   ifelse(is.na(lapop$psc11n),NA, 0))
      
      
      lapop$access_latrine <- ifelse(lapop$psc11n %in% c(5,7),1,
                                     ifelse(is.na(lapop$psc11n),NA,0))
      
      data_improved_san <- select(lapop, access_sewer,access_septic, access_latrine)
      data_improved_san$improved<- +(rowSums(data_improved_san, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san)) == 0) > 0)
      lapop$improved_san <-data_improved_san$improved
      
      
      lapop$access_sewer_exclusive <- ifelse(is.na(lapop$access_san_exclusive) & is.na(lapop$access_sewer),NA,
                                       ifelse(lapop$access_sewer == 1 & lapop$access_san_exclusive == 1, 1,0))
      
        
     lapop$access_septic_exclusive <- ifelse(is.na(lapop$access_san_exclusive) & is.na(lapop$access_septic),NA,
                                            ifelse(lapop$access_septic == 1 & lapop$access_san_exclusive == 1, 1,0))
    
    
     lapop$access_latrine_exclusive <- ifelse(is.na(lapop$access_san_exclusive) & is.na(lapop$access_latrine),NA,
                                           ifelse(lapop$access_latrine == 1 & lapop$access_san_exclusive == 1, 1,0))
      
      data_improved_san_ex <- select(lapop, access_sewer_exclusive,access_septic_exclusive, access_latrine_exclusive)
      data_improved_san$improved_ex<- +(rowSums(data_improved_san_ex, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san_ex)) == 0) > 0)
      lapop$improved_san_exclusive <-data_improved_san$improved_ex
      
      
      lapop$no_san_access <- lapop$psc12n_none
      
      lapop$open_def <- ifelse(lapop$psc12n %in% c(1,2),0,
                               ifelse(lapop$psc12n %in% c(3) & lapop$psc11an %in% c(1, 77), 0,
                                  ifelse(lapop$psc11an == 2, 1,NA)))
      
      
      lapop$access_latrines_unimproved <- ifelse(is.na(lapop$psc11n),NA,
                                                 ifelse(!lapop$psc11n %in% c(1,2,5,7),1, 0))
      

      lapop$access_latrines_unimproved_exclusive <- ifelse(lapop$improved_san_exclusive ==1,0,
                                                           ifelse(lapop$improved_san_exclusive ==0,1, NA))
      

      lapop$sewage_treated <- ifelse(is.na(lapop$psc15_treatment), 0, lapop$psc15_treatment) ## only asked to those with sewer connections
      lapop$sewage_dont_know <- ifelse(is.na(lapop$psc15),0,lapop$psc15) ## only asked to those with sewer connections
      lapop$sewage_contaminates <- ifelse(is.na(lapop$psc15_contaminates),0, lapop$psc15_contaminates)  ## only asked to those with sewer connections


      
# We correct the dummy variables for the svyby function, replacing NAs with 0 for the calculations. The original NA
# values are important to keep in the lapop data set. For cleanliness we work with a data set dedicated to processing.
lapop_processing <- select(lapop,pais,iso3, strata, upm, weight1500,income, scope, gender,quintile, 
                           twe_usd_pc, 
                           twe_usd,
                           access_water_piped_house,
                           access_water_piped_plot,water_trucked,
                           water_on_premises,
                           water_distr,
                           consume_distr,
                           consume_other,
                           consume_bottled,
                           improved_w_access,
                           water_daily,
                           water_dist_daily,
                           access_water_piped_house_daily,
                           access_water_piped_plot_daily,
                           w_treatment,
                           w_dist_treatment, 
                           reason_w_treatment_taste,
                           reason_w_treatment_color,
                           reason_w_treatment_quality,
                           reason_w_treatment_continuity,
                           bottled_color,
                           bottled_continuity,
                           bottled_quality,
                           bottled_custom,
                           bottled_taste,
                           piped_ou,
                           other_ou,
                           unimproved_ou,
                           we_piped_water,
                           we_bottled,
                           we_trucked,
                           twe_usd,
                           satisfied_service,
                           unsatisfied_service,
                           access_san_exclusive,                      
                           access_sewer,
                           access_septic, 
                           access_latrine,
                           improved_san,
                           access_sewer_exclusive,
                           access_septic_exclusive,
                           access_latrine_exclusive, 
                           improved_san_exclusive,
                           no_san_access,
                           open_def,
                           access_latrines_unimproved, 
                           access_latrines_unimproved_exclusive, 
                           sewage_treated,
                           sewage_dont_know,
                           sewage_contaminates,
                           uniq_id)



#lapop_processing$psc1t1 ## add a field about psc1t1 weather tap water is treated by family
#There are 4 Colombian records that do not have weights: test<-lapop_processing[is.na(lapop_processing$weight1500),]

lapop_processing<-lapop_processing[!is.na(lapop_processing$weight1500),]

library(srvyr)
lapop2021_sd <- as_survey_design(lapop_processing, 
                                 ids = upm,
                                 strata = strata,
                                 weight = weight1500,
                                 nest=TRUE)


dimensions <- list("iso3", c("iso3", "scope"), c("iso3","gender"), c("iso3","quintile"),  c("iso3","scope", "quintile"))
                   #, 
    #                              c("iso3","scope", "gender"), c("iso3","scope", "quintile"), c("iso3","gender","quintile"), 
     #                            c("iso3","scope", "gender", "quintile"))


#test <- lapop[!is.na(lapop$weight1500),]
#test$iws <- ifelse(is.na(test$psc1n), NA, 
 #                  ifelse(test$psc1n %in% c(1,2,3,4,5,6,8,10,11,12,13), 1,0))

#lapop2021_sd <- as_survey_design(test, 
 #                                ids = upm,
#                                 strata = strata,
#                                 weight = weight1500,
#                                 nest=TRUE)
#test2 <- test[test$iso3== "MEX",]  
#summary_d <- lapop2021_sd%>%                     
#  dplyr::group_by(iso3) %>%                            
#  srvyr::summarise(iws = survey_mean(iws, na.rm = T,  vartype = c("ci")))
                   #consume_other = survey_mean(consume_other, na.rm = T,  vartype = c("ci")),
                   #consume_bottled = survey_mean(consume_bottled, na.rm = T,  vartype = c("ci")),
                   #improved_w_access = survey_mean(improved_w_access, na.rm = T,  vartype = c("ci")))
                   
for (i in 1:length(dimensions)){
  
  print(paste0("Dimension: ",dimensions[i]," ", "Num"," ", i, "/",length(dimensions))) 
  
  summary_d <- lapop2021_sd%>%                     
    dplyr::group_by_at(vars(one_of(dimensions[[i]]))) %>%                            
    srvyr::summarise(twe_usd_pc = survey_mean(twe_usd_pc, na.rm = T,  vartype = c("ci")), 
                     twe_usd = survey_mean(twe_usd, na.rm = T,  vartype = c("ci")),
                     access_water_piped_house = survey_mean(access_water_piped_house, na.rm = T,  vartype = c("ci")),
                     access_water_piped_plot = survey_mean(access_water_piped_plot, na.rm = T,  vartype = c("ci")),
                     water_trucked = survey_mean(water_trucked, na.rm = T,  vartype = c("ci")),
                     water_on_premises = survey_mean(water_on_premises, na.rm = T,  vartype = c("ci")),
                     water_distr = survey_mean(water_distr, na.rm = T,  vartype = c("ci")),
                     consume_distr = survey_mean(consume_distr, na.rm = T,  vartype = c("ci")),
                     consume_other = survey_mean(consume_other, na.rm = T,  vartype = c("ci")),
                     consume_bottled = survey_mean(consume_bottled, na.rm = T,  vartype = c("ci")),
                     improved_w_access = survey_mean(improved_w_access, na.rm = T,  vartype = c("ci")),
                     water_daily = survey_mean(water_daily, na.rm = T,  vartype = c("ci")),
                     water_dist_daily = survey_mean(water_dist_daily, na.rm = T,  vartype = c("ci")),
                     access_water_piped_house_daily = survey_mean(access_water_piped_house_daily, na.rm = T,  vartype = c("ci")),
                     access_water_piped_plot_daily = survey_mean(access_water_piped_plot_daily, na.rm = T,  vartype = c("ci")),
                     w_dist_treatment = survey_mean(w_dist_treatment, na.rm = T,  vartype = c("ci")),
                     w_treatment = survey_mean(w_treatment, na.rm = T,  vartype = c("ci")),
                     reason_w_treatment_taste = survey_mean(reason_w_treatment_taste, na.rm = T,  vartype = c("ci")),
                     reason_w_treatment_color = survey_mean(reason_w_treatment_color, na.rm = T,  vartype = c("ci")),
                     reason_w_treatment_quality = survey_mean(reason_w_treatment_quality, na.rm = T,  vartype = c("ci")),
                     reason_w_treatment_continuity = survey_mean(reason_w_treatment_continuity, na.rm = T,  vartype = c("ci")),
                     bottled_color = survey_mean(bottled_color, na.rm = T,  vartype = c("ci")),
                     bottled_continuity = survey_mean(bottled_continuity, na.rm = T,  vartype = c("ci")),
                     bottled_quality = survey_mean(bottled_quality, na.rm = T,  vartype = c("ci")),
                     bottled_custom = survey_mean(bottled_custom, na.rm = T,  vartype = c("ci")),
                     bottled_taste = survey_mean(bottled_taste, na.rm = T,  vartype = c("ci")),
                     piped_ou= survey_mean(piped_ou, na.rm = T,  vartype = c("ci")),
                     other_ou= survey_mean(other_ou, na.rm = T,  vartype = c("ci")),
                     unimproved_ou= survey_mean(unimproved_ou, na.rm = T,  vartype = c("ci")),
                     we_piped_water= survey_mean(we_piped_water, na.rm = T,  vartype = c("ci")),
                     we_bottled= survey_mean(we_bottled, na.rm = T,  vartype = c("ci")),
                     we_trucked= survey_mean(we_trucked, na.rm = T,  vartype = c("ci")),
                     satisfied_service= survey_mean(satisfied_service, na.rm = T,  vartype = c("ci")),
                     unsatisfied_service= survey_mean(unsatisfied_service, na.rm = T,  vartype = c("ci")),
                     access_san_exclusive= survey_mean(access_san_exclusive, na.rm = T,  vartype = c("ci")),
                     access_sewer= survey_mean(access_sewer, na.rm = T,  vartype = c("ci")),
                     access_septic= survey_mean(access_septic, na.rm = T,  vartype = c("ci")),
                     access_latrine= survey_mean(access_latrine, na.rm = T,  vartype = c("ci")),
                     improved_san = survey_mean(improved_san, na.rm = T,  vartype = c("ci")),
                     access_sewer_exclusive= survey_mean(access_sewer_exclusive, na.rm = T,  vartype = c("ci")),
                     access_septic_exclusive= survey_mean(access_septic_exclusive, na.rm = T,  vartype = c("ci")),
                     access_latrine_exclusive= survey_mean(access_latrine_exclusive, na.rm = T,  vartype = c("ci")),
                     improved_san_exclusive = survey_mean(improved_san_exclusive, na.rm = T,  vartype = c("ci")),
                     no_san_access= survey_mean(no_san_access, na.rm = T,  vartype = c("ci")),
                     open_def= survey_mean(open_def, na.rm = T,  vartype = c("ci")),
                     access_latrines_unimproved= survey_mean(access_latrines_unimproved, na.rm = T,  vartype = c("ci")),
                     access_latrines_unimproved_exclusive= survey_mean(access_latrines_unimproved_exclusive, na.rm = T,  vartype = c("ci")),
                     sewage_treated= survey_mean(sewage_treated, na.rm = T,  vartype = c("ci")),
                     sewage_dont_know= survey_mean(sewage_dont_know, na.rm = T,  vartype = c("ci")),
                     sewage_contaminates= survey_mean(sewage_contaminates, na.rm = T,  vartype = c("ci")),
                     count_respondents = length(unique(uniq_id)))
  
  
  if(i == 1){
    summary <-summary_d                        # Create summary data frame if it doesnt exist
  }else{
    summary <-rbind.fill(summary, summary_d)        # append the current data frame
  }
  
}

lapop_summary<-summary


lapop_summary$quintile <- ifelse(is.na(lapop_summary$quintile), "total",lapop_summary$quintile)
lapop_summary$gender <- ifelse(is.na(lapop_summary$gender), "all", lapop_summary$gender)
lapop_summary$scope <- ifelse(is.na(lapop_summary$scope), "country", lapop_summary$scope)


## reorder columns

lapop_summary_filtered<-lapop_summary[lapop_summary$scope != "no scope data",]
lapop_summary_filtered<-lapop_summary_filtered[lapop_summary_filtered$quintile != "no income data",]


lapop_2021<- select(lapop_summary_filtered, 1,152:155, 2:151)


write.csv(lapop_2021, "output_data/lapop_2021.csv")
lapop_2019<-read.csv("output_data/lapop_2018_19.csv")


lapop_2021$year <- 2021
lapop_2019$year <- 2019
library(plyr)

lapop_19_21<- rbind.fill(lapop_2021, lapop_2019)

write.csv(lapop_19_21, "output_data/lapop_2019_2021.csv")
