
rm(list=ls())
load("LAPOP IDB 2018-9 (from merge).rda")
library(hutils)
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
lapop <-merge 



mean(is.na(lapop$idnum))
mean(is.na(lapop$uniq_id))
mean(is.na(lapop$q10new))
mean(is.na(lapop$pais))
mean(is.na(lapop$psc1))
mean(is.na(lapop$weight1500 ))

lapop <- lapop[!is.na(lapop$psc1n),]

iso <- tribble(~pais, ~iso3,
               "Mexico"	,"MEX",
               "Guatemala"	,"GTM",
               "El Salvador"	,"SLV",
               "Honduras"	,"HND",
               "Nicaragua"	,"NIC",
               "Costa Rica"	,"CRI",
               "Panama"	,"PAN",
               "Colombia"	,"COL",
               "Ecuador"	,"ECU",
               "Bolivia"	,"BOL",
               "Peru"	,"PER",
               "Paraguay"	,"PRY",
               "Chile"	,"CHL",
               "Uruguay"	,"URY",
               "Brazil"	,"BRA",
               "Venezuela"	,"VEN",
               "Argentina"	,"ARG",
               "Dominican Republic"	,"DOM",
               "Haiti"	,"HTI",
               "Jamaica"	,"JAM",
               "Guyana"	,"GUY",
               "Trinidad and Tobago"	,"TTO",
               "Belize"	,"BLZ",
               "Suriname"	,"SUR",
               "Bahamas"	,"BHS",
               "Barbados"	,"BRB")

lapop <- 
  lapop %>% 
  left_join(iso, by='pais')


lapop$iso3 <- labelled(c(lapop$iso3), 
                            label="ISO 3 Code")

# year
# lapop$year_survey <- format(lapop$fecha,"%Y")
lapop$year_survey <- lapop$wave


# For dimensions we will have country, scope, quintile, gender
# iso	

# scope	
mean(is.na(lapop$ur))
lapop$scope <- ifelse(lapop$ur == 1, "urban", "rural")
mean(is.na(lapop$scope))
#      View(lapop[is.na(lapop$scope),])

#no missing values
#lapop$scope <- ifelse(is.na(lapop$scope), "no scope data", lapop$scope)


# quintile	

lapop<-mutate_ntile(lapop, "q10new", n=5, weights = "weight1500", by = "iso3",
                    new.col = "quintile", character.only = TRUE, overwrite = TRUE,
                    check.na = FALSE)

mean(is.na(lapop$quintile))

# gender

lapop$gender <- ifelse(lapop$q1 ==1, "male", 
                       ifelse(lapop$q1 ==2, "female",NA ))
mean(is.na(lapop$gender))

lapop$gender <- ifelse(is.na(lapop$q1), "na value",lapop$gender)

# The water access related variables included in the household survey data set will also
# be included here: 


lapop$access_water_piped_house <- ifelse(lapop$psc1 ==1, 1,0)      
lapop$access_water_piped_plot <- ifelse(lapop$psc1 == 2, 1,0)
lapop$water_trucked <- ifelse(lapop$psc1 %in% c(12,13) ,1,0) ## included 12 (Carreta con tanque pequeño/tambor) in this, maybe remove

# psc1n (main drinking water source), psc2n (main source for other purposes), psc2f1 (if other water sources are piped into house or plot)
# psc1n are counted if piped to house, plot, trucked water or rainwater harvesting

lapop$water_on_premises <- ifelse(lapop$psc1 %in% c(1,2,10,12,13), 1,
                                #  ifelse(lapop$psc1 == lapop$psc2n & lapop$psc1 %in% c(3,4,5,6,7,8,14) & lapop$psc2f1 %in% c(2,3),1,
                                         ifelse(is.na(lapop$psc1),NA, 0))
    ### PSC2F1 doesnt exist in lapop 2018/19 so this variable is defined slightly differently, included 12? 

# should not just be sum of piped to plot, piped to house and trucked, which is what the below does. 


## testing by taking out is na clause 

lapop$water_distr <- ifelse(lapop$psc1 %in% c(1,2) | lapop$psc2 %in% c(1,2),1,0)
lapop$consume_distr <- ifelse(lapop$psc1 %in% c(1,2),1,
                              ifelse(is.na(lapop$psc1), NA,0))
lapop$consume_other <- ifelse(lapop$psc1 %in% c(3,4,5,6,8,10,11,12,13),1, #added 12, maybe take out
                              ifelse(is.na(lapop$psc1),NA,0))



lapop$consume_bottled <- ifelse(lapop$psc1 == 11,1,0)
lapop$consume_unimproved <- ifelse(lapop$psc1 %in% c(7,14),1,0)


improved_w_access <- select(lapop,consume_distr,consume_other)
improved_w_access$improved<- +(rowSums(improved_w_access, na.rm = TRUE) * NA ^ (rowSums(!is.na(improved_w_access)) == 0) > 0)
lapop$improved_w_access <-improved_w_access$improved


test<-lapop %>%
  group_by(iso3) %>%
  dplyr::summarise(consume_distr= weighted.mean(consume_distr, w=weight1500, na.rm =T),
            consume_other= weighted.mean(consume_other, w=weight1500, na.rm =T),
            improved_w_access= weighted.mean(improved_w_access, w=weight1500, na.rm =T))

lapop$water_daily <- ifelse(lapop$psc9 %in% c(0:15) ,1,
                            ifelse(is.na(lapop$psc9), NA,
                                   ifelse(lapop$psc9 %in% c(16:50),0,0))) ## This is not comparible -- lapop 2021 asks if they have sufficient water

## maybe delete water_daily
lapop$water_dist_daily <- ifelse((lapop$psc1 %in% c(1,2) | lapop$psc2 %in% c(1,2)) & lapop$psc9 %in% c(1:15),1,
                                 ifelse(is.na(lapop$psc9), NA,
                                        ifelse((lapop$psc1 %in% c(1,2) | lapop$psc2 %in% c(1,2)) & lapop$psc9 %in% c(16:50),0,NA))) ## LAPOP 2021 asks to everyone, LAPOP 2018 asks to only those with piped. water_daily may be redundant in this dataset 

lapop$access_water_piped_house_daily <- ifelse(lapop$access_water_piped_house ==1 & lapop$water_dist_daily ==1, 1,
                                               ifelse(lapop$access_water_piped_house == 0 | lapop$water_dist_daily == 0, 0, NA))



lapop$access_water_piped_plot_daily <- ifelse(lapop$access_water_piped_plot ==1 & lapop$water_dist_daily ==1, 1,
                                              ifelse(lapop$access_water_piped_plot == 0 | lapop$water_dist_daily == 0, 0, NA ))
# dont asked about treatment 
#lapop$w_treatment <- ifelse(lapop$psc1t1 == 1, 0,
 #                           ifelse(is.na(lapop$psc1t1) & lapop$psc1n == 11,0,
  #                                 ifelse(lapop$psc1t1 %in% c(2,3,4,5,6,7,77), 1, NA)))


#lapop$w_dist_treatment <- ifelse(!lapop$psc1n %in% c(1,2), NA, 
   #                              ifelse(lapop$psc1n %in% c(1,2) & lapop$psc1t1 == 1, 0,
    #                                    ifelse(lapop$psc1n %in% c(1,2) & lapop$psc1t1 %in% c(2,3,4,5,6,7,77), 1, NA)))



#lapop$reason_w_treatment_taste <- ifelse(lapop$treated_why_simplified %in% c(1),1,0)
#lapop$reason_w_treatment_color <- ifelse(lapop$treated_why_simplified %in% c(2),1,0)
#lapop$reason_w_treatment_quality <- ifelse(lapop$treated_why_simplified %in% c(3,4),1,0)
#lapop$reason_w_treatment_continuity <- ifelse(lapop$treated_why_simplified %in% c(5),1,0)
#lapop$reason_w_treatment_custom <- ifelse(lapop$treated_why_simplified %in% c(6),1,0)

#lapop$bottled_color
#lapop$bottled_continuity
#lapop$bottled_quality
#lapop$bottled_custom
#lapop$bottled_taste

lapop$piped_ou <- ifelse(lapop$psc2 %in% c(1,2),1,
                         ifelse(is.na(lapop$psc2),NA, 0))
lapop$other_ou <- ifelse(lapop$psc2 %in% c(4,5,6,8,10,11,12,13),1,0) ## added 12?
lapop$unimproved_ou <- ifelse(lapop$psc2 %in% c(7,14),1,0)


# Lapop 2020 does not ask about expenses or satisfaction
#lapop$we_piped_water <- lapop$psc2r1_usd
#lapop$we_bottled <- lapop$psc1c4_usd
#lapop$we_trucked <- lapop$psc2c1_usd
#lapop$twe_usd 
#lapop$twe_usd_pc
#lapop$satisfied_service <- ifelse(lapop$sd5new2 %in% c(1,2),1,0)
#lapop$unsatisfied_service <- ifelse(lapop$sd5new2 %in% c(3,4),1,0)


# The sanitation access related variables included in the household survey data set will also
# be included here:

lapop$access_san_exclusive <-  ifelse(lapop$psc12 == 1, 0,
                                      ifelse(lapop$psc12 ==2, 1, NA))


lapop$access_sewer <- ifelse(lapop$psc11 %in% c(1),1,
                             ifelse(is.na(lapop$psc11),NA, 0))


lapop$access_septic <- ifelse(lapop$psc11 %in% c(2),1,
                              ifelse(is.na(lapop$psc11),NA, 0))

## combo with psc11 and psc11a
lapop$access_latrine <- ifelse(lapop$psc11 %in% c(6) & lapop$psc11a%in% c(1,2,4) ,1,
                               ifelse(is.na(lapop$psc11),NA,0))  ## 6 is latrine in argentina survey but pozo not connected to anything in other countries 

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


lapop$no_san_access <- ifelse(lapop$psc11a %in% c(7),1,
                              ifelse(is.na(lapop$psc11a),NA,0))
## doesnt isolate open def
#lapop$open_def <- ifelse(lapop$psc11a %in% c(1,2),0,
#                         ifelse(lapop$psc12n %in% c(3) & lapop$psc11an %in% c(1, 77), 0,
 #                               ifelse(lapop$psc11an == 2, 1,NA)))


lapop$access_latrines_unimproved <- ifelse(is.na(lapop$psc11),NA,
                                           ifelse(lapop$psc11 %in% c(3,4,5) & lapop$psc11a %in% c(3,5,6,77),1, 0)) ## Maybe recheck this


lapop$access_latrines_unimproved_exclusive <- ifelse(lapop$improved_san_exclusive ==1,0,
                                                     ifelse(lapop$improved_san_exclusive ==0,1, NA))


#lapop$sewage_treated <- ifelse(is.na(lapop$psc15_treatment), 0, lapop$psc15_treatment) ## only asked to those with sewer connections
#lapop$sewage_dont_know <- ifelse(is.na(lapop$psc15),0,lapop$psc15) ## only asked to those with sewer connections
lapop$sewage_contaminates <- ifelse(lapop$psc11 %in% c(3)| lapop$psc11a %in% c(5,6,7),1,0) ## not compatible because 2019 only asks to those with sewer connections



# We correct the dummy variables for the svyby function, replacing NAs with 0 for the calculations. The original NA
# values are important to keep in the lapop data set. For cleanliness we work with a data set dedicated to processing.
lapop_processing <- select(lapop,pais,iso3, strata, upm, weight1500, scope, gender, quintile,
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
                           piped_ou,
                           other_ou,
                           unimproved_ou,
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
                           access_latrines_unimproved, 
                           access_latrines_unimproved_exclusive, 
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
    srvyr::summarise(access_water_piped_house = survey_mean(access_water_piped_house, na.rm = T,  vartype = c("ci")),
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
                     piped_ou= survey_mean(piped_ou, na.rm = T,  vartype = c("ci")),
                     other_ou= survey_mean(other_ou, na.rm = T,  vartype = c("ci")),
                     unimproved_ou= survey_mean(unimproved_ou, na.rm = T,  vartype = c("ci")),
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
                     access_latrines_unimproved= survey_mean(access_latrines_unimproved, na.rm = T,  vartype = c("ci")),
                     access_latrines_unimproved_exclusive= survey_mean(access_latrines_unimproved_exclusive, na.rm = T,  vartype = c("ci")),
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
lapop_summary <- lapop_summary[lapop_summary$gender != "na value",]
lapop_summary_filtered<-lapop_summary[lapop_summary$scope != "no scope data",]
#lapop_summary_filtered<-lapop_summary_filtered[lapop_summary_filtered$quintile != "no income data",]


lapop_2018_19<- select(lapop_summary_filtered, 1,90:92, 2:89)


write.csv(lapop_2018_19, "lapop_2018_19.csv", row.names = F)

save(lapop_2018_19, file ="lapop_2018_19.Rda")

