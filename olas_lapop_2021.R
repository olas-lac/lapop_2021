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
library(tibble)
library(readxl)

options( scipen = 20 )

load("input_data/lapop_water.rda")
mean(is.na(lapop$idnum))
mean(is.na(lapop$uniq_id))
mean(is.na(lapop$q10newt))
mean(is.na(lapop$pais))

 lapop <- lapop[!is.na(lapop$psc1n),]
 lapop<-lapop%>%
   dplyr::rename(pais_no = pais)
 
 
 iso <- tribble(~pais, ~iso3,~pais_no,
                "Mexico"	,"MEX",1,
                "Guatemala"	,"GTM",2,
                "El Salvador"	,"SLV",3,
                "Honduras"	,"HND",4,
                "Nicaragua"	,"NIC",5,
                "Costa Rica"	,"CRI",6,
                "Panama"	,"PAN",7,
                "Colombia"	,"COL",8,
                "Ecuador"	,"ECU",9,
                "Bolivia"	,"BOL",10,
                "Peru"	,"PER",11,
                "Paraguay"	,"PRY",12,
                "Chile"	,"CHL",13,
                "Uruguay"	,"URY",14,
                "Brazil"	,"BRA",15,
                "Venezuela"	,"VEN",16,
                "Argentina"	,"ARG",17,
                "Dominican Republic"	,"DOM",21,
                "Haiti"	,"HTI",22,
                "Jamaica"	,"JAM",23,
                "Guyana"	,"GUY",24)
 
 lapop <- 
   lapop %>% 
   left_join(iso, by=c('pais_no'= "pais_no", "iso3" = "iso3"))

 
 lapop$iso3 <- labelled(c(lapop$iso3), 
                        label="ISO 3 Code")
 

 lapop_income<-read.csv("input_data/2021 Income Ranges.csv")

 xchange_rates<- read_xls("input_data/exchange_rates_wb.xls")
 # Convert income information: 
 xc <- select(xchange_rates, `Country Code` ,`2021` )
 
 xc2021<-filter(xc, xc$`Country Code` %in% iso$iso3)
 
 income<-left_join(lapop_income, xc2021, by = c("iso3" = "Country Code"))
 
 income$min <- as.numeric(income$min)
 income$max <- as.numeric(income$max)
 
 income$income_min_usd <- income$min/income$`2021`
 
 
 income$income_max_usd <- income$max/income$`2021`
 
 income2<-select(income,iso3, q10newt,income_min_usd, income_max_usd)
 
 lapop<-left_join(lapop, income2, by= c("iso3" = "iso3", "q10newt" = "q10newt" ))
 `%notin%` <- Negate(`%in%`)
 lapop<-lapop[lapop$country %notin% c( "Haiti","Jamaica"),]
 df <- data.frame(pais = unique(lapop$pais),
                  pais_number = 1:18)
 
 lapop_income<-left_join(lapop, df, by= c("pais" = "pais"))
 
 
 write_dta(
   lapop_income,
   "lapop_2021_income.dta",
   label = attr(data, "label")
 )
 
 
 
 
 ## Impute incomes in stata
 ## Add imputed incomes back in
 
 files<-list.files("stata_income_generation/")
 
 for(i in 1:length(files)){
   print(files[i])
   data<-read_dta(paste0("stata_income_generation/", files[i]))
   if(i == 1){
     df<-data
   } 
   if(i >1){
     df<-rbind(df, data)
   }
 }
 
 ggplot(df, aes(x = iinc))+
   geom_histogram()+
   xlim(0,10000)+
   facet_wrap(~iso3)
 max(df$iinc)
 
 lapop<-df[df$iinc<1000000,] # There is one household with an insane income (x10^32)
 
 lapop<-df
 
 save(lapop, file ="C:/Users/jesse/Desktop/Data Projects/survey_comparison/lapop_inputs/lapop_2020_21_demos.Rda") 

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
     
      
      lapop$access_water_piped_home <- ifelse(lapop$psc1n ==1, 1,0)      
      lapop$access_water_piped_plot <- ifelse(lapop$psc1n == 2, 1,0)
      # psc1n (main drinking water source), psc2n (main source for other purposes), psc2f1 (if other water sources are piped into house or plot)
      # psc1n are counted if piped to house, plot, trucked water or rainwater harvesting
      
      lapop$water_on_premises <- ifelse(lapop$psc2f1 %in% c(1,2), 1,
                              ifelse(lapop$psc1n == lapop$psc2n & lapop$psc1n %in% c(4,5,6,7,8,14) & lapop$psc2f1 %in% c(2,3),1,
                                     ifelse(is.na(lapop$psc1n),NA, 0)))
      
      
      # should not just be sum of piped to plot, piped to house and trucked, which is what the below does. 
      #data_on_prem <- select(lapop,access_water_piped_house,access_water_piped_plot, water_trucked)
      #data_on_prem$prem<- +(rowSums(data_on_prem, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_on_prem)) == 0) > 0)
      #lapop$water_on_premises <-data_on_prem$prem
      
      
    
      
      lapop$water_distr <- ifelse(lapop$psc1n %in% c(1,2) | lapop$psc2n %in% c(1,2),1,0)
      lapop$consume_distr <- ifelse(lapop$psc1n %in% c(1,2),1,
                                   ifelse(is.na(lapop$psc1n), NA,0))
      
      lapop$consume_delivered<- ifelse(lapop$psc1n %in% c(13),1,
                                       ifelse(is.na(lapop$psc1n),NA,0))
      
      lapop$consume_bottled <- ifelse(lapop$psc1n %in% c(11,15),1,
                                    ifelse(is.na(lapop$psc1n),NA,0))
      
      lapop$consume_impspring<- ifelse(lapop$psc1n %in% c(8),1,
                                       ifelse(is.na(lapop$psc1n),NA,0))
      
      lapop$consume_impwell<- ifelse(lapop$psc1n %in% c(5,6),1,
                                     ifelse(is.na(lapop$psc1n),NA,0))
    
      lapop$consume_other <- ifelse(lapop$psc1n %in% c(77),1,
                                    ifelse(is.na(lapop$psc1n),NA,0))
      
      lapop$consume_publictap<- ifelse(lapop$psc1n %in% c(4),1,
                                       ifelse(is.na(lapop$psc1n),NA,0))
      
      lapop$consume_rain<- ifelse(lapop$psc1n %in% c(10),1,
                                  ifelse(is.na(lapop$psc1n),NA,0))
      
      
      lapop$consume_unimproved <- ifelse(lapop$psc1n %in% c(7,14),1,
                                         ifelse(is.na(lapop$psc1n), NA,0))
      
      lapop$improved_w_access <- ifelse(lapop$psc1n %in% c(1,2,4,5,6,8,9,10,11,12,13,15),1,
                                        ifelse(is.na(lapop$psc1n), NA, 0))
      

      # improved_w_access <- select(lapop,consume_distr,consume_other)
      # improved_w_access$improved<- +(rowSums(improved_w_access, na.rm = TRUE) * NA ^ (rowSums(!is.na(improved_w_access)) == 0) > 0)
      # lapop$improved_w_access <-improved_w_access$improved
      
      
      lapop$water_daily <- ifelse(lapop$psc7n %in% c(4:7),1,
                                  ifelse(is.na(lapop$psc2n), NA,
                                         ifelse(lapop$psc7n %in% c(0:3)|lapop$psc2n >2,0,0)))

      
     # lapoptest<-select(lapop,iso3,water_daily, psc2n,psc7n)
      #      
      #       lapop2021_sd <- as_survey_design(lapoptest,
      #                                      ids = upm,
      #                                       strata = strata,
      #                                       weight = wt,
      #                                       nest=TRUE)
      #       test <- lapop2021_sd%>%
      #        dplyr::group_by(iso3) %>%
      #        srvyr::summarise(no_san_access = survey_mean(no_san_access, na.rm = T,  vartype = NULL),
      #                         san_sewer = survey_mean(san_sewer, na.rm = T,  vartype = NULL),
      #                         san_septic = survey_mean(san_septic, na.rm = T,  vartype = NULL),
      #                         san_implatrine = survey_mean(san_implatrine, na.rm = T,  vartype = NULL),
      #                         san_other = survey_mean(san_other, na.rm = T,  vartype = NULL),
      #                         san_unimp = survey_mean(san_unimp, na.rm = T,  vartype = NULL),
      #                         san_ecolatrine = survey_mean(san_ecolatrine, na.rm = T,  vartype = NULL),)
      # test$test <- test$no_san_access+test$san_sewer +test$san_septic +test$san_implatrine +test$san_other +test$san_unimp +test$san_ecolatrine
      
      
      
      
      # lapop$water_dist_daily <- ifelse((lapop$psc1n %in% c(1,2) | lapop$psc2n %in% c(1,2)) & lapop$psc9n == 2,1,
      #                                  ifelse(is.na(lapop$psc9n), NA,
      #                                         ifelse((lapop$psc1n %in% c(1,2) | lapop$psc2n %in% c(1,2)) & lapop$psc9n ==1,0,NA))) ## asked to everyone... adjusted to just be the family has sufficient water
      # 
      # lapop$access_water_piped_house_daily <- ifelse(lapop$access_water_piped_house ==1 & lapop$water_dist_daily ==1, 1,
      #                                                       ifelse(lapop$access_water_piped_house == 0 | lapop$water_dist_daily == 0, 0, NA))
      # 
# 
#       
#       lapop$access_water_piped_plot_daily <- ifelse(lapop$access_water_piped_plot ==1 & lapop$water_dist_daily ==1, 1,
#                                                     ifelse(lapop$access_water_piped_plot == 0 | lapop$water_dist_daily == 0, 0, NA ))

      lapop$w_treatment <- ifelse(lapop$psc1t1 == 1, 0,
                                  ifelse(is.na(lapop$psc1t1) & lapop$psc1n == 11,0,
                                         ifelse(lapop$psc1t1 %in% c(2,3,4,5,6,7,77), 1, NA)))
      
      
      lapop$w_dist_treatment <- ifelse(!lapop$psc1n %in% c(1,2), 0, 
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

    
      
      lapop$ou_bottled <- ifelse(lapop$psc2n %in% c(11,15),1,
                               ifelse(is.na(lapop$psc2n),NA,0))
      lapop$ou_delivered<- ifelse(lapop$psc2n %in% c(13),1,
                                  ifelse(is.na(lapop$psc2n),NA,0))
      lapop$ou_distr<- ifelse(lapop$psc2n %in% c(1,2),1,
                              ifelse(is.na(lapop$psc2n),NA,0))
      lapop$ou_impspring<- ifelse(lapop$psc2n %in% c(8),1,
                                  ifelse(is.na(lapop$psc2n),NA,0))
      lapop$ou_impwell<- ifelse(lapop$psc2n %in% c(5,6),1,
                                ifelse(is.na(lapop$psc2n),NA,0))
      
      lapop$ou_publictap<- ifelse(lapop$psc2n %in% c(4),1,
                                ifelse(is.na(lapop$psc2n),NA,0))
      lapop$ou_rain<- ifelse(lapop$psc2n %in% c(10),1,
                             ifelse(is.na(lapop$psc2n),NA,0))
      
      lapop$ou_other <- ifelse(lapop$psc2n %in% c(77,15),1,
                                  ifelse(is.na(lapop$psc2n),NA,0))
      lapop$ou_unimproved <- ifelse(lapop$psc2n %in% c(7,14),1,
                                      ifelse(is.na(lapop$psc2n),NA,0))
      
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
    #  lapop$access_san_exclusive <-  lapop$psc12n_exclusive
   #   lapop$access_sewer <- lapop$psc11n_t_sewer
      lapop$san_exclusive<- ifelse(lapop$psc12n %in% c(1), 1, 
                                   ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0))
      
      lapop$no_san_access <- ifelse(lapop$psc12n==3, 1, 
                                    ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0))
      
      lapop$san_sewer<- ifelse(lapop$psc11n %in% c(1), 1, 
                               ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0)) 
      
      lapop$san_septic <- ifelse(lapop$psc11n %in% c(2), 1, 
                                 ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0))  
      
      lapop$san_implatrine <- ifelse(lapop$psc11n %in% c(3,5), 1, 
                                     ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0))
      
      lapop$san_other <-ifelse(lapop$psc11n %in% c(4)| (is.na(lapop$psc11n)&lapop$psc12n %in% c(1,2)), 1, 
                               ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0))

      lapop$san_unimp<- ifelse(lapop$psc11n %in% c(6), 1, 
                               ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0))
      
      lapop$san_ecolatrine <- ifelse(lapop$psc11n %in% c(7), 1, 
                                    ifelse(is.na(lapop$psc12n)&is.na(lapop$psc11n),NA, 0))
#       lapop<-filter(lapop,!is.na(lapop$wt))
# 
#       lapoptest<-select(lapop,iso3,psc11n, psc12n,no_san_access,san_sewer,san_septic,san_implatrine,san_other,san_unimp,san_ecolatrine)
#       lapoptest$test <- lapoptest$no_san_access+lapoptest$san_sewer +lapoptest$san_septic +lapoptest$san_implatrine +lapoptest$san_other +lapoptest$san_unimp +lapoptest$san_ecolatrine
# 
#       lapop2021_sd <- as_survey_design(lapop,
#                                      ids = upm,
#                                       strata = strata,
#                                       weight = wt,
#                                       nest=TRUE)
#       test <- lapop2021_sd%>%
#        dplyr::group_by(iso3) %>%
#        srvyr::summarise(no_san_access = survey_mean(no_san_access, na.rm = T,  vartype = NULL),
#                         san_sewer = survey_mean(san_sewer, na.rm = T,  vartype = NULL),
#                         san_septic = survey_mean(san_septic, na.rm = T,  vartype = NULL),
#                         san_implatrine = survey_mean(san_implatrine, na.rm = T,  vartype = NULL),
#                         san_other = survey_mean(san_other, na.rm = T,  vartype = NULL),
#                         san_unimp = survey_mean(san_unimp, na.rm = T,  vartype = NULL),
#                         san_ecolatrine = survey_mean(san_ecolatrine, na.rm = T,  vartype = NULL),)
# test$test <- test$no_san_access+test$san_sewer +test$san_septic +test$san_implatrine +test$san_other +test$san_unimp +test$san_ecolatrine



     # lapop$sewage_contaminates<-ifelse(lapop$psc11n %in% c(6), 1, 
     #                                 ifelse(is.na(lapop$psc11n),NA, 0))
     
      data_improved_san <- select(lapop, san_sewer,san_septic, san_implatrine, san_ecolatrine)
      data_improved_san$improved<- +(rowSums(data_improved_san, na.rm = TRUE) * NA ^ (rowSums(!is.na(data_improved_san)) == 0) > 0)
      lapop$improved_san <-data_improved_san$improved
     
      lapop$open_def <- ifelse(lapop$psc12n %in% c(1,2),0,
                               ifelse(lapop$psc12n %in% c(3) & lapop$psc11an %in% c(1, 77), 0,
                                  ifelse(lapop$psc11an == 2, 1,NA)))
      
      
      lapop$sewage_treated <- ifelse(is.na(lapop$psc15_treatment), 0, lapop$psc15_treatment) ## only asked to those with sewer connections
     
      lapop$sewage_contaminates <- ifelse(is.na(lapop$psc15_contaminates),0, lapop$psc15_contaminates)  ## only asked to those with sewer connections


      
# We correct the dummy variables for the svyby function, replacing NAs with 0 for the calculations. The original NA
# values are important to keep in the lapop data set. For cleanliness we work with a data set dedicated to processing.
lapop_processing <- select(lapop,pais,iso3, strata, upm, wt,income, scope, gender,quintile, 
                           twe_usd_pc, 
                           twe_usd,
                           access_water_piped_home,
                           access_water_piped_plot,
                           water_on_premises,
                           water_distr,
                           consume_distr,
                           consume_other,
                           consume_bottled,
                           consume_delivered,
                           consume_impspring,
                           consume_impwell,
                           consume_publictap,
                           consume_rain,
                           consume_unimproved, 
                           ou_bottled ,
                           ou_delivered,
                           ou_distr,
                           ou_impspring,
                           ou_impwell,
                           ou_other ,
                           ou_publictap,
                           ou_rain,
                           ou_unimproved ,
                           improved_w_access,
                           water_daily,
                           #water_dist_daily,
                           #access_water_piped_house_daily,
                           #access_water_piped_plot_daily,
                           w_treatment,
                           w_dist_treatment, 
                           reason_w_treatment_taste,
                           reason_w_treatment_color,
                           reason_w_treatment_quality,
                           reason_w_treatment_continuity,
                           we_piped_water,
                           we_bottled,
                           we_trucked,
                           twe_usd,
                           satisfied_service,
                           unsatisfied_service,
                          #access_san_exclusive, 
                           san_ecolatrine, 
                           san_exclusive,
                           san_implatrine, 
                           san_other ,
                           san_septic, 
                           san_sewer,
                           san_unimp,
                           #access_sewer,
                           #access_septic, 
                           #access_latrine,
                           improved_san,
                           #access_sewer_exclusive,
                           #access_septic_exclusive,
                           #access_latrine_exclusive, 
                           #improved_san_exclusive,
                           no_san_access,
                           open_def,
                           #access_latrines_unimproved, 
                           #access_latrines_unimproved_exclusive, 
                           sewage_treated,
                           sewage_contaminates,
                           uniq_id)



#lapop_processing$psc1t1 ## add a field about psc1t1 weather tap water is treated by family
#There are 4 Colombian records that do not have weights: test<-lapop_processing[is.na(lapop_processing$weight1500),]

lapop_processing<-lapop_processing[!is.na(lapop_processing$wt),]

lapop_processing<-filter(lapop_processing, twe_usd<quantile(lapop$twe_usd,0.99)) 


library(srvyr)
lapop2021_sd <- as_survey_design(lapop_processing, 
                                 ids = upm,
                                 strata = strata,
                                 weight = wt,
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
    srvyr::summarise(twe_usd_pc = survey_mean(twe_usd_pc, na.rm = T,  vartype = NULL), 
                     twe_usd = survey_mean(twe_usd, na.rm = T,  vartype = NULL),
                     access_water_piped_home = survey_mean(access_water_piped_home, na.rm = T,  vartype = NULL),
                     access_water_piped_plot = survey_mean(access_water_piped_plot, na.rm = T,  vartype = NULL),
                     #water_trucked = survey_mean(water_trucked, na.rm = T,  vartype = NULL),
                     water_on_premises = survey_mean(water_on_premises, na.rm = T,  vartype = NULL),
                     water_distr = survey_mean(water_distr, na.rm = T,  vartype = NULL),
                     consume_distr = survey_mean(consume_distr, na.rm = T,  vartype = NULL),
                     consume_other = survey_mean(consume_other, na.rm = T,  vartype = NULL),
                     consume_bottled = survey_mean(consume_bottled, na.rm = T,  vartype = NULL),
                     consume_delivered = survey_mean(consume_delivered, na.rm = T,  vartype = NULL),
                     consume_impspring = survey_mean(consume_impspring, na.rm = T,  vartype = NULL),
                     consume_impwell = survey_mean(consume_impwell, na.rm = T,  vartype = NULL),
                     consume_publictap = survey_mean(consume_publictap, na.rm = T,  vartype = NULL),
                     consume_rain = survey_mean(consume_rain, na.rm = T,  vartype = NULL),
                     consume_unimproved  = survey_mean(consume_unimproved , na.rm = T,  vartype = NULL),
                     ou_bottled  = survey_mean(ou_bottled, na.rm = T,  vartype = NULL),
                     ou_delivered = survey_mean(ou_delivered, na.rm = T,  vartype = NULL),
                     ou_distr = survey_mean(ou_distr, na.rm = T,  vartype = NULL),
                     ou_impspring = survey_mean(ou_impspring, na.rm = T,  vartype = NULL),
                     ou_impwell = survey_mean(ou_impwell, na.rm = T,  vartype = NULL),
                     ou_other  = survey_mean(ou_other , na.rm = T,  vartype = NULL),
                     ou_publictap  = survey_mean(ou_publictap , na.rm = T,  vartype = NULL),
                     ou_rain  = survey_mean(ou_rain , na.rm = T,  vartype = NULL),
                     ou_unimproved   = survey_mean(ou_unimproved, na.rm = T,  vartype = NULL),
                     san_ecolatrine    = survey_mean(san_ecolatrine , na.rm = T,  vartype = NULL),
                     san_exclusive   = survey_mean(san_exclusive, na.rm = T,  vartype = NULL),
                     san_implatrine    = survey_mean(san_implatrine , na.rm = T,  vartype = NULL),
                     san_other    = survey_mean(san_other , na.rm = T,  vartype = NULL),
                     san_septic    = survey_mean(san_septic , na.rm = T,  vartype = NULL),
                     san_sewer    = survey_mean(san_sewer , na.rm = T,  vartype = NULL),
                     san_unimp    = survey_mean(san_unimp , na.rm = T,  vartype = NULL),
                     improved_w_access = survey_mean(improved_w_access, na.rm = T,  vartype = NULL),
                     water_daily = survey_mean(water_daily, na.rm = T,  vartype = NULL),
                     #water_dist_daily = survey_mean(water_dist_daily, na.rm = T,  vartype = NULL),
                     #access_water_piped_house_daily = survey_mean(access_water_piped_house_daily, na.rm = T,  vartype = NULL),
                     #access_water_piped_plot_daily = survey_mean(access_water_piped_plot_daily, na.rm = T,  vartype = NULL),
                     w_dist_treatment = survey_mean(w_dist_treatment, na.rm = T,  vartype = NULL),
                     w_treatment = survey_mean(w_treatment, na.rm = T,  vartype = NULL),
                     reason_w_treatment_taste = survey_mean(reason_w_treatment_taste, na.rm = T,  vartype = NULL),
                     reason_w_treatment_color = survey_mean(reason_w_treatment_color, na.rm = T,  vartype = NULL),
                     reason_w_treatment_quality = survey_mean(reason_w_treatment_quality, na.rm = T,  vartype = NULL),
                     reason_w_treatment_continuity = survey_mean(reason_w_treatment_continuity, na.rm = T,  vartype = NULL),
                    # piped_ou= survey_mean(piped_ou, na.rm = T,  vartype = NULL),
                    # other_ou= survey_mean(other_ou, na.rm = T,  vartype = NULL),
                    # unimproved_ou= survey_mean(unimproved_ou, na.rm = T,  vartype = NULL),
                     we_piped_water= survey_mean(we_piped_water, na.rm = T,  vartype = NULL),
                     we_bottled= survey_mean(we_bottled, na.rm = T,  vartype = NULL),
                     we_trucked= survey_mean(we_trucked, na.rm = T,  vartype = NULL),
                     satisfied_service= survey_mean(satisfied_service, na.rm = T,  vartype = NULL),
                     unsatisfied_service= survey_mean(unsatisfied_service, na.rm = T,  vartype = NULL),
                    # access_san_exclusive= survey_mean(access_san_exclusive, na.rm = T,  vartype = NULL),
                    # access_sewer= survey_mean(access_sewer, na.rm = T,  vartype = NULL),
                    # access_septic= survey_mean(access_septic, na.rm = T,  vartype = NULL),
                    # access_latrine= survey_mean(access_latrine, na.rm = T,  vartype = NULL),
                     improved_san = survey_mean(improved_san, na.rm = T,  vartype = NULL),
                    # access_sewer_exclusive= survey_mean(access_sewer_exclusive, na.rm = T,  vartype = NULL),
                    # access_septic_exclusive= survey_mean(access_septic_exclusive, na.rm = T,  vartype = NULL),
                    # access_latrine_exclusive= survey_mean(access_latrine_exclusive, na.rm = T,  vartype = NULL),
                    # improved_san_exclusive = survey_mean(improved_san_exclusive, na.rm = T,  vartype = NULL),
                     no_san_access= survey_mean(no_san_access, na.rm = T,  vartype = NULL),
                     open_def= survey_mean(open_def, na.rm = T,  vartype = NULL),
                    # access_latrines_unimproved= survey_mean(access_latrines_unimproved, na.rm = T,  vartype = NULL),
                    #access_latrines_unimproved_exclusive= survey_mean(access_latrines_unimproved_exclusive, na.rm = T,  vartype = NULL),
                     sewage_treated= survey_mean(sewage_treated, na.rm = T,  vartype = NULL),
                     sewage_contaminates= survey_mean(sewage_contaminates, na.rm = T,  vartype = NULL),
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

#a<-select(lapop, iso3, quintile, scope,psc2c1, psc1c4, psc2r1,psc2c1_usd, psc1c4_usd, psc2r1_usd, twe_usd)
#View(filter(a, iso3 == "BRA" & scope == "urban"&quintile ==5))



lapop_2021<- select(lapop_summary_filtered, 1,51:54, 2:50)

lapop_2021_long<- pivot_longer(lapop_2021, 6:54, names_to = "indicator", values_to = "value")
`%!in%` <- Negate(`%in%`)
lapop_2021_long$value <- ifelse(lapop_2021_long$indicator %!in% c("twe_usd","twe_usd_pc"),lapop_2021_long$value*100,lapop_2021_long$value)

write.csv(lapop_2021_long, "output_data/lapop_2021_long.csv", row.names = F)
write.csv(lapop_2021, "output_data/lapop_2021.csv")
