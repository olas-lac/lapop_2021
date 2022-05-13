
load("lapop_2021.rda")    
validation<-lapop_2021
validation <- validation[validation$scope != "no scope data",]

validation$unimproved_test <- validation$improved_san+validation$access_latrines_unimproved 
validation$unimproved_exclusive_test <- validation$access_latrines_unimproved_exclusive + validation$improved_san_exclusive

lapop_validation <- validation %>%
  group_by(iso3)%>%
  dplyr::summarise(
    on_prem = mean(round(water_on_premises ,digits=4)==round(access_water_piped_house +access_water_piped_plot +water_trucked ,digits=4), na.rm= TRUE),
    improved_w  = mean(round(improved_w_access, digits= 5)==round(consume_distr+ consume_other, digits=5), na.rm=TRUE),
    truck = mean(water_trucked 	<= consume_other, na.rm=TRUE),
    bottled = mean(consume_bottled <= consume_other, na.rm=TRUE),
    daily_house = mean(access_water_piped_house_daily <= access_water_piped_house , na.rm=TRUE),
    daily_plot = mean(access_water_piped_plot_daily<= water_dist_daily, na.rm=TRUE),
    septic_exclusive1 = mean(access_septic_exclusive 	<= access_septic , na.rm=TRUE),
    septic_exclusive2 = mean(access_septic_exclusive 	<= access_san_exclusive , na.rm=TRUE),
    sewer_exclusive1 = mean(access_sewer_exclusive  <= access_sewer , na.rm=TRUE),
    sewer_exlcusive2 = mean(access_sewer_exclusive  <= access_san_exclusive , na.rm=TRUE),
    improved_san  = mean(round(improved_san, digits=4)==round(access_sewer  + access_septic  + access_latrine  ,digits=4), na.rm=TRUE),
    improved_san_ex = mean(round(improved_san_exclusive , digits=4) == round(access_latrine_exclusive  +access_septic_exclusive +access_sewer_exclusive , digits=4), na.rm=TRUE),
    unimproved_san  = mean(unimproved_test, na.rm = TRUE), # these do add up to 1, not sure why they are not 
    unimproved_san_ex = mean(unimproved_exclusive_test, na.rm = TRUE))

mean(round(validation$access_latrines_unimproved+ validation$improved_san, digits=4) == 1.000)


validation <- validation[validation$scope != "no scope data",]
test <- validation %>%
  group_by(iso3, scope, gender, quintile)%>%
  dplyr::summarise(
    unimproved_san  = mean(round(access_latrines_unimproved+ improved_san), digits=4 == 1.000))# these do add up to 1, not sure why they are not 










validation$test <- validation$access_latrines_unimproved + validation$improved_san
mean(validation$test)

on_prem_bad <- validation[round(validation$water_on_premises, digits=4)!=round((validation$access_water_piped_house+validation$access_water_piped_plot+validation$water_trucked),digits = 4),] 


truckedbad <- validation[validation$water_trucked>validation$consume_other,] 
test <- select(truckedbad, iso3, quintile, scope, gender, truckedbad, water_trucked, consume_other)


septic_ex <- validation[validation$access_septic_exclusive >validation$access_san_exclusive,] 

test <- select(septic_ex, iso3, quintile, scope, gender, truckedbad, water_trucked, consume_other)


sewer_ex <- validation[validation$access_sewer_exclusive >validation$access_san_exclusive,] 

test <- select(septic_ex, iso3, quintile, scope, gender, sewer_ex, water_trucked, consume_other)


improved_san <- validation[validation$improved_san!= (validation$access_sewer  + validation$access_septic  + validation$access_latrine),] 
test <- select(improved_san, improved_san, access_sewer, access_septic, access_latrine)
test$test <- test$access_latrine+test$access_septic+ test$access_sewer

unimproved <-  validation[validation$access_latrines_unimproved  +validation$improved_san  !=	1,]
test <- select( unimproved, iso3, scope, gender, quintile,access_latrines_unimproved,improved_san)
test$test<- test$access_latrines_unimproved + test$improved_san
