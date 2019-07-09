### set working directory ##
setwd("~/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/")

### load the packages... This needs to be done every time. ###
library(mousetrap)
library(foreach)
library(tidyverse)
library(readbulk)
library(plyr)
# library(utils)
# library(stats)
# library(pracma)
library(tidyr)
# library(magrittr)
# library(graphics)
# library(grDevices)
# library(ggplot2)
# library(scales)
# library(psych)
# library(Rcpp)
# library(diptest)
# library(RColorBrewer)
#library(cstab)
#library(fastcluster)
#library(parallel)
#library(fields)

### read in all the MT files for session one ###
session1files <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S1/", 
                            pattern = "*.mt", full.names = TRUE, recursive = FALSE)
session1.data <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S1/", fun = read_mt, 
                           extension = ".mt")
session1.data <- mt_import_wide(session1.data)
foreach(file = session1files) %do% {
  ### create "rate" variable (0 = Positive, 1 = Negative) ###
  session1.data$data$rate <- ifelse(session1.data$data$response == "POSITIVE", 0,
                                    ifelse(session1.data$data$response == "BLUE", 0, 1))

  ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
  session1.data$data$correct <- ifelse(session1.data$data$condition == "Angry",  
                                       ifelse(session1.data$data$rate == 1, 1, 0), 
                                       ifelse(session1.data$data$condition == "Happy", 
                                              ifelse(session1.data$data$rate == 0, 1, 0), 
                                              ifelse(session1.data$data$condition == "Red", 
                                                     ifelse(session1.data$data$rate == 1, 1, 0),
                                                     ifelse(session1.data$data$condition == "Blue", 
                                                            ifelse(session1.data$data$rate == 0, 1, 0), NA))))
  
  ### Create column to average for each face ###
  session1.data$data$ang_rate <- ifelse(session1.data$data$condition == "Angry", session1.data$data$rate, NA)
  session1.data$data$hap_rate <- ifelse(session1.data$data$condition == "Happy", session1.data$data$rate, NA)
  session1.data$data$sur_rate <- ifelse(session1.data$data$condition == "Surprise", session1.data$data$rate, NA)
  session1.data$data$blu_rate <- ifelse(session1.data$data$condition == "Blue", session1.data$data$rate, NA)
  session1.data$data$red_rate <- ifelse(session1.data$data$condition == "Red", session1.data$data$rate, NA)
  session1.data$data$pur_rate <- ifelse(session1.data$data$condition == "Purple", session1.data$data$rate, NA)
  
  ### Reaction times ###
  session1.data$data$ang_RT <- ifelse(session1.data$data$condition == "Angry", 
                                      ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
  session1.data$data$hap_RT <- ifelse(session1.data$data$condition == "Happy", 
                                      ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
  session1.data$data$sur_p_RT <- ifelse(session1.data$data$condition == "Surprise", 
                                        ifelse(session1.data$data$rate == 0, session1.data$data$RT, NA), NA)
  session1.data$data$sur_n_RT <- ifelse(session1.data$data$condition == "Surprise", 
                                        ifelse(session1.data$data$rate == 1, session1.data$data$RT, NA), NA)
  session1.data$data$blu_RT <- ifelse(session1.data$data$condition == "Blue", 
                                      ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
  session1.data$data$red_RT <- ifelse(session1.data$data$condition == "Red", 
                                      ifelse(session1.data$data$correct == 1, session1.data$data$RT, NA), NA)
  session1.data$data$pur_b_RT <- ifelse(session1.data$data$condition == "Purple", 
                                        ifelse(session1.data$data$rate == 0, session1.data$data$RT, NA), NA)
  session1.data$data$pur_r_RT <- ifelse(session1.data$data$condition == "Purple", 
                                        ifelse(session1.data$data$rate == 1, session1.data$data$RT, NA), NA)
  
  ### Create informative condition variables (i.e., angry, happy, sur-neg, sur-pos, etc.) ###
  session1.data$data$condition.rating <- ifelse(session1.data$data$condition == "Angry",  
                                                ifelse(session1.data$data$rate == 1, "Angry", NA), 
                                                
                                                ifelse(session1.data$data$condition == "Happy", 
                                                       ifelse(session1.data$data$rate == 0, "Happy", NA), 
                                                       
                                                       ifelse(session1.data$data$condition == "Surprise",  
                                                              ifelse(session1.data$data$rate == 1, "SurNeg", "SurPos"), 
                                                              
                                                              ifelse(session1.data$data$condition == "Red", 
                                                                     ifelse(session1.data$data$rate == 1, "Red", NA),
                                                                     
                                                                     ifelse(session1.data$data$condition == "Blue", 
                                                                            ifelse(session1.data$data$rate == 0, "Blue", NA),
                                                                            
                                                                            ifelse(session1.data$data$condition == "Purple",
                                                                                   ifelse(session1.data$data$rate == 1, "PurpleRed", "PurpleBlue"), NA))))))
  
}

session1.ratings.table <- (ddply(session1.data$data, "subjID", summarise, 
                                 ang_rate1 = mean(ang_rate, na.rm = TRUE),
                                 hap_rate1 = mean(hap_rate, na.rm = TRUE),
                                 sur_rate1 = mean(sur_rate, na.rm = TRUE),
                                 red_rate1 = mean(red_rate, na.rm = TRUE),
                                 blu_rate1 = mean(blu_rate, na.rm = TRUE),
                                 pur_rate1 = mean(pur_rate, na.rm = TRUE),
                                 ang_RT1 = mean(ang_RT, na.rm = TRUE),
                                 hap_RT1 = mean(hap_RT, na.rm = TRUE),
                                 sur_p_RT1 = mean(sur_p_RT, na.rm = TRUE),
                                 sur_n_RT1 = mean(sur_n_RT, na.rm = TRUE),
                                 red_RT1 = mean(red_RT, na.rm = TRUE),
                                 blu_RT1 = mean(blu_RT, na.rm = TRUE),
                                 pur_b_RT1 = mean(pur_b_RT, na.rm = TRUE),
                                 pur_r_RT1 = mean(pur_r_RT, na.rm = TRUE)))

### session1.data MT Measures ###
{### get derivatives ###
  session1.data <- mt_derivatives(session1.data)
  
  ### Flip traSurp.Negectories ###
  session1.data <- mt_remap_symmetric(session1.data) 
  
  ### adSurp.Negust to identical start/end traSurp.Negectories ###
  session1.data <- mt_align_start_end(session1.data) 
  
  ### normalize, there's some issues w/ trials w/ 0 variability: hence this weird bit ###
  session1.data <- mt_time_normalize(session1.data) 

  ### add measures ###
  session1.data <- mt_measures(session1.data)}
### get your data collapsed per sub, merge this with the final data! ###
session1.data.persub <- mt_aggregate_per_subject(session1.data, use = "measures", subject_id = "subjID" )
### merge MT measures w/ rating data ###
session1.ratings.table <- merge(session1.data.persub, session1.ratings.table, by = "subjID")
### write output for session1 ###
write.csv(session1.ratings.table, "session1_MT.csv")
mt_plot_aggregate(session1.data, use = "trajectories")


### read in all the MT files for session one ###
session2files <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S2/", 
                            pattern = "*.mt", full.names = TRUE, recursive = FALSE)
session2.data <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S2/", fun = read_mt, 
                           extension = ".mt")
session2.data <- mt_import_wide(session2.data)
foreach(file = session2files) %do% {
  ### create "rate" variable (0 = Positive, 1 = Negative) ###
  session2.data$data$rate <- ifelse(session2.data$data$response == "POSITIVE", 0,
                                    ifelse(session2.data$data$response == "BLUE", 0, 1))
  
  ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
  session2.data$data$correct <- ifelse(session2.data$data$condition == "Angry",  
                                       ifelse(session2.data$data$rate == 1, 1, 0), 
                                       ifelse(session2.data$data$condition == "Happy", 
                                              ifelse(session2.data$data$rate == 0, 1, 0), 
                                              ifelse(session2.data$data$condition == "Red", 
                                                     ifelse(session2.data$data$rate == 1, 1, 0),
                                                     ifelse(session2.data$data$condition == "Blue", 
                                                            ifelse(session2.data$data$rate == 0, 1, 0), NA))))
  
  ### Create column to average for each face ###
  session2.data$data$ang_rate <- ifelse(session2.data$data$condition == "Angry", session2.data$data$rate, NA)
  session2.data$data$hap_rate <- ifelse(session2.data$data$condition == "Happy", session2.data$data$rate, NA)
  session2.data$data$sur_rate <- ifelse(session2.data$data$condition == "Surprise", session2.data$data$rate, NA)
  session2.data$data$blu_rate <- ifelse(session2.data$data$condition == "Blue", session2.data$data$rate, NA)
  session2.data$data$red_rate <- ifelse(session2.data$data$condition == "Red", session2.data$data$rate, NA)
  session2.data$data$pur_rate <- ifelse(session2.data$data$condition == "Purple", session2.data$data$rate, NA)
  
  ### Reaction times ###
  session2.data$data$ang_RT <- ifelse(session2.data$data$condition == "Angry", 
                                      ifelse(session2.data$data$correct == 1, session2.data$data$RT, NA), NA)
  session2.data$data$hap_RT <- ifelse(session2.data$data$condition == "Happy", 
                                      ifelse(session2.data$data$correct == 1, session2.data$data$RT, NA), NA)
  session2.data$data$sur_p_RT <- ifelse(session2.data$data$condition == "Surprise", 
                                        ifelse(session2.data$data$rate == 0, session2.data$data$RT, NA), NA)
  session2.data$data$sur_n_RT <- ifelse(session2.data$data$condition == "Surprise", 
                                        ifelse(session2.data$data$rate == 1, session2.data$data$RT, NA), NA)
  session2.data$data$blu_RT <- ifelse(session2.data$data$condition == "Blue", 
                                      ifelse(session2.data$data$correct == 1, session2.data$data$RT, NA), NA)
  session2.data$data$red_RT <- ifelse(session2.data$data$condition == "Red", 
                                      ifelse(session2.data$data$correct == 1, session2.data$data$RT, NA), NA)
  session2.data$data$pur_b_RT <- ifelse(session2.data$data$condition == "Purple", 
                                        ifelse(session2.data$data$rate == 0, session2.data$data$RT, NA), NA)
  session2.data$data$pur_r_RT <- ifelse(session2.data$data$condition == "Purple", 
                                        ifelse(session2.data$data$rate == 1, session2.data$data$RT, NA), NA)
  
  ### Create informative condition variables (i.e., angry, happy, sur-neg, sur-pos, etc.) ###
  session2.data$data$condition.rating <- ifelse(session2.data$data$condition == "Angry",  
                                                ifelse(session2.data$data$rate == 1, "Angry", NA), 
                                                
                                                ifelse(session2.data$data$condition == "Happy", 
                                                       ifelse(session2.data$data$rate == 0, "Happy", NA), 
                                                       
                                                       ifelse(session2.data$data$condition == "Surprise",  
                                                              ifelse(session2.data$data$rate == 1, "SurNeg", "SurPos"), 
                                                              
                                                              ifelse(session2.data$data$condition == "Red", 
                                                                     ifelse(session2.data$data$rate == 1, "Red", NA),
                                                                     
                                                                     ifelse(session2.data$data$condition == "Blue", 
                                                                            ifelse(session2.data$data$rate == 0, "Blue", NA),
                                                                            
                                                                            ifelse(session2.data$data$condition == "Purple",
                                                                                   ifelse(session2.data$data$rate == 1, "PurpleRed", "PurpleBlue"), NA))))))
  
}

session2.ratings.table <- (ddply(session2.data$data, "subjID", summarise, 
                                 ang_rate2 = mean(ang_rate, na.rm = TRUE),
                                 hap_rate2 = mean(hap_rate, na.rm = TRUE),
                                 sur_rate2 = mean(sur_rate, na.rm = TRUE),
                                 red_rate2 = mean(red_rate, na.rm = TRUE),
                                 blu_rate2 = mean(blu_rate, na.rm = TRUE),
                                 pur_rate2 = mean(pur_rate, na.rm = TRUE),
                                 ang_RT2 = mean(ang_RT, na.rm = TRUE),
                                 hap_RT2 = mean(hap_RT, na.rm = TRUE),
                                 sur_p_RT2 = mean(sur_p_RT, na.rm = TRUE),
                                 sur_n_RT2 = mean(sur_n_RT, na.rm = TRUE),
                                 red_RT2 = mean(red_RT, na.rm = TRUE),
                                 blu_RT2 = mean(blu_RT, na.rm = TRUE),
                                 pur_b_RT2 = mean(pur_b_RT, na.rm = TRUE),
                                 pur_r_RT2 = mean(pur_r_RT, na.rm = TRUE)))


### session2.data MT Measures ###
{### get derivatives ###
  session2.data <- mt_derivatives(session2.data)
  
  ### Flip traSurp.Negectories ###
  session2.data <- mt_remap_symmetric(session2.data) 
  
  ### adSurp.Negust to identical start/end traSurp.Negectories ###
  session2.data <- mt_align_start_end(session2.data) 
  
  ### normalize, there's some issues w/ trials w/ 0 variability: hence this weird bit ###
  session2.data <- mt_time_normalize(session2.data) 
  
  ### add measures ###
  session2.data <- mt_measures(session2.data)}
### get your data collapsed per sub, merge this with the final data! ###
session2.data.persub <- mt_aggregate_per_subject(session2.data, use = "measures", subject_id = "subjID" )
### merge MT measures w/ rating data ###
session2.ratings.table <- merge(session2.data.persub, session2.ratings.table, by = "subjID")
### write output for session2 ###
write.csv(session2.ratings.table, "session2_MT.csv")
mt_plot_aggregate(session2.data, use = "tn_trajectories")



### read in all the MT files for session one ###
session3files <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S3/", 
                            pattern = "*.mt", full.names = TRUE, recursive = FALSE)
session3.data <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S3/", fun = read_mt, 
                           extension = ".mt")
session3.data <- mt_import_wide(session3.data)
foreach(file = session3files) %do% {
  ### create "rate" variable (0 = Positive, 1 = Negative) ###
  session3.data$data$rate <- ifelse(session3.data$data$response == "POSITIVE", 0,
                                    ifelse(session3.data$data$response == "BLUE", 0, 1))
  
  ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
  session3.data$data$correct <- ifelse(session3.data$data$condition == "Angry",  
                                       ifelse(session3.data$data$rate == 1, 1, 0), 
                                       ifelse(session3.data$data$condition == "Happy", 
                                              ifelse(session3.data$data$rate == 0, 1, 0), 
                                              ifelse(session3.data$data$condition == "Red", 
                                                     ifelse(session3.data$data$rate == 1, 1, 0),
                                                     ifelse(session3.data$data$condition == "Blue", 
                                                            ifelse(session3.data$data$rate == 0, 1, 0), NA))))
  
  ### Create column to average for each face ###
  session3.data$data$ang_rate <- ifelse(session3.data$data$condition == "Angry", session3.data$data$rate, NA)
  session3.data$data$hap_rate <- ifelse(session3.data$data$condition == "Happy", session3.data$data$rate, NA)
  session3.data$data$sur_rate <- ifelse(session3.data$data$condition == "Surprise", session3.data$data$rate, NA)
  session3.data$data$blu_rate <- ifelse(session3.data$data$condition == "Blue", session3.data$data$rate, NA)
  session3.data$data$red_rate <- ifelse(session3.data$data$condition == "Red", session3.data$data$rate, NA)
  session3.data$data$pur_rate <- ifelse(session3.data$data$condition == "Purple", session3.data$data$rate, NA)
  
  ### Reaction times ###
  session3.data$data$ang_RT <- ifelse(session3.data$data$condition == "Angry", 
                                      ifelse(session3.data$data$correct == 1, session3.data$data$RT, NA), NA)
  session3.data$data$hap_RT <- ifelse(session3.data$data$condition == "Happy", 
                                      ifelse(session3.data$data$correct == 1, session3.data$data$RT, NA), NA)
  session3.data$data$sur_p_RT <- ifelse(session3.data$data$condition == "Surprise", 
                                        ifelse(session3.data$data$rate == 0, session3.data$data$RT, NA), NA)
  session3.data$data$sur_n_RT <- ifelse(session3.data$data$condition == "Surprise", 
                                        ifelse(session3.data$data$rate == 1, session3.data$data$RT, NA), NA)
  session3.data$data$blu_RT <- ifelse(session3.data$data$condition == "Blue", 
                                      ifelse(session3.data$data$correct == 1, session3.data$data$RT, NA), NA)
  session3.data$data$red_RT <- ifelse(session3.data$data$condition == "Red", 
                                      ifelse(session3.data$data$correct == 1, session3.data$data$RT, NA), NA)
  session3.data$data$pur_b_RT <- ifelse(session3.data$data$condition == "Purple", 
                                        ifelse(session3.data$data$rate == 0, session3.data$data$RT, NA), NA)
  session3.data$data$pur_r_RT <- ifelse(session3.data$data$condition == "Purple", 
                                        ifelse(session3.data$data$rate == 1, session3.data$data$RT, NA), NA)
  
  ### Create informative condition variables (i.e., angry, happy, sur-neg, sur-pos, etc.) ###
  session3.data$data$condition.rating <- ifelse(session3.data$data$condition == "Angry",  
                                                ifelse(session3.data$data$rate == 1, "Angry", NA), 
                                                
                                                ifelse(session3.data$data$condition == "Happy", 
                                                       ifelse(session3.data$data$rate == 0, "Happy", NA), 
                                                       
                                                       ifelse(session3.data$data$condition == "Surprise",  
                                                              ifelse(session3.data$data$rate == 1, "SurNeg", "SurPos"), 
                                                              
                                                              ifelse(session3.data$data$condition == "Red", 
                                                                     ifelse(session3.data$data$rate == 1, "Red", NA),
                                                                     
                                                                     ifelse(session3.data$data$condition == "Blue", 
                                                                            ifelse(session3.data$data$rate == 0, "Blue", NA),
                                                                            
                                                                            ifelse(session3.data$data$condition == "Purple",
                                                                                   ifelse(session3.data$data$rate == 1, "PurpleRed", "PurpleBlue"), NA))))))
  
}

session3.ratings.table <- (ddply(session3.data$data, "subjID", summarise, 
                                 ang_rate3 = mean(ang_rate, na.rm = TRUE),
                                 hap_rate3 = mean(hap_rate, na.rm = TRUE),
                                 sur_rate3 = mean(sur_rate, na.rm = TRUE),
                                 red_rate3 = mean(red_rate, na.rm = TRUE),
                                 blu_rate3 = mean(blu_rate, na.rm = TRUE),
                                 pur_rate3 = mean(pur_rate, na.rm = TRUE),
                                 ang_RT3 = mean(ang_RT, na.rm = TRUE),
                                 hap_RT3 = mean(hap_RT, na.rm = TRUE),
                                 sur_p_RT3 = mean(sur_p_RT, na.rm = TRUE),
                                 sur_n_RT3 = mean(sur_n_RT, na.rm = TRUE),
                                 red_RT3 = mean(red_RT, na.rm = TRUE),
                                 blu_RT3 = mean(blu_RT, na.rm = TRUE),
                                 pur_b_RT3 = mean(pur_b_RT, na.rm = TRUE),
                                 pur_r_RT3 = mean(pur_r_RT, na.rm = TRUE)))


### session3.data MT Measures ###
{### get derivatives ###
  session3.data <- mt_derivatives(session3.data)
  
  ### Flip traSurp.Negectories ###
  session3.data <- mt_remap_symmetric(session3.data) 
  
  ### adSurp.Negust to identical start/end traSurp.Negectories ###
  session3.data <- mt_align_start_end(session3.data) 
  
  ### normalize, there's some issues w/ trials w/ 0 variability: hence this weird bit ###
  session3.data <- mt_time_normalize(session3.data) 
  
  ### add measures ###
  session3.data <- mt_measures(session3.data)}
### get your data collapsed per sub, merge this with the final data! ###
session3.data.persub <- mt_aggregate_per_subject(session3.data, use = "measures", subject_id = "subjID" )
### merge MT measures w/ rating data ###
session3.ratings.table <- merge(session3.data.persub, session3.ratings.table, by = "subjID")
### write output for session3 ###
write.csv(session3.ratings.table, "session3_MT.csv")
mt_plot_aggregate(session3.data, use = "tn_trajectories")



### read in all the MT files for session one ###
session4files <- list.files(path = "/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S4/", 
                            pattern = "*.mt", full.names = TRUE, recursive = FALSE)
session4.data <- read_bulk("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/06132019/S4/", fun = read_mt, 
                           extension = ".mt")
session4.data <- mt_import_wide(session4.data)
foreach(file = session4files) %do% {
  ### create "rate" variable (0 = Positive, 1 = Negative) ###
  session4.data$data$rate <- ifelse(session4.data$data$response == "POSITIVE", 0,
                                    ifelse(session4.data$data$response == "BLUE", 0, 1))
  
  ### create "correct response" variable for clearly valenced faces (0 = Incorrect, 1 = Correct) ###
  session4.data$data$correct <- ifelse(session4.data$data$condition == "Angry",  
                                       ifelse(session4.data$data$rate == 1, 1, 0), 
                                       ifelse(session4.data$data$condition == "Happy", 
                                              ifelse(session4.data$data$rate == 0, 1, 0), 
                                              ifelse(session4.data$data$condition == "Red", 
                                                     ifelse(session4.data$data$rate == 1, 1, 0),
                                                     ifelse(session4.data$data$condition == "Blue", 
                                                            ifelse(session4.data$data$rate == 0, 1, 0), NA))))
  
  ### Create column to average for each face ###
  session4.data$data$ang_rate <- ifelse(session4.data$data$condition == "Angry", session4.data$data$rate, NA)
  session4.data$data$hap_rate <- ifelse(session4.data$data$condition == "Happy", session4.data$data$rate, NA)
  session4.data$data$sur_rate <- ifelse(session4.data$data$condition == "Surprise", session4.data$data$rate, NA)
  session4.data$data$blu_rate <- ifelse(session4.data$data$condition == "Blue", session4.data$data$rate, NA)
  session4.data$data$red_rate <- ifelse(session4.data$data$condition == "Red", session4.data$data$rate, NA)
  session4.data$data$pur_rate <- ifelse(session4.data$data$condition == "Purple", session4.data$data$rate, NA)
  
  ### Reaction times ###
  session4.data$data$ang_RT <- ifelse(session4.data$data$condition == "Angry", 
                                      ifelse(session4.data$data$correct == 1, session4.data$data$RT, NA), NA)
  session4.data$data$hap_RT <- ifelse(session4.data$data$condition == "Happy", 
                                      ifelse(session4.data$data$correct == 1, session4.data$data$RT, NA), NA)
  session4.data$data$sur_p_RT <- ifelse(session4.data$data$condition == "Surprise", 
                                        ifelse(session4.data$data$rate == 0, session4.data$data$RT, NA), NA)
  session4.data$data$sur_n_RT <- ifelse(session4.data$data$condition == "Surprise", 
                                        ifelse(session4.data$data$rate == 1, session4.data$data$RT, NA), NA)
  session4.data$data$blu_RT <- ifelse(session4.data$data$condition == "Blue", 
                                      ifelse(session4.data$data$correct == 1, session4.data$data$RT, NA), NA)
  session4.data$data$red_RT <- ifelse(session4.data$data$condition == "Red", 
                                      ifelse(session4.data$data$correct == 1, session4.data$data$RT, NA), NA)
  session4.data$data$pur_b_RT <- ifelse(session4.data$data$condition == "Purple", 
                                        ifelse(session4.data$data$rate == 0, session4.data$data$RT, NA), NA)
  session4.data$data$pur_r_RT <- ifelse(session4.data$data$condition == "Purple", 
                                        ifelse(session4.data$data$rate == 1, session4.data$data$RT, NA), NA)
  
  ### Create informative condition variables (i.e., angry, happy, sur-neg, sur-pos, etc.) ###
  session4.data$data$condition.rating <- ifelse(session4.data$data$condition == "Angry",  
                                                ifelse(session4.data$data$rate == 1, "Angry", NA), 
                                                
                                                ifelse(session4.data$data$condition == "Happy", 
                                                       ifelse(session4.data$data$rate == 0, "Happy", NA), 
                                                       
                                                       ifelse(session4.data$data$condition == "Surprise",  
                                                              ifelse(session4.data$data$rate == 1, "SurNeg", "SurPos"), 
                                                              
                                                              ifelse(session4.data$data$condition == "Red", 
                                                                     ifelse(session4.data$data$rate == 1, "Red", NA),
                                                                     
                                                                     ifelse(session4.data$data$condition == "Blue", 
                                                                            ifelse(session4.data$data$rate == 0, "Blue", NA),
                                                                            
                                                                            ifelse(session4.data$data$condition == "Purple",
                                                                                   ifelse(session4.data$data$rate == 1, "PurpleRed", "PurpleBlue"), NA))))))
  
}

session4.ratings.table <- (ddply(session4.data$data, "subjID", summarise, 
                                 ang_rate4 = mean(ang_rate, na.rm = TRUE),
                                 hap_rate4 = mean(hap_rate, na.rm = TRUE),
                                 sur_rate4 = mean(sur_rate, na.rm = TRUE),
                                 red_rate4 = mean(red_rate, na.rm = TRUE),
                                 blu_rate4 = mean(blu_rate, na.rm = TRUE),
                                 pur_rate4 = mean(pur_rate, na.rm = TRUE),
                                 ang_RT4 = mean(ang_RT, na.rm = TRUE),
                                 hap_RT4 = mean(hap_RT, na.rm = TRUE),
                                 sur_p_RT4 = mean(sur_p_RT, na.rm = TRUE),
                                 sur_n_RT4 = mean(sur_n_RT, na.rm = TRUE),
                                 red_RT4 = mean(red_RT, na.rm = TRUE),
                                 blu_RT4 = mean(blu_RT, na.rm = TRUE),
                                 pur_b_RT4 = mean(pur_b_RT, na.rm = TRUE),
                                 pur_r_RT4 = mean(pur_r_RT, na.rm = TRUE)))


### session4.data MT Measures ###
{### get derivatives ###
  session4.data <- mt_derivatives(session4.data)
  
  ### Flip traSurp.Negectories ###
  session4.data <- mt_remap_symmetric(session4.data) 
  
  ### adSurp.Negust to identical start/end traSurp.Negectories ###
  session4.data <- mt_align_start_end(session4.data) 
  
  ### normalize, there's some issues w/ trials w/ 0 variability: hence this weird bit ###
  session4.data <- mt_time_normalize(session4.data) 
  
  ### add measures ###
  session4.data <- mt_measures(session4.data)}
### get your data collapsed per sub, merge this with the final data! ###
session4.data.persub <- mt_aggregate_per_subject(session4.data, use = "measures", subject_id = "subjID" )
### merge MT measures w/ rating data ###
session4.ratings.table <- merge(session4.data.persub, session4.ratings.table, by = "subjID")
### write output for session4 ###
write.csv(session4.ratings.table, "session4_MT.csv")
mt_plot_aggregate(session4.data, use = "tn_trajectories")








### merge ratings tables for all sessions ####
# full.data <- merge(session1.ratings.table, session2.ratings.table, by = "subjID", all.x = TRUE, all.y = TRUE)
# full.data <- merge(full.data, session3.ratings.table, by = "subjID", all.x = TRUE, all.y = TRUE)
# full.data <- merge(full.data, session4.ratings.table, by = "subjID", all.x = TRUE, all.y = TRUE)

full.data <- merge(session1.ratings.table, session2.ratings.table, by = "subjID")
full.data <- merge(full.data, session3.ratings.table, by = "subjID")
full.data <- merge(full.data, session4.ratings.table, by = "subjID")

full.data$sur_rate_week1 <- rowMeans(full.data[,c("sur_rate1","sur_rate2")], na.rm = TRUE)
full.data$sur_rate_week2 <- rowMeans(full.data[,c("sur_rate3","sur_rate4")], na.rm = TRUE)
full.data2 <- full.data[,c(58:59)]
full.data2 <- na.omit(full.data2)
rate.data_long <- gather(full.data2, key = time, value = "Percent Negative",  
                         sur_rate_week1, sur_rate_week2)

ggplot(rate.data_long, aes(time, `Percent Negative`) ) +
  geom_violin(scale = "area") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)

## Frequentist
summary(aov(`Percent Negative` ~ time, data = rate.data_long))  # conduct frequentist one-way ANOVA

## Bayesian
oneWayAOV.Fstat(F = 10.08, N = 25, J = 2, simple = TRUE)  # convert F statistic to BF
anovaBF(choice ~ condition, data = datafile)  # conduct Bayesian one-way ANOVA to check
extractBF(ttestBF(formula = choice ~ condition, data = datafile))$bf  # conduct Bayesian t-test to check
