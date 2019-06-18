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

collapse.by.sub.session1 <- session1.data

session1.ratings.table <- (ddply(collapse.by.sub.session1$data, "subjID", summarise, 
                                 ang_rate = mean(ang_rate, na.rm = TRUE),
                                 hap_rate = mean(hap_rate, na.rm = TRUE),
                                 sur_rate = mean(sur_rate, na.rm = TRUE),
                                 red_rate = mean(red_rate, na.rm = TRUE),
                                 blu_rate = mean(blu_rate, na.rm = TRUE),
                                 pur_rate = mean(pur_rate, na.rm = TRUE),
                                 ang_RT = mean(ang_RT, na.rm = TRUE),
                                 hap_RT = mean(hap_RT, na.rm = TRUE),
                                 sur_p_RT = mean(sur_p_RT, na.rm = TRUE),
                                 sur_n_RT = mean(sur_n_RT, na.rm = TRUE),
                                 red_RT = mean(red_RT, na.rm = TRUE),
                                 blu_RT = mean(blu_RT, na.rm = TRUE),
                                 pur_b_RT = mean(pur_b_RT, na.rm = TRUE),
                                 pur_r_RT = mean(pur_r_RT, na.rm = TRUE)))

### Session1.data MT Measures ###
{### get derivatives ###
  session1.data <- mt_derivatives(session1.data)
  
  ### Flip traSurp.Negectories ###
  session1.data <- mt_remap_symmetric(session1.data) 
  
  ### adSurp.Negust to identical start/end traSurp.Negectories ###
  session1.data <- mt_align_start_end(session1.data) 
  
  ### normalize, there's some issues w/ trials w/ 0 variability: hence this weird bit ###
  session1.data$data$pos_var <- apply(session1.data$trajectories[,,"xpos"],1,var,na.rm=TRUE) + apply(session1.data$trajectories[,,"ypos"],1,var,na.rm=TRUE)
  table(session1.data$data$pos_var==0)
  session1.data <- mt_subset(session1.data, pos_var>0)
  session1.data <- mt_time_normalize(session1.data) 
  
  ### add measures ###
  session1.data <- mt_measures(session1.data)}
### get your data collapsed per sub, merge this with the final data! ###
session1.data.persub <- mt_aggregate_per_subject(session1.data, use = "measures", subject_id = "subjID" )
### merge MT measures w/ rating data ###
session1.ratings.table <- merge(session1.data.persub, session1.ratings.table, by = "subjID")
### write output for session1 ###
write.csv(session1.ratings.table, "Session1_MT.csv")
mt_plot_aggregate(session1.data, use = "tn_trajectories",
                  color = "condition.rating")
