### set wd ###
setwd("~/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/")

### load packages ###
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(psych)){
  install.packages("psych")
  library(psych)
}

### import data ###
data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Analyses/Data/qualtrics_s3/qdata_20190515.csv")

### preview data ###
#View(data)

### remove junk rows ###
### typically row 1 and 2 in qualtrics csv files ###
data <- data[-(c(1,2)),]

### study notes report that 80503 completed the qualtrics twice, the earlier data should be used 
### although his email said session one..? 
### Removed row named 8, but row names are now off because of deletions... ###
data <- data[-(6),]

### row names reset ###
rownames(data) <- NULL

### separate ERQ and ID from full data ###
ERQ.data <- data[,c(11:21)]

### convert choice text to numeric ###
for(i in 2:ncol(ERQ.data)) {
  ERQ.data[,i] <- recode(ERQ.data[,i], 
                           "1-Strongly disagree" = 1,
                           "2" = 2, "3" = 3, "4-Neutral" = 4,
                           "5" = 5, "6" = 6, "7-Strongly agree" = 7)
}

### Claudia, see if you can find a way to get averages from rowSums without ###f
### needing to divide by # of questions (e.g., 6 or 4 in this example)###
### then we can set na.rm = TRUE ###
### add items for participant scores and create column in survey.data ###
ERQ.data$ERQ_CR <-  (rowSums(ERQ.data[,c("erq_1", "erq_3", "erq_5", 
                               "erq_7", "erq_8", "erq_10")], 
                        na.rm = FALSE)  / 6 )

ERQ.data$ERQ_ES <- ( rowSums(ERQ.data[,c("erq_2", "erq_4", "erq_6", 
                               "erq_9")], 
                        na.rm = FALSE) / 4 )

### add CR and ES scores back to original data file ###
data$ERQ_CR <- ERQ.data$ERQ_CR
data$ERQ_ES <- ERQ.data$ERQ_ES


### separate SWLS from data ### 
### create SWLS dataframe ###
SWLS.data <- data[,c(11, 139:143)]

### convert choice text to numeric ###
for(i in 2:ncol(SWLS.data)) {
  SWLS.data[,i] <- recode(SWLS.data[,i], 
                         "Strongly disagree" = 1,
                         "Disagree" = 2, "Slightly disagree" = 3,
                         "Neither agree nor disagree" = 4, "Slightly agree" = 5,
                         "Agree" = 6, "Strongly agree" = 7)
}
### add items for participant scores and create column in SWLS dataframe ###
SWLS.data$TOTAL <- (rowSums(SWLS.data[,c(2:6)], na.rm = FALSE))

### add SWLS scores back to original data file ###
data$SWLS <- SWLS.data$TOTAL

### separate STAI-ST from full data ###
### create SWLS dataframe ###
STAI.data <- data[,c(11, 61:100)]

### convert choice text to numeric ###
for(i in 2:21) {
  STAI.data[,i] <- recode(STAI.data[,i], 
                          "Not at all" = 1, "Somewhat" = 2, 
                          "Moderately so" = 3, "Very much so" = 4)
}
for(i in 22:41) {
  STAI.data[,i] <- recode(STAI.data[,i], 
                          "Almost never" = 1, "Sometimes" = 2, 
                          "Often" = 3, "Almost always" = 4)
}

### add scores ###








### write out the cleaned and scored data file ###
write.csv(data, "~/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/Data/qualtrics_s3/cleaned.scored.q3_20190516.csv")
