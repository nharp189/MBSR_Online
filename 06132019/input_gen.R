setwd("~/Documents/Nick-Grad/Neta_Lab/exercise_study/MBSR_Online/")
library(readxl)
file <- read_xlsx("totalData_dates_time_20190607.xlsx")
file <- file[,c(1:3,5:6,8:9,11:12,14:15)]
s1 <- file[,c(3,1)]
list(s1)

s1$SUBJ <- as.character(s1$SUBJ)
s1 <- as.matrix(s1)
mt <- rep("_S1.mt", length(s1))
s1 <- cbind(s1,mt)
s1 <-as.data.frame(s1)
s1$mt <- paste(s1$SUBJ, s1$mt, sep = "")
print(s1$mt)
s1 <- na.omit(s1)
View(s1)
s1 <- as.matrix(s1)
write.table(s1, file="input.txt", row.names=FALSE, col.names=FALSE, quote = FALSE)
