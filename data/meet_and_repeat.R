#### Data wrangling for exercise 6 ####
## 2020-11-27
# Antti Korhonen


# read in needed data using data.table package
library(data.table)

BPRS <- fread("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt")
RATS <- fread("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt")

# in RATS data 13 columns names was detected but the data had 14 columns. 
# Column V1 was duplicat with ID column so deleting column V1
RATS$V1 <- NULL

# BPRS data had 40 observations of 11 variables. Subjects were divided to 2 treatment groups and
# had observations from 8 weeks
names(BPRS)
summary(BPRS)
str(BPRS)

# RATS data had 16 observations of 13 variables. Subjects were divided to 3 groups and had 
# observations from 11 weekdays? (WD)
names(RATS)
summary(RATS)
str(RATS)

# converting categorical variables of both dataset into factors
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# Convert to long form and add the week variable to BPRSL and time variable to RATS
library(dplyr)
library(tidyr)
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject) %>% mutate(week = as.integer(substr(weeks,5,5)))
RATSL <- RATS %>% gather(key = WD, value = Weight, -ID, -Group) %>% mutate(Time = as.integer(substr(WD,3,4))) 
  
# Extract the week number of BPRSL
BPRSL <-  BPRSL 

# Take a glimpse at the BPRSL and RATSL data
glimpse(BPRSL)
names(BPRSL)
summary(BPRSL)
glimpse(RATSL)
names(RATSL)
summary(RATSL)

# BPRSL data had now 5 variables ("treatment", "subject", "weeks", "bprs","week" ) and 
# 360 observations (8 weeks * 40 subjects = 320)
# RATS data had 5 variables ("ID","Group","WD","Weight","Time") and 176 observations (11 WD * 16 ID = 176). 
# In long format data columns each subject or ID existed as many times there were weeks or WD.s

# save created files as .csv to data folder
fwrite(BPRSL, "F:/IODS_usb/IODS-project/data/BPRSL.csv")
fwrite(BPRS, "F:/IODS_usb/IODS-project/data/BPRS.csv")
fwrite(RATSL, "F:/IODS_usb/IODS-project/data/RATSL.csv")
fwrite(RATS, "F:/IODS_usb/IODS-project/data/RATS.csv")
