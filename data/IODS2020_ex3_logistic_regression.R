# 2020-11-09
# author: Antti Korhonen

#### IODS 2020, exercise 3: Logistic regression ####

#### Data wrangling ####

# Student Performance Data Set (student.zip) for the exercise downloaded from Machine learning repository
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# https://archive.ics.uci.edu/ml/machine-learning-databases/00320/

#Data Set Information:

# This data approach student achievement in secondary education of two Portuguese schools. 
# The data attributes include student grades, demographic, social and school related features) 
# and it was collected by using school reports and questionnaires. Two datasets are provided regarding 
# the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). 
# In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. 
# Important note: the target attribute G3 has a strong correlation with attributes G2 and G1. 
# This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond 
# to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, 
# but such prediction is much more useful (see paper source for more details).

# P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. 
# In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, 
# Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7.

# read in the data using data.table package

library(data.table)

math <- fread("E:/IODS_usb/IODS-project/data/ex3_student-mat.csv")
por <- fread("E:/IODS_usb/IODS-project/data/ex3_student-por.csv")

# insert id's to dataframes

student_por$id <- 1000+1:nrow(student_por)
student_mat$id <- 2000+1:nrow(student_mat)


# bind dataframes and select variables

student <- rbind(student_mat,student_por)

# Define own id for both datasets
library(dplyr)
por_id <- 1000+1:nrow(por)
math_id <- 1000+1:nrow(math)

# Which columns vary in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets
join_cols <- setdiff(colnames(por_id),free_cols)

pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))

# Combine datasets to one long data
#   NOTE! There are NO 382 but 370 students that belong to both datasets
#         Original joining/merging example is erroneous!
pormath <- por_id %>% 
  bind_rows(math_id) %>%
  # Aggregate data (more joining variables than in the example)  
  group_by(.dots=join_cols) %>%  
  # Calculating required variables from two obs  
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     #  Rounded mean for numerical
    paid=first(paid),                   #    and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  ) %>%
  # Remove lines that do not have exactly one obs from both datasets
  #   There must be exactly 2 observations found in order to joining be succesful
  #   In addition, 2 obs to be joined must be 1 from por and 1 from math
  #     (id:s differ more than max within one dataset (649 here))
  filter(n==2, id.m-id.p>650) %>%  
  # Join original free fields, because rounded means or first values may not be relevant
  inner_join(pormath_free,by=c("id.p"="id"),suffix=c("",".p")) %>%
  inner_join(pormath_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )

# Save created data to folder 'data' as an .csv
library(data.table)
fwrite(pormath,file="F:/IODS_usb/IODS-project/data/pormath.csv")

