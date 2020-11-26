#### Data wrangling for exercise 5 ####
## 2020-11-20
# Antti Korhonen

# metadata of human development data (hd) and gender inequality data (gii) 
# http://hdr.undp.org/en/content/human-development-index-hdi
# http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf

# original datasource to human data
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt

# renamed variables in the exercise 4
#    HDI = Human.Development.Index..HDI.,
#    GNIperCap = Gross.National.Income..GNI..per.Capita,
#    GNIrank_HDIrank = GNI.per.Capita.Rank.Minus.HDI.Rank,
#    EYE = Expected.Years.of.Education,
#    LEB = Life.Expectancy.at.Birth,
#    MYE = Mean.Years.of.Education
#    GII = Gender.Inequality.Index..GII.,
#    MMR = Maternal.Mortality.Ratio,
#    ABR = Adolescent.Birth.Rate,
#    parRep = Percent.Representation.in.Parliament,
#    edu2F = Population.with.Secondary.Education..Female.,
#    edu2M = Population.with.Secondary.Education..Male.,
#    labF = Labour.Force.Participation.Rate..Female.,
#    labM = Labour.Force.Participation.Rate..Male.

# read in data created in exercise 4
library(data.table)
human <- fread("F:/IODS_usb/IODS-project/data/human_ex4.csv")

# structure of the human data 
str(human)
# There are 195 observations (countries) of 19 variables. 
# The HDI was created to emphasize that people and their capabilities should be the ultimate criteria 
# for assessing the development of a country, not economic growth alone. 

# transforming the GNI per capita from character to numeric
human$GNIperCap <- as.numeric(gsub(",","",human$GNIperCap))

# rows to keep "Country", "edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F" 
# names own data "Country", "edu2FMratio", "labFMratio", "EYE", "LEB", "GNIperCap", "MMR", "ABR", "parRep"
names(human)
# choose the wanted columns by column number
human <- subset(human, select = c("Country", "edu2FMratio", "labFMratio", "EYE", "LEB", 
                                  "GNIperCap", "MMR", "ABR", "parRep"))
  
# shortening and renaming some of the columns once more to avoid confusion in future
names(human)
library(dplyr)
human <- human %>% 
  rename(edu2_FM = edu2FMratio,
         lab_FM = labFMratio,
         life_exp = LEB,
         edu_exp = EYE,
         GNI = GNIperCap
  )
               
# Removing all rows with missing values             
human <- na.omit(human)     

# Removing regions and leaving only countries. region variables ("Arab States", "East Asia and the Pacific", 
# "Europe and Central Asia", "Latin America and the Caribbean", "South Asia, "Sub-Saharan Africa", "World")

library(tidyverse)

# rows to delete 
rows_del <- c("Arab States", "East Asia and the Pacific", "Europe and Central Asia", "Latin America and the Caribbean", 
              "South Asia", "Sub-Saharan Africa", "World")

# deleting unwanted rows (ending up to 155 observations of 8 variables)
human <- human[ ! human$Country %in% rows_del, ]

# assigning country names as row values
row.names(human) <- human$Country
human$Country <- NULL 

# write data to a file
fwrite(human, "F:/IODS_usb/IODS-project/data/human.csv", row.names = TRUE)

