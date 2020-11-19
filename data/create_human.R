#### Data wrangling for exercise 5 ####
## 2020-11-18
# Antti Korhonen

# metadata of human development data (hd) and gender inequality data (gii) 
# http://hdr.undp.org/en/content/human-development-index-hdi
# http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf


# read in data 
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# structure, dimensions and summaries of the data
str(hd)
str(gii)
dim(hd)
dim(gii)
summary(hd)
summary(gii)

# rename variables
library(dplyr)
names(hd)

hd <- hd %>% 
  rename(
    HDI = Human.Development.Index..HDI.,
    GNIperCap = Gross.National.Income..GNI..per.Capita,
    GNIrank_HDIrank = GNI.per.Capita.Rank.Minus.HDI.Rank,
    EYE = Expected.Years.of.Education,
    LEB = Life.Expectancy.at.Birth,
    MYE = Mean.Years.of.Education
  )

names(gii)

gii <- gii %>% 
  rename(GII = Gender.Inequality.Index..GII.,
         MMR = Maternal.Mortality.Ratio,
         ABR = Adolescent.Birth.Rate,
         parRep = Percent.Representation.in.Parliament,
         edu2F = Population.with.Secondary.Education..Female.,
         edu2M = Population.with.Secondary.Education..Male.,
         labF = Labour.Force.Participation.Rate..Female.,
         labM = Labour.Force.Participation.Rate..Male.
  )

# creating two variables to dataset gii
gii <- mutate(gii, edu2FMratio = edu2F / edu2M)
gii <- mutate(gii, labFMratio = labF / labM)

# join the two datasets to create human data
human <- merge(hd, gii, by="Country")
