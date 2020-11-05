
# 2020-11-03
# author: Antti Korhonen

#### IODS 2020, exercise 2: regression and model validation ####

#### EXERCISE 2, Data wrangling ####

# package to read in the data
library(data.table) 

# using fread function of data.table package to read in the data
lrn14 <- fread("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt")

# dimensions and structure of the data 
#(dataframe includes 183 observations of 60 variables (59 integer and 1 character))
str(lrn14)
dim(lrn14)

# package needed to create variables deep, stra and surf
library(dplyr)

# deep, surface and strategic questions of the dataset
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30", "D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

# create column "attitude" by scaling the column "Attitude"
lrn14$attitude <- lrn14$Attitude / 10

# choose columns to keep
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
learning2014 <- select(lrn14, one_of(keep_columns))
str(learning2014)

# print out the column names of the data
colnames(learning2014)

# change the name of the second column
colnames(learning2014)[2] <- "age"
colnames(learning2014)[7] <- "points"

# select rows where points is greater than zero
learning2014 <- filter(learning2014, points > 0)

# check the working directory "E:/IODS_usb/IODS-project"
getwd()

# writing learning2014 data as .csv in to the local data folder. Using fwrite function of data.table package
fwrite(learning2014, "E:/IODS_usb/IODS-project/data/learning2014.csv")

# and reading the data back into R
learning2014 <- fread("E:/IODS_usb/IODS-project/data/learning2014.csv")

# structure of the data
str(learning2014)

#Classes ‘data.table’ and 'data.frame':	166 obs. of  7 variables:
#  $ gender  : chr  "F" "M" "F" "M" ...
# $ age     : int  53 55 49 53 49 38 50 37 37 42 ...
# $ attitude: num  3.7 3.1 2.5 3.5 3.7 3.8 3.5 2.9 3.8 2.1 ...
# $ deep    : num  3.58 2.92 3.5 3.5 3.67 ...
# $ stra    : num  3.38 2.75 3.62 3.12 3.62 ...
# $ surf    : num  2.58 3.17 2.25 2.25 2.83 ...
# $ points  : int  25 12 24 10 22 21 21 31 24 26 ...
# - attr(*, ".internal.selfref")=<externalptr> 

# head of the data
head(learning2014)

#     gender age  attitude    deep      stra     surf       points
# 1:      F  53      3.7      3.583333  3.375   2.583333      25
# 2:      M  55      3.1      2.916667  2.750   3.166667      12
# 3:      F  49      2.5      3.500000  3.625   2.250000      24
# 4:      M  53      3.5      3.500000  3.125   2.250000      10
# 5:      M  49      3.7      3.666667  3.625   2.833333      22
# 6:      F  38      3.8      4.750000  3.625   2.416667      21


#### EXERCISE 2, DATA ANALYSIS ####

#### TASK 1  ####
# reading in and structure of the data

# needed packages
library(data.table)

# reading the data into R
learning2014 <- fread("E:/IODS_usb/IODS-project/data/learning2014.csv")

# structure and dimensions of the data 
# data.frame with 166 obs. (students) of 7 variables (gender, age, attitude, deep, stra, surf, points)
str(learning2014)
dim(learning2014)

#### TASK 2 ####
# a graphical overview and summaries of the data 

# to fit the sraight line with lm in scatterplot matrix
# https://stackoverflow.com/questions/39667830/how-to-insert-trendlines-in-scatterplot-matrix

panel.lm <- function (x, y,  pch = par("pch"), col.lm = "red",  ...) {   
  ymin <- min(y)
  ymax <- max(y)
  xmin <- min(x)
  xmax <- max(x)
  ylim <- c(min(ymin,xmin),max(ymax,xmax))
  xlim <- ylim
  points(x, y, pch = pch,ylim = ylim, xlim= xlim,...)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(lm(y[ok]~ x[ok]), 
           col = col.lm, ...)
}

# draw a scatter plot matrix of the variables in learning2014.
# [,2:7] excludes the first column (gender)
pairs(learning2014[,2:7], panel = panel.lm)

# access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs() and draw the plot. Divided by gender
p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
p

# create an plot matrix with ggpairs(). All subjects
ggpairs(learning2014, lower = list(combo = wrap("facethist", bins = 20)))

# summaries of the variables
summary(learning2014)

# gender               age           attitude          deep            stra            surf           points     
# Length:166         Min.   :17.00   Min.   :1.400   Min.   :1.583   Min.   :1.250   Min.   :1.583   Min.   : 7.00  
# Class :character   1st Qu.:21.00   1st Qu.:2.600   1st Qu.:3.333   1st Qu.:2.625   1st Qu.:2.417   1st Qu.:19.00  
# Mode  :character   Median :22.00   Median :3.200   Median :3.667   Median :3.188   Median :2.833   Median :23.00  
#                    Mean   :25.51   Mean   :3.143   Mean   :3.680   Mean   :3.121   Mean   :2.787   Mean   :22.72  
#                    3rd Qu.:27.00   3rd Qu.:3.700   3rd Qu.:4.083   3rd Qu.:3.625   3rd Qu.:3.167   3rd Qu.:27.75  
#                    Max.   :55.00   Max.   :5.000   Max.   :4.917   Max.   :5.000   Max.   :4.333   Max.   :33.00  


#### TASKS 3 AND 4 ####

# create a regression model with multiple explanatory variables
my_model2 <- lm(points ~ attitude, data = learning2014)

# print out a summary of the model
summary(my_model2)

# Call:  lm(formula = points ~ attitude + age + stra, data = learning2014)

# Residuals:
#  Min       1Q       Median     3Q       Max 
# -18.1149  -3.2003   0.3303    3.4129   10.7599 

# Coefficients:
#               Estimate  Std. Error  t value   Pr(>|t|)    
# (Intercept)   10.89543    2.64834     4.114   6.17e-05 ***
#  attitude     3.48077     0.56220     6.191   4.72e-09 ***
#  age         -0.08822     0.05302    -1.664     0.0981 .  
#  stra         1.00371     0.53434     1.878     0.0621 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 5.26 on 162 degrees of freedom
# Multiple R-squared:  0.2182,	Adjusted R-squared:  0.2037 
# F-statistic: 15.07 on 3 and 162 DF,  p-value: 1.07e-08

#### Call: lm(formula = points ~ attitude, data = learning2014) ####

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -16.9763  -3.2119   0.4339   4.1534  10.6645 

# Coefficients:
#               Estimate   Std. Error   t value   Pr(>|t|)    
# (Intercept)   11.6372     1.8303      6.358     1.95e-09 ***
#  attitude     3.5255      0.5674      6.214     4.12e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 5.32 on 164 degrees of freedom
# Multiple R-squared:  0.1906,	Adjusted R-squared:  0.1856 
# F-statistic: 38.61 on 1 and 164 DF,  p-value: 4.119e-09


#### TASK 5 ####

# draw diagnostic plots using the plot() function. Choose the plots 1 (residual vs. fitted), 
# 2 (Normal QQ-plot) and 5 (Residuals vs. leverage)
par(mfrow = c(2,2))
plot(my_model2, which = c(1,2,5))

