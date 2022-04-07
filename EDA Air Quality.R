install.packages("tibble")
library(dplyr) #A Grammar of Data Manipulation
library(tibble) #modern take on data frames.
install.packages("dlookr")
library(dlookr) #Tools for Data Diagnosis, Exploration, Transformation (main library)

# View Data
head(airquality)

##---Data diagnose---##

str(head(airquality))

#Diagnosis variabel numerik
diagnose_numeric(airquality)

#Diagnosis outlier dan missing value
diagnose_outlier(airquality)

airquality %>%
  plot_outlier(Wind)

plot_na_intersect(airquality)

#MAGIC TIMEEE
diagnose_web_report(airquality)

#Statistics Descriptive
summary(airquality)

describe(airquality)

#Normality check
normality(airquality)

airquality %>%
  plot_normality(Ozone, Wind)

#Correlation Matrix
correlate(airquality)

plot_correlate(airquality)

#MAGIC TIMEEE
eda_web_report(airquality)

##---Data Transformation--##

#Handling missing value
#Remove missing value
NROW(airquality$Ozone)

x <- na.omit(airquality$Ozone)
NROW(x)

#Impute missing value
ozone_impute <- imputate_na(airquality, Ozone, method = "mean")
summary(ozone_impute)

plot(ozone_impute)

#Handling outlier
#Remove outlier
Q1 <- quantile(airquality$Wind, .25)
Q3 <- quantile(airquality$Wind, .75)
IQR <- IQR(airquality$Wind)
no_outliers <- subset(airquality, airquality$Wind > (Q1 - 1.5*IQR) & airquality$Wind < (Q3 + 1.5*IQR))
NROW(no_outliers)

#Impute outlier
out_wind <- imputate_outlier(airquality, Wind, method = "capping")
summary(out_wind)

plot(out_wind)

#Standardization
air_trans<- transform(airquality$Wind, method = "minmax")
boxplot(air_trans)

#Mengatasi skewness

Ozone_log = transform(airquality$Ozone, method = "log")
summary(Ozone_log)

plot(Ozone_log)
