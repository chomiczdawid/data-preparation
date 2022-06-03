# Libraries
library(dplyr)
library(ggplot2)
library(VIM)
library(gridExtra)
library(corrplot)


# Data loading
d1 <- read.csv("recipeData.csv", sep = ",")


### Initial analysis of every variable

# ABV
table(d1$ABV)
# scale: ratio
# unit: percent
# descriptive statistics
summary(d1$ABV)
# visualisation
ggplot(d1, aes(x=ABV)) + 
  geom_histogram(color="white", fill="darkgrey")

# OG - Original Gravity
table(d1$OG)
# scale: ratio
# unit: 
# descriptive statistics
summary(d1$OG)
# visualisation
ggplot(d1, aes(x=OG)) + 
  geom_histogram(color="white", fill="darkgrey")

# FG - Final Gravity
table(d1$FG)
# scale: ratio
# unit: 
# descriptive statistics
summary(d1$FG)
# visualisation
ggplot(d1, aes(x=FG)) + 
  geom_histogram(color="white", fill="darkgrey")

# Boil Size
table(d1$BoilSize)
# scale: ratio
# unit: galon
# descriptive statistics
summary(d1$BoilSize)
# visualisation
ggplot(d1, aes(x=BoilSize)) + 
  geom_histogram(color="white", fill="darkgrey")

# Boil Time
table(d1$BoilTime)
# scale: ratio
# unit: minutes
# descriptive statistics
summary(d1$BoilTime)
# visualisation
ggplot(d1, aes(x=BoilTime)) + 
  geom_histogram(color="white", fill="darkgrey")

# Boil Gravity
class(d1$BoilGravity)
d1$BoilGravity <- as.numeric(d1$BoilGravity)
table(d1$BoilGravity)
# scale: ratio
# unit: 
# descriptive statistics
summary(d1$BoilGravity)
# visualisation
ggplot(d1, aes(x=BoilGravity)) + 
  geom_histogram(color="white", fill="darkgrey")

# Efficiency
table(d1$Efficiency)
# scale: ratio
# unit: percent
# descriptive statistics
summary(d1$Efficiency)
# visualisation
ggplot(d1, aes(x=Efficiency)) + 
  geom_histogram(color="white", fill="darkgrey")

# Mash Thickness
class(d1$MashThickness)
d1$MashThickness <- as.numeric(d1$MashThickness)
table(d1$MashThickness)
# scale: ratio
# unit: quarts of water per pound of grain
# descriptive statistics
summary(d1$MashThickness)
# visualisation
ggplot(d1, aes(x=MashThickness)) + 
  geom_histogram(color="white", fill="darkgrey")

# Sugar Scale
table(d1$SugarScale)
# scale: dichotomous
# visualisation
ggplot(d1 , aes(x=factor(SugarScale), fill=factor(SugarScale))) + 
  geom_bar() +
  theme(legend.position="none")

# Brew Method
table(d1$BrewMethod)
# scale: nominal
# visualisation
ggplot(d1 , aes(x=factor(BrewMethod), fill=factor(BrewMethod))) + 
  geom_bar() +
  theme(legend.position="none")

# Primary Temperature
class(d1$PrimaryTemp)
d1$PrimaryTemp <- as.numeric(d1$PrimaryTemp)
table(d1$PrimaryTemp)
# scale: interval
# unit: temperatura (Fahrenheit/Celsjusz/etc)
# descriptive statistics
summary(d1$PrimaryTemp)
# visualisation
ggplot(d1, aes(x=MashThickness)) + 
  geom_histogram(color="white", fill="darkgrey")


### Imputation of missing data
dim(d1)[1] # number of all observations
summary(aggr(d1, plot=FALSE))

## Boil Gravity
# Mean imputation
d1<-d1%>%
  mutate(BoilGravity2=if_else(is.na(BoilGravity), mean(BoilGravity,na.rm = T), BoilGravity))
# Median imputation
d1<-d1%>%
  mutate(BoilGravity3=if_else(is.na(BoilGravity), median(BoilGravity,na.rm = T), BoilGravity))
# Volatility comparison
summary(d1$BoilGravity)
var(d1$BoilGravity, na.rm = TRUE)
summary(d1$BoilGravity2)
var(d1$BoilGravity2)
summary(d1$BoilGravity3)
var(d1$BoilGravity3)
# Median imputation is better because variance is closer to the original.

## Mash Thickness
# Mean imputation
d1<-d1%>%
  mutate(MashThickness2=if_else(is.na(MashThickness), mean(MashThickness,na.rm = T), MashThickness))
# Median imputation
d1<-d1%>%
  mutate(MashThickness3=if_else(is.na(MashThickness), median(MashThickness,na.rm = T), MashThickness))
# Volatility comparison
summary(d1$MashThickness)
var(d1$MashThickness, na.rm = TRUE)
summary(d1$MashThickness2)
var(d1$MashThickness2)
summary(d1$MashThickness3)
var(d1$MashThickness3)
# Median imputation is better because variance is closer to the original.

## Primary Temperature
# Mean imputation
d1<-d1%>%
  mutate(PrimaryTemp2=if_else(is.na(PrimaryTemp), mean(PrimaryTemp,na.rm = T), PrimaryTemp))
# Median imputation
d1<-d1%>%
  mutate(PrimaryTemp3=if_else(is.na(PrimaryTemp), median(PrimaryTemp,na.rm = T), PrimaryTemp))
# Volatility comparison
summary(d1$PrimaryTemp)
var(d1$PrimaryTemp, na.rm = TRUE)
summary(d1$PrimaryTemp2)
var(d1$PrimaryTemp2)
summary(d1$PrimaryTemp3)
var(d1$PrimaryTemp3)
# Median imputation is better because variance is closer to the original.


### Outliers analysis
check_outlier <- function(x) {
  # plots
  par(mfrow = c(1, 3))
  hist(x, main = "Histogram")
  boxplot(x, main = "Boxplot")
  qqnorm(x, main = "Normal Q-Q plot")
  # stats
  mean <- mean(x)
  std <- sd(x)
  Tmin <- mean-(3*std)
  Tmax <- mean+(3*std)
  IQR <- IQR(x)
  # outliers
  outliers <- length(x[which(x < Tmin | x > Tmax)])
  return(cbind(mean, std, Tmin, Tmax, IQR, outliers))
}
remove_outliers <- function(x) {
  mean <- mean(x)
  std <- sd(x)
  Tmin <- mean-(3*std)
  Tmax <- mean+(3*std)
  return(x[which(x > Tmin & x < Tmax)])
}

# ABV
check_outlier(d1$ABV)
ABV_2 <- remove_outliers(d1$ABV)
summary(ABV_2)
# OG
check_outlier(d1$OG)
OG_2 <- remove_outliers(d1$OG)
summary(OG_2)
# FG
check_outlier(d1$FG)
FG_2 <- remove_outliers(d1$FG)
summary(FG_2)
# Boil Size
check_outlier(d1$BoilSize)
BoilSize_2 <- remove_outliers(d1$BoilSize)
summary(BoilSize_2)
# Boil Time
check_outlier(d1$BoilTime)
BoilTime_2 <- remove_outliers(d1$BoilTime)
summary(BoilTime_2)
# Boil Gravity
check_outlier(d1$BoilGravity3)
BoilGravity_2 <- remove_outliers(d1$BoilGravity3)
summary(BoilGravity_2)
# Efficiency
check_outlier(d1$Efficiency)
Efficiency_2 <- remove_outliers(d1$Efficiency)
summary(Efficiency_2)
# Mash Thickness
check_outlier(d1$MashThickness3)
MashThickness_2 <- remove_outliers(d1$MashThickness3)
summary(MashThickness_2)
# Primary Temperature
check_outlier(d1$PrimaryTemp3)
PrimaryTemp_2 <- remove_outliers(d1$PrimaryTemp3)
summary(PrimaryTemp_2)


### Selection of variables for the study
# Correlation analysis between variables
d_cor <- data.frame(d1$ABV, d1$OG, d1$BoilSize, d1$BoilTime, d1$BoilGravity3, d1$Efficiency, d1$MashThickness3, d1$PrimaryTemp3)
cor_matrix <- cor(d_cor)
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black")

# High correlation occurred only between the OG and Boil Gravity variables.
# Except for this pair, the variables showed almost zero correlation, so all of the variables examined above, except for Boil Gravity, will be selected for the study.

### Data sampling
# Selection of units will be done using stratified sampling to keep equal proportions of each brewing method.
d2 <- data.frame(d1$BeerID, d1$ABV, d1$OG, d1$BoilSize, d1$BoilTime, d1$Efficiency, d1$MashThickness3, d1$SugarScale, d1$BrewMethod, d1$PrimaryTemp3)
colnames(d2) <- c("BeerID","ABV","OG","BoilSize","BoilTime","Efficiency","MashThickness","SugarScale","BrewMethod","PrimaryTemp")

set.seed(1)
d2 %>%
  group_by(BrewMethod) %>%
  sample_n(., 1000) -> sample1

