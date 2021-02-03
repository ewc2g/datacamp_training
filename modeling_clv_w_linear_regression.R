# Pass a vector of strings with packages required in the analysis
InstallPackages <- function(a){
  # check to see if packages in passed vector are already installed. 
  # If not, then put the package name in new.packages
  new.packages <- a[!(a %in% installed.packages()[ , "Package"])]
  # If there is anything listed in new.packages, then install it  
  if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  # Require all packages in original passed vector of required packages
  sapply(a, require, character.only = TRUE)
}
# Use InstallPackage"s
pkgs <- c("tidyverse",
          "ggplot2",
          "dplyr",
          "corrplot",
          "GGally",
          "rms",
          "stats",
          "MASS")

InstallPackages(pkgs)


#LOAD DATA
setwd("C:/Users/eric.clements/OneDrive - Ramsey Solutions/eric.clements/Documents/Current Learning/Machine Learning for Marketing Analytics/Modeling CLV")
salesData <- read.csv("salesData.csv", stringsAsFactors = FALSE)
str(salesData)

#### CORRELATIONS ####
######################

#Structure of dataset
str(salesData, give.attr = FALSE)


#CHECK CORRELATIONS
salesData %>% 
  select(nOrders, nItems, margin, futureMargin, marginPerOrder, marginPerItem, itemsPerOrder, daysSinceLastOrder, returnRatio, shareOwnBrand, shareVoucher, shareSale) %>% 
  cor() %>% corrplot()


#PAIRPLOT OF OUR VARIABLES
ggpairs(salesData[,c(2,3,4,5,6,7,8,9,11,12,13,14,15)])


# Visualization of correlations
salesData %>% select_if(is.numeric) %>%
  select(-id) %>%
  cor() %>%
  corrplot()


# Frequent stores
ggplot(salesData) +
  geom_boxplot(aes(x = mostFreqStore, y = salesThisMon))


# Preferred brand
ggplot(salesData) +
  geom_boxplot(aes(x = preferredBrand, y = salesThisMon))

## WE WANT A HIGH CORRELATION FACTOR TO HELP PREDICT FUTURE SALES/MARGIN/REVENUE WHATEVER
## SALES LAST 3 MONTHS ARE HIGHLY CORRELATED WITH SALES THIS MONTH, SHOULD BE A GOOD PREDICTOR


# Model specification using lm
salesSimpleModel <- lm(salesThisMon ~ salesLast3Mon, 
                       data = salesData)


# Looking at model summary
summary(salesSimpleModel)

#To systematically check all model variables for multi-colinearity we calculate the Variance Inflation Factors
#Using the vif function from the package rms
#A vif higher than 5 is problematic and values above 10 indicate poor regression estimates


# Estimating the full model
salesModel1 <- lm(salesThisMon ~ . - id, 
                  data = salesData)


# Checking variance inflation factors
vif(salesModel1)


# Estimating new model by removing information on brand
salesModel2 <- lm(salesThisMon ~ . - id - preferredBrand - nBrands, 
                  data = salesData)


# Checking variance inflation factors
vif(salesModel2)


#Read in new data set that the model will use to predict future revenues
salesData2_4 <- read.csv("salesData2_4.csv", stringsAsFactors = FALSE)


# getting an overview of new data
summary(salesData2_4)


# predicting sales
predSales5 <- predict(salesModel2, newdata = salesData2_4)


# calculating mean of future sales
mean(predSales5)

exp(-4.34+0.68)/(1+exp(-4.34+0.68))
exp(1.5497)

fr <- (15 * 1.5497)
exp(-4.34+(15*1.5497))/(1+exp(-4.34+(15*1.5497)))
