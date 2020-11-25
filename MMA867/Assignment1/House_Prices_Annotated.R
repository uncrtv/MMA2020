##### MMA867 - PREDICTIVE MODELING #####
##### ASSIGNMENT 1
##### Nick Nguyen
##### Last Updated: April 27, 2019
##### Annotations Added: November 25, 2020

##### General Comments #####
## This is the first assignment I ever did in the MMA program,
## so the code is very messy/rudimentary and the process is not very streamlined.
## I was also taught to run individual lines/blocks of code at a time in RStudio,
## resulting in some code being out of order, so don't try to run this entire script.
## I only knew the basics of R and how to use basic libraries like
## caret and glmnet.
## The assignment was also restricted to linear regression only
## so no trees or tree-based algorithms (RF, XGBoost) were used.
## I did manage to manually create some ensembles though.

##### LIBRARIES #####
## `mice` for missing values and imputation
library(mice)

## `corrplot` for correlation plots
library(corrplot)

## `caret` for all your machine learning needs
library(caret)

## `randomForest` for Random Forests
library(randomForest)

## `glmnet` for regularized regression models (LASSO, Ridge)
library(glmnet)

## This line just prevents the console output from being cut off if it's too long
options(max.print=100000)

##### LOAD THE DATA #####
## Data Source:
## https://www.kaggle.com/c/house-prices-advanced-regression-techniques
training <- read.csv('train.csv')
submit <- read.csv('test.csv')

## Create a `SalePrice` column in the submission list
submit$SalePrice <- NA

## Merge the two datasets so that any data cleaning
## or transformation happens on both
data <- rbind(training, submit)

##### EXPLORATION #####
## See the first few lines using `head()`, structure using `str()`,
## and summary statistics using `summary()`
head(data)
str(data)
summary(data)

summary(submit)

##### OUTLIERS #####
#data <- subset(data, (GrLivArea < 4000))

##### CLEANING #####
# First change SubClass to factors, the values have no numerical meaning.
data$MSSubClass <- as.factor(data$MSSubClass)

# Same for all the date-related ones
data$MoSold <- as.factor(data$MoSold)
data$YrSold <- as.factor(data$YrSold)
#data$YearBuilt <- as.factor(data$YearBuilt)
#data$YearRemodAdd <- as.factor(data$YearRemodAdd)

# For categorical variables, we can add NA as a level. This makes sense.
data$Alley <- addNA(data$Alley)
data$MasVnrType <- addNA(data$MasVnrType)
data$BsmtQual <- addNA(data$BsmtQual)
data$BsmtCond <- addNA(data$BsmtCond)
data$BsmtExposure <- addNA(data$BsmtExposure)
data$BsmtFinType1 <- addNA(data$BsmtFinType1)
data$BsmtFinType2 <- addNA(data$BsmtFinType2)
data$Electrical <- addNA(data$Electrical)
data$FireplaceQu <- addNA(data$FireplaceQu)
data$GarageType <- addNA(data$GarageType)
data$GarageFinish <- addNA(data$GarageFinish)
data$GarageQual <- addNA(data$GarageQual)
data$GarageCond <- addNA(data$GarageCond)
data$PoolQC <- addNA(data$PoolQC)
data$Fence <- addNA(data$Fence)
data$MiscFeature <- addNA(data$MiscFeature)

# GarageYrBlt is also a categorical variable (year number)
#data$GarageYrBlt <- addNA(data$GarageYrBlt)

# For "area" variables, NA should mean 0, so we will substitute
data$MasVnrArea[is.na(data$MasVnrArea)] <- 0
data$GarageYrBlt[is.na(data$GarageYrBlt)] <- 0
data$GarageArea[is.na(data$GarageArea)] <- 0
data$GarageCars[is.na(data$GarageCars)] <- 0
data$BsmtFinSF1[is.na(data$BsmtFinSF1)] <- 0
data$BsmtFinSF2[is.na(data$BsmtFinSF2)] <- 0
data$BsmtUnfSF[is.na(data$BsmtUnfSF)] <- 0
data$TotalBsmtSF[is.na(data$TotalBsmtSF)] <- 0
data$BsmtFullBath[is.na(data$BsmtFullBath)] <- 0
data$BsmtHalfBath[is.na(data$BsmtHalfBath)] <- 0

# LotFrontage can use mean substitution
data$LotFrontage[is.na(data$LotFrontage)] <- mean(data$LotFrontage, na.rm = TRUE)

# Zoning: use the most popular value
# Side note: why doesn't R have a function for this???
summary(data$MSZoning)
data$MSZoning[is.na(data$MSZoning)] <- "RL"
summary(data$Functional)
data$Functional[is.na(data$Functional)] <- "Typ"
summary(data$Utilities)
data$Utilities[is.na(data$Utilities)] <- "AllPub"
summary(data$SaleType)
data$SaleType[is.na(data$SaleType)] <- "WD"
summary(data$KitchenQual)
data$KitchenQual[is.na(data$KitchenQual)] <- "TA"
summary(data$Exterior1st)
summary(data$Exterior2nd)
data$Exterior1st[is.na(data$Exterior1st)] <- "VinylSd"
data$Exterior2nd[is.na(data$Exterior2nd)] <- "VinylSd"

# Let's see what's still missing...
md.pattern(data)

##### The following commented section was created
##### before I decided to merge the training and submission sets
##### but left in because I couldn't bring myself to delete it :-)

# # Let's clean the test data as well
# submit$MSSubClass <- as.factor(submit$MSSubClass)
# 
# # Same for all the date-related ones
# submit$MoSold <- as.factor(submit$MoSold)
# submit$YrSold <- as.factor(submit$YrSold)
# #submit$YearBuilt <- as.factor(submit$YearBuilt)
# #submit$YearRemodAdd <- as.factor(submit$YearRemodAdd)
# 
# submit$Alley <- addNA(submit$Alley)
# submit$MasVnrType <- addNA(submit$MasVnrType)
# submit$BsmtQual <- addNA(submit$BsmtQual)
# submit$BsmtCond <- addNA(submit$BsmtCond)
# submit$BsmtExposure <- addNA(submit$BsmtExposure)
# submit$BsmtFinType1 <- addNA(submit$BsmtFinType1)
# submit$BsmtFinType2 <- addNA(submit$BsmtFinType2)
# submit$Electrical <- addNA(submit$Electrical)
# submit$FireplaceQu <- addNA(submit$FireplaceQu)
# submit$GarageType <- addNA(submit$GarageType)
# submit$GarageFinish <- addNA(submit$GarageFinish)
# submit$GarageQual <- addNA(submit$GarageQual)
# submit$GarageCond <- addNA(submit$GarageCond)
# submit$PoolQC <- addNA(submit$PoolQC)
# submit$Fence <- addNA(submit$Fence)
# submit$MiscFeature <- addNA(submit$MiscFeature)
# 
# # GarageYrBlt is also a categorical variable (year number)
# #submit$GarageYrBlt <- addNA(submit$GarageYrBlt)
# 
# # For "area" variables, NA should mean 0, so we will substitute
# submit$LotFrontage[is.na(submit$LotFrontage)] <- 0
# submit$MasVnrArea[is.na(submit$MasVnrArea)] <- 0
# 
# # Change the 150 as well
# submit$MSSubClass[submit$MSSubClass == "150"] <- NA
# 
# submit <- na.roughfix(submit)
# 
# 
# 
# md.pattern(submit)


##############
submit$Alley <- addNA(submit$Alley)
submit$MasVnrType <- addNA(submit$MasVnrType)
submit$BsmtQual <- addNA(submit$BsmtQual)
submit$BsmtCond <- addNA(submit$BsmtCond)
submit$BsmtExposure <- addNA(submit$BsmtExposure)
submit$BsmtFinType1 <- addNA(submit$BsmtFinType1)
submit$BsmtFinType2 <- addNA(submit$BsmtFinType2)
submit$Electrical <- addNA(submit$Electrical)
submit$FireplaceQu <- addNA(submit$FireplaceQu)
submit$GarageType <- addNA(submit$GarageType)
submit$GarageFinish <- addNA(submit$GarageFinish)
submit$GarageQual <- addNA(submit$GarageQual)
submit$GarageCond <- addNA(submit$GarageCond)
submit$PoolQC <- addNA(submit$PoolQC)
submit$Fence <- addNA(submit$Fence)
submit$MiscFeature <- addNA(submit$MiscFeature)

submit$GarageYrBlt <- addNA(submit$GarageYrBlt)
submit$LotFrontage <- addNA(submit$LotFrontage)
submit$MasVnrArea <- addNA(submit$MasVnrArea)

submit$SaleType <- addNA(submit$SaleType)
#submit$KitchenQual <- addNA(submit$KitchenQual)
submit$Exterior2nd <- addNA(submit$Exterior2nd)
#submit$Exterior1st <- addNA(submit$Exterior1st)
#submit$Utilities <- addNA(submit$Utilities)
#submit$MSZoning <- addNA(submit$MSZoning)
submit$MSZoning[is.na(submit$MSZoning)] <- "RL"
submit$Utilities[is.na(submit$Utilities)] <- "AllPub"
submit$Exterior1st[is.na(submit$Exterior1st)] <- "VinylSd"
submit$Functional[is.na(submit$Functional)] <- "Typ"
submit$KitchenQual[is.na(submit$KitchenQual)] <- "TA"
#submit$SaleType[is.na(submit$Utilities)] <- "AllPub"

md.pattern(submit)


##### MISSING DATA #####
## Ignore this section, I believe this was done before I decided
## to manually fill in the missing values
md.pattern(imputed_1)

submit_imputed <- mice(submit, m = 1, maxit = 5, method = 'cart')
submit_imputed1 <- complete(submit_imputed, 1)
md.pattern(submit_imputed1)

##### SPLITTING #####
## Two ways of splitting data:
## 1. Manually, as below, splitting into observations with ID <= 1200 and ID >= 1201

train <- subset(data, (Id <= 1200))
test  <- subset(data, (Id >= 1201))
# We have to split at 1250 because row 1231 contains the only RRAe in the Condition2 column
# for the entire dataset. Sigh...

## This is only necessary for `caret` because it will throw an error when making predictions
## if it encounters a factor level that it's never seen before in the training set
train_1 <- subset(imputed_1, (Id <= 1300))
test_1 <- subset(imputed_1, (Id >= 1301))

outcome <- data$SalePrice

## 2. Using `createDataPartition` from caret to randomly select observations
## only specifying the ratio. I chose 0.5 here for some reason
## List = False means that it will return the ID of observations, which we will index to create the splits below
partition <- createDataPartition(y=outcome,
                                 p=.5,
                                 list=F)

train <- data[partition,]
test <- data[-partition,]

##### REGRESSION #####
# One with everything - Dalai Lama
## Create linear regression model (using `lm`) with all features
r001 <- lm (SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape
            + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2
            + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle
            + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond
            + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2
            + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical
            + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath
            + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces
            + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual
            + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch
            + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold, train_1)

## Look at the summary of the model (coefficients, p-values, etc.)
summary(r001)

## Generate a prediction
predicted_test <- predict(r001, test_11)

## Manually removing some observations because they were giving errors
test_11 <- subset(test_1, (Id != 1371 & Id != 1322 & Id != 1380 & Id != 1387))

## Calculating the log RMSE (manually)
## You can see it in the code:
## sqrt: root
## mean: mean
## log: log
## ^2: squared
sqrt(mean((log(test_11$SalePrice) - log(predicted_test))^2))

## Create log price column so it's faster to calculate the log RMSE in the future
train_1$LogPrice <- log(train_1$SalePrice)


## Second model, removing features with high p-values (meaning they are not significant)
## Not gonna comment on the code here, similar to the first model
r002 <- lm (LogPrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape
            + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2
            + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle
            + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond
            + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2
            + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical
            + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath
            + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces
            + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual
            + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch
            + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold, train_1)

predicted_test002 <- exp(predict(r002, test_11))

#errors002 <- abs((test_11$SalePrice - predicted_test002)/test_11$SalePrice)*100
#mean(errors002)

rmse002 <- sqrt(mean((log(test_11$SalePrice) - log(predicted_test002))^2))

summary(r002)

## Create another model, removing some more features
r004 <- lm (LogPrice ~ MSZoning + LotArea
            + LandContour + Utilities + LandSlope + Neighborhood + Condition1 + Condition2
            + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle
            + RoofMatl + Exterior1st
            + BsmtQual + BsmtExposure + BsmtFinSF1
            + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir
            + X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + BsmtHalfBath + FullBath
            + HalfBath + KitchenQual + TotRmsAbvGrd + Functional
            + GarageType + GarageArea + GarageQual
            + WoodDeckSF + EnclosedPorch + ScreenPorch
            , train_1)

summary(r004)

p004 <- exp(predict(r004, test_11))

rmse004 <- sqrt(mean((log(test_11$SalePrice) - log(p004))^2))

##### REGRESSION USING IMPUTED DATASETS #####
## I think there were too many errors so I decided to use MICE to impute the dataset
## and then create models for more stability
imputed_1$LogPrice <- log(imputed_1$SalePrice)

r005 <- lm (LogPrice ~ MSZoning + LotArea
            + LandContour + Utilities + LandSlope + Neighborhood + Condition1 + Condition2
            + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle
            + RoofMatl + Exterior1st
            + BsmtQual + BsmtExposure + BsmtFinSF1
            + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir
            + X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath + BsmtHalfBath + FullBath
            + HalfBath + KitchenQual + TotRmsAbvGrd + Functional
            + GarageType + GarageArea + GarageQual
            + WoodDeckSF + EnclosedPorch + ScreenPorch
            , imputed_1)

## Look at coef, p-values
summary(r005)

## Looks good, generate predictions on the submission dataset
p005 <- exp(predict(r005, submit))
p005 <- exp(predict(r005, submit_imputed1))

## One last check to make sure the predictions for submission look good,
## no errors, no missing values
summary(p005)
md.pattern(p005)
print(p005)

## Create a dataframe with the row ID next to the predictions
## (predictions were only a 1-dimensional vector before this)
result_005 <- data.frame(submit$Id, p005)

## Name the columns per the specifications for submission
colnames(result_005) <- c('Id', 'SalePrice')

## Write the dataframe to a CSV file and submit to Kaggle
write.csv(result_005, 'p005.csv', row.names = FALSE)

# This model got 0.13 something leaderboard, top 52%. Not good enough. Let's reset.

##### SECOND ATTEMPT #####
# Find collinearity
## using the correlation heatmap/matrix/whatever you want to call it
imputed1n <- imputed1[, sapply(imputed1, is.numeric)]
correlation <- cor(imputed1n)
corrplot(correlation)

## The below sections will be similar to above
# One with everything
r06 <- lm(SalePrice ~ ., train)
summary(r06)

# Remove collinear variables: TotalBsmtSF, TotRmsAbvGrd, GarageArea
r07 <- lm (SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape
    + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2
    + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle
    + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond
    + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2
    + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC + CentralAir + Electrical
    + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath
    + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + Functional + Fireplaces
    + FireplaceQu + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageQual
    + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch
    + PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold, train)
summary(r07)

# Drop insignificant
r08 <- lm (SalePrice ~ MSSubClass + MSZoning + LotArea
           + LotConfig + LandSlope + Neighborhood + Condition1
           + OverallQual + OverallCond + YearBuilt + YearRemodAdd
           + RoofMatl + MasVnrArea 
           + BsmtQual + BsmtExposure + BsmtFinSF1
           + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC
           + X1stFlrSF + X2ndFlrSF  
           + BedroomAbvGr + KitchenQual + Functional
           + WoodDeckSF + ScreenPorch
           , train)
summary(r08)

# Drop more
r09 <- lm (SalePrice ~ MSSubClass + MSZoning + LotArea
           + LotConfig + LandSlope + Neighborhood + Condition1
           + OverallQual + OverallCond + YearBuilt + YearRemodAdd
           + RoofMatl + MasVnrArea 
           + BsmtQual + BsmtExposure + BsmtFinSF1
           + BsmtFinSF2 + BsmtUnfSF + HeatingQC
           + X1stFlrSF + X2ndFlrSF  
           + BedroomAbvGr + KitchenQual
           + WoodDeckSF + ScreenPorch
           , train)
summary(r09)

test2 <- subset(test, (YearBuilt != 1905 & YearBuilt != 1917 & YearBuilt != 1872 & YearBuilt != 1911
                       & RoofMatl != "Roll"))

p09 <- predict(r09, test2)

rmse09 <- sqrt(mean((log(test2$SalePrice) - log(p09))^2))

# Use log
## It is at this point where I figured I should try to predict the Log Price instead of the actual price
## This flattens the distribution and can improve the model if the relationship is not linear
train$LogPrice <- log(train$SalePrice)
r10 <- lm (LogPrice ~ MSSubClass + MSZoning + LotArea
           + LotConfig + LandSlope + Neighborhood + Condition1
           + OverallQual + OverallCond + YearBuilt + YearRemodAdd
           + RoofMatl + MasVnrArea 
           + BsmtQual + BsmtExposure + BsmtFinSF1
           + BsmtFinSF2 + BsmtUnfSF + HeatingQC
           + X1stFlrSF + X2ndFlrSF  
           + KitchenQual
           + WoodDeckSF + ScreenPorch
           , train)
summary(r10)
p10 <- exp(predict(r10, test2))
rmse10 <- sqrt(mean((log(test2$SalePrice) - log(p09))^2))

p10_1 <- exp(predict(r10, submit))

# Do the whole train data
data$LogPrice <- log(data$SalePrice)

r11 <- lm (LogPrice ~ MSSubClass + MSZoning + LotArea
           + LotConfig + LandSlope + Neighborhood + Condition1
           + OverallQual + OverallCond + YearBuilt + YearRemodAdd
           + RoofMatl + MasVnrArea 
           + BsmtQual + BsmtExposure + BsmtFinSF1
           + BsmtFinSF2 + BsmtUnfSF + HeatingQC
           + X1stFlrSF + X2ndFlrSF  
           + KitchenQual
           + WoodDeckSF + ScreenPorch
           , data)
summary(r11)

p11 <- exp(predict(r11, submit))
summary(p11)

## Second submission
result11 <- data.frame(Id = submit$Id, SalePrice = p11)
write.csv(result11, 'p11.csv', row.names = FALSE)

# This one got 0.12, top 39%

r12 <- lm (LogPrice ~ MSSubClass + MSZoning + LotArea  
           + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2
           + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle
           + RoofMatl + MasVnrArea + ExterQual
           + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1
           + BsmtFinSF2 + BsmtUnfSF
           + X1stFlrSF + X2ndFlrSF + FullBath
           + BedroomAbvGr + KitchenQual
           + GarageCars + GarageQual
           + GarageCond + WoodDeckSF + ScreenPorch
           + PoolArea + PoolQC + MoSold + YrSold, train)
summary(r12)

test3 <- subset(test, (Condition2 != "RRAe" & RoofStyle != "Shed" & RoofMatl != "ClyTile" & RoofMatl != "Roll"))

p12 <- exp(predict(r12, test3))
rmse12 <- sqrt(mean((log(test3$SalePrice) - log(p12))^2))


r13 <- lm (LogPrice ~ MSSubClass + MSZoning + LotArea  
           + LandSlope + Neighborhood + Condition1 + Condition2
           + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle
           + RoofMatl + MasVnrArea
           + BsmtQual + BsmtCond + BsmtExposure + BsmtFinSF1
           + BsmtFinSF2 + BsmtUnfSF
           + X1stFlrSF + X2ndFlrSF
           + KitchenQual
           + GarageCars + GarageQual
           + WoodDeckSF + ScreenPorch
           + YrSold, data)
summary(r13)

p13 <- exp(predict(r13, submit))
summary(p11)

result13 <- data.frame(Id = submit$Id, SalePrice = p13)
write.csv(result13, 'p13.csv', row.names = FALSE)

##### THIRD ATTEMPT #####
# Feature Engineering
# Log Price
data$LogPrice <- log(data$SalePrice)

# Total SF
data$TotalSF <- data$TotalBsmtSF + data$X1stFlrSF + data$X2ndFlrSF
data$TotalBaths <- data$FullBath + data$BsmtFullBath + 0.5 * (data$HalfBath + data$BsmtHalfBath)

# Remove some highly collinear variables
# Find collinearity
# data_num <- data[, sapply(data, is.numeric)]
# correlation <- cor(data_num)
# corrplot(correlation)

# Drop
#data <- subset(data, select = - c(Utilities, Street, PoolQC))

# Split train and test again
#training_clean <- subset(data, (Id <= 1460))
testing_clean <- subset(data, (Id >= 1461))
#summary(training_clean$SalePrice)
#plot(training_clean$GrLivArea, training_clean$SalePrice)

# Remove outliers in train
training_clean <- subset(data, (Id <= 1460 & Id != 524 & Id != 1299))


##### FOLD 1 #####
y5 <- training_clean$LogPrice
x55 <- training_clean[,-c(81,82)]
x5 <- model.matrix(Id ~ ., x55)[,-1]

train1 <- subset(training_clean, (Id <= 1000))
y1 <- train1$LogPrice
x11 <- train1[,-c(81,82)]
x1 <- model.matrix(Id ~ ., x11)[,-1]
test1 <- subset(training_clean, (Id > 1000))
x21 <- test1[,-c(81,82)]
x2 <- model.matrix(Id ~ ., x21)[,-1]

## Create LASSO model (thus alpha = 1)
lasso_fit <- glmnet(x = x1, y = y1, alpha = 1)
summary(lasso_fit)
plot(lasso_fit, xvar = "lambda")

## Cross-validation to find optimal lambda
crossval <- cv.glmnet(x = x1, y = y1, alpha = 1)
plot(crossval)
penalty <- crossval$lambda.min
log(penalty)
lasso2 <- glmnet(x = x1, y = y1, alpha = 1, lambda = penalty)
coef(lasso2)

## Predict and calculate RMSE
p1 <- exp(predict(lasso2, s = penalty, newx = x2))
rmse30 <- sqrt(mean((log(p1) - log(test1$SalePrice))^2))

## Export LASSO predictions, submit to Kaggle
x31 <- testing_clean[,-c(81,82)]
x3 <- model.matrix(Id ~ ., x31)[,-1]
p31 <- exp(predict(lasso2, s = penalty, newx = x3))

result31 <- data.frame(Id = submit$Id, SalePrice = p31)
colnames(result31) <- c('Id', 'SalePrice')
write.csv(result31, 'p31.csv', row.names = FALSE)

## Create Ridge regression model (alpha = 0)
ridge_fit <- glmnet(x = x1, y = y1, alpha = 0)
ridge_crossval <- cv.glmnet(x = x1, y = y1, alpha = 0)
ridge_penalty <- ridge_crossval$lambda.min
ridge2 <- glmnet(x = x1, y = y1, alpha = 0, lambda = ridge_penalty)
coef(ridge2)
ridge_p31 <- exp(predict(ridge2, s = ridge_penalty, newx = x2))
rmse30_ridge <- sqrt(mean((log(ridge_p31) - log(test1$SalePrice))^2))

## Export Ridge predictions for submission
ridge_result31 <- data.frame(Id = submit$Id, SalePrice = ridge_p31)
colnames(ridge_result31) <- c('Id', 'SalePrice')
write.csv(ridge_result31, 'ridge_p31.csv', row.names = FALSE)

##### 4 FOLDS #####
## In this section I manually created a rudimentary ensemble by splitting the dataset into 4
## and training a model on each partition, then average the predictions
## The code should be popular to LASSO above, just repeated 4 times on different datasets
# Split into train1, train2, train3, train4
train1 <- subset(training_clean, (Id > 365))
train2 <- subset(training_clean, (Id <= 365 | Id > 365*2))
train3 <- subset(training_clean, (Id <= 365*2 | Id > 365*3))
train4 <- subset(training_clean, (Id <= 365*3))

# Test
test1 <- subset(training_clean, (Id <= 365))
test2 <- subset(training_clean, (Id > 365 & Id <= 365*2))
test3 <- subset(training_clean, (Id > 365*2 & Id <= 365*3))
test4 <- subset(training_clean, (Id > 365*3))

# LASSO Fold 1
y1 <- train1$LogPrice
x1 <- model.matrix(Id ~ ., train1[,-c(81,82)])[,-1]
x1t <- model.matrix(Id ~ ., test1[,-c(81,82)])[,-1]

#lasso1 <- glmnet(x = x1, y = y1, alpha = 1)
crossval1 <- cv.glmnet(x = x1, y = y1, alpha = 1)
#plot(crossval)
penalty1 <- crossval1$lambda.min
#log(penalty)
lasso1 <- glmnet(x = x1, y = y1, alpha = 1, lambda = penalty1)
#coef(lasso2)
pl1 <- exp(predict(lasso1, s = penalty1, newx = x1t))
rmsepl1 <- sqrt(mean((log(pl1) - log(test1$SalePrice))^2))

# LASSO Fold 2
y2 <- train2$LogPrice
x2 <- model.matrix(Id ~ ., train2[,-c(81,82)])[,-1]
x2t <- model.matrix(Id ~ ., test2[,-c(81,82)])[,-1]

crossval2 <- cv.glmnet(x = x2, y = y2, alpha = 1)
penalty2 <- crossval2$lambda.min
lasso2 <- glmnet(x = x2, y = y2, alpha = 1, lambda = penalty2)
pl2 <- exp(predict(lasso2, s = penalty2, newx = x2t))
rmsepl2 <- sqrt(mean((log(pl2) - log(test2$SalePrice))^2))

# LASSO Fold 3
y3 <- train3$LogPrice
x3 <- model.matrix(Id ~ ., train3[,-c(81,82)])[,-1]
x3t <- model.matrix(Id ~ ., test3[,-c(81,82)])[,-1]

crossval3 <- cv.glmnet(x = x3, y = y3, alpha = 1)
penalty3 <- crossval3$lambda.min
lasso3 <- glmnet(x = x3, y = y3, alpha = 1, lambda = penalty3)
pl3 <- exp(predict(lasso3, s = penalty3, newx = x3t))
rmsepl3 <- sqrt(mean((log(pl3) - log(test3$SalePrice))^2))

# LASSO Fold 4
y4 <- train4$LogPrice
x4 <- model.matrix(Id ~ ., train4[,-c(81,82)])[,-1]
x4t <- model.matrix(Id ~ ., test4[,-c(81,82)])[,-1]

crossval4 <- cv.glmnet(x = x4, y = y4, alpha = 1)
penalty4 <- crossval4$lambda.min
lasso4 <- glmnet(x = x4, y = y4, alpha = 1, lambda = penalty4)
pl4 <- exp(predict(lasso4, s = penalty4, newx = x4t))
rmsepl4 <- sqrt(mean((log(pl4) - log(test4$SalePrice))^2))

# Predict with the different models
testing_mm <- model.matrix(Id ~ ., testing_clean[,-c(81,82)])[,-1]
ps1 <- exp(predict(lasso1, s = penalty1, newx = testing_mm))
ps2 <- exp(predict(lasso2, s = penalty2, newx = testing_mm))
ps3 <- exp(predict(lasso3, s = penalty3, newx = testing_mm))
ps4 <- exp(predict(lasso4, s = penalty4, newx = testing_mm))

## Create a dataframe with the predictions of each model and use the mean as final prediction
pred40 <- data.frame(ps1, ps2, ps3, ps4)
colnames(pred40) <- c('ps1',"ps2", "ps3", 'ps4')
pred40$SalePrice <- (pred40$ps1+pred40$ps2+pred40$ps3+pred40$ps4)/4
write.csv(pred40, 'pred43.csv')

p40 <- data.frame(testing_clean$Id, pred40$SalePrice)
colnames(p40) <- c('Id', 'SalePrice')
write.csv(p40, 'p40.csv', row.names = FALSE)

#### Ridge #####
## Same thing but with Ridge, we don't need to split the dataset anymore
## It's already done above.
rcv1 <- cv.glmnet(x = x1, y = y1, alpha = 0)
rpen1 <- rcv1$lambda.min
ridge1 <- glmnet(x = x1, y = y1, alpha = 0, lambda = rpen1)
pr1 <- exp(predict(ridge1, s = rpen1, newx = x1t))
rmsepr1 <- sqrt(mean((log(pr1) - log(test1$SalePrice))^2))

rcv2 <- cv.glmnet(x = x2, y = y2, alpha = 0)
rpen2 <- rcv2$lambda.min
ridge2 <- glmnet(x = x2, y = y2, alpha = 0, lambda = rpen2)
pr2 <- exp(predict(ridge2, s = rpen2, newx = x2t))
rmsepr2 <- sqrt(mean((log(pr2) - log(test2$SalePrice))^2))

rcv3 <- cv.glmnet(x = x3, y = y3, alpha = 0)
rpen3 <- rcv3$lambda.min
ridge3 <- glmnet(x = x3, y = y3, alpha = 0, lambda = rpen3)
pr3 <- exp(predict(ridge3, s = rpen3, newx = x3t))
rmsepr3 <- sqrt(mean((log(pr3) - log(test3$SalePrice))^2))

rcv4 <- cv.glmnet(x = x4, y = y4, alpha = 0)
rpen4 <- rcv4$lambda.min
ridge4 <- glmnet(x = x4, y = y4, alpha = 0, lambda = rpen4)
pr4 <- exp(predict(ridge4, s = rpen4, newx = x4t))
rmsepr4 <- sqrt(mean((log(pr4) - log(test4$SalePrice))^2))

# Predict with the different models
#testing_mm <- model.matrix(Id ~ ., testing_clean[,-c(81,82)])[,-1]
psr1 <- exp(predict(ridge1, s = rpen1, newx = testing_mm))
psr2 <- exp(predict(ridge2, s = rpen2, newx = testing_mm))
psr3 <- exp(predict(ridge3, s = rpen3, newx = testing_mm))
psr4 <- exp(predict(ridge4, s = rpen4, newx = testing_mm))

## Average the predictions to get final prediction
pred_ridge40 <- data.frame(psr1, psr2, psr3, psr4)
colnames(pred_ridge40) <- c('psr1',"psr2", "psr3", 'psr4')
pred_ridge40$SalePrice <- (pred_ridge40$psr1+pred_ridge40$psr2+pred_ridge40$psr3+pred_ridge40$psr4)/4
write.csv(pred_ridge40, 'predridge43.csv')

p40 <- data.frame(testing_clean$Id, pred40$SalePrice)
colnames(p40) <- c('Id', 'SalePrice')
write.csv(p40, 'p40.csv', row.names = FALSE)

##### ElasticNet #####
## The same thing as above but with an ElasticNet model (alpha = 0.1)
rlcv1 <- cv.glmnet(x = x1, y = y1, alpha = 0.1)
rlpen1 <- rlcv1$lambda.min
rl1 <- glmnet(x = x1, y = y1, alpha = 0.1, lambda = rlpen1)
prl1 <- exp(predict(rl1, s = rlpen1, newx = x1t))
rmseprl1 <- sqrt(mean((log(prl1) - log(test1$SalePrice))^2))

rlcv2 <- cv.glmnet(x = x2, y = y2, alpha = 0.1)
rlpen2 <- rlcv2$lambda.min
rl2 <- glmnet(x = x2, y = y2, alpha = 0.1, lambda = rlpen2)
prl2 <- exp(predict(rl2, s = rlpen2, newx = x2t))
rmseprl2 <- sqrt(mean((log(prl2) - log(test2$SalePrice))^2))

rlcv3 <- cv.glmnet(x = x3, y = y3, alpha = 0.1)
rlpen3 <- rlcv3$lambda.min
rl3 <- glmnet(x = x3, y = y3, alpha = 0.1, lambda = rlpen3)
prl3 <- exp(predict(rl3, s = rlpen3, newx = x3t))
rmseprl3 <- sqrt(mean((log(prl3) - log(test3$SalePrice))^2))

rlcv4 <- cv.glmnet(x = x4, y = y4, alpha = 0.1)
rlpen4 <- rlcv4$lambda.min
rl4 <- glmnet(x = x4, y = y4, alpha = 0.1, lambda = rlpen4)
prl4 <- exp(predict(rl4, s = rlpen4, newx = x4t))
rmseprl4 <- sqrt(mean((log(prl4) - log(test4$SalePrice))^2))

psrl1 <- exp(predict(rl1, s = rlpen1, newx = testing_mm))
psrl2 <- exp(predict(rl2, s = rlpen2, newx = testing_mm))
psrl3 <- exp(predict(rl3, s = rlpen3, newx = testing_mm))
psrl4 <- exp(predict(rl4, s = rlpen4, newx = testing_mm))

pred_rl40 <- data.frame(psrl1, psrl2, psrl3, psrl4)
colnames(pred_rl40) <- c('psrl1',"psrl2", "psrl3", 'psrl4')
pred_rl40$SalePrice <- (pred_rl40$psrl1+pred_rl40$psrl2+pred_rl40$psrl3+pred_rl40$psrl4)/4
write.csv(pred_rl40, 'predrl40.csv')

##### Meta model ##### -------------
## This is a rudimentary attempt at stacking models.
## The idea is to create a "meta model" that predicts the sale price
## using the predictions of the underlying models (and the training data) as input.
## The rationale is that this meta model will identify strengths and weaknesses of each model
## and weigh their predictions accordingly, creating superior predictions than a simple average.

## I used the LASSO models here because they had the best performance.

# There are some magic numbers coding in here,
# I would fix if I had more time

## The column `meta` will be fore predictions, so we name it accordingly on the test set
meta_test <- cbind(testing_clean, meta = pred40$SalePrice)

## Merge the four LASSO models' predictions to the dataset
lasso_predictions <- rbind(pl1, pl2, pl3, pl4)
meta_train <- cbind(training_clean, meta = lasso_predictions)

## Change the column name to `meta` as discussed above
colnames(meta_train)[85] <- "meta"
colnames(meta_test)[85] <- "meta"

## Train a new model on this new dataset
metatestmm <- model.matrix(Id ~ ., meta_test[,-c(81,82)])[,-1]
metay <- meta_train$LogPrice
metax <- model.matrix(Id ~ ., meta_train[,-c(81,82)])[,-1]

metacv <- cv.glmnet(x = metax, y = metay, alpha = 0)
metapen <- metacv$lambda.min
metafit <- glmnet(x = metax, y = metay, alpha = 0, lambda = metapen)

## Export predictions
metapred <- exp(predict(metafit, s = metapen, newx = metatestmm))
write.csv(metapred, 'metapred42.csv')

coef(metafit)

#####nfolds#####
## This was an earlier attempt at n-folds, discard this
yfold <- training_clean$LogPrice
xfold <- model.matrix(Id ~ ., training_clean[,-c(81,82)])[,-1]
#x1t <- model.matrix(Id ~ ., test1[,-c(81,82)])[,-1]

#lasso1 <- glmnet(x = x1, y = y1, alpha = 1)
cvfold <- cv.glmnet(x = xfold, y = yfold, alpha = 1, nfolds = 100)
#plot(crossval)
pfold <- cvfold$lambda.min
#log(penalty)
lfold <- glmnet(x = xfold, y = yfold, alpha = 1, lambda = pfold)
#coef(lasso2)
pfold <- exp(predict(lfold, s = pfold, newx = testing_mm))
write.csv(pfold, 'fold.csv')
