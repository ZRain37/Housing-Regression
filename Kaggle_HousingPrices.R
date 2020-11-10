library(caret)
library(data.table)
library(Boruta)
library(pROC)
library(dplyr)
library(plyr)

HouseIndex <- read.csv("~/Downloads/Kaggle_houseprice.csv", header=FALSE)
View(HouseIndex)
library(readr)
HouseIndex <- read_csv("Downloads/Kaggle_houseprice.csv")

ID.Var <- "Id"
Tar.Var <- "SalePrice"
sample.df <- as.data.frame(HouseIndex)

candidate.var <- setdiff(names(sample.df), c(ID.Var, Tar.Var))
data.type <- sapply(candidate.var, function(x){class(sample.df[[x]])})
table(data.type)
print(data.type)

explan.att <- setdiff(names(sample.df), c(ID.Var, Tar.Var))
data.classes <- sapply(explan.att, function(x){class(sample.df[[x]])})
unique.classes <- unique(data.classes)
attr.data.types <- lapply(unique.classes, function(x){names(data.classes[data.classes == x])})
names(attr.data.types) <- unique.classes

response <- sample.df$SalePrice
sample.df <- sample.df[candidate.var]

for(x in attr.data.types$integer){
  sample.df[[x]][is.na(sample.df[[x]])] <- -1
}
for(x in attr.data.types$character){
  sample.df[[x]][is.na(sample.df[[x]])] <- "*MISSING*"
}
if(any(is.na(sample.df))){
  sample.df[is.na(sample.df)] <- 0
}

set.seed(13)
bor.results <- Boruta(sample.df, response,
                      maxRuns = 101,
                      doTrace = 0)
print(bor.results)
getSelectedAttributes(bor.results)
plot(bor.results)

confirmedAtt <- getSelectedAttributes(bor.results)
print(confirmedAtt)

linMod <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+Alley+LandContour+
               Neighborhood+BldgType+HouseStyle+OverallQual+OverallCond+
               YearBuilt+YearRemodAdd+Exterior1st+Exterior2nd+MasVnrArea+
               ExterQual+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+
               BsmtFinType2+BsmtUnfSF+TotalBsmtSF+HeatingQC+CentralAir+KitchenQual+
               GrLivArea+BsmtFullBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+
               TotRmsAbvGrd+Functional, data = HouseIndex)
summary(linMod)

linMod <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+Alley+LandContour+
               Neighborhood+BldgType+HouseStyle+OverallQual+OverallCond+YearBuilt+
               YearRemodAdd+Exterior1st+Exterior2nd+MasVnrArea+ExterQual+Foundation+
               BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+
               BsmtUnfSF+TotalBsmtSF+HeatingQC+CentralAir+1stFlrSF+2ndFlrSF+
               GrLivArea+BsmtFullBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+
               KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+FireplaceQu+GarageType+
               GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+GarageCond+
               PavedDrive+WoodDeckSF+OpenPorchSF+Fence, data = HouseIndex)
summary(linMod)
