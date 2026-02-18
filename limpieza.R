
# install.packages("mlbench")
library(mlbench)

data("BostonHousing")
set.seed(100)

BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
View(BostonHousing)

# Reto 1
temp1 <- BostonHousing
temp1[sample(1:nrow(temp1), 50), "crim"] <- NA
View(temp1)


# install.packages("mice")
library("mice")

# Reto 2
md.pattern(BostonHousing)
md.pattern(temp1)


lm(medv ~ ptratio + rad, data = BostonHousing, na.action = na.omit)

BostonHousing$nox = NA
BostonHousingsinnox = BostonHousing[ , -5]


# Reto 3 
temp1$zn = NA
temp1$zn <- NULL


# install.packages("Hmisc")
library(Hmisc)
impute(BostonHousing$ptratio, mean)
impute(BostonHousing$ptratio, median)

# install.packages("DMwR")
library(DMwR)

actuals <- BostonHousing$ptratio[is.na(BostonHousing$ptratio)]
predictedsmean <- rep(mean(BostonHousing$ptratio, na.rm=T), length(actuals))
predictedsmedian <- rep(median(BostonHousing$ptratio, na.rm=T), length(actuals))

regr.eval(actuals, predictedsmean)
regr.eval(actuals, predictedsmedian)

# kknOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])
# anyNA(kknOutput)



#### SEMANA 4
temp1[sample(1:nrow(temp1), 45), "rm"] <- NA

temp1Imputation <- knnImputation(temp1[, !names(temp1) %in% "rm"])
anyNA(temp1Imputation)


library(rpart)
class_mod <- rpart(rad ~ . - medv, 
                   data = BostonHousing[!is.na(BostonHousing$rad), ],
                   method = "class", 
                   na.action = na.omit)

anova_mod <- rpart(ptratio ~ . - medv, 
                   data = BostonHousing[!is.na(BostonHousing$ptratio), ],
                   method = "anova", 
                   na.action = na.omit)

rad_pred <- predict(anova_mod, BostonHousing[is.na(BostonHousing$ptratio)])

# medir precision
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predictedsRPART <- pratio_pred
regr.eval(actuals, predictedsRPART)

