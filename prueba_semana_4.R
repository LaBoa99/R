library(rpart)
library(rpart.plot)
library(DMwR)
library(Hmisc)
library(mlbench)
library(mice)

data("BostonHousing")
set.seed(100)

original <- BostonHousing

BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
head(BostonHousing)

BostonHousing$ptratio
impute(BostonHousing$ptratio, mean)
impute(BostonHousing$ptratio, median)

actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
actuals
predictedsmean <- rep(mean(BostonHousing$ptratio, na.rm=T), length(actuals))
predictedsmedian <- rep(median(BostonHousing$ptratio, na.rm=T), length(actuals))
regr.eval(actuals, predictedsmean)
regr.eval(actuals, predictedsmedian)


knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])
anyNA(knnOutput)
names(BostonHousing)
knnOutput$ptratio
knnOutput$rad

class_mod <- rpart(rad ~ . - medv, 
                   data = BostonHousing[!is.na(BostonHousing$rad), ],
                   method = "class", 
                   na.action = na.omit)

anova_mod <- rpart(ptratio ~ . - medv, 
                   data = BostonHousing[!is.na(BostonHousing$ptratio), ],
                   method = "anova", 
                   na.action = na.omit)

rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad), ])
pratio_pred <- predict(anova_mod, BostonHousing[is.na(BostonHousing$ptratio), ])

# medir precision
actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predictedsRPART <- pratio_pred
regr.eval(actuals, predictedsRPART)

# RETOOOOO
temp1 <- BostonHousing
temp1[sample(1:nrow(temp1), 45), "rm"] <- NA
anova_mod <- rpart(rm ~ . - medv, 
                   data = temp1[!is.na(temp1$rm), ],
                   method = "anova", 
                   na.action = na.omit)
rpart.plot(anova_mod)

# OTRA COSA DOWQJDLKJSD
miceMod <- mice(BostonHousing[, !names(BostonHousing) %in% "medv"], method="rf")
miceOutput <- complete(miceMod)
anyNA(miceOutput)

actuals <- original$ptratio[is.na(BostonHousing$ptratio)]
predictedsMICE <- miceOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals, predictedsMICE)
stripplot(predictedsMICE)
plot(predictedsMICE)


# RETOOOOOOOOOOOOOOOOOOOO
temp1[sample(1:nrow(temp1), 45), "rm"] <- NA

actuals <- original$rm[is.na(temp1$rm)]
tempPredictedsMICE <- miceOutput[is.na(temp1$rm), "rm"]

regr.eval(actuals, tempPredictedsMICE)
