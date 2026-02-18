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
library(mice)

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
predictedsmedian <- reqp(median(BostonHousing$ptratio, na.rm=T), length(actuals))

regr.eval(actuals, predictedsmean)
regr.eval(actuals, predictedsmedian)

kknOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])
anyNA(kknOutput)




