#' ---
#' title: "Accuracy Assessment for RF classifier"
#' author: "Emily Ury"
#' date: "May 11, 2020"
#' output: github_document
#' ---
#'


#' This script is for building a RF classifier for vegetation
#' of coastal NC and the accuracy assessment of each of the three 
#' classifiers (one fore each Landsat series, 5, 7 & 8). 
#' 
#' We use a k-fold cross-validation (k=5) for generating **user's accuracy**
#' and **producer's accuracy** for each random forest model. 





library(dismo)

library(sp)
library(raster)
library(rgdal)
library(sf)
library(caret)
library(randomForest)
library(rsample)
library(plyr)

#setwd("D:/111319_Landsat_timeseries")
#setwd("E:/111319_Landsat_timeseries")
#setwd("C:/Users/uryem/Desktop/Landsat_timeseries")
setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/111319_Landsat_timeseries")



## get raster data
# 2017 Landsat composite
composite17 <- "composite2017.tif"
raster <- raster(composite17)
stack <- brick(composite17)

## get vector data 
# 2017 validation points
vps <- read.csv("2017_validationPoints_2.csv")
par(mfrow = c(1,1), las = 1, oma = c(0,0,0,0), cex = 1, mar=c(4,4,3,1))
plot(raster)
points(vps$long, vps$lat, pch = "+", cex = 0.5)

count(vps$class)

########################################################3

names(stack)

# make the validation points into a spatial points dataframe
crs <- crs(raster)
vps.sp <- SpatialPointsDataFrame(vps[,3:4], vps, proj4string = crs)
vps.sp
classes <- rasterize(vps.sp, stack, field = 'class')

cols <- c("aquamarine3", "palegreen4", "lightgoldenrod1", "brown3", "burlywood4")

# mask raster brick to training points
covmask <- mask(stack, classes)
names(classes) <- "class"
trainingbrick <- addLayer(covmask,classes)
#plot(trainingbrick)
valuetable <- getValues(trainingbrick)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)
#head(valuetable)
valuetable$class <- factor(valuetable$class, levels = c(1:5))


##
####
######
####
##


##### make the RF model
set.seed(123)
valuetable$random = sample(2, nrow(valuetable), replace=TRUE, prob=c(0.7,0.3))
trainData = valuetable[valuetable$random==1,]
testData = valuetable[valuetable$random==2,]

modelRF <- randomForest(x=trainData[,c(1:12)], y=trainData$class, importance = TRUE)
#modelRF

### Accuracy assessment

prediction <- predict(modelRF, newdata=testData)
#table(prediction, testData$class)
cm <- table(prediction, testData$class)
accuracy <- (sum(diag(cm)))/sum(cm)
accuracy  ## 92.45 (seed:123)



cm
accuracy

rownames(cm) <- c("pine", "deciduous", "marsh", "ghost", "shrub")
colnames(cm) <- c("pine", "deciduous", "marsh", "ghost", "shrub")

n <- sum(cm)



## https://rspatial.org/raster/rs/5-supclassification.html


set.seed(99)
j <- kfold(valuetable, k = 5, by=valuetable$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- valuetable[j!= k, ]
  test <- valuetable[j == k, ]
  model <- randomForest(as.factor(class)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(model, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <-  c("pine", "deciduous", "marsh", "ghost", "shrub")
rownames(conmat) <-  c("pine", "deciduous", "marsh", "ghost", "shrub")
conmat

n <- sum(conmat)
n
## [1] 1600
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
OA

rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa

# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc

mean(PA)
mean(UA)



##  overall model
modelRF <- randomForest(x=valuetable[,c(1:12)], y=valuetable$class, importance = TRUE)



### Map prediction

names(stack)
names(valuetable) ## these must match
map17 <- predict(stack, model=modelRF, na.rm=TRUE)
plot(map17, col = cols, legend = FALSE)
legend("topright", legend=c("mixed", "pine", "marsh", "ghost forest", "shrub"), fill=cols, bg="white")
text(-76.2, 35.9, "Alligator River", cex = 2)
text(-76.2, 35.87, "2017", cex = 3)

### output map to table
#table2017 <- as.data.frame(map17, xy = TRUE)
#table2017 <- table2017[which(table2017$layer_value != 0),]


### Make RF model for Landsat 5 series

composite10 <- "composite2010.tif"
stack10 <- brick(composite10)
names(stack10) <- c("composite2017.1",  "composite2017.2",  "composite2017.3",
                    "composite2017.4",  "composite2017.5", "composite2017.6",
                    "composite2017.7",  "composite2017.8",  "composite2017.9",
                    "composite2017.10", "composite2017.11", "composite2017.12")
vps10 <- read.csv("2010_validationPoints_3.csv")
count(vps10$class)
vps10.sp <- SpatialPointsDataFrame(vps10[,3:4], vps10, proj4string = crs)
vps10.sp
classes10 <- rasterize(vps10.sp, stack10, field = 'class')

# mask raster brick to training points
covmask10 <- mask(stack10, classes10)
names(classes10) <- "class"
trainingbrick10 <- addLayer(covmask10,classes10)
#plot(trainingbrick)
valuetable10 <- getValues(trainingbrick10)
valuetable10 <- na.omit(valuetable10)
valuetable10 <- as.data.frame(valuetable10)
#head(valuetable)
valuetable10$class <- factor(valuetable10$class, levels = c(1:5))

set.seed(127)
valuetable10$random = sample(2, nrow(valuetable10), replace=TRUE, prob=c(0.7,0.3))
trainData10 = valuetable10[valuetable10$random==1,]
testData10 = valuetable10[valuetable10$random==2,]



modelRF_10 <- randomForest(x=trainData10[,c(1:12)], y=trainData10$class, importance = TRUE)

names(valuetable10) ## these must match
map10 <- predict(stack10, model=modelRF_10, na.rm=TRUE)
plot(map10, col = cols, legend = FALSE)
legend("topright", legend=c("mixed", "pine", "marsh", "ghost forest", "shrub"), fill=cols, bg="white")
text(-76.2, 35.9, "Alligator River", cex = 2)
text(-76.2, 35.87, "2010", cex = 3)


### full model
modelRF_10 <- randomForest(x=valuetable10[,c(1:12)], y=valuetable10$class, importance = TRUE)


####
# AA
####

set.seed(99)
j <- kfold(valuetable, k = 5, by=valuetable$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- valuetable10[j!= k, ]
  test <- valuetable10[j == k, ]
  model <- randomForest(as.factor(class)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(model, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <-  c("pine", "deciduous", "marsh", "ghost", "shrub")
rownames(conmat) <-  c("pine", "deciduous", "marsh", "ghost", "shrub")
conmat

n <- sum(conmat)
n
## [1] 1600
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
OA

rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa

# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc

mean(PA)
mean(UA)






### Make RF model for Landsat 7 series

composite12 <- "composite2012.tif"
stack12 <- brick(composite12)
names(stack12) <- c("composite2017.1",  "composite2017.2",  "composite2017.3",
                    "composite2017.4",  "composite2017.5", "composite2017.6",
                    "composite2017.7",  "composite2017.8",  "composite2017.9",
                    "composite2017.10", "composite2017.11", "composite2017.12")
vps12 <- read.csv("2012_validationPoints_2.csv")
count(vps12$class)
vps12.sp <- SpatialPointsDataFrame(vps12[,3:4], vps12, proj4string = crs)
vps12.sp
classes12 <- rasterize(vps12.sp, stack12, field = 'class')

# mask raster brick to training points
covmask12 <- mask(stack12, classes12)
names(classes12) <- "class"
trainingbrick12 <- addLayer(covmask12,classes12)
#plot(trainingbrick)
valuetable12 <- getValues(trainingbrick12)
valuetable12 <- na.omit(valuetable12)
valuetable12 <- as.data.frame(valuetable12)
#head(valuetable)
valuetable12$class <- factor(valuetable12$class, levels = c(1:7))
#valuetable12$class[is.na(valuetable12$class)] <- 6
#valuetable12$class[valuetable$class==NA]<-6

set.seed(126)
valuetable12$random = sample(2, nrow(valuetable12), replace=TRUE, prob=c(0.7,0.3))
trainData12 = valuetable12[valuetable12$random==1,]
testData12 = valuetable12[valuetable12$random==2,]

modelRF_12 <- randomForest(x=trainData12[,c(1:12)], y=trainData12$class, importance = TRUE)

cols77 <- c("aquamarine3", "palegreen4", "lightgoldenrod1", "brown3", "burlywood4", "black", "palegreen4")


names(valuetable12) ## these must match
map12 <- predict(stack12, model=modelRF_12, na.rm=TRUE)
plot(map12, col = cols77, legend = FALSE)
legend("topright", legend=c("mixed", "pine", "marsh", "ghost forest", "shrub", "burn"), fill=cols7, bg="white")
text(-76.15, 35.72, "Alligator River", cex = 1)
text(-76.15, 35.75, "2012", cex = 2)


prediction12 <- predict(modelRF_12, newdata=testData12)
cm <- table(prediction12, testData12$class)
cm
accuracy <- (sum(diag(cm)))/sum(cm)
accuracy  ##  94.0% OVERALL

## Final model 2012
modelRF_12 <- randomForest(x=valuetable12[,c(1:12)], y=valuetable12$class, importance = TRUE)




####
# AA
####

set.seed(9999)
j <- kfold(valuetable, k = 5, by=valuetable$class)
table(j)

x <- list()
for (k in 1:5) {
  train <- valuetable12[j!= k, ]
  test <- valuetable12[j == k, ]
  model <- randomForest(as.factor(class)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(model, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <-  c("pine", "deciduous", "marsh", "ghost", "shrub", "burn", "dry pine")
rownames(conmat) <-  c("pine", "deciduous", "marsh", "ghost", "shrub", "burn", "dry pine")
conmat

n <- sum(conmat)
n
## [1] 1600
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
OA

rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa

# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc

mean(PA)
mean(UA)





