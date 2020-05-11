#' ---
#' title: "Classification of Landsat Composite with Random Forest"
#' author: "Emily Ury"
#' date: "May 11, 2020"
#' output: github_document
#' ---
#'


#' This script is for building a RF classifier for vegetation
#' for the project to investigate vegetation change in coastal
#' North Carolina within and around the Alligator River National
#' Wildlife Refuge. 
#' 


library(sp)
library(raster)
library(rgdal)
library(sf)
library(caret)
library(randomForest)
library(rsample)

setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/ARNWR Project")

## get raster data
# 2017 Landsat composite
composite17 <- "composite2017.tif"
raster <- raster(composite17)
stack <- brick(composite17)
names(stack) <- c("composite2017.1",  "composite2017.2",  "composite2017.3",
                  "composite2017.4",  "composite2017.5", "composite2017.6",
                  "composite2017.7",  "composite2017.8",  "composite2017.9",
                  "composite2017.10", "composite2017.11", "composite2017.12")

## get vector data 
# 2017 validation points
vps <- read.csv("2017_validationPoints_2.csv")
par(mfrow = c(1,1), las = 1, oma = c(0,0,0,0), cex = 1, mar=c(4,4,3,1))
plot(raster)
points(vps$long, vps$lat, pch = "+", cex = 0.5)


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




##  overall model
modelRF <- randomForest(x=valuetable[,c(1:12)], y=valuetable$class, importance = TRUE)



### Map prediction

names(stack)
names(valuetable) ## these must match
map17 <- predict(stack, model=modelRF, na.rm=TRUE)
plot(map17, col = cols, legend = FALSE)
legend("topright", legend=c("mixed", "pine", "marsh", "ghost forest", "shrub"), fill=cols, bg="white")
text(-76.15, 35.9, "Alligator River", cex = 2)
text(-76.15, 35.87, "2017", cex = 3)

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


### Make RF model for Landsat 7 series

composite12 <- "composite2012.tif"
stack12 <- brick(composite12)
names(stack12) <- c("composite2017.1",  "composite2017.2",  "composite2017.3",
                    "composite2017.4",  "composite2017.5", "composite2017.6",
                    "composite2017.7",  "composite2017.8",  "composite2017.9",
                    "composite2017.10", "composite2017.11", "composite2017.12")
vps12 <- read.csv("2012_validationPoints_2.csv")

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
legend("topright", legend=c("mixed", "pine", "marsh", "ghost forest", "shrub", "burn"), fill=cols77, bg="white")
text(-76.15, 35.72, "Alligator River", cex = 1)
text(-76.15, 35.75, "2012", cex = 2)


prediction12 <- predict(modelRF_12, newdata=testData12)
cm <- table(prediction12, testData12$class)
cm
accuracy <- (sum(diag(cm)))/sum(cm)
accuracy  ##  94.0% OVERALL

## Final model 2012
modelRF_12 <- randomForest(x=valuetable12[,c(1:12)], y=valuetable12$class, importance = TRUE)






##### Map prediction on a different input year

### Match the classifier to the composites within the correct date range

## Use: modelRF for 2015 - 2019
## Use: modelRF10 for 1985-2011
## Use: modelRF12 for 2012



## 2019
composite18 <- "composite2019.tif"   ## switch out this composite
stack18 <- brick(composite18)
names(stack18) <- c("composite2017.1",  "composite2017.2",  "composite2017.3",
                    "composite2017.4",  "composite2017.5", "composite2017.6",
                    "composite2017.7",  "composite2017.8",  "composite2017.9",
                    "composite2017.10", "composite2017.11", "composite2017.12")
map19 <- predict(stack18, model=modelRF, na.rm=TRUE)    ## switch out the model here
plot(map19, col = cols, legend = FALSE)
#legend("topright", legend=c("mixed", "pine", "marsh", "ghost forest", "shrub"), fill=cols, bg="white")
#text(-76.2, 35.9, "Alligator River", cex = 2)
text(-76.1, 35.9, "2019", cex = 3)

