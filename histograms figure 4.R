
#' ---
#' title: "Histograms of forest distribution - Figure 4"
#' author: "Emily Ury"
#' date: "May 11, 2020"
#' output: github_document
#' ---
#' 
#' Set description here


setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/111319_Landsat_timeseries")

#setwd("D:/111319_Landsat_timeseries") 
library(sp)
library(raster)
library(rgdal)
library(tidyr)
library(dplyr)
library(reshape2)
library(plyr)


## read in data (all years stacked raster)
all.data <- "class_predictors2.tif"
# make it a raster brick
all.data <- brick(all.data)
### output map to data frame
stack.df <- as.data.frame(all.data, xy = TRUE)
head(stack.df)
names(stack.df)

names(stack.df) <- c("x", "y", "map85", "map87", "map89", "map90", "map92", "map93",
                     "map94", "map96", "map97", "map99", "map00", "map02", "map04",
                     "map07", "map08", "map10", "map11", "map12", "map15", "map16", 
                     "map17", "map18", "map19", "elevation", "flowlines", "channels", 
                     "coast", "eastcoast", "dcoast", "dEcoast", "dflow",
                     "dchan", "dmarsh", "elev0")
stack.df <- stack.df[which(stack.df$map85 != 0),]
stack.df[1:25][is.na(stack.df[1:25])] <- 0

df <- stack.df
df$col2019 <- df$map19
df[1:25][df[1:25] == 2] <- 1  ## merges pine and deciduous into one catergory, "forest"

nrow(df[which(df$map85 != df$map19),])
nrow(df)
nrow(df[which(df$map85 != df$map19),])/nrow(df)

## change since 2011
nrow(df[which(df$map11 != df$map19),])
nrow(df)
nrow(df[which(df$map11 != df$map19),])/nrow(df)

######### create an indicator for pixels transitioning to GF at any point
data <- mutate(df, GF = ifelse(map85 == 4| map87 == 4 | map89 == 4 | map90 == 4 | map92 == 4 | 
                                 map93==4 | map94 == 4| map96 == 4 |
                                 map97 == 4| map99 ==4 | map00 == 4 | map02 == 4 | map04 == 4 | 
                                 map07 ==4 | map08 ==4 | map10 == 4 |  map11 == 4|
                                 map12 == 4| map15 == 4| map16== 4 | 
                                 map17 == 4| map18 == 4| map19== 4, 1, 0))

## start w df (all forest pixels = 1)

forest <- data[which(data$map85==1),]
floss <- mutate(forest, floss = ifelse(map19 !=1, 1, 0))
fast<- mutate(floss, fast = ifelse(GF==1 & floss==1, 1, 0))
slow<- mutate(fast, slow = ifelse(GF==0 & floss==1, 1, 0))
nrow(floss[which(floss$floss==1),])
nrow(fast[which(fast$fast==1),])
nrow(slow[which(slow$slow==1),])
fchng <- mutate(slow, fchng = ifelse(fast == 1, 3, ifelse(slow == 1, 2, 1)))
count(fchng$fchng)
count(fchng$fchng)*0.09 ## this is the area in Hectares of: 
                                        # (1)Forest not changing 
                                        # (2)Forest changing not through GF state
                                        # (3)Forest changing through GF
newd <- fchng

### histograms

forest.loss <- newd[which(newd$floss == 1),]
forest.ghost <- newd[which(newd$GF == 1),]
forest.ghost1 <- newd[which(newd$fast == 1),]

nrow(forest.loss)/nrow(newd)
nrow(forest.ghost)/nrow(newd) # proportion of all forest that went through GF
nrow(forest.ghost1)/nrow(newd) # proportion of all forest that went through GF that didn't recover to F

nrow(forest.ghost1)/nrow(forest.loss) # proportion of forest lost through gf state
nrow(forest.ghost1)*0.09    ## hectares


### Histograms - Figure 4 - overlapping histograms with forest outline, labels at the top

par(mar = c(3,4,3,1))
par(mfrow = c(2,3), cex = 1.1)

hist(newd$dEcoast, breaks = 30, ylim = c(0,80000), main = "Distance to \n Sound (m)", xlab = NA, cex.main = 0.9)
hist(forest.loss$dEcoast, breaks = 30, ylim = c(0,80000),col = "gray50", main = NA, xlab = "Distance to \n the Sound (m)", add = TRUE)
hist(forest.ghost1$dEcoast, breaks = 30, ylim = c(0,80000),col = "brown3", main = NA, add=TRUE)
#mtext("A",3,1,las=1, adj = 0, cex = 1.2)

hist(newd$elevation, breaks = 30, ylim = c(0,130000), main = "Elevation (m)", xlab = NA, cex.main = 0.9, ylab = NA)
hist(forest.loss$elevation, breaks = 30, ylim = c(0,130000), col = "gray50", main = NA, xlab = "Elevation (m)", add = TRUE)
hist(forest.ghost1$elevation, breaks = 30, ylim = c(0,130000),col = "brown3", main = NA, add=TRUE)
#mtext("B",3,1,las=1, adj = 0, cex = 1.2)

hist(newd$dchan, breaks = 30, ylim = c(0,140000), main = "Distance to \n Channel (m)", xlab = NA, cex.main = 0.9, ylab = NA )
hist(forest.loss$dchan, breaks = 30, ylim = c(0,140000),col = "gray50", main = NA, xlab = "Distance to \n Channel (m)", add = TRUE)
hist(forest.ghost1$dchan, breaks = 30, ylim = c(0,140000),col = "brown3", main = NA, add=TRUE)
#mtext("C",3,1,las=1, adj = 0, cex = 1.2)
