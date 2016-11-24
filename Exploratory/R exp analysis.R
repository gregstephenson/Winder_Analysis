rm(list = ls())
library(reshape)
library(stringr)

rawData <- read.csv("Raw_Data/Downtime Details Report Sub.csv")
mData <- melt(rawData)
t1 <- cast(mData, equipnum + descript~Cause, sum)

row.names(t1) <- t1$descript
t1 <- t1[,3:44]
dt_matrix <- data.matrix(t1)
colvect <- rev(heat.colors(500))
dt_heatmap <- heatmap(dt_matrix, Rowv=NA, Colv=NA, col=colvect, scale="column", margins=c(15,15))

filter <- as.character(rawData$descript)
factors <- as.character(rawData$descript)
factors[1:length(factors)] <- "Other"

cF <- grep("conveyor|cv", filter, ignore.case = TRUE, value = FALSE)
filter[cF] <- "Conveyors"
factors[cF] <- "Conveyors"

cF <- grep("crusher|lube", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Crusher"
factors[cF] <- "Crusher"

cF <- grep("ore pass", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Ore Passes"
factors[cF] <- "Ore Passes"

cF <- grep("vent", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Ventilation"
factors[cF] <- "Ventilation"

cF <- grep("skip", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Skips"
factors[cF] <- "Skips"

cF <- grep("shaft|tail", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Shaft"
factors[cF] <- "Shaft"

cF <- grep("power", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Power"
factors[cF] <- "Power"

cF <- grep("rope", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Ropes"
factors[cF] <- "Ropes"

cF <- grep("hoist", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Hoist"
factors[cF] <- "Hoist"

cF <- grep("truck|bogger|firing|underground", filter, ignore.case = TRUE, value = FALSE) 
filter[cF] <- "Mining"
factors[cF] <- "Mining"

consolidation1 <- rawData
consolidation1$descript <- factors
mData <- melt(consolidation1)
t2 <- cast(mData, descript~Cause, sum)

row.names(t2) <- t2$descript
t2 <- t2[,3:44]
dt_matrix <- data.matrix(t2)
colvect <- rev(heat.colors(50000))
dt_heatmap <- heatmap(dt_matrix, Rowv=NA, Colv=NA, col=colvect, scale="column", margins=c(15,15))
