rm(list = ls())
library(reshape)
library(stringr)
source('Exploratory/helperFunctions.R')

rawData <- read.csv("Raw_Data/Downtime Details Report Sub.csv")
mData <- melt(rawData)
t1 <- cast(mData, equipnum + descript~Cause, sum)

row.names(t1) <- t1$descript
t1 <- t1[,3:44]
dt_matrix <- data.matrix(t1)
colvect <- rev(heat.colors(500))
dt_heatmap <- heatmap(dt_matrix, Rowv=NA, Colv=NA, col=colvect, scale="none", margins=c(15,15))

# De-granularize the plot
consolidation1 <- rawData

# Collate equipment descriptions into roughly functional areas

oldFactors = c("conveyor", "cv", "crusher", "lube", "ore pass", "vent", "skip", "shaft", "tail", "power", "rope", "hoist", "truck", "bogger", "firing", "underground")
newFactors = c("Conveyors", "Conveyors", "Crusher", "Crusher", "Ore Passes", "Ventillation", "Skips", "Shaft", "Shaft", "Power", "Ropes", "Hoist", "Mining", "Mining", "Mining", "Mining")
factors <- combineFactorsByRegularExpression(as.character(rawData$descript), oldFactors, newFactors)
consolidation1$descript <- factors

mData <- melt(consolidation1)
t2 <- cast(mData, descript~Problem, sum)

row.names(t2) <- t2$descript
t2 <- t2[,2:6]
dt_matrix <- data.matrix(t2)
colvect <- rev(heat.colors(1999))
myBreaks <- seq(0.0, 350000, length.out = 2000)
dt_heatmap <- heatmap(dt_matrix, Rowv=NA, Colv=NA, col=colvect, scale="none", margins=c(15,15), breaks = myBreaks)

u <- consolidation1$descript == "Mining"
tMining <- consolidation1[u,]
t3 <- tMining[,c("Cause", "delmins")]
t3 <- aggregate(. ~Cause, t3, sum)

#Can make these Display $Values for the exec summary
t7 <- aggregate(delmins ~ Month + Year + Cause, tMining, FUN = sum)
t7$Date <- as.POSIXct(paste(t7$Year, t7$Month, "01", sep = "-"))
ggplot(t7, aes(x=Date, y=delmins, fill=Cause)) + geom_bar(stat="identity") + facet_wrap(~ Cause)
t8 <- subset(t7, Cause != "Emergency" & Cause != "")
ggplot(t8, aes(x=Date, y=delmins, fill=Cause)) + geom_bar(stat="identity") + facet_wrap(~ Cause)
t9 <- subset(t8, Date > as.POSIXct("2015-11-01"))
ggplot(t9, aes(x=Date, y=delmins, fill=Cause)) + geom_bar(stat="identity") +geom_smooth(method=lm, level=0.7) + facet_wrap(~ Cause)
