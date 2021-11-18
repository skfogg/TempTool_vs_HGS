library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)
library(HGSReader)

littleHypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)
littleHypoEqualStorage18 <- hyporheicBins(18, 0, 60, 182*86400, 0.25, 11.98, b = -1.39)

# Using TSZ Stats instead of hyporheicBins:
drt <- TSZStats(18, 60, 182*86400, 11.98, factor = 2, alpha = 1.39)
drt$entering[1]*0.25
littleHypoBins$entering[1]

drt$meanWaterAge[1]
littleHypoBins$meanWaterAge[1]

drt$meanResTime

es <- TSZStats(18, 60, 182*86400, 11.98, factor = 0, alpha = 1.39)
es$meanResTime

TSZStats(c(60, drt$to[7], 182*86400), 60, 182*86400, 11.98, factor = 2, alpha = 1.39)

plot((littleHypoBins$to - littleHypoBins$from)/3600,
     pch = 18, ylim = c(0,3000),
     ylab = "TSZ Residence Time Range",
     xlab = "TSZ")
points((littleHypoEqualStorage18$to - littleHypoEqualStorage18$from)/ 3600, pch = 18, col = "orange")
legend("topleft", c("RT Range doubles", "Equal Storage"), pch = 18, col = c("black", "orange"))

points(2^(0:17), col = "dodgerblue")


plot(littleHypoBins$entering, pch = 18,
     ylab = "Water Flux Entering", xlab = "TSZ")
points(littleHypoEqualStorage18$entering, pch = 18, col = "orange")
legend("topright", c("RT Range doubles", "Equal Storage"), pch = 18, col = c("black", "orange"))

plot(littleHypoBins$returning, pch = 18,
     ylab = "Water Flux Returning", xlab = "TSZ",
     ylim = c(0, littleHypoEqualStorage18$returning[1]))
points(littleHypoEqualStorage18$returning, pch = 18, col = "orange")
legend("topright", c("RT Range doubles", "Equal Storage"), pch = 18, col = c("black", "orange"))

plot(littleHypoBins$aquiferStorage, pch = 18,
     ylab = "Aquifer Storage", xlab = "TSZ")
points(littleHypoEqualStorage18$aquiferStorage, pch = 18, col = "orange")
legend("topleft", c("RT Range doubles", "Equal Storage"), pch = 18, col = c("black", "orange"))

###
###
### TempTool Output
###
###

load("temptool_output/TempTool_oneOutputChannel.RData")
littlehypo <- oneChannel

load("temptool_output/equalTSZStorage.RData")
equalS <- oneChannel

hgs_output_location <- "C:/Users/skati/Box/TempTool_vs_HGS_hgs_output/hgs_output"
hgshz <- readRDS(paste0(hgs_output_location, "/HGSTempTool_Run3.RData"))
hgsbinned <- readRDS("hgsbinned.RData")
hgstimes <- mdy_hms("01-01-2016 00:00:00") + as.data.frame(hgshz[1,1,5,,])$Time - 365*86400*7
# hgshzTS_1 <- xts(zoo(hgshz[1,1,5,,"temp"]), order.by = hgstimes)

inputtemp <- read.table("IskulpaaRiverTemp.txt", skip = 1,
           col.names = c("s", "e", "temp"))
inputT <- xts(zoo(inputtemp$temp, order.by = ymd_hms("2014-01-01 00:00:00") + inputtemp$s))

plot.zoo(littlehypo$cTemp2$svValue["2016"], lwd = 2)
lines(as.zoo(equalS$cTemp2$svValue["2016"]),
      col = adjustcolor("orange", alpha.f = 0.5),
      lwd = 2)

plot.zoo(inputT["2016-08-13"], lwd = 2, col = "dodgerblue")
lines(as.zoo(littlehypo$tsz1Temp$svValue["2016-08-13"]))
lines(as.zoo(equalS$tsz1Temp$svValue["2016-08-13"]),
      col = "orange")
lines(as.zoo(littlehypo$tsz7Temp$svValue["2016-08-13"]), col = "gray")

meantsz1_tsz7 <- xts(
  zoo(
    rowMeans(
      cbind(littlehypo$tsz1Temp$svValue["2016-08-13"],
            littlehypo$tsz7Temp$svValue["2016-08-13"])),
    order.by = index(littlehypo$tsz7Temp$svValue["2016-08-13"])))
lines(as.zoo(meantsz1_tsz7), col = "brown")

meantsz1_thru_tsz7 <- xts(
  zoo(
    rowMeans(
      cbind(littlehypo$tsz1Temp$svValue["2016-08-13"],
            littlehypo$tsz2Temp$svValue["2016-08-13"],
            littlehypo$tsz3Temp$svValue["2016-08-13"],
            littlehypo$tsz4Temp$svValue["2016-08-13"],
            littlehypo$tsz5Temp$svValue["2016-08-13"],
            littlehypo$tsz6Temp$svValue["2016-08-13"],
            littlehypo$tsz7Temp$svValue["2016-08-13"])),
    order.by = index(littlehypo$tsz7Temp$svValue["2016-08-13"])))
lines(as.zoo(meantsz1_thru_tsz7), col = "forestgreen")

meantsz1_thru_tsz8 <- xts(
  zoo(
    rowMeans(
      cbind(littlehypo$tsz1Temp$svValue["2016-08-13"],
            littlehypo$tsz2Temp$svValue["2016-08-13"],
            littlehypo$tsz3Temp$svValue["2016-08-13"],
            littlehypo$tsz4Temp$svValue["2016-08-13"],
            littlehypo$tsz5Temp$svValue["2016-08-13"],
            littlehypo$tsz6Temp$svValue["2016-08-13"],
            littlehypo$tsz7Temp$svValue["2016-08-13"],
            littlehypo$tsz8Temp$svValue["2016-08-13"])),
    order.by = index(littlehypo$tsz7Temp$svValue["2016-08-13"])))
lines(as.zoo(meantsz1_thru_tsz8), col = "deeppink")

meantsz1_tsz8 <- xts(
  zoo(
    rowMeans(
      cbind(littlehypo$tsz1Temp$svValue["2016-08-13"],
            littlehypo$tsz8Temp$svValue["2016-08-13"])),
    order.by = index(littlehypo$tsz7Temp$svValue["2016-08-13"])))
lines(as.zoo(meantsz1_tsz8), col = "royalblue")
lines(as.zoo(littlehypo$tsz8Temp$svValue["2016-08-13"]), col = "darkgray")

plot.zoo(littlehypo$tsz7Temp$svValue["2016-08-13"])
lines(as.zoo(equalS$tsz1Temp$svValue["2016-08-13"]),
      col = "orange")
lines(as.zoo(inputT["2016-08-13"]), col = "dodgerblue")
# lines(as.zoo(hgsbinned$hgsbin3["2016-08-13"]),
#       col = "magenta")

round(littleHypoBins$meanWaterAge,1)
round(littleHypoEqualStorage18$meanWaterAge,1)

plot.zoo(inputT["2016-08-13"], lwd = 2, col = "dodgerblue")
for(i in 2:8){
  lines(as.zoo(littlehypo[[i]]$svValue["2016-08-13"]), col = hcl.colors(10, "Magenta")[i-1])
}

plot.zoo(inputT["2016-08-13"], lwd = 2, col = "dodgerblue")
for(i in 2:9){
  lines(as.zoo(equalS[[i]]$svValue["2016-08-13"]), col = hcl.colors(10, "YlOrBr")[i-1])
}



plot.zoo(littlehypo$tsz1Temp$svValue["2016-08-13"])
for(i in 3:19){
  lines(as.zoo(littlehypo[[i]]$svValue["2016-08-13"]),
        col = hcl.colors(16)[i-2])
}

plot.zoo(equalS$tsz1Temp$svValue["2016-08-13"])
for(i in 3:19){
  lines(as.zoo(equalS[[i]]$svValue["2016-08-13"]),
        col = hcl.colors(16)[i-2])
}

jan1 <- c(coredata(littlehypo[[2]]$svValue["2016"])[1],
          coredata(littlehypo[[3]]$svValue["2016"])[1],
          coredata(littlehypo[[4]]$svValue["2016"])[1],
          coredata(littlehypo[[5]]$svValue["2016"])[1],
          coredata(littlehypo[[6]]$svValue["2016"])[1],
          coredata(littlehypo[[7]]$svValue["2016"])[1],
          coredata(littlehypo[[8]]$svValue["2016"])[1],
          coredata(littlehypo[[9]]$svValue["2016"])[1],
          coredata(littlehypo[[10]]$svValue["2016"])[1],
          coredata(littlehypo[[11]]$svValue["2016"])[1],
          coredata(littlehypo[[12]]$svValue["2016"])[1],
          coredata(littlehypo[[13]]$svValue["2016"])[1],
          coredata(littlehypo[[14]]$svValue["2016"])[1],
          coredata(littlehypo[[15]]$svValue["2016"])[1],
          coredata(littlehypo[[16]]$svValue["2016"])[1],
          coredata(littlehypo[[17]]$svValue["2016"])[1],
          coredata(littlehypo[[18]]$svValue["2016"])[1],
          coredata(littlehypo[[19]]$svValue["2016"])[1])
plot(littleHypoBins$meanWaterAge, jan1, type = "l")

plot(hgshz[,1,5,1,"X"]/vx, hgshz[,1,5,1,"temp"], type = "l")
lines(littleHypoBins$meanWaterAge, jan1, type = "o", col = "dodgerblue")

plot(hgshz[,1,5,1,"X"]/vx, hgshz[,1,5,1,"temp"], type = "l")
abline(v = littleHypoBins$to, col = "grey")
points(littleHypoBins$meanWaterAge, jan1, col = "dodgerblue")

## Zoom in
plot(hgshz[,1,5,1,"X"]/vx, hgshz[,1,5,1,"temp"], type = "l", xlim = c(-1, 86400*3),
     ylim = c(3,6))
abline(v = littleHypoBins$to, col = "grey")
points(littleHypoBins$meanWaterAge, jan1, col = "dodgerblue")

## Zoom in more
plot(hgshz[,1,5,1,"X"]/vx, hgshz[,1,5,1,"temp"], type = "l", xlim = c(-1, 60400),
     ylim = c(4,5))
abline(v = littleHypoBins$to, col = "grey")
points(littleHypoBins$meanWaterAge, jan1, col = "dodgerblue")

## Zoom in more
plot(hgshz[,1,5,1,"X"]/vx, hgshz[,1,5,1,"temp"], type = "o", xlim = c(-1, 2000),
     ylim = c(4,5))
abline(v = littleHypoBins$to, col = "grey")
points(littleHypoBins$meanWaterAge, jan1, col = "dodgerblue")


 points(littleHypoBins$to, jan1, col = "magenta")
points(littleHypoBins$from, jan1, col = "magenta")


