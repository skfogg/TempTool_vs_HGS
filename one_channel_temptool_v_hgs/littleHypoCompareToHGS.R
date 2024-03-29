###
### Plot
###

library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)
library(HGSReader)

littleHypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)

hyporheicBins(18, 2, 60, 182*86400, 0.25, hyporheicExchange = 4.8, b=-1.39)


### HGS ###
hgs_output_location <- "C:/Users/skati/Box/TempTool_vs_HGS_hgs_output/hgs_output"
hgshz <- readRDS(paste0(hgs_output_location, "/HGSTempTool_Run3.RData"))
dim(hgshz)

hgshz[,1,5,1,"X"]

v <- readRDS(paste0(hgs_output_location, "/HGSTempTool_Run3_velocity.RData"))
dim(v)
plot(v[,1,5,1,"Vx"])
plot(v[,1,5,1,"Vz"], type = "l")

(max_vx <- max(v[,1,5,1,"Vx"]))
(min_vx <- min(v[,1,5,1,"Vx"]))
(mean_vx <- mean(v[,1,5,1,"Vx"]))

(vx <- (v[1,1,5,1,"Vx"]))

vx*0.25

hgsResTime <- (1/vx)*hgshz[,1,5,1,"X"]
head(hgsResTime)
hgsResTime

hgsrt <- data.frame(rt = hgsResTime,
                    m = hgshz[,1,5,1,"X"],
                    xidx = 1:length(hgshz[,1,5,1,"X"]),
                    ttbin = numeric(length(hgshz[,1,5,1,"X"])),
                    ttmeanWA = numeric(length(hgshz[,1,5,1,"X"])))

## Bin up HGS output nodes into corresponding
## TempTool Bins. If the HGS residence time of a node
## is greater than a TSZ's min rt and less than or equal to
## a TSZ's max rt, then that HGS node gets 'assigned' that TSZ
## number.
for(i in 3:18){
  hgsrt[with(hgsrt, rt > littleHypoBins$to[i-1] & rt <= littleHypoBins$to[i]),]$ttbin <- i
  hgsrt[with(hgsrt, rt > littleHypoBins$to[i-1] & rt <= littleHypoBins$to[i]),]$ttmeanWA <- littleHypoBins$meanWaterAge[i]
}


## Now take the mean water temperature of all HGS nodes
## with residence times that fall within a TSZ RT range.
## This mean value will be compared the the temperature
## the corresponding TSZ from the TempTool output.
binoverwrite <- numeric(576)
hgstimes <- mdy_hms("01-01-2016 00:00:00") + as.data.frame(hgshz[1,1,5,,])$Time - 365*86400*7
for(j in 3:18){
  for (i in 1:576){
    binoverwrite[i] <- mean(hgshz[hgsrt[with(hgsrt, ttbin == j),]$xidx,1,5,i,"temp"])
    # hgsbin3[i] <- mean(hgshz[hgsrt[with(hgsrt, ttbin == j),]$xidx,1,5,i,"temp"])
  }
  binoverwrite <- xts(zoo(binoverwrite, order.by = hgstimes))
  assign(paste0("hgsbin", j), binoverwrite)
}

## Group the binned HGS means into a list and save it:
objectNames <- c(paste0("hgsbin", 3:18))
names(objectNames) <- objectNames
hgsbinned <- as.list(objectNames)
for(i in 1:length(objectNames)){
  hgsbinned[[i]] <- get(objectNames[i])
}
saveRDS(hgsbinned, "hgsbinned.RData")

###################
### TempTool ###
#connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

load("temptool_output/TempTool_oneOutputChannel.RData")
tthz <- oneChannel
time2plot <- "2016"
temptoolcolors <- hcl.colors(18, palette = "viridis")

png("TempToolHZTemperatures.png", width = 800*5, height = 400*5,
    res = 72*5)
plot.zoo(tthz$tsz1Temp$svValue[time2plot],
         ylim = c(0,22),
         ylab = "Temperature (C)",
         main = "TempTool Hyporheic Temperatures")
for(i in 1:18){
  lines(as.zoo(tthz[[i+1]]$svValue[time2plot]),
        col = temptoolcolors[i],
        lwd = 2)
}
dev.off()

png("HGSHZTemperatures.png", width = 800*5, height = 400*5,
    res = 72*5)
plot.zoo(hgsbinned$hgsbin3,
         ylim = c(0,22),
         ylab = "Temperature (C)",
         main = "HGS Hyporheic Temperatures")
for(i in 1:16){
  lines(as.zoo(hgsbinned[[i]]),
        col = temptoolcolors[3:18][i],
        lwd = 2)
}
dev.off()

## Bin to Bin comparison
for(temptoolbin in 3:18){
hgsbin <- temptoolbin-2
# png(paste0("BinCompare", temptoolbin, "_August13.png"),
#     width = 600*5,
#     height = 300*5,
#     res=72*5)
plot.zoo(tthz[[temptoolbin]]$svValue["2016-08-13"],
         ylab = "Temperature (C)",
         ylim = c(0,22),
         main = paste0("TSZ ", temptoolbin, ": ", round(littleHypoBins$from[temptoolbin]), "-", round(littleHypoBins$to[temptoolbin]), " seconds"))
lines(as.zoo(hgsbinned[[hgsbin]]["2016-08-13"]),
      col = "gold", lwd =2)
text(mdy_hms("02-20-2016 00:00:00"), 15, labels = paste0("TT mean WA:", round(littleHypoBins$meanWaterAge[temptoolbin])))
text(mdy_hms("02-20-2016 00:00:00"), 12, labels = paste0("HGS mean WA: ", round(mean(hgsrt[with(hgsrt, ttbin == temptoolbin),]$rt))))

# dev.off()
}


somecolors <- hcl.colors(19)
hgshzTS_3 <- xts(zoo(hgshz[3,1,5,,"temp"]), order.by = hgstimes)

plotT <- ""

plot.zoo(tthz[[3]]$svValue["2016-06/2016-08"], col = somecolors[1])
lines(as.zoo(hgshzTS_3["2016-06/2016-08"]), col = somecolors[12])
lines(as.zoo(hgsbinned[[1]]["2016-06/2016-08"]), col = somecolors[15])

plot.zoo(tthz[[4]]$svValue["2016-08-13"], col = somecolors[1])
# lines(as.zoo(hgshzTS_3["2016-06/2016-08"]), col = somecolors[12])
lines(as.zoo(hgsbinned[[2]]["2016-08-13"]), col = somecolors[15])


plot(coredata(tthz[[3]]$svValue["2016-08-13"]) - coredata(hgshzTS_3["2016-08-13"]))
abline(h = 0)

## Bin3 daily mean
plot.zoo(apply.daily(tthz$tsz3Temp$svValue[time2plot], mean))
lines(as.zoo(apply.daily(hgsbinned$hgsbin3, mean)),
      col = "gold",
      lwd = 2)


binnum <- 6
plot.zoo(tthz$tsz18Temp$svValue[time2plot])
lines(as.zoo(hgsbinned$hgsbin18), col = "gold", lwd = 2)


## Bin5
plot.zoo(apply.daily(tthz$tsz14Temp$svValue[time2plot], mean))
lines(as.zoo(apply.daily(hgsbinned$hgsbin14, mean)),
      col = "gold",
      lwd = 2)
mean(hgsrt[with(hgsrt, ttbin == 14),]$rt) -
  mean(hgsrt[with(hgsrt, ttbin == 14),]$ttmeanWA)


####
data.frame(rt = hgsResTime, m = hgshz[,2,15,1,"X"])

hgsoutputmeters <- littleHypoBins$to *vx

round(hgsoutputmeters,2)


## Chinese Finger trap
plot(hgshz[,1,5,1,"X"], hgshz[,1,5,1,"temp"], type = "l",
     ylim = c(0,22))
for (i in 2:576){
  lines(hgshz[,1,5,i,"X"], hgshz[,1,5,i,"temp"])
}

# means1 <- numeric(15)
# for(tti in 3:18){
#  means1[tti-2] <- mean(hgshz[hgsrt[with(hgsrt, ttbin == tti),]$xidx, 1, 5, 1, "temp"])
# }
# plot(means1)

riverInput <- read.table("IskulpaaRiverTemp.txt", skip = 1, col.names = c("s", "e", "temp") )
riverin <- xts(zoo(riverInput$temp), order.by = mdy_hms("01-01-2015 00:00:00")+riverInput$s)

hgshzTS_1 <- xts(zoo(hgshz[1,1,5,,"temp"]), order.by = hgstimes)


plot.zoo(riverin["2016-08-01"], col = "dodgerblue")
#lines(as.zoo(tthz$cTemp2$svValue["2016-08-01"]), col = "goldenrod")
lines(as.zoo(tthz$tsz1Temp$svValue["2016-08-01"]), col = "gold")
lines(as.zoo(hgshz[]))

plot.zoo(riverin["2016"], col = "dodgerblue")
lines(as.zoo(hgshzTS_1), col = "magenta")


# not sure what refTempInput is:
refTempInput <- read.table("refTempInput.txt", skip = 1, col.names = c("s", "e", "temp") )
refT <- xts(zoo(refTempInput$temp), order.by = mdy_hms("01-01-2015 00:00:00")+refTempInput$s)

plot.zoo(refT["2016"])
lines(as.zoo(riverin["2016"]), col = "red")

mean_vx*86400
