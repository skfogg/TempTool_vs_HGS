library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)
library(HGSReader)

### Load the TempTool output (little HZ)
load("temptool_output/TempTool_oneOutputChannel.RData")
littlehypo <- oneChannel
littlehypobins <-  TSZStats(18, 60, 182*86400, 11.98, factor = 2, alpha = 1.39)

#### High HZ
load("temptool_output/highHypo_OneChannel.RData")
highhypo <- oneChannel
highhypobins <-  TSZStats(18, 60, 182*86400, 35.95, factor = 2, alpha = 1.39)

#### heat out from high HZ
load("temptool_output/tszHeatOut_high.RData")

## TempTool Hyporheic Zone Output
plot.zoo(littlehypo[[2]]$svValue["2016"])
mapply(function(x,c) lines(as.zoo(x$svValue), col = c),
       x = littlehypo,
       c = hcl.colors(19))

plot.zoo(highhypo[[2]]$svValue["2016"])
mapply(function(x,c) lines(as.zoo(x$svValue), col = c),
       x = littlehypo,
       c = hcl.colors(19))

## Upwelling Output
plot.zoo(highhypo$cTemp2$svValue["2016"])
lines(as.zoo(littlehypo$cTemp2$svValue["2016"]),
      col = "blue")

inputtemp <- read.table("IskulpaaRiverTemp.txt",
           skip = 1, col.names = c("s", "e", "temp"))
inputtemp <- xts(zoo(inputtemp$temp, order.by = mdy_hms("01-01-2014 00:00:00")+inputtemp$s))

plot.zoo(inputtemp["2016"], col = "dodgerblue")
lines(as.zoo(highhypo$cTemp2$svValue["2016"]),
      col = "red")
lines(as.zoo(littlehypo$cTemp2$svValue["2016"]),
      col = "orange")

## TT daily mean calc
little_dailymeans <- lapply(littlehypo, apply.daily, mean)
high_dailymeans <- lapply(highhypo, apply.daily, mean)

## TT daily mean plot
plot.zoo(little_dailymeans[[2]]$svValue["2016"])
mapply(function(x,c) lines(as.zoo(x$svValue), col = c),
       x = little_dailymeans,
       c = hcl.colors(19))
plot.zoo(high_dailymeans[[2]]$svValue["2016"])
mapply(function(x,c) lines(as.zoo(x$svValue), col = c),
       x = high_dailymeans,
       c = hcl.colors(19))


## Calc phase function
findphase <- function(x){
  d <- index(x["2016"][coredata(x["2016"]) == max(x["2016"])])
  return(floor(as.numeric(julian(d, origin = mdy("01-01-2016")))))
}

## Associate phase with residence time/water age
little_phases <- data.frame(WA = littlehypobins$meanWaterAge, RT = littlehypobins$meanResTime, phaseDay = numeric(18))
for(i in 2:19){
  little_phases$phaseDay[i-1] <- findphase(little_dailymeans[[i-1]]$svValue)
}

high_phases <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, phaseDay = numeric(18))
for(i in 2:19){
  high_phases$phaseDay[i-1] <- findphase(high_dailymeans[[i-1]]$svValue)
}

## Calc Ranges
little_ranges <- data.frame(WA = littlehypobins$meanWaterAge, RT = littlehypobins$meanResTime, range = numeric(18))
for(i in 2:19){
  little_ranges$range[i-1] <- round(max(little_dailymeans[[i-1]]$svValue["2016"]) - min(little_dailymeans[[i-1]]$svValue["2016"]), 2)
}

high_ranges <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, range = numeric(18))
for(i in 2:19){
  high_ranges$range[i-1] <- round(max(high_dailymeans[[i-1]]$svValue["2016"]) - min(high_dailymeans[[i-1]]$svValue["2016"]), 2)
}



#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
plot(phaseDay ~ I(WA/86400), little_phases,
     ylim = c(200, 360),
     xlim = c(0, 365),
     ylab = "Local Phase (Day of the Year)",
     xlab = "Residence Time (Days)",
     col = "forestgreen")
points(phaseDay ~ I(WA/86400), high_phases,
       col = "orange")
# Helton's fit line
lines(205*exp(1)^(0.0015*seq(1,365)), col = "dodgerblue")
legend("topleft", c("TempTool Output", "HGS Output", "Helton 2012 Best Fit Line"),
       pch = c(1,1,NA), lty = c(NA, NA,1), col = c("black","orangered3", "dodgerblue"))

########################################
## Re-Create Helton's Plot: Range v. RT
########################################
plot(range ~ I(WA/86400),
     little_ranges,
     ylim = c(0, 20),
     xlim = c(0, 365),
     ylab = "Temperature Range (C)",
     xlab = "Residence Time (Days)",
     col = "forestgreen")
points(range~I(WA/86400), high_ranges, col = "orange")
lines(16.2*exp(1)^(-0.0065*seq(1,365)), col = "dodgerblue")
legend("topright", c("TempTool Output", "HGS Output", "Helton 2012 Best Fit Line"),
       pch = c(1,1,NA), lty = c(NA, NA,1), col = c("black","orangered3", "dodgerblue"))

