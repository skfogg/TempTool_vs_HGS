

###
### Create plots of local phase (day of peak temp)
### and temperature range as a
### function of hydrologic residence time, akin to
### Helton et al. 2012 plots
###

library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)
library(HGSReader)

### Load the TempTool output
load("temptool_output/TempTool_oneOutputChannel.RData")
littlehypo <- oneChannel
littlehypobins <-  TSZStats(18, 60, 182*86400, 11.98, factor = 2, alpha = 1.39)

## TempTool Hyporheic Zone Output
plot.zoo(littlehypo[[2]]$svValue["2016"])
mapply(function(x,c) lines(as.zoo(x$svValue), col = c),
       x = littlehypo,
       c = hcl.colors(19))

## TT daily mean calc
tt_dailymeans <- lapply(littlehypo, apply.daily, mean)

## TT daily mean plot
plot.zoo(tt_dailymeans[[2]]$svValue["2016"])
mapply(function(x,c) lines(as.zoo(x$svValue), col = c),
       x = tt_dailymeans,
       c = hcl.colors(19))

## Calc phase function
findphase <- function(x){
  d <- index(x["2016"][coredata(x["2016"]) == max(x["2016"])])
  return(floor(as.numeric(julian(d, origin = mdy("01-01-2016")))))
}

## Associate phase with residence time/water age
tt_phases <- data.frame(WA = littlehypobins$meanWaterAge, RT = littlehypobins$meanResTime, phaseDay = numeric(18))
for(i in 2:19){
  tt_phases$phaseDay[i-1] <- findphase(tt_dailymeans[[i-1]]$svValue)
}

#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
plot(phaseDay ~ I(RT/86400), tt_phases,
     ylim = c(200, 360),
     xlim = c(0, 365),
     ylab = "Local Phase (Day of the Year)",
     xlab = "Residence Time (Days)")
# Helton's fit line
lines(205*exp(1)^(0.0015*seq(1,365)), col = "dodgerblue")
legend("topleft", c("TempTool Output", "Helton 2012 Best Fit Line"),
       pch = c(1,NA), lty = c(NA,1), col = c("black", "dodgerblue"))

## Log the X?
plot(phaseDay ~ I(RT/86400), tt_phases, log = "x")

## Associate Temperature Range with residence time/water age
calcrange <- function(x) max(x)-min(x)
tt_ranges <- data.frame(WA = littlehypobins$meanWaterAge, RT = littlehypobins$meanResTime, range = numeric(18))
for(i in 2:19){
  tt_ranges$range[i-1] <- round(max(tt_dailymeans[[i-1]]$svValue) - min(tt_dailymeans[[i-1]]$svValue), 2)
}

########################################
## Re-Create Helton's Plot: Range v. RT
########################################
plot(range ~ I(RT/86400),
     tt_ranges,
     ylim = c(0, 20),
     xlim = c(0, 365),
     ylab = "Temperature Range (C)",
     xlab = "Residence Time (Days)")
lines(16.2*exp(1)^(-0.0065*seq(1,365)), col = "dodgerblue")
legend("topleft", c("TempTool Output", "Helton 2012 Best Fit Line"),
       pch = c(1,NA), lty = c(NA,1), col = c("black", "dodgerblue"))


#######################
#######################
### Load the HGS output
#######################
#######################
hgs_output_location <- "C:/Users/skati/Box/TempTool_vs_HGS_hgs_output/hgs_output"
hgshz <- readRDS(paste0(hgs_output_location, "/HGSTempTool_Run3.RData"))
v <- readRDS(paste0(hgs_output_location, "/HGSTempTool_Run3_velocity.RData"))
hgsbinned <- readRDS("hgsbinned.RData")
hgstimes <- mdy_hms("01-01-2016 00:00:00") + as.data.frame(hgshz[1,1,5,,])$Time - 365*86400*7

vx <- (v[1,1,5,1,"Vx"])
hgsResTime <- (1/vx)*hgshz[,1,5,1,"X"]

hgsrt <- data.frame(rt = hgsResTime,
                    m = hgshz[,1,5,1,"X"],
                    xidx = 1:length(hgshz[,1,5,1,"X"]),
                    ttbin = numeric(length(hgshz[,1,5,1,"X"])),
                    ttmeanWA = numeric(length(hgshz[,1,5,1,"X"])),
                    ttmeanRT = numeric(length(hgshz[,1,5,1,"X"])))
for(i in 3:18){
  hgsrt[with(hgsrt, rt > littlehypobins$to[i-1] & rt <= littlehypobins$to[i]),]$ttbin <- i
  hgsrt[with(hgsrt, rt > littlehypobins$to[i-1] & rt <= littlehypobins$to[i]),]$ttmeanWA <- littlehypobins$meanWaterAge[i]
  hgsrt[with(hgsrt, rt > littlehypobins$to[i-1] & rt <= littlehypobins$to[i]),]$ttmeanRT <- littlehypobins$meanResTime[i]
}

hgsmeanAge <- numeric(16)
for(i in 3:18){
  hgsmeanAge[i-2] <- mean(subset(hgsrt, ttbin == i)$rt)
}


## HGS Hyporheic Zone Output
plot.zoo(hgsbinned[[1]])
mapply(function(x,c) lines(as.zoo(x), col = c),
       x = hgsbinned,
       c = hcl.colors(16))

## HGS daily mean calc
hgs_dailymeans <- lapply(hgsbinned, apply.daily, mean)

## HGS Hyporheic Daily Means plot
plot.zoo(hgs_dailymeans[[1]], type = "o")
mapply(function(x,c) lines(as.zoo(x), col = c, type = "o"),
       x = hgs_dailymeans,
       c = hcl.colors(16))

## Calc phase function
findphase <- function(x){
  d <- index(x["2016"][coredata(x["2016"]) == max(x["2016"])])
  return(floor(as.numeric(julian(d, origin = mdy("01-01-2016")))))
}

findphase(hgs_dailymeans[[1]])

## Associate phase with residence time/water age
hgs_phases <- data.frame(age = hgsmeanAge, phaseDay = numeric(16))
for(i in 1:16){
  hgs_phases$phaseDay[i] <- findphase(hgs_dailymeans[[i]])
}

#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
plot(phaseDay ~ I(age/86400), hgs_phases,
     ylim = c(200, 360),
     xlim = c(0, 365),
     ylab = "Local Phase (Day of the Year)",
     xlab = "Residence Time (Days)",
     col = "orangered3")
# Helton's fit line
lines(205*exp(1)^(0.0015*seq(1,365)), col = "dodgerblue")
legend("topleft", c("HGS Output", "Helton 2012 Best Fit Line"),
       pch = c(1,NA), lty = c(NA,1), col = c("orangered3", "dodgerblue"))

## Associate Temperature Range with age
hgs_ranges <- data.frame(age = hgsmeanAge, range = numeric(16))
for(i in 1:16){
 hgs_ranges$range[i] <- round(max(hgs_dailymeans[[i]]) - min(hgs_dailymeans[[i]]), 2)
}

########################################
## Re-Create Helton's Plot: Range v. RT
########################################
plot(range ~ I(age/86400),
     hgs_ranges,
     ylim = c(0, 20),
     xlim = c(0, 365),
     ylab = "Temperature Range (C)",
     xlab = "Residence Time (Days)",
     col = "orangered3")
lines(16.2*exp(1)^(-0.0065*seq(1,365)), col = "dodgerblue")
legend("topleft", c("HGS Output", "Helton 2012 Best Fit Line"),
       pch = c(1,NA), lty = c(NA,1), col = c("orangered3", "dodgerblue"))



#### ALL ON ONE PLOTS ####

#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
plot(phaseDay ~ I(age/86400), hgs_phases,
     ylim = c(200, 360),
     xlim = c(0, 365),
     ylab = "Local Phase (Day of the Year)",
     xlab = "Residence Time (Days)",
     col = "orangered3")
points(phaseDay ~ I(RT/86400), tt_phases)
# Helton's fit line
lines(205*exp(1)^(0.0015*seq(1,365)), col = "dodgerblue")
legend("topleft", c("TempTool Output", "HGS Output", "Helton 2012 Best Fit Line"),
       pch = c(1,1,NA), lty = c(NA, NA,1), col = c("black","orangered3", "dodgerblue"))

########################################
## Re-Create Helton's Plot: Range v. RT
########################################
plot(range ~ I(age/86400),
     hgs_ranges,
     ylim = c(0, 20),
     xlim = c(0, 365),
     ylab = "Temperature Range (C)",
     xlab = "Residence Time (Days)",
     col = "orangered3")
points(range~I(RT/86400), tt_ranges)
lines(16.2*exp(1)^(-0.0065*seq(1,365)), col = "dodgerblue")
legend("topright", c("TempTool Output", "HGS Output", "Helton 2012 Best Fit Line"),
       pch = c(1,1,NA), lty = c(NA, NA,1), col = c("black","orangered3", "dodgerblue"))

