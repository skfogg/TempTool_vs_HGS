####
####
## Compare Helton fit lines to
## TempTool Thermal insulation v capacitance
####
####




load("temptool_output/tszHeatOut_high.RData")
load("temptool_output/tszHeatOut_little.RData")
load("temptool_output/highhypo870.RData")
load("temptool_output/littlehypo870.RData")

timeindex <- index(highhypo$cTemp2)

zooit <- function(x,t){
  return(xts(zoo(x, order.by = t)))
}

#### HEAT SHIT #####
heatout_high <- lapply(tszHeatOut_high, zooit, t = timeindex)
heatout_little <- lapply(tszHeatOut_little, zooit, t = timeindex)

plot.zoo(heatout_high[[1]]["2016"], ylim = c(0,50))
mapply(function(x,c) lines(as.zoo(x["2016"]), col = c),
       heatout_high,
       hcl.colors(18))

plot.zoo(heatout_little[[1]]["2016"], ylim = c(0,50))
mapply(function(x,c) lines(as.zoo(x["2016"]), col = c),
       heatout_little,
       hcl.colors(18))

#### TEMP SHIT ####
plot.zoo(highHypo870[[2]]$svValue["2016"], ylim = c(0,25))
mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
       highHypo870[2:19],
       hcl.colors(18))

plot.zoo(littleHypo870[[2]]$svValue["2016"], ylim = c(0,25))
mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
       littleHypo870[2:19],
       hcl.colors(18))

## TT daily mean calc
little_dailymeans <- lapply(littleHypo870, apply.daily, mean)
high_dailymeans <- lapply(highHypo870, apply.daily, mean)

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

little_phases_lm <- lm(phaseDay ~ WA, little_phases)
summary(little_phases_lm)

high_phases_lm <- lm(phaseDay ~ WA, high_phases)
summary(high_phases_lm)

#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
png("plots/localphase_v_RT.png", width = 600*5, height = 600*5, res = 72*5)
plot(I(phaseDay) ~ I(RT/86400), little_phases,
     ylim = c(200, 325),
     xlim = c(0, 182),
     ylab = "Local Phase (Day of Year)",
     xlab = "Residence Time (Days)",
     bg = "orange",
     pch = 21,
     main = "Local Phase ~ Residence Time")
points(I(phaseDay) ~ I(RT/86400), high_phases,
       bg = "orangered3", pch = 21)
# Helton's fit line
lines((205*exp(1)^(0.0015*seq(0.01,182))), col = "dodgerblue")
legend("topleft", c("little HE", "high HE", "Helton 2012 Best Fit Line"),
       pch = c(21,21,NA), lty = c(NA, NA,1), col = c(1,1, "dodgerblue"),
       pt.bg = c("orange", "orangered3", NA))
dev.off()

#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
png("plots/phaselag_v_RT.png", width = 600*5, height = 600*5, res = 72*5)
plot(I(phaseDay - little_phases_lm$coefficients[1]) ~ I(RT/86400), little_phases,
     ylim = c(0.1, 100),
     xlim = c(0, 182),
     ylab = "Phase Lag (Days)",
     xlab = "Residence Time (Days)",
     bg = "orange",
     pch = 21,
     main = "Phase Lag ~ Residence Time")
points(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
       bg = "orangered3", pch = 21)
# Helton's fit line
lines((205*exp(1)^(0.0015*seq(0.01,182))-205), col = "dodgerblue")
legend("topleft", c("little HE", "high HE", "Helton 2012 Best Fit Line"),
       pch = c(21,21,NA), lty = c(NA, NA,1), col = c(1,1, "dodgerblue"),
       pt.bg = c("orange", "orangered3", NA))
dev.off()


#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
png("plots/phaselag_v_logRT.png", width = 600*5, height = 600*5, res = 72*5)
plot(I(phaseDay - little_phases_lm$coefficients[1]) ~ I(RT/86400), little_phases,
     ylim = c(0.1, 80),
     xlim = c(0.001, 182),
     ylab = "Phase Lag (Days)",
     xlab = "Residence Time (log(Days))",
     bg = "orange",
     pch = 21,
     log = "x",
     main = "Phase Lag ~ log(Residence Time)")
points(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
       bg = "orangered3", pch = 21)
# Helton's fit line
lines((205*exp(1)^(0.0015*seq(0.01,182))-205), col = "dodgerblue")
legend("topleft", c("little HE", "high HE", "Helton 2012 Best Fit Line"),
       pch = c(21,21,NA), lty = c(NA, NA,1), col = c(1,1, "dodgerblue"),
       pt.bg = c("orange", "orangered3", NA))
dev.off()

########################################
## Re-Create Helton's Plot: Range v. RT
########################################
png("plots/range_v_RT.png", width = 600*5, height = 600*5, res = 72*5)
plot(range ~ I(WA/86400),
     little_ranges,
     ylim = c(0, 25),
     xlim = c(0, 182),
     ylab = "Temperature Range (C)",
     xlab = "Residence Time (Days)",
     bg = "orange",
     pch = 21,
     main = "Range ~ Residence Time")
points(range~I(WA/86400), high_ranges, pch = 21, bg = "orangered3")
lines(16.2*exp(1)^(-0.0065*seq(1,182)), col = "dodgerblue")
legend("topright", c("little HE", "high HE", "Helton 2012 Best Fit Line"),
       pch = c(21,21,NA), lty = c(NA, NA,1), col = c(1,1, "dodgerblue"),
       pt.bg = c("orange", "orangered3", NA))
dev.off()




