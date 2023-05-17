####
####
## Compare Helton fit lines to
## TempTool Thermal insulation v capacitance
####
####


library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)
library(HGSReader)

# load("temptool_output/tszHeatOut_high.RData")
# load("temptool_output/tszHeatOut_little.RData")
load("temptool_output/highhypo870.RData")
load("temptool_output/medhypo870.RData")
load("temptool_output/littlehypo870.RData")
helton <- read.csv("Helton_2014_Figure_5.csv")
highhypobins <- TSZStats(18, 60, 182*86400, 35.94, factor = 2, alpha = 1.39)
medhypobins <- TSZStats(18, 60, 182*86400, 23.96, factor = 2, alpha = 1.39)
littlehypobins <- TSZStats(18, 60, 182*86400, 11.98, factor = 2, alpha = 1.39)


highhypo <- highHypo870
medhypo <- medHypo870
lowhypo <- littleHypo870

timeindex <- index(highhypo$cTemp2)

zooit <- function(x,t){
  return(xts(zoo(x, order.by = t)))
}

#### HEAT SHIT #####
# heatout_high <- lapply(tszHeatOut_high, zooit, t = timeindex)
# heatout_little <- lapply(tszHeatOut_little, zooit, t = timeindex)
#
# plot.zoo(heatout_high[[1]]["2016"], ylim = c(0,50))
# mapply(function(x,c) lines(as.zoo(x["2016"]), col = c),
#        heatout_high,
#        hcl.colors(18))
#
# plot.zoo(heatout_little[[1]]["2016"], ylim = c(0,50))
# mapply(function(x,c) lines(as.zoo(x["2016"]), col = c),
#        heatout_little,
#        hcl.colors(18))

#### TEMP SHIT ####
plot.zoo(highHypo870[[2]]$svValue["2016"], ylim = c(0,25))
mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
       highHypo870[2:19],
       hcl.colors(18))

plot.zoo(medHypo870[[2]]$svValue["2016"], ylim = c(0,25))
mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
       medHypo870[2:19],
       hcl.colors(18))

plot.zoo(littleHypo870[[2]]$svValue["2016"], ylim = c(0,25))
mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
       littleHypo870[2:19],
       hcl.colors(18))

## TT daily mean calc
little_dailymeans <- lapply(littleHypo870, apply.daily, mean)
med_dailymeans <- lapply(medHypo870, apply.daily, mean)
high_dailymeans <- lapply(highHypo870, apply.daily, mean)

## TT daily mean plot
# plot.zoo(little_dailymeans[[2]]$svValue["2016"])
# mapply(function(x,c) lines(as.zoo(x$svValue), col = c),
#        x = little_dailymeans,
#        c = hcl.colors(19))
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

med_phases <- data.frame(WA = medhypobins$meanWaterAge, RT = medhypobins$meanResTime, phaseDay = numeric(18))
for(i in 2:19){
  med_phases$phaseDay[i-1] <- findphase(med_dailymeans[[i-1]]$svValue)
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

med_ranges <- data.frame(WA = medhypobins$meanWaterAge, RT = medhypobins$meanResTime, range = numeric(18))
for(i in 2:19){
  med_ranges$range[i-1] <- round(max(med_dailymeans[[i-1]]$svValue["2016"]) - min(med_dailymeans[[i-1]]$svValue["2016"]), 2)
}

high_ranges <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, range = numeric(18))
for(i in 2:19){
  high_ranges$range[i-1] <- round(max(high_dailymeans[[i-1]]$svValue["2016"]) - min(high_dailymeans[[i-1]]$svValue["2016"]), 2)
}

## Calc Amp Ratio
little_amp_ratio <- data.frame(WA = littlehypobins$meanWaterAge, RT = littlehypobins$meanResTime, amp_ratio = numeric(18))
for(i in 2:19){
  little_amp_ratio$amp_ratio[i-1] <- ((max(little_dailymeans[[i-1]]$svValue["2016"]) - min(little_dailymeans[[i-1]]$svValue["2016"]))/2)/((max(little_dailymeans[[1]]$svValue["2016"]) - min(little_dailymeans[[1]]$svValue["2016"]))/2)
}

med_amp_ratio <- data.frame(WA = medhypobins$meanWaterAge, RT = medhypobins$meanResTime, amp_ratio = numeric(18))
for(i in 2:19){
  med_amp_ratio$amp_ratio[i-1] <- ((max(med_dailymeans[[i-1]]$svValue["2016"]) - min(med_dailymeans[[i-1]]$svValue["2016"]))/2)/((max(med_dailymeans[[1]]$svValue["2016"]) - min(med_dailymeans[[1]]$svValue["2016"]))/2)
}

high_amp_ratio <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, amp_ratio = numeric(18))
for(i in 2:19){
  high_amp_ratio$amp_ratio[i-1] <- ((max(high_dailymeans[[i-1]]$svValue["2016"]) - min(high_dailymeans[[i-1]]$svValue["2016"]))/2)/((max(high_dailymeans[[1]]$svValue["2016"]) - min(high_dailymeans[[1]]$svValue["2016"]))/2)
}


# little_phases_lm <- lm(phaseDay ~ WA, little_phases)
# summary(little_phases_lm)

### Empirical models ####
## LINEAR ###
little_phases_lm <- lm(phaseDay ~ RT, little_phases)
summary(little_phases_lm)

little_ranges_lm <- lm(range ~ RT, little_ranges)
summary(little_ranges_lm)


med_phases_lm <- lm(phaseDay ~ RT, med_phases)
summary(med_phases_lm)

med_ranges_lm <- lm(range ~ RT, med_ranges)
summary(med_ranges_lm)


high_phases_lm <- lm(phaseDay ~ RT, high_phases)
summary(high_phases_lm)

high_ranges_lm <- lm(range ~ RT, high_ranges)
summary(high_ranges_lm)

## EXPONENTIAL ##
high_phases_exp <- nls(phaseDay ~ y*exp(z*I(RT/86400)), data = high_phases, start = list(y = 210, z = 0.01) )
summary(high_phases_exp)

high_range_exp <- nls(range ~ y*exp(z*I(RT/86400)), data = high_ranges, start = list(y = 20, z = 0.01) )
summary(high_range_exp)

helton_phases_exp <- nls(Phase ~ y*exp(z*Avg_Res_Time_days), data = helton, start = list(y = 210, z = 0.001))
summary(helton_phases_exp)

helton_ranges_exp <- nls(FitRange ~ y*exp(z*Avg_Res_Time_days), data = helton, start = list(y = 16, z = 0.001))
summary(helton_ranges_exp)

## POWER ##
# doesn't work-->
# helton_ranges_pwr <- nls(FitRange ~ (y*Avg_Res_Time_days)^(z), data = helton, start = list(y = 210, z = 1.5))


#### Multiple LR MODELS #####
# Q: Are TempTool values the within the error of Helton's values?

allphases <- data.frame(phase = c(high_phases$phaseDay - 208.2237, helton$Phase - 205),
                        rt = c(high_phases$RT, helton$Avg_Res_Time_days),
                        model = c(rep("temptool", times=nrow(high_phases)), rep("helton", times = nrow(helton))))

allranges <- data.frame(range = c(high_ranges$range - 19.2901, helton$Phase - 16.2),
                        rt = c(high_ranges$RT, helton$Avg_Res_Time_days),
                        model = c(rep("temptool", times=nrow(high_ranges)), rep("helton", times = nrow(helton))))

combined_phases <- lm(phase ~ rt + model, data = allphases)
summary(combined_phases)

combined_ranges <- lm(range ~ rt + model, data = allranges)
summary(combined_ranges)


#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
# png("plots/localphase_v_RT.png", width = 600*5, height = 600*5, res = 72*5)
# plot(I(phaseDay) ~ I(RT/86400), little_phases,
#      ylim = c(200, 325),
#      xlim = c(0, 182),
#      ylab = "Local Phase (Day of Year)",
#      xlab = "Residence Time (Days)",
#      bg = "orange",
#      pch = 21,
#      main = "Local Phase ~ Residence Time")
# points(I(phaseDay) ~ I(RT/86400), high_phases,
#        bg = "orangered3", pch = 21)
# # Helton's fit line
# lines((205*exp(1)^(0.0015*seq(0.01,182))), col = "dodgerblue")
# legend("topleft", c("little HE", "high HE", "Helton 2012 Best Fit Line"),
#        pch = c(21,21,NA), lty = c(NA, NA,1), col = c(1,1, "dodgerblue"),
#        pt.bg = c("orange", "orangered3", NA))
# dev.off()

#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
png("plots/phaselag_v_RT_3.png", width = 580*5, height = 600*5, res = 72*5)
plot(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
     ylim = c(0.1, 150),
     xlim = c(0, 300),
     ylab = "Phase Lag (Days)",
     xlab = "Residence Time (Days)",
     bg = "orangered3",
     pch = 23,
     cex = 1.3)
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, bg = "lightblue", col = "dodgerblue", cex = 1.3)
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, bg = "lightblue", col = "dodgerblue", cex = 1.3)

lines((205*exp(1)^(0.0015*seq(0.01,300))-205), col = "dodgerblue", lwd = 2)
legend("topleft", c("High HE Scenario", "Helton data & best fit line"),
       pch = c(23,21), lty = c(NA,1), col = c(1, "dodgerblue"),
       pt.bg = c("orangered3", "lightblue"), lwd = c(NA, 2))
dev.off()

#######
#######
#######
hypocols <- hcl.colors(6, "Vik")[1:3]
png("plots/phaselag_v_RT_all.png", width = 580*5, height = 600*5, res = 72*5)
plot(I(phaseDay - little_phases_lm$coefficients[1]) ~ I(RT/86400), little_phases,
     ylim = c(0.1, 150),
     xlim = c(0, 300),
     ylab = "Phase Lag (Days)",
     xlab = "Residence Time (Days)",
     bg = hypocols[1],
     pch = 23,
     cex = 1.3)
points(I(phaseDay - med_phases_lm$coefficients[1]) ~ I(RT/86400), med_phases,
       bg = hypocols[2],
       pch = 23,
       cex = 1.3)
points(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
       bg = hypocols[3],
       pch = 23,
       cex = 1.3)
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, bg = "orangered3", col = "black", cex = 1.3)
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, bg = "orangered3", col = "black", cex = 1.3)

lines((205*exp(1)^(0.0015*seq(0.01,300))-205), col = "dodgerblue", lwd = 2)
legend("topleft", c("High HE Scenario", "Helton data & best fit line"),
       pch = c(23,21), lty = c(NA,1), col = c(1, "dodgerblue"),
       pt.bg = c("orangered3", "lightblue"), lwd = c(NA, 2))
dev.off()


plot(I(phaseDay - little_phases_lm$coefficients[1]) ~ I(RT/86400), little_phases,
     ylim = c(-1, 20),
     xlim = c(0, 20),
     ylab = "Phase Lag (Days)",
     xlab = "Residence Time (Days)",
     bg = hypocols[1],
     pch = 23,
     cex = 1,
     type = "o")
points(I(phaseDay - med_phases_lm$coefficients[1]) ~ I(RT/86400), med_phases,
       bg = hypocols[2],
       pch = 23,
       cex = 1,
       type = "o")
points(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
       bg = hypocols[3],
       pch = 23,
       cex = 1,
       type = "o")

#######################################
## Re-Create Helton's Plot: Phase v. RT
#######################################
# png("plots/phaselag_v_logRT.png", width = 600*5, height = 600*5, res = 72*5)
# plot(I(phaseDay - little_phases_lm$coefficients[1]) ~ I(RT/86400), little_phases,
#      ylim = c(0.1, 80),
#      xlim = c(0.001, 182),
#      ylab = "Phase Lag (Days)",
#      xlab = "Residence Time (log(Days))",
#      bg = "orange",
#      pch = 21,
#      log = "x",
#      main = "Phase Lag ~ log(Residence Time)")
# points(Phase ~ Avg_Res_Time_days, helton, pch = 21, bg = "lightblue", col = "dodgerblue")
# points(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
#        bg = "orangered3", pch = 21)
# # Helton's fit line
# lines((205*exp(1)^(0.0015*seq(0.01,182))-205), col = "dodgerblue")
# legend("topleft", c("little HE", "high HE", "Helton 2012 Best Fit Line"),
#        pch = c(21,21,NA), lty = c(NA, NA,1), col = c(1,1, "dodgerblue"),
#        pt.bg = c("orange", "orangered3", NA))
# dev.off()

########################################
## Re-Create Helton's Plot: Range v. RT
########################################


png("plots/range_v_RT_3.png", width = 600*5, height = 600*5, res = 72*5)
plot(I(range - high_ranges_lm$coefficients[1]) ~ I(RT/86400),
     high_ranges,
     ylim = c(-14,0),
     xlim = c(0, 300),
     ylab = expression(paste(Delta, "Temperature Range ( ", degree,"C)")),
     xlab = "Residence Time (Days)",
     bg = "orangered3",
     pch = 23,
     cex = 1.3)
points(I(FitRange-16.2) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, col = "dodgerblue", bg = "lightblue", cex = 1.3)
points(I(FitRange-16.2) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, col = "dodgerblue", bg = "lightblue", cex = 1.3)
lines((16.2*exp(1)^(-0.0065*seq(1,300)) - 16.2), col = "dodgerblue", lwd = 2)
legend("topright", c("High HE Scenario", "Helton data & best fit line"),
       pch = c(23,21), lty = c(NA,1), col = c(1, "dodgerblue"),
       pt.bg = c("orangered3", "lightblue"), lwd = c(NA, 2))


dev.off()


### BOTH ####
pt.cex <- 2.5
png("plots/phaselag_range_v_RT_3.png", width = 580*5, height = 1200*5, res = 72*5)
par(mfrow = c(2,1),
    mar = c(1,5,1,1),
    oma = c(4,0,0,0),
    bty = "l",
    cex.axis = 1.8,
    cex.lab = 2)
plot(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
     ylim = c(0.1, 160),
     xlim = c(0, 305),
     ylab = "Phase Lag (days)",
     xlab = "Residence Time (days)",
     bg = "orangered3",
     pch = 23,
     cex = pt.cex,
     xaxt = "n")
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, bg = "lightblue", col = "dodgerblue", cex = pt.cex)
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, bg = "lightblue", col = "dodgerblue", cex = pt.cex)
lines((205*exp(1)^(0.0015*seq(0.01,305))-205), col = "dodgerblue", lwd = 3)
# lines((coef(helton_phases_exp)[1]*exp(coef(helton_phases_exp)[2]*seq(0.01,305)))-coef(helton_phases_exp)[1],
#       col = "blue", lwd = 3)
points(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
       bg = "orangered3",
       pch = 23,
       cex = pt.cex)
# lines((209*exp(0.0031*seq(0.01,305))-209), col = "black", lwd = 2)
legend("top", c("High HE Scenario", "Nyack Floodplain"),
       pch = c(23,21), lty = c(NA,1), col = c(1, "dodgerblue"),
       pt.bg = c("orangered3", "lightblue"), lwd = c(NA, 3),
       bty = "n",
       cex = 2.5)
# legend("topleft", c("High HE Scenario", "Helton data & best fit line"),
#        pch = c(23,21), lty = c(NA,1), col = c(1, "dodgerblue"),
#        pt.bg = c("orangered3", "lightblue"), lwd = c(NA, 2))
axis(1, at = c(0,50,100,150,200,250,300), labels = T)


plot(I(amp_ratio) ~ I(RT/86400),
     high_amp_ratio,
     ylim = c(0,1.145),
     xlim = c(0, 305),
     ylab = "Amplitude Ratio",
     xlab = "Residence Time (days)",
     bg = "orangered3",
     pch = 23,
     cex = pt.cex)

points(I(amp_ratio) ~ I(RT/86400),
       med_amp_ratio,
       pch = 23, cex = pt.cex,
       bg = hypocols[2])
points(I(amp_ratio) ~ I(RT/86400),
       little_amp_ratio,
       pch = 23, cex = pt.cex,
       bg = hypocols[1])


points(I((FitRange/2)/(16.2/2)) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, col = "dodgerblue", bg = "lightblue", cex = pt.cex)
points(I((FitRange/2)/(16.2/2)) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, col = "dodgerblue", bg = "lightblue", cex = pt.cex)
# lines((16.2*exp(1)^(-0.0065*seq(1,305)) - 16.2), col = "dodgerblue", lwd = 3)
lines((exp(1)^(-0.0065*seq(1,305))), col = "dodgerblue", lwd = 3)

# lines(exp(1)^(-0.0065*seq(1,305)), col = "purple", lwd = 3)
# lines((coef(helton_ranges_exp)[1]*exp(coef(helton_ranges_exp)[2]*seq(0.01,305)))-coef(helton_ranges_exp)[1],
      # col = "blue", lwd = 3)

points(I(amp_ratio) ~ I(RT/86400),
       high_amp_ratio,
       bg = "orangered3",
       pch = 23,
       cex = pt.cex)
# lines((19.3*exp(-0.0025*seq(0.01,305))-19.3), col = "black", lwd = 2)

mtext("Hyporheic Water Age (days)", outer = T, side = 1, line = 2, cex = 2)
dev.off()


##########
########## Scaled axis
##########
png("plots/phaselag_range_scaled.png", width = 580*5, height = 1200*5, res = 72*5)
par(mfrow = c(2,1),
    mar = c(1,5,1,1),
    oma = c(9,0,0,0),
    bty = "l",
    cex.axis = 1.8,
    cex.lab = 2)
plot(I(phaseDay - high_phases_lm$coefficients[1]) ~ I((RT*2.5)/86400), high_phases,
     ylim = c(0.1, 160),
     xlim = c(0, 305),
     ylab = "Phase Lag (days)",
     xlab = "Residence Time (days)",
     bg = "orangered3",
     pch = 23,
     cex = pt.cex,
     xaxt = "n")
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, bg = "lightblue", col = "dodgerblue", cex = pt.cex)
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, bg = "lightblue", col = "dodgerblue", cex = pt.cex)
lines((205*exp(1)^(0.0015*seq(0.01,305))-205), col = "dodgerblue", lwd = 3)
# lines((coef(helton_phases_exp)[1]*exp(coef(helton_phases_exp)[2]*seq(0.01,305)))-coef(helton_phases_exp)[1],
#       col = "blue", lwd = 3)
points(I(phaseDay - high_phases_lm$coefficients[1]) ~ I((RT*2.5)/86400), high_phases,
       bg = "orangered3",
       pch = 23,
       cex = pt.cex)
# lines((209*exp(0.0031*seq(0.01,305))-209), col = "black", lwd = 2)
legend("top", c("High HE Scenario", "Nyack Floodplain"),
       pch = c(23,21), lty = c(NA,1), col = c(1, "dodgerblue"),
       pt.bg = c("orangered3", "lightblue"), lwd = c(NA, 3),
       bty = "n",
       cex = 2.5)


plot(I(amp_ratio) ~ I((RT*2.5)/86400),
     high_amp_ratio,
     ylim = c(0,1.145),
     xlim = c(0, 305),
     ylab = "Amplitude Ratio",
     xlab = "Residence Time (days)",
     bg = "orangered3",
     pch = 23,
     cex = pt.cex)

# points(I(amp_ratio) ~ I(RT/86400),
#        med_amp_ratio,
#        pch = 23, cex = pt.cex,
#        bg = hypocols[2])
# points(I(amp_ratio) ~ I(RT/86400),
#        little_amp_ratio,
#        pch = 23, cex = pt.cex,
#        bg = hypocols[1])


points(I((FitRange/2)/(16.2/2)) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, col = "dodgerblue", bg = "lightblue", cex = pt.cex)
points(I((FitRange/2)/(16.2/2)) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, col = "dodgerblue", bg = "lightblue", cex = pt.cex)
# lines((16.2*exp(1)^(-0.0065*seq(1,305)) - 16.2), col = "dodgerblue", lwd = 3)
lines((exp(1)^(-0.0065*seq(1,305))), col = "dodgerblue", lwd = 3)


points(I(amp_ratio) ~ I((RT*2.5)/86400),
       high_amp_ratio,
       bg = "orangered3",
       pch = 23,
       cex = pt.cex)
# lines((19.3*exp(-0.0025*seq(0.01,305))-19.3), col = "black", lwd = 2)
abline(h = exp(-1))
text(25, exp(-1)+0.03, "exp(-1)", cex = 1.4)
abline(h = exp(-0.5))
text(25, exp(-0.5)+0.03, "exp(-0.5)", cex = 1.4)
abline(h = exp(-0.25))
text(25, exp(-0.25)+0.03, "exp(-0.25)", cex = 1.4)


mtext("Nyack Hyporheic Water Age (days)", outer = T, side = 1, line = 2, cex = 2)

axis(1, at = c(0,50,100,150,200,250,300), labels = c(0,50,100,150,200,250,300)/2.5, line = 4, outer = T)
mtext("TempTool Hyporheic Water Age (days)", outer = T, side = 1, line = 7, cex = 2)
dev.off()

##### OTHER STUFF#####

# mean + (0.5Range)*cos(d - LocalPhase)*2pi/365
par(mfrow = c(2,1))
# helton <- helton[order(helton$Avg_Res_Time_days),]
d = 1:365
plot(helton$FitMean[1] + (0.5*helton$FitRange[1])*cos((d-helton$Phase[1])*(2*pi/365)),
     type = "l",
     ylim = c(-3,22),
     ylab = expression(paste("Temperature ( ", degree,"C)")),
     xlab ="Day of Year")
for(i in 1:nrow(helton)){
  lines(helton$FitMean[i] + (0.5*helton$FitRange[i])*cos((d-helton$Phase[i])*(2*pi/365)),
          col = c("lightblue1", "lightblue2", "lightblue3",
                  hcl.colors(nrow(helton)-3, "Burg"))[i],
        lwd = c(rep(4,times = 3),
                rep(2, times = nrow(helton)-3))[i])
}

plot(1:366, coredata(high_dailymeans[[1]]$svValue["2016"]),
     col = "lightblue1", lwd = 3, type = "l",
     ylim = c(-3,22),
     ylab = expression(paste("Temperature ( ", degree,"C)")),
     xlab ="Day of Year")
mapply(function(x,c,l) lines(coredata(x$svValue["2016"]), col = c, lwd = l),
       x = high_dailymeans,
       c = c("lightblue1", hcl.colors(18, "Burg")),
       l = c(4, rep(2, times = 18)))
mtext("Day of Year", outer = T, side = 1, line = 2, cex = 2)

helton <- helton[order(helton$Avg_Res_Time_days),]
helton_full <- array(NA, c(365, 19), dimnames = list(1:365, round(helton$Avg_Res_Time_days, 1)))
for(i in 1:nrow(helton)){
  helton_full[,i] <- helton$FitMean[i] + (0.5*helton$FitRange[i])*cos((d-helton$Phase[i])*(2*pi/365))
}

par(mfrow = c(1,1))
png("plots/helton_temp_across_res_time.png", width = 800*5, height = 600*5, res = 72*5)
par(cex.axis=1.4,
    cex.lab = 1.4,
    cex = 1.3)
plot(helton$Avg_Res_Time_days[3:19], helton_full[18,3:19], type = "o", ylim = c(-4, 19), pch = 20,
     ylab = "Temperature (C)",
     xlab = "Hydrologic Residence Time (Days)")
# abline(v = subset(helton, Model_Layer == "9000")$Avg_Res_Time_days, col = "gold", lwd = 1)
index <- c(18,110,201,292)
for(i in 1:4){
  points(helton$Avg_Res_Time_days[3:19], helton_full[index[i],3:19],
         pch = 20,
         type= "o",
         col = hcl.colors(4, "Cork")[i])
  text(35, helton_full[index[i],3]-c(1,3.75,1,1)[i], labels = c("Jan-18", "Apr-20", "Jul-20", "Oct-19")[i],
       col =  hcl.colors(4, "Cork")[i],
       pos = 3)
}
dev.off()

abline(v = subset(helton, Model_Layer == "9000")$Avg_Res_Time_days, col = "gold", lwd = 1)
