####
####
## Compare Helton fit lines to
## TempTool Thermal insulation v capacitance
####
####




# load("temptool_output/tszHeatOut_high.RData")
# load("temptool_output/tszHeatOut_little.RData")
load("temptool_output/highhypo870.RData")
load("temptool_output/littlehypo870.RData")
helton <- read.csv("Helton_2014_Figure_5.csv")
highhypobins <- TSZStats(18, 60, 182*86400, 35.94, factor = 2, alpha = 1.39)


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

plot.zoo(littleHypo870[[2]]$svValue["2016"], ylim = c(0,25))
mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
       littleHypo870[2:19],
       hcl.colors(18))

## TT daily mean calc
little_dailymeans <- lapply(littleHypo870, apply.daily, mean)
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
# little_phases <- data.frame(WA = littlehypobins$meanWaterAge, RT = littlehypobins$meanResTime, phaseDay = numeric(18))
# for(i in 2:19){
#   little_phases$phaseDay[i-1] <- findphase(little_dailymeans[[i-1]]$svValue)
# }

high_phases <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, phaseDay = numeric(18))
for(i in 2:19){
  high_phases$phaseDay[i-1] <- findphase(high_dailymeans[[i-1]]$svValue)
}

## Calc Ranges
# little_ranges <- data.frame(WA = littlehypobins$meanWaterAge, RT = littlehypobins$meanResTime, range = numeric(18))
# for(i in 2:19){
#   little_ranges$range[i-1] <- round(max(little_dailymeans[[i-1]]$svValue["2016"]) - min(little_dailymeans[[i-1]]$svValue["2016"]), 2)
# }

high_ranges <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, range = numeric(18))
for(i in 2:19){
  high_ranges$range[i-1] <- round(max(high_dailymeans[[i-1]]$svValue["2016"]) - min(high_dailymeans[[i-1]]$svValue["2016"]), 2)
}

# little_phases_lm <- lm(phaseDay ~ WA, little_phases)
# summary(little_phases_lm)

### Empirical models ####
## LINEAR ###
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
helton_ranges_pwr <- nls(FitRange ~ (y*Avg_Res_Time_days)^(z), data = helton, start = list(y = 210, z = 1.5))


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
png("plots/phaselag_range_v_RT_2.png", width = 580*5, height = 1200*5, res = 72*5)
par(mfrow = c(2,1),
    mar = c(1,5,1,1),
    oma = c(4,0,0,0),
    bty = "l",
    cex.axis = 1.8,
    cex.lab = 2)
plot(I(phaseDay - high_phases_lm$coefficients[1]) ~ I(RT/86400), high_phases,
     ylim = c(0.1, 160),
     xlim = c(0, 305),
     ylab = expression(paste(Delta, " Phase (Days)")),
     xlab = "Residence Time (Days)",
     bg = "orangered3",
     pch = 23,
     cex = pt.cex,
     xaxt = "n")
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, bg = "lightblue", col = "dodgerblue", cex = pt.cex)
points(I(Phase-205) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, bg = "lightblue", col = "dodgerblue", cex = pt.cex)
lines((205*exp(1)^(0.0015*seq(0.01,305))-205), col = "dodgerblue", lwd = 3)
lines((coef(helton_phases_exp)[1]*exp(coef(helton_phases_exp)[2]*seq(0.01,305)))-coef(helton_phases_exp)[1],
      col = "blue", lwd = 3)
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


plot(I(range - high_ranges_lm$coefficients[1]) ~ I(RT/86400),
     high_ranges,
     ylim = c(-17,3),
     xlim = c(0, 305),
     ylab = expression(paste(Delta, "Temperature Range ( ", degree,"C)")),
     xlab = "Residence Time (Days)",
     bg = "orangered3",
     pch = 23,
     cex = pt.cex)
points(I(FitRange-16.2) ~ Avg_Res_Time_days, subset(helton, Model_Layer == "surface"), pch = 22, col = "dodgerblue", bg = "lightblue", cex = pt.cex)
points(I(FitRange-16.2) ~ Avg_Res_Time_days, subset(helton, Model_Layer != "surface"), pch = 21, col = "dodgerblue", bg = "lightblue", cex = pt.cex)
lines((16.2*exp(1)^(-0.0065*seq(1,305)) - 16.2), col = "dodgerblue", lwd = 3)
lines(exp(1)^(-0.0065*seq(1,305)), col = "purple", lwd = 3)
lines((coef(helton_ranges_exp)[1]*exp(coef(helton_ranges_exp)[2]*seq(0.01,305)))-coef(helton_ranges_exp)[1],
      col = "blue", lwd = 3)

points(I(range - high_ranges_lm$coefficients[1]) ~ I(RT/86400),
       high_ranges,
       bg = "orangered3",
       pch = 23,
       cex = pt.cex)
# lines((19.3*exp(-0.0025*seq(0.01,305))-19.3), col = "black", lwd = 2)

mtext("Hydrologic Residence Time (Days)", outer = T, side = 1, line = 2, cex = 2)
dev.off()

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

