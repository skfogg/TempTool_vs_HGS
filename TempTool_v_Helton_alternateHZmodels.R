####
#### Per Geoff's Request:
#### To Model Tests with High HE:
#### (1) 435 Sediment Specific heat in HZ (0.5 basaltic SpHeat)
#### (2) HZ porosity at 30%
####
####

library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(devtools)
#install_github("skfogg/temptoolr")
library(temptool)

helton <- read.csv("Helton_2014_Figure_5.csv")

plot(MathMean ~ Avg_Res_Time_days, helton)

plot(FitRange ~ Avg_Res_Time_days, helton)

load("temptool_output/highHypo870.RData")
highhypobins <- TSZStats(18, 60, 182*86400, 35.94, factor = 2, alpha = 1.39)


high_dailymeans <- lapply(highHypo870, apply.daily, mean)
high_435_dailymeans <- lapply(highHypo435, apply.daily, mean)
high_30_dailymeans <- lapply(highHypo30porosity, apply.daily, mean)

## Calc phase function
findphase <- function(x){
  d <- index(x["2016"][coredata(x["2016"]) == max(x["2016"])])
  return(floor(as.numeric(julian(d, origin = mdy("01-01-2016")))))
}

high_phases <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, phaseDay = numeric(18))
high_phases_435 <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, phaseDay = numeric(18))
high_phases_30 <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, phaseDay = numeric(18))

for(i in 2:19){
  high_phases$phaseDay[i-1] <- findphase(high_dailymeans[[i-1]]$svValue)
  high_phases_435$phaseDay[i-1] <- findphase(high_435_dailymeans[[i-1]]$svValue)
  high_phases_30$phaseDay[i-1] <- findphase(high_30_dailymeans[[i-1]]$svValue)
}

high_ranges <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, range = numeric(18))
high_435_ranges <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, range = numeric(18))
high_30_ranges <- data.frame(WA = highhypobins$meanWaterAge, RT = highhypobins$meanResTime, range = numeric(18))
for(i in 2:19){
  high_ranges$range[i-1] <- round(max(high_dailymeans[[i-1]]$svValue["2016"]) - min(high_dailymeans[[i-1]]$svValue["2016"]), 2)
  high_435_ranges$range[i-1] <- round(max(high_435_dailymeans[[i-1]]$svValue["2016"]) - min(high_dailymeans[[i-1]]$svValue["2016"]), 2)
  high_30_ranges$range[i-1] <- round(max(high_30_dailymeans[[i-1]]$svValue["2016"]) - min(high_dailymeans[[i-1]]$svValue["2016"]), 2)

  }


### Local Phases Plot ####
png("plots/localphase_v_RT_2.png", width = 600*5, height = 600*5, res = 72*5)
plot(I(phaseDay) ~ I(RT/86400), high_phases,
     ylim = c(200, 325),
     xlim = c(0, 300),
     ylab = "Local Phase (Day of Year)",
     xlab = "Residence Time (Days)",
     bg = "orangered3",
     pch = 21,
     main = "Local Phase ~ Residence Time")
points(I(phaseDay) ~ I(RT/86400), high_phases_435, pch = 21, bg = "limegreen")
points(I(phaseDay) ~ I(RT/86400), high_phases_30, pch = 21, bg = "gold")
points(Phase ~ Avg_Res_Time_days, helton, pch = 21, bg = "lightblue", col = "dodgerblue")
# Helton's fit line
lines((205*exp(1)^(0.0015*seq(0.01,300))), col = "dodgerblue")
legend("topleft", c("High HE",
                    "Sediment Specific Heat = 435",
                    "Porosity = 30%",
                    "Helton"),
       pch = c(21,21,21,21), lty = c(NA, NA,NA,1), col = c(1,1, 1,"dodgerblue"),
       pt.bg = c("orangered3", "limegreen", "gold", "lightblue"))
dev.off()

### Range Plot ####
png("plots/range_v_RT_2.png", width = 600*5, height = 600*5, res = 72*5)
plot(range ~ I(RT/86400),
     high_ranges,
     ylim = c(0, 25),
     xlim = c(0, 300),
     ylab = "Temperature Range (C)",
     xlab = "Residence Time (Days)",
     bg = "orangered3",
     pch = 21,
     main = "Range ~ Residence Time")
points(range~I(RT/86400), high_435_ranges, pch = 21, bg = "limegreen")
points(range~I(RT/86400), high_30_ranges, pch = 21, bg = "gold")
points(FitRange ~ Avg_Res_Time_days, helton,pch = 21, col = "dodgerblue", bg = "lightblue")
lines(16.2*exp(1)^(-0.0065*seq(1,300)), col = "dodgerblue")
legend("topleft", c("High HE",
                    "Sediment Specific Heat = 435",
                    "Porosity = 30%",
                    "Helton"),
       pch = c(21,21,21,21), lty = c(NA, NA,NA,1), col = c(1,1, 1,"dodgerblue"),
       pt.bg = c("orangered3", "limegreen", "gold", "lightblue"))
dev.off()

