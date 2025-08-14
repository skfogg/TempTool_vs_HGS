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

plot.zoo(highHypo870[[2]]$svValue["2016"], ylim = c(0,25))
mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
       highHypo870[2:19],
       hcl.colors(18))
#
# plot.zoo(medHypo870[[2]]$svValue["2016"], ylim = c(0,25))
# mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
#        medHypo870[2:19],
#        hcl.colors(18))
#
# plot.zoo(littleHypo870[[2]]$svValue["2016"], ylim = c(0,25))
# mapply(function(x,c) lines(as.zoo(x$svValue["2016"]), col = c),
#        littleHypo870[2:19],
#        hcl.colors(18))

rawlongtemps <- function(x, bindf){
  july <- data.frame(temp = numeric(19),
                     tsz = 0:18,
                     rt = c(0, bindf$meanWaterAge))
  oct <- data.frame(temp = numeric(19),
                    tsz = 0:18,
                    rt = c(0, bindf$meanWaterAge))
  jan <- data.frame(temp = numeric(19),
                    tsz = 0:18,
                    rt = c(0, bindf$meanWaterAge))
  apr <- data.frame(temp = numeric(19),
                    tsz = 0:18,
                    rt = c(0, bindf$meanWaterAge))

  for(i in 1:19){
    july$temp[i] <- apply.daily(x[[i]]$svValue["2016-07-20"], mean)
    oct$temp[i] <- apply.daily(x[[i]]$svValue["2016-10-19"], mean)
    apr$temp[i] <- apply.daily(x[[i]]$svValue["2016-04-20"], mean)
    jan$temp[i] <- apply.daily(x[[i]]$svValue["2016-01-28"], mean)
  }
  return(list(jan = jan, apr = apr, jul = july, oct = oct))
}

returnlong <- function(x, bindf){
  july <- data.frame(temp = numeric(19),
                     tsz = 0:18,
                     rt = c(0, bindf$to))
  oct <- data.frame(temp = numeric(19),
                    tsz = 0:18,
                    rt = c(0, bindf$to))
  jan <- data.frame(temp = numeric(19),
                    tsz = 0:18,
                    rt = c(0, bindf$to))
  apr <- data.frame(temp = numeric(19),
                    tsz = 0:18,
                    rt = c(0, bindf$to))

  for(i in 1:19){
    july$temp[i] <- apply.daily(x[[i]]$svValue["2016-07-20"], mean)
    oct$temp[i] <- apply.daily(x[[i]]$svValue["2016-10-19"], mean)
    apr$temp[i] <- apply.daily(x[[i]]$svValue["2016-04-20"], mean)
    jan$temp[i] <- apply.daily(x[[i]]$svValue["2016-01-28"], mean)
  }

  lo_jul <- loess(temp ~ rt, july)
  lo_jan <- loess(temp ~ rt, jan)
  lo_apr <- loess(temp ~ rt, apr)
  lo_oct <- loess(temp ~ rt, oct)

  p_jan <- predict(lo_jan, newdata = seq(0, jan$rt[19], length.out = 100))
  p_apr <- predict(lo_apr, newdata = seq(0, apr$rt[19], length.out = 100))
  p_jul <- predict(lo_jul, newdata = seq(0, july$rt[19], length.out = 100))
  p_oct <- predict(lo_oct, newdata = seq(0, oct$rt[19], length.out = 100))

  return(list(jan = p_jan, apr = p_apr, jul = p_jul, oct = p_oct))
}

lowsmooth <- returnlong(lowhypo, bindf = littlehypobins)
medsmooth <- returnlong(medhypo, bindf = medhypobins)
highsmooth <- returnlong(highhypo, bindf = highhypobins)

save(highsmooth, file = "highsmooth.RData")
save(medsmooth, file = "medsmooth.RData")
save(lowsmooth, file = "lowsmooth.RData")

highrawt <- rawlongtemps(highhypo, bindf = highhypobins)
helcol <- hcl.colors(4, "Cork")

png("plots/highHypo_across_res_times.png",
    width = 800*5,
    height = 600*5,
    res = 72*5)
par(cex.axis = 1.5,
    cex.lab = 1.5,
    cex = 1.3)
plot(temp~ I(rt/86400), highrawt[[1]], type = "o",
     ylim = c(2,22),
     xlim = c(0,300),
     col = helcol[1],
     pch = 20,
     lwd = 2)
lines(temp~I(rt/86400), highrawt[[2]], type = "o",
      col = helcol[2],
      pch = 20,
      lwd = 2)
lines(temp~I(rt/86400), highrawt[[3]], type = "o",
      col = helcol[3],
      pch = 20,
      lwd = 2)
lines(temp~I(rt/86400), highrawt[[4]], type = "o",
      col = helcol[4],
      pch = 20,
      lwd = 2)
dev.off()

riveramp <- (july$temp[1] - jan$temp[1])/2
riveramp*exp(-1)

(jan$temp[19] - july$temp[19])/2

locol <- 'gold'
mcol <- 'orange'
hcol <- 'red'

newdat <- seq(0, oct$rt[19]*1, length.out = 100)
plot(newdat, lowsmooth$jan, type = "n", ylim = c(-1,24),
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Hyporheic Water Age (s)")
lines(newdat, lowsmooth$jan, col=locol, lwd=2)
lines(newdat, lowsmooth$apr, col=locol, lwd=2)
lines(newdat, lowsmooth$jul, col=locol, lwd=2)
lines(newdat, lowsmooth$oct, col=locol, lwd=2)

lines(seq(0, july$rt[19], length.out = 100), medsmooth$jan, col=mcol, lwd=2)
lines(seq(0, jan$rt[19], length.out = 100), medsmooth$apr, col=mcol, lwd=2)
lines(seq(0, apr$rt[19], length.out = 100), medsmooth$jul, col=mcol, lwd=2)
lines(seq(0, oct$rt[19], length.out = 100), medsmooth$oct, col=mcol, lwd=2)

lines(seq(0, july$rt[19], length.out = 100), highsmooth$jan, col=hcol, lwd=2)
lines(seq(0, jan$rt[19], length.out = 100), highsmooth$apr, col=hcol, lwd=2)
lines(seq(0, apr$rt[19], length.out = 100), highsmooth$jul, col=hcol, lwd=2)
lines(seq(0, oct$rt[19], length.out = 100), highsmooth$oct, col=hcol, lwd=2)



allt <- data.frame(p_jan, p_apr, p_jul, p_oct, row.names = seq(0, oct$rt[19], length.out = 100))
amps <- apply(allt, MARGIN = 1, FUN = function(x) (max(x)-min(x))/2)

allt <- cbind(allt, amps)
plot(amps)

riveramp*exp(-1)

(max(allt)-min(allt))/2

helton <- read.csv("Helton_2014_Figure_5.csv")
