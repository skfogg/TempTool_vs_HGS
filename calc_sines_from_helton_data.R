helton <- read.csv("Helton_2014_Figure_5.csv")
tester <- subset(helton, Avg_Res_Time_days == min(Avg_Res_Time_days))

#amp*sin(2*pi*f*t + phase); phase/2pif = apparent time shift in sec
amp <- tester$FitRange/2
f <- 1/(86400*365)
phase <- ((tester$Phase/365)*360)/(2*pi)

t <- seq(86400,365*86400, by = 86400)
wave <- amp*sin((2*pi*f*t))
wave2 <- amp*cos((2*pi*f*t) + (phase+0.3)) #*(pi/180)) #- (phase/(2*pi*f)))
plot(t/86400, wave, type = "l")
lines(t/86400, wave2, col = "red")
abline(v = 214.55)

f <- 1/(86400*365)
t <- seq(86400,365*86400, by = 86400)
waves <- data.frame(jday = 1:365, res_time = 0, temp = 0)

calc_wave <- function(helton_range, helton_phase, helton_mean, helton_restime){

  f <- 1/(86400*365)
  t <- seq(86400,365*86400, by = 86400)

  amp <- helton_range/2
  phase <- ((helton_phase/365)*360)/(2*pi)
  wave <- amp*cos((2*pi*f*t) + (phase)) + helton_mean

  waves <- data.frame(jday = 1:365,
                      day = ymd("2020-01-01") + seq(0, 364, by = 1),
                      res_time = helton_restime,
                      temp = wave)
  return(waves)
}



wvs <- mapply(calc_wave,
                helton_range = helton$FitRange,
                helton_phase = helton$Phase,
                helton_mean = helton$FitMean,
                helton_restime = helton$Avg_Res_Time_days,
            SIMPLIFY = F)
waves <- rbind(wvs[[1]], wvs[[2]])
for(i in 3:length(wvs)){
  waves <- rbind(waves, wvs[[i]])
}
as.factor(waves$res_time)

sort()

plot(wvs[[1]]$jday, wvs[[1]]$temp, type = "l")

mapply(function(x,c) lines(x$jday, x$temp, col = c),
       x = wvs,
       c = hcl.colors(19))

for(i in 1:nrow(helton)){
  amp <- helton$FitRange[i]/2
  phase <- ((helton$Phase[i]/365)*360)/(2*pi)
  wave <- amp*cos((2*pi*f*t) + (phase)) + helton$FitMean[i]

  waves <- data.frame(jday = 1:365,
                      day = ymd("2020-01-01") + seq(0, 364, by = 1),
                      res_time = helton$Avg_Res_Time_days,
                      temp = wave)
}

plot(wvs[[1]]$jday, wvs[[1]]$temp, type = "l")
abline(v = helton$Phase)


#### Start over ####

get_waves <- function(tester){
  amp <- tester$FitRange/2
  maxt <- tester$FitMean + amp
  mint <- tester$FitMean - amp
  maxday <- tester$Phase
  minday <- tester$Phase - (365/2)
  meanday <- (maxday - minday)/2 + minday
  meanday2 <- (maxday - minday)/2 + maxday
  meant <- tester$FitMean
  phase <- (tester$Phase - 365/4) * (pi/180)

  f <- 1/(86400*365)
  t <- seq(86400,365*86400, by = 86400)

  wave <- (amp*sin((2*pi*(1/(365*86400))*t) - phase)) + meant

  # two <- data.frame(temp = c(mint, meant, maxt, meant), time = c(minday, meanday, maxday, meanday2))

  # model <-nls(temp ~ amp*sin((2*pi*(1/365)*time) + p) + meant,
  #             data = two,
  #             start = list(p = floor(phase)))
  #

  # cosine <- data.frame(amp = amp, mean = meant, phase = phase, modelphase = coef(model))

  return(wave)
}



plot(get_waves(tester = tester))
plot(get_waves(tester = helton[1,]))
abline(v = helton$Phase[1])

allwaves <- list(get_waves(helton[1,]))
for(i in 2:nrow(helton)){
  thiswave <- get_waves(helton[i,])
  allwaves[[i]] <- thiswave
}

plot(allwaves[[1]], type = "l")
mapply(function(x, c) lines(x, col = c),
       allwaves,
       hcl.colors(19, "Blues"))

plot(rep(helton$Avg_Res_Time_days[1], times = 4), allwaves[[1]][c(18,110,201,292)],
     xlim = c(0,305),
     ylim = c(-2,22))
for(i in 2:nrow(helton)){
  points(rep(helton$Avg_Res_Time_days[i], times = 4), allwaves[[i]][c(18,110,201,292)])
}

orderedwaves <- data.frame(rt = rep(sort(helton$Avg_Res_Time_days), each = 4),
           jday = rep(c(18,110,201,292), times = 19),
           temp = NA)
for(i in 1:nrow(helton)){
  orderedwaves[orderedwaves$rt == helton$Avg_Res_Time_days[i],]$temp <- allwaves[[i]][c(18,110,201,292)]
}

plot(c(mean(subset(orderedwaves, jday == 18)[1:3,]$rt), subset(orderedwaves, jday == 18)[4:19,]$rt),
     c(mean(subset(orderedwaves, jday == 18)[1:3,]$temp), subset(orderedwaves, jday == 18)[4:19,]$temp),
     type = "o",
     ylim = c(-2,19),
     col = hcl.colors(4, "Cork")[1],
     xlim = c(0,305))
lines(c(mean(subset(orderedwaves, jday == 110)[1:3,]$rt), subset(orderedwaves, jday == 110)[4:19,]$rt),
      c(mean(subset(orderedwaves, jday == 110)[1:3,]$temp), subset(orderedwaves, jday == 110)[4:19,]$temp),
      type = "o",
      col = hcl.colors(4, "Cork")[2])
lines(c(mean(subset(orderedwaves, jday == 201)[1:3,]$rt), subset(orderedwaves, jday == 201)[4:19,]$rt),
      c(mean(subset(orderedwaves, jday == 201)[1:3,]$temp), subset(orderedwaves, jday == 201)[4:19,]$temp),
      type = "o",
      col = hcl.colors(4, "Cork")[3])
lines(c(mean(subset(orderedwaves, jday == 292)[1:3,]$rt), subset(orderedwaves, jday == 292)[4:19,]$rt),
      c(mean(subset(orderedwaves, jday == 292)[1:3,]$temp), subset(orderedwaves, jday == 292)[4:19,]$temp),
      type = "o",
      col = hcl.colors(4, "Cork")[4])







###############
###############
amp
tdays <- 1:365
freq <- 1/365
omega <- 2*pi*freq
phaserad <- tester$Phase
plot(amp*sin(omega*tdays + (tester$Phase/365)*(2*pi/3)), type = "l")
abline(v = tester$Phase)

plot(amp*sin((2*pi*(1/(365*86400))*t)),
     type = "l",
     col ="forestgreen")
abline(v = 365/4)
lines()
lines(amp*sin((2*pi*(1/(365*86400))*t) - ((91.25 + 32.05) * (pi/180))) + meant,
              col = "red")
abline(v = tester$Phase)

tester$Phase/4
meanday
###

