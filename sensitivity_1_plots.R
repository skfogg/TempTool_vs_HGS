#install_github("FluvialLandscapeLab/HGSReader")
library(HGSReader)
library(raster)
library(scatterplot3d)
library(RColorBrewer)
source('C:/Users/Katie Fogg/Desktop/HGSwork/incrementDim.R')
library(animation)
library(sp)
library(gstat)
library(raster)
library(dismo)
library(deldir)
library(rgeos)

### Output Times
yr7Jdays <- seq(365*86400*7, by = 15*86400, length.out = 24)

yr7Jdays_fulldays <- unlist(lapply(yr7Jdays,
                                   function(x)
                                     seq(x, by = 3600, length.out = 23)))
allHGSTimes <- data.frame(idx = 1:length(yr7Jdays_fulldays), time = yr7Jdays_fulldays)
timeidx <- subset(allTimes, time %in% yr7Jdays)$idx
monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

timePalette <- colorRampPalette(c("purple", "skyblue", "forestgreen"))

### File Loading
directory <- "C:/Users/Katie Fogg/Desktop/HGSwork/April2020/Sensitivity_1/"
modelruns <- c("soil_2.0m", "soil_1.0m", "soil_0.5m", "soil_0.3m", "soil_0.1m")
controlruns <- c("control_2.0m_riveronly", "control_2.0m_soilonly", "control_0.1m_riveronly", "control_0.1m_soilonly")

#### DO NOT RUN AFTER FIRST TIME ####
f <- lapply(modelruns,
            function(x)
            HGSFile(paste0(directory, x, "/temperatureTestero.pm.dat")))
fc <- lapply(controlruns,
             function(x)
               HGSFile(paste0(directory, x, "/temperatureTestero.pm.dat")))


# Takes over an hour
g <- lapply(f,
            function(x)
            HGSGetData(x,
                       variables = c("X", "Y", "Z", "temp"),
                       blockNumbers = timeidx))
names(g) <- modelruns

gc <- lapply(fc,
             function(x)
               HGSGetData(x,
                          variables = c("temp"),
                          blockNumbers = timeidx))
names(gc) <- controlruns
saveRDS(gc, file = paste0(directory, "gc_controlruns.RData"))

sat <- lapply(f,
              function(x)
                HGSGetData(x,
                           variables = c("Sat"),
                           blockNumbers = 1))
names(sat) <- modelruns

### Takes too long to run
g2 <- lapply(f,
            function(x)
              HGSGetData(x,
                         variables = c("temp")
                         ))

#### ALTERNATIVELY, load .RData ####
load(paste0(directory, "sensitivity_1_2_history.RData"))
gc <- readRDS(paste0(directory, "gc_controlruns.RData"))


#### Change Plotting Variables Here ####
zIDX <- c(bedrock = 16, hz = 32, bedrock1 = 1, bedrock10 = 10)
xIDX <- c(m0 = 1,
          m0.5 = 6,
          m1.0 = 11,
          m1.5 = 16,
          m2.0 = 22,
          m4.0 = 41,
          m10.9 = 50,
          m60.9 = 75,
          m110.9 = 100,
          m160.9 = 125,
          m210.9 = 150,
          m310.9 = 200,
          m410.9 = 250,
          m510.9 = 300,
          m610.9 = 350,
          m710.9 = 400)
everyother <- seq(2,24,by=2)

modelstructures <- lapply(modelruns,
                          function(x)
                            g[[x]][,,,1,c("X","Y","Z")])
names(modelstructures) <- modelruns
modellist <- gc
model2plot <- "control_0.1m_riveronly"
times2plot <- everyother
structure2plot <- subset(modelruns, str_extract(modelruns, "...m") == str_extract(model2plot, "...m"))

#### Saturation as a function of z ####
saturations <- function(x){
  plot(sat[[x]][10,2,45:as.numeric(names(tail(modellist[[x]][1,2,,1,"Z"],1))),1,"Sat"],
       modellist[[x]][10,2,45:as.numeric(names(tail(modellist[[x]][1,2,,1,"Z"],1))),1,"Z"],
       type = "o",
       lwd = 1,
       xlim = c(0,1),
       ylim = c(37.8,40),
       ylab = "Z",
       xlab = "Saturation",
       main = x)
  abline(h=38, lty = 2)
}

png(paste0(directory, "saturations.png"),
    width = 800*5,
    height = 500*5,
    res = 72*5)
par(mfrow = c(1,5),
    mar = c(2,2,4,1),
    oma = c(3,3,0,0),
    cex.main = 2,
    cex.lab = 1.5,
    cex.axis = 1.3)
lapply(modelruns,
       saturations
)
mtext("Saturation", side = 1, line = 1, outer = T, cex = 1.3)
mtext("Z", side = 2, line = 1, outer = T, cex = 1.3)
dev.off()



#### Temperature as a Function of X- flow path length ####
png(paste0(directory, model2plot, "_xplot.png"),
    width = 800*5,
    height = 500*5,
    res = 72*5)
par(cex.main = 2,
    cex.lab = 1.5,
    cex.axis = 1.3)
plot(1:10,
     1:10,
     type = "n",
     ylim = c(0,22),
     xlim = c(0,1000),
     ylab = "Temperature",
     xlab = "Flow Path Length (m)",
     main = model2plot,
     lwd = 2)
mapply(function(model, structure, z, t, c)
  lines(modelstructures[[structure]][,2,z,"X"],
        modellist[[model]][,2,z,t,"temp"],
        col = c,
        lwd = 2),
  model2plot,
  structure2plot,
  zIDX[c("hz")],
  times2plot,
  timePalette(length(times2plot)))
legend("topright",
       monthnames,
       col = timePalette(length(times2plot)),
       lwd = 2,
       ncol = 6)
dev.off()




#### Temperature across X- Animation ####
tempxplot <- function(model, structure, z, t, c, c2){
  plot(modelstructures[[structure]][,2,z,"X"],
       modellist[[model]][,2,z,t,"temp"],
         col = c,
         lwd = 2,
         #main = monthnames,
         ylim = c(1, 24),
         type = "l")
  lines(modelstructures[[structure]][,2,z,"X"],
        g[["soil_0.1m"]][,2,z,t,"temp"],
        col = c2,
        lwd = 2)
}


saveHTML({
for (i in 1:24){
  plot(modelstructures[[structure2plot]][,2,zIDX["hz"],"X"],
       modellist[[model2plot]][,2,zIDX["hz"],i,"temp"],
       col = "gray",
       lwd = 2,
       main = rep(monthnames, each = 2)[i],
       ylim = c(1, 24),
       type = "l",
       ylab = "Temperature",
       xlab = "X")
  lines(modelstructures[[structure2plot]][,2,zIDX["hz"],"X"],
        g[["soil_1.0m"]][,2,zIDX["hz"],i,"temp"],
        col = "dodgerblue",
        lwd = 2)
}
},
img.name = model2plot,
htmlfile = "soil1.0_tempx_controlcompare.html",
interval = 0.24,
ani.width = 800,
ani.height = 400,
ani.res = 72*5
)



#### Temperature across Time (annual values only) ####
xvalues <- xIDX
depths <- zIDX

plotList <- lapply(depths,
                   function(d)
                     lapply(xvalues,
                            function(x) data.frame(times = as.numeric(names(modellist[[model2plot]][x,2,d,,"temp"])),
                                                   temps = as.numeric(modellist[[model2plot]][x,2,d,,"temp"]))
                   ))

xPalette <- colorRampPalette(brewer.pal(9,"Blues")[-c(1:2)])
whatdataisthis <- model2plot

png(paste0(directory, model2plot, "_tplot.png"),
    width = 800*5,
    height = 500*5,
    res = 72*5)
par(cex.main = 2,
    cex.lab = 1.5,
    cex.axis = 1.3)
plot(temps~I(times-times[1]),
     data = plotList$hz$m0.5,
     type = "n",
     ylab = "Temperature",
     xlab = "Time (Julian Day)",
     ylim = c(2,22),
     main = whatdataisthis,
     xaxt = "n"
     )
mapply(function(z, c) lines(temps~I(times-times[1]),
                            data = z,
                            col = c,
                            lwd = 3),
       plotList$hz[c("m0.5", "m4.0", "m10.9", "m60.9", "m510.9")],
       xPalette(5)
)
legend("topleft", bty = "n",
       c("0.5", "4.0", "10.9", "60.9", "510.9"),
       col = xPalette(5),
       lwd = 3,
       cex = 1.2)
axis(1,
     at = with(plotList$hz$m0.5, times-times[1])[everyother-1],
     labels = with(plotList$hz$m0.5, (times-times[1])/86400)[everyother-1])
dev.off()





#### Temperature as a Function of Z- depth ####
png(paste0(directory, model2plot, "_zplot.png"),
    width = 700*5,
    height = 900*5,
    res = 72*5)
par(cex.main = 2,
    cex.lab = 1.5,
    cex.axis = 1.3)
plot(1:10,
     1:10,
     type = "n",
     ylim = c(10,40),
     xlim = c(0,24),
     ylab = "Depth",
     xlab = "Temperature",
     main = model2plot,
     lwd = 2)
abline(h = modelstructures[[structure2plot]][xIDX["m0.5"],2,17,"Z"], lty = 1)
text(23, modelstructures[[structure2plot]][xIDX["m0.5"],2,17,"Z"], labels = "Bedrock", pos = 1)
abline(h = modelstructures[[structure2plot]][xIDX["m0.5"],2,47,"Z"], lty = 5)
points(23, modelstructures[[structure2plot]][xIDX["m0.5"],2,47,"Z"]+0.17, pch = 6)
mapply(function(model, structure, x, t, c)
  lines(modellist[[model]][x,2,,t,"temp"],
        modelstructures[[structure]][x,2,,"Z"],
        col = c,
        lwd = 2),

  model2plot,
  structure2plot,
  xIDX[c("m0.5")],
  times2plot,
  timePalette(length(times2plot)))
legend("bottomright",
       monthnames,
       col = timePalette(length(times2plot)),
       lwd = 2,
       ncol = 1)
dev.off()




#### Raster Pre-Processing ####
## times are 't'
spardf <- as.data.frame(modellist[[model2plot]][,2,,1,c("X", "Z", "temp")])

sparlist <- lapply(t, function(x)
  as.data.frame(modellist[[model2plot]][,2,,x,c("X","Z", "temp")])[,-1])

for(i in 1:24){
  class(sparlist[[i]]) <- "data.frame"
  sp::coordinates(sparlist[[i]]) <- c("X", "Z")
}

# Empty raster
allblank <- raster(ncols = 2000, nrows = 500, xmn = 0, xmx = 1000, ymn = 0, ymx = tail(modellist[[model2plot]][1,2,,1,"Z"], 1))
allblank
# Nearest neighbor interpolation
glist <- as.list(1:24)
for(i in 1:24){
  glist[[i]] <- gstat(formula = temp~1, locations = sparlist[[i]], nmax = 5, set=list(idp=0))
}

nnlist <- as.list(1:24)
for(i in 1:24){
  nnlist[[i]] <- interpolate(allblank, glist[[i]])
}

nnmsklist <- as.list(1:24)
alluvium_5mbedrock_polygon <- readWKT(paste0("POLYGON((0 30, 1000 20, 1000 ", tail(modellist[[model2plot]][496,2,,1,"Z"],1), ", 0 ", tail(modellist[[model2plot]][1,2,,1,"Z"],1),", 0 30))"))
for(i in 1:24){
  nnmsklist[[i]] <- mask(nnlist[[i]], alluvium_5mbedrock_polygon)
}

#saveRDS(nnmsklist, file = paste0(directory, "nnmsklist_", model2plot, ".RData"))

#### Temperature Raster Animation ####
readRDS(file = paste0(directory, "nnmsklist_", model2plot, ".RData"))

image(nnmsklist[[20]],
      ylim = c(20,40),
      #col = tempPalette2(50),
      col = hcl.colors(50, "YlOrRd", rev = T),
      zlim = c(min(modellist[[model2plot]][,2,,,"temp"]),
               max(modellist[[model2plot]][,2,,,"temp"])))
abline(a = 38,
       b = -(1/100),
       col = "black",
       lty = 3)
abline(a = 35,
       b = -(1/100),
       col = "black",
       lty = 3)

library(animation)

rasterHeatPlot <- function(x){
  layout(matrix(1:2,ncol=2), width = c(2,1), height = c(1,1))
  image(nnmsklist[[x]],
        ylim = c(20,40),
        col = tempPalette2(50),
        #col = hcl.colors(50, "YlOrRd", rev = TRUE),
        zlim = c(min(modellist[[model2plot]][,2,,,"temp"]),
                 max(modellist[[model2plot]][,2,,,"temp"])),
        main = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), each = 2)[x])
  abline(a = 38,
         b = -(1/100),
         col = "black",
         lty = 3)
  abline(a = 35,
         b = -(1/100),
         col = "black",
         lty = 3)
  legend_image <- as.raster(matrix(hcl.colors(12, "YlOrRd", rev = F), ncol=1))
  plot(c(0,2), c(min(modellist[[model2plot]][,2,,,"temp"]),
                 max(modellist[[model2plot]][,2,,,"temp"])),
       type = 'n', axes = F,xlab = '', ylab = '', main = 'temperature')
  text(x = 1.5,
       y = seq(min(modellist[[model2plot]][,2,,,"temp"]),
                      max(modellist[[model2plot]][,2,,,"temp"]),
                      l = 5),
       labels = round(seq(min(modellist[[model2plot]][,2,,,"temp"]),
                    max(modellist[[model2plot]][,2,,,"temp"]),
                    l = 5),1))
  rasterImage(legend_image, 0, min(modellist[[model2plot]][,2,,,"temp"]), 1, max(modellist[[model2plot]][,2,,,"temp"]))
}


saveHTML({
  lapply(t, rasterHeatPlot)
},
img.name = model2plot,
htmlfile = "soil2.0_rasterAnimation.html",
interval = 0.5,
ani.width = 800,
ani.height = 400,
ani.res = 72*5
)

# saveGIF({
#   lapply(t, rasterHeatPlot)
# },
# movie.name = model2plot,
# img.name = "soil2.0m_gifanimation.gif",
# interval = 0.5,
# ani.width = 800,
# ani.height = 400
# )


#### Temperature color blobs, across x- and z- ####
tempPalette <- colorRamp(c("aliceblue", "dodgerblue", "red"))
tempPalette2 <- colorRampPalette(c("aliceblue", "dodgerblue", "red"))


hztemps <- modellist[[model2plot]][,,,,"temp"]
tround <- round(hztemps, 1)
tNorm <- (tround - min(tround))/(max(tround) - min(tround))
tempcols <- rgb(tempPalette(tNorm), max = 255)

temp <- incrementDim(modellist[[model2plot]], dimIDX = 5, newVals = tempcols, incrementName = "color")

par(bg = "black",
    mar = c(1,1,1,1),
    col.main = "white")

plotBlack <- function(x){
  plot(temp[,2,,x,"X"],
       temp[,2,,x,"Z"],
       col = temp[,2,,x,"color"],
       pch = ".",
       cex = 2.8,
       main = x,
       ylab = "Z",
       xlab = "X"
       #ylim = c(34.9,40.0),
       #xlim = c(0, 150)
  )
  abline(a = 38,
         b = -(1/100),
         col = "yellow",
         lty = 2)
}

t <- 1:24
#lapply(t, plotBlack)

#### Animate Temperature Blobs ####
saveHTML({
  par(bg = "black",
      mar = c(1,1,2,1),
      col.main = "white")
  lapply(t, plotBlack)
},
img.name = model2plot,
htmlfile = "soil2.0ani_test1.html",
interval = 0.24,
ani.width = 800,
ani.height = 400,
ani.res = 72*5
)


min(modellist[[model2plot]][,,,1,"Z"])
max(modellist[[model2plot]][,,,1,"Z"])

min(modellist[[model2plot]][,,,1,"X"])
max(modellist[[model2plot]][,,,1,"X"])

as.data.frame(matrix(nrow =  40, ncol = 1000))


image(x = 1:496,
      y = 1:67,
      z = modellist[[model2plot]][,2,,1,"temp"])



#### CONTROL TESTS ####
spup <- HGSFile(paste0(directory, "soil_2.0m_2/temperatureTestero.pm.dat"))
spinup <- HGSGetData(spup, variables = c("X", "Y", "Z", "temp"), blockNumbers = timeidx)

spround <- round(spinup[,,,,"temp"], 1)
spNorm <- (spround - min(spround))/(max(spround) - min(spround))
spCols <- rgb(tempPalette(spNorm), max = 255)


sp <- incrementDim(spinup, dimIDX = 5, newVals = spCols, incrementName = "color")
plot(sp[,2,,1,"X"],
     sp[,2,,1,"Z"],
     col = sp[,2,,1,"color"],
     pch = ".",
     cex=2.5,
     main = "spinup"
     #ylim = c(34.9,40.0),
     #xlim = c(0, 150)
)
zvalue <- c(1:16)
tp <- 18
plot(sp[,2,1,tp,"X"],
     sp[,2,1,tp,"temp"],
     type = "l",
     ylim = c(4, 22))
lapply(zvalue,
       function(x)
       lines(sp[,2,x,tp,"X"],
             sp[,2,x,tp,"temp"]))
lapply(c(17:47),
       function(x)
         lines(sp[,2,x,tp,"X"],
               sp[,2,x,tp,"temp"],
               col = "blue"))


#### Saturation color blobs
satPalette <- colorRamp(c("linen", "blue"))


