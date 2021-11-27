## Packages ##
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
library(stringr)
library(scales)
library(paletteer)

allruns <- "soil_1.0m"
#####################
#### Import Data ####
#####################

## Change Folder Paths to change data ##
sunnyfolderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\sunny_iskulpaa_soil_input\\"
shadyfolderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\fake_shady_iskulpaa\\"

for(i in 1:length(allruns)){
  assign(paste0(allruns[i], "_means_sunny"), readRDS(paste0(sunnyfolderpath, allruns[i], "\\", allruns[i], "_dailymeans.RData")))
  assign(paste0(allruns[i], "_structure_sunny"), readRDS(paste0(sunnyfolderpath, allruns[i], "\\", allruns[i], "_modelstructure.RData")))
}

for(i in 1:length(allruns)){
  assign(paste0(allruns[i], "_means_shady"), readRDS(paste0(shadyfolderpath, allruns[i], "\\", allruns[i], "_dailymeans.RData")))
  assign(paste0(allruns[i], "_structure_shady"), readRDS(paste0(shadyfolderpath, allruns[i], "\\", allruns[i], "_modelstructure.RData")))
}

velocity <- readRDS("C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\sensitivity\\sunny_iskulpaa_soil_input\\soil_1.0m\\soil_1.0m_velocity.RData")
RT_times <- c(86400, 86400*7, 86400*14, 86400*30, 86400*60, 86400*120, 86400*180)
meters_at_RT_times <- mean(velocity[,1,32,,"Vx"])*RT_times


#######################
##### Output Times ####
#######################
yr7Jdays <- seq(365*86400*7, by = 15*86400, length.out = 24)

yr7Jdays_fulldays <- unlist(lapply(yr7Jdays, 
                                   function(x)
                                     seq(x, by = 3600, length.out = 24)))
normalJday <- (yr7Jdays- (365*86400*7))/86400

everyother <- seq(2,24,by=2)
everyothermonth <- everyother[seq(1,12,by=2)]

monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthnames_abrv <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

daysinmonth <- c(31, 28, 31, 
                 30, 31, 30,
                 31, 31, 30, 
                 31, 30, 31)

dayofyr <- paste(monthnames_abrv[1], seq(1:daysinmonth[1]))
for(i in 2:12){
  dayofyr <- c(dayofyr, paste(monthnames_abrv[i], seq(1:daysinmonth[i])))
}
jframe <- data.frame(outputnumber = 1:24, jday = normalJday+1, dayofyear = dayofyr[normalJday+1])
seasons <- subset(jframe, jday %in% c(1,91,181,271))

## DELETE??###
## bedrock = 34.4m, hz = 36.5 ##
zIDX <- c(bedrock = 16, hz = 32)
depths <- zIDX

####################
#### Color Shiz ####
####################
timePalette <- colorRampPalette(c("purple", "thistle", "dodgerblue", "skyblue", "seagreen"))
xPalette <- colorRampPalette(c("dodgerblue","purple", "red"))
timecolpal <- "jcolors::pal12"

sunnyribboncolor <- "OrYel"
shadyribboncolor <- "Mint"


###############
#### Plotz ####
###############
plotfolderpath <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\"
times <- everyothermonth

hz_zdepths <- list(soil_1.0m = c(11:41))
models <- allruns

################################
### Ribbon Plots: All Depths ###
################################
linewidth = 1
xcutoff <- 445 #800 meter flow path

for(i in 1:length(models)){
  png(paste0(plotfolderpath, models[i],"_allseasons.png"),
      width = 600*5,
      height = 900*5,
      res = 72*5)
  layout(matrix(c(1,5, 
                  2,5,
                  3,6,
                  4,6), nrow = 4, ncol = 2, byrow = T), widths = c(5,1))
  #layout.show(6)
  par(mar = c(3.1,4.1,0.1,0.1),
      oma = c(5,3,5,0),
      cex.main = 1.8,
      cex.axis = 1.5)
  
for(s in 1:nrow(seasons)){
  plot(get(paste0(models[i], "_structure_sunny"))[1:xcutoff,2,hz_zdepths[[1]][1],"X"], 
         get(paste0(models[i], "_means_sunny"))[hz_zdepths[[i]][1],1:xcutoff,seasons[s,1]],
         main = "",
         ylab = "",
         xlab = "",
         type = "n",
         lwd = linewidth,
         ylim = c(2,22))
    
    mapply(function(z,c) lines(get(paste0(models[i], "_structure_sunny"))[1:xcutoff,2,z,"X"], 
                               get(paste0(models[i], "_means_sunny"))[z,1:xcutoff,seasons[s,1]],
                               col = c,
                               lwd = 1),
           hz_zdepths[[i]],
           hcl.colors(31, sunnyribboncolor))
    mapply(function(z,c) lines(get(paste0(models[i], "_structure_shady"))[1:xcutoff,2,z,"X"], 
                               get(paste0(models[i], "_means_shady"))[z,1:xcutoff,seasons[s,1]],
                               col = c,
                               lwd = 1),
           hz_zdepths[[i]],
           hcl.colors(31, shadyribboncolor))
    text(0,20,labels = paste(seasons[s,3]), pos = 4, cex = 2)
  }
  
  par(mar = c(0,0,3,0)+0.1)
  plot(seq(0,1,length.out=31),
       1:31,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       frame = F,
       main = "Sunny Soil",
       xlim = c(-1,1),
       ylim = c(0,32))
  mapply(function(y, c) segments(x0 = 0, y0=y, x1 = 1, y1=y,col = c, lwd = 6),
         1:31,
         hcl.colors(31, sunnyribboncolor)[seq(31,1,-1)])
  text(0, 32.1, labels = c("HZ depth (cm)"), cex = 1.5)
  mapply(function(y, ylabel) text(0, y, labels = ylabel, pos = 2, cex = 1.5),
         c(0,5,10,15,20,25,30)+1,
         seq(300,0,-50))
  
  #par(mar = c(0,0,2,0)+0.2)
  plot(seq(0,1,length.out=31),
       1:31,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       frame = F,
       main = "Shady Soil",
       xlim = c(-1,1),
       ylim = c(0,32))
  mapply(function(y, c) segments(x0 = 0, y0=y, x1 = 1, y1=y,col = c, lwd = 6),
         1:31,
         hcl.colors(31, shadyribboncolor)[seq(31,1,-1)])
  text(0, 32.1, labels = c("HZ depth (cm)"), cex = 1.5)
  mapply(function(y, ylabel) text(0, y, labels = ylabel, pos = 2, cex = 1.5),
         c(0,5,10,15,20,25,30)+1,
         seq(300,0,-50))
  
  mtext(expression(paste("Temperature (", degree, "C)")), side = 2, outer = T, line = 1, cex = 1.5)
  mtext("Flow Path Length (m)", side = 1, outer = T, line = 1, cex = 1.5)
  mtext(paste0("Overlying Sediment Depth: ", str_extract(models[i], "[0-9].[0-9]"), " m"), side = 3, outer = T, line = 2, cex = 2)
  #axis(1, at = meters_at_RT_times, labels = RT_times/86400, outer = T, line = 3)
  #mtext("Residence Time (Days)", side = 1, outer = T, line = 3)
  dev.off()
}

##################################################
### Ribbon Plots: Shallow, Deep, Middle Depths ###
##################################################
hz_zdepths$soil_1.0m <-c(11,median(hz_zdepths$soil_1.0m),41)
linewidth = c(1,2,1)
linetype = c(1,1,1)

for(i in 1:length(models)){
  png(paste0(plotfolderpath, models[i],"_allseasons_sdm.png"),
      width = 600*5,
      height = 900*5,
      res = 72*5)
  layout(matrix(c(1,1, 
                  2,2,
                  3,3,
                  4,4,
                  5,6), nrow = 5, ncol = 2, byrow = T), widths = c(1,1))
  
  for(s in 1:nrow(seasons)){
    plot(get(paste0(models[i], "_structure_sunny"))[,2,hz_zdepths[[1]][1],"X"], 
         get(paste0(models[i], "_means_sunny"))[hz_zdepths[[i]][1],,seasons[s,1]],
         main = paste(models[i], seasons[s,3]),
         ylab = expression(paste("Temperature (", degree, "C)")),
         xlab = "Flow Path Length (m)",
         type = "n",
         lwd = linewidth,
         ylim = c(2,22))
    
    mapply(function(z,c,ltype,lwidth) lines(get(paste0(models[i], "_structure_sunny"))[,2,z,"X"], 
                               get(paste0(models[i], "_means_sunny"))[z,,seasons[s,1]],
                               col = c,
                               lwd = lwidth,
                               lty = ltype),
           hz_zdepths[[i]],
           hcl.colors(3, sunnyribboncolor),
           linetype,
           linewidth)
    mapply(function(z,c,ltype,lwidth) lines(get(paste0(models[i], "_structure_shady"))[,2,z,"X"], 
                               get(paste0(models[i], "_means_shady"))[z,,seasons[s,1]],
                               col = c,
                               lwd = lwidth,
                               lty = ltype),
           hz_zdepths[[i]],
           hcl.colors(4, shadyribboncolor)[1:3],
           linetype,
           linewidth)
  }
  
  #par(mar = c(0,0,2,0)+0.2)
  plot(seq(0,1,length.out=3),
       1:3,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       frame = F,
       main = "Sunny Soil",
       ylim =c(1,3.5))
  mapply(function(y, c, ltype, lwidth) segments(x0 = 0.3, y0=y, x1 = 1, y1=y,col = c, lwd = lwidth, lty = ltype),
         1:3,
         hcl.colors(3, sunnyribboncolor)[seq(3,1,-1)],
         linetype,
         linewidth)
  text(0.13, 3.4, labels = c("HZ Depth"))
  mapply(function(y, ylabel) text(0, y, labels = ylabel, pos = 4),
         y = 1:3,
         ylabel = c("bottom (3 m)", "middle (1.5 m)", "top (0 m)"))
  
  plot(seq(0,1,length.out=3),
       1:3,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       frame = F,
       main = "Shady Soil",
       ylim =c(1,3.5))
  mapply(function(y, c, ltype, lwidth) segments(x0 = 0.3, y0=y, x1 = 1, y1=y,col = c, lwd = lwidth, lty = ltype),
         1:3,
         hcl.colors(4, shadyribboncolor)[seq(3,1,-1)],
         linetype,
         linewidth)
  text(0.13, 3.4, labels = c("HZ Depth"))
  mapply(function(y, ylabel) text(0, y, labels = ylabel, pos = 4),
         y = 1:3,
         ylabel = c("bottom (3 m)", "middle (1.5 m)", "top (0 m)"))
  
  dev.off()
}


#################################
### Ribbon Plot for All Times ###
#################################
t=1
hz_zdepths <- list(soil_1.0m = c(11:41))
xcutoff <- 445 #800 meter flow path


for(i in 1:length(models)){
for(t in 1:24){
  png(paste0(plotfolderpath, models[i], "_ribbon\\", models[i], "_", t, ".png"),
      width = 800*5,
      height = 500*5,
      res = 72*5)
  par(mar = c(8,4,4,2) +0.1)
  plot(get(paste0(models[i], "_structure_sunny"))[1:xcutoff,2,hz_zdepths[[1]][1],"X"], 
       get(paste0(models[i], "_means_sunny"))[hz_zdepths[[i]][1],1:xcutoff,t],
       main = paste(models[i], ":", jframe[t,3]),
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       type = "n",
       lwd = linewidth,
       ylim = c(2,22))
  
  mapply(function(z,c) lines(get(paste0(models[i], "_structure_sunny"))[1:xcutoff,2,z,"X"], 
                             get(paste0(models[i], "_means_sunny"))[z,1:xcutoff,t],
                             col = c,
                             lwd = 1),
         hz_zdepths[[i]],
         hcl.colors(31, sunnyribboncolor))
  mapply(function(z,c) lines(get(paste0(models[i], "_structure_shady"))[1:xcutoff,2,z,"X"], 
                             get(paste0(models[i], "_means_shady"))[z,1:xcutoff,t],
                             col = c,
                             lwd = 1),
         hz_zdepths[[i]],
         hcl.colors(31, shadyribboncolor))
  
  axis(1, at = meters_at_RT_times, labels = RT_times/86400,
       line = 5)
  mtext("Mean Water Age (Days)", side = 1, line = 7)
  
  dev.off()
}
}

###############################
### Temperature Across Time ###
###############################
xvalues <- c(m1.0 = 11, m4.0 = 41, m10.9 = 50, m60.9 = 75, m110.9 = 100, m510.9 = 300)

sunnyxcolors <- "BrwnYl"
shadyxcolors <- "Teal"

whatx <- 6
plot(1:24, 
     get(paste0(models[i], "_means_sunny"))[hz_zdepths[[i]][26],xvalues[whatx],],
     xaxt = "n",
     ylab = expression(paste("Temperature (", degree, "C)")),
     xlab = "Date",
     main = paste0(models[i], ": ", names(xvalues[whatx])),
     type = "l",
     col = "white",
     lwd = 2,
     ylim = c(0,20))
mapply(function(x, c) lines(1:24, 
                         get(paste0(models[i], "_means_sunny"))[hz_zdepths[[i]][26],x,],
                         col = c,
                         lwd =2),
       xvalues,
       hcl.colors(6, sunnyxcolors))

mapply(function(x, c) lines(1:24, 
                            get(paste0(models[i], "_means_shady"))[hz_zdepths[[i]][26],x,],
                            col = c,
                            lwd = 2),
       xvalues,
       hcl.colors(6, shadyxcolors))

axis(1, at=everyother, labels = jframe[everyother,3])

