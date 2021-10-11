###
### Re-Run temptool with new Umatilla hyporheic size
### December 2020
### High HE Scenario

library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

umatillaBins_High <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 50, b=-1.39)
load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/littleHypoInitTemps.RData")

setParameters(firstBin = 1,
              lastBin = 18,
              odbcConnection = connect,
              initTemps = littleHypoInitTemps,
              surfaceShade = 0,
              channelSurfaceArea = 1,
              channelVolume = 0.5,
              binStats = umatillaBins_High)

# UPDATE channelout_0002 (negative)
sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_face_channelin
                         SET init_heat_face_channelin.Water = '-", umatillaBins_High$entering[1],
                         "' WHERE (ID = 'channelout_0002);"))
# UPDATE channelin_0001 (positive)
sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_face_channelin
                         SET init_heat_face_channelin.Water = '", umatillaBins_High$entering[1],
                         "' WHERE (ID = 'channelout_0001);"))

###
####
##### RUN IN ECLIPSE
###
###
##
runid <- "new umatilla aquifer size, high scenario 12.5m water storage"

assign("cTemp2", tts(odbcConnection = connect,
                     holonName = "channel_0002",
                     tableName = "temp_signal_output",
                     runID = runid,
                     xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_000", z),
                                         tableName = "temp_signal_output",
                                         runID = runid,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  } else {
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_00", z),
                                         tableName = "temp_signal_output",
                                         runID = runid,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  }
}

# CREATE LIST OF ALL RUN DATA
objectNames <- c("cTemp2", paste0("tsz", 1:18, "Temp"))

names(objectNames) <- objectNames
outList <- as.list(objectNames)

for(i in 1:length(objectNames)){
  outList[[i]] <- get(objectNames[i])
}

newUmatillaHigh<- outList

save(newUmatillaHigh, file = "C:/Users/t24x137/Desktop/TempTool_2020/newUmatillaHigh.RData")


