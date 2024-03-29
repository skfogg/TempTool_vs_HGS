library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

umatillaBins_2third <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 16.67, b=-1.39)
initTempValues <- readRDS("C:\\Users\\t24x137\\Desktop\\TempTool_2020\\umatilla_initTempValues.RData")

firstbin <- 1
lastbin <- 18
whichaquifer <- umatillaBins_2third
# INITIALIZE HYPORHEIC SUB-ZONE TEMPERATURES
for (z in firstbin:lastbin){
  if (z < 10){
    sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET Temp ='", initTempValues[z+1], "' WHERE `ID`='hyporheic_000", z, "';"))
  } else {
    sqlQuery(connect, paste0("UPDATE `temptoolfour`.`init_heat_cell_hyporheic` SET `Temp`='", initTempValues[z+1], "' WHERE `ID`='hyporheic_00", z, "';"))
  }
}

# INITIALIZE CHANNEL TEMPERATURE
sqlQuery(connect, paste0("UPDATE `temptoolfour`.`default_celltype` SET `DefaultVal` = '", initTempValues[1], "' WHERE default_celltype.CellType = 'channel' AND default_celltype.StateVal = 'Temp';"))

# UPDATE HYPORHEIC VOLUME AKA "AQUIFER VOLUME"
aquiferVolume <- sum(whichaquifer[firstBin:lastBin,]$aquiferStorage)
sqlQuery(connect, paste0("UPDATE temptoolfour.default_celltype SET default_celltype.defaultval = '", aquiferVolume, "' WHERE default_celltype.celltype ='hyporheic' And default_celltype.stateval ='Volume';"))

# UPDATE HYPORHEIC SUBZONE VOLUMES
for (z in firstBin:lastBin){
  if (z < 10){
    sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET HypoVolume='", whichaquifer$aquiferStorage[z], "' WHERE ID = 'hyporheic_000", z, "';"))
  } else {
    sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_cell_hyporheic SET HypoVolume='", whichaquifer$aquiferStorage[z], "' WHERE ID = 'hyporheic_00", z, "';"))
  }
}

# UPDATE HYPORHEIC INFLOW
sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_edge_gwflow SET InFlow ='", whichaquifer$entering[1], "' WHERE ID = 'bedto_0001';"))

# UPDATE INTERZONE INFLOWS
for (z in (firstBin+1):lastBin){
  if (z < 10){
    sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_interzone SET InFlow ='", whichaquifer$entering[z], "' WHERE ID = 'bedto_000", z, "';"))
  } else {
    sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_interzone SET InFlow ='", whichaquifer$entering[z], "' WHERE ID = 'bedto_00", z, "';"))
  }
}

# UPDATE RETURN FLOWS
for (z in firstBin:lastBin){
  if (z < 10){
    sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_discharge SET OutFlow = '", whichaquifer$returning[z], "' WHERE ID = 'bedfrom_000", z, "';"))
  } else {
    sqlQuery(connect, paste0("UPDATE temptoolfour.init_heat_edge_gwflow_discharge SET OutFlow = '", whichaquifer$returning[z], "' WHERE ID ='bedfrom_00", z, "';"))
  }
}

# UPDATE FLOW BOUNDARY CONDITIONS
sqlQuery(connect, paste0("UPDATE `temptoolfour`.`init_heat_face_channelin` SET `Water` = '-", whichaquifer$entering[1], "' WHERE (`ID` = 'channelout_0002');"))
sqlQuery(connect, paste0("UPDATE `temptoolfour`.`init_heat_face_channelin` SET `Water` = '", whichaquifer$entering[1], "' WHERE (`ID` = 'channelin_0001');"))

###
###
### Run in Eclipse
###
###


runid <- "umatilla 2/3rds: i.e. moderate scenario"

assign("cTemp2", tts(odbcConnection = connect,
                     holonName = "channel_0002",
                     tableName = "temp_signal_output",
                     runID = runid,
                     xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600),
                     select = "svValue"))

for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_000", z),
                                         tableName = "temp_signal_output",
                                         runID = runid,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600),
                                         select = "svValue"))
  } else {
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_00", z),
                                         tableName = "temp_signal_output",
                                         runID = runid,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600),
                                         select = "svValue"))
  }
}

# CREATE LIST OF ALL RUN DATA
objectNames <- c("cTemp2", paste0("tsz", 1:18, "Temp"))
names(objectNames) <- objectNames
outList <- as.list(objectNames)
for(i in 1:length(objectNames)){
  outList[[i]] <- get(objectNames[i])
}

umatilla2thirds <- outList

save(umatilla2thirds, file = "C:/Users/t24x137/Desktop/TempTool_2020/umatilla2Thirds.RData")









