###
### GET MODEL RUN FROM 12/5/2020
### Two Channel Cells,
### Each with individual atm cells on top
### Iskulpaa river temperature input

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

runDescription <- "Two Channel, Atm, Iskulpaa Input"

assign("cTemp1", tts(odbcConnection = connect,
                    holonName = "channel_0001",
                    tableName = "temp_signal_output",
                    runID = runDescription,
                    xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

assign("cTemp2", tts(odbcConnection = connect,
                     holonName = "channel_0002",
                     tableName = "temp_signal_output",
                     runID = runDescription,
                     xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))


for(z in 1:18){
  if(z < 10){
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_000", z),
                                         tableName = "temp_signal_output",
                                         runID = runDescription,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  } else {
    assign(paste0("tsz", z, "Temp"), tts(odbcConnection = connect,
                                         holonName = paste0("hyporheic_00", z),
                                         tableName = "temp_signal_output",
                                         runID = runDescription,
                                         xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))
  }
}

# CREATE LIST OF ALL RUN DATA
objectNames <- c("cTemp1", "cTemp2", paste0("tsz", 1:18, "Temp"))
# assign(paste0("output_run", runID), lapply(objectNames, get))

names(objectNames) <- objectNames
outList <- as.list(objectNames)

for(i in 1:length(objectNames)){
  outList[[i]] <- get(objectNames[i])
}
twoChannelAtm <- outList

save(twoChannelAtm, file = "C:/Users/t24x137/Desktop/TempTool_2020/twoChannelAtm.RData")



