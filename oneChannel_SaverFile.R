###
### GET MODEL RUN FROM 12/6/2020
### One Outflow Channel Cell,
### NO atm cell on top
### Iskulpaa river temperature input straight the hypo_0001

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

runDescription <- "One Outflow Channel, NO ATM, Iskulpaa Input"
#
# assign("cTemp1", tts(odbcConnection = connect,
#                      holonName = "channel_0001",
#                      tableName = "temp_signal_output",
#                      runID = runDescription,
#                      xtsIndex = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

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
objectNames <- c("cTemp2", paste0("tsz", 1:18, "Temp"))
# assign(paste0("output_run", runID), lapply(objectNames, get))

names(objectNames) <- objectNames
outList <- as.list(objectNames)

for(i in 1:length(objectNames)){
  outList[[i]] <- get(objectNames[i])
}
oneChannel <- outList

save(oneChannel, file = "C:/Users/t24x137/Desktop/TempTool_2020/oneOutputChannel.RData")
