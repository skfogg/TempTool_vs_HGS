###
### Two Channel Plots
###

iskulpaa <- read.csv("C:\\Users\\t24x137\\Desktop\\TempTool_2020\\IskulpaaRiverTemp_TempToolFormat.csv")

plot(iskulpaa$value[0:20000], col = "red", type = "l")
lines(coredata(twoChannel$cTemp1$svValue), type = "l")


plot(twoChannelAtm$cTemp1$svValue)

plot.zoo(twoChannel$cTemp1$svValue)
lines(as.zoo(twoChannel$cTemp2$svValue),
      col = "dodgerblue")

plot.zoo(twoChannelAtm$cTemp1$svValue)
lines(as.zoo(twoChannelAtm$cTemp2$svValue),
      col = "forestgreen")


