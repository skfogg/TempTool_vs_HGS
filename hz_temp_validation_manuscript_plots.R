heltonjan <- subset(orderedwaves, jday == 18)
heltonapr <- subset(orderedwaves, jday == 110)
heltonjul <- subset(orderedwaves, jday == 201)
heltonoct <- subset(orderedwaves, jday == 292)

lo_jan_helton <- loess(temp ~ rt, subset(orderedwaves, jday == 18))
lo_apr_helton <- loess(temp ~ rt, subset(orderedwaves, jday == 110))
lo_jul_helton <- loess(temp ~ rt, heltonjul)
lo_oct_helton <- loess(temp ~ rt, subset(orderedwaves, jday == 292))


p_jan_helton <- predict(lo_jan_helton, newdata = seq(0, 305, length.out = 400))
p_apr_helton <- predict(lo_apr_helton, newdata = seq(0, 305, length.out = 400))
p_jul_helton <- predict(lo_jul_helton, newdata = seq(0, 305, length.out = 400))
p_oct_helton <- predict(lo_oct_helton, newdata = seq(0, 305, length.out = 400))

smoothrt <- seq(0, 182*86400, length.out = 100)
lty_model <- 1
lty_helton <- 2

model_range <- c(min(unlist(highsmooth)), max(unlist(highsmooth)))
helton_range <- c(min(orderedwaves$temp), max(orderedwaves$temp))

modelmean <- mean(unlist(highsmooth))
heltonmean <- mean(orderedwaves$temp)

#scalehelton <- modelmean/heltonmean
scalehelton <- diff(model_range)/diff(helton_range)

helcol <- hcl.colors(4, "Zissou 1")

png("plots/helton_compare_model_rt.png",
    width = 900*5,
    height = 600*5,
    res = 72*5)
par(mar = c(5,5,2,5),
    cex = 1.1,
    cex.axis = 1.1,
    cex.lab = 1.1)
plot(smoothrt/86400, highsmooth$jan-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,300),
     col = helcol[1],
     pch = 20,
     lwd = 2,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = "Residence Time (days)")
mapply(function(x, c) lines(smoothrt/86400, x-modelmean, lwd = 2, lty = lty_model, col = c),
       highsmooth,
       helcol)
mapply(function(x, c) points(heltonjan$rt*0.6, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16))),
       list(heltonjan, heltonapr, heltonjul, heltonoct),
       helcol)
mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
       list(p_jan_helton, p_apr_helton, p_jul_helton, p_oct_helton),
       helcol)
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)
legend("topright", c("Jan-18", "Apr-20", "Jul-20", "Oct-19"),
       fill = helcol)
dev.off()

#################
#### TT only ####
#################
png("plots/temptool_hypo_temps.png",
    width = 900*5,
    height = 600*5,
    res = 72*5)
par(mar = c(5,5,2,5),
    cex = 1.1,
    cex.axis = 1.1,
    cex.lab = 1.1)
plot(smoothrt/86400, highsmooth$jan-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,182),
     col = helcol[1],
     pch = 20,
     lwd = 2,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("Hyporheic Temperature (", degree, "C)")),
     xlab = "Residence Time (days)")
mapply(function(x, c) lines(smoothrt/86400, x-modelmean, lwd = 2, lty = lty_model, col = c),
       highsmooth,
       helcol)
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
legend("topright", c("Jan-18", "Apr-20", "Jul-20", "Oct-19"),
       fill = helcol)
dev.off()

#######################
#### Facet by date ####
#######################
png("plots/temptool_hypo_temps_helton_dots.png",
    width = 700*5,
    height = 1200*5,
    res = 72*5)
par(mar = c(5,5,2,5),
    cex = 1.3,
    cex.axis = 1.3,
    cex.lab = 1.3,
    mfrow = c(4,1),
    oma = c(3,0,0,0))
plot(smoothrt/86400, highsmooth$jan-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,182),
     col = helcol[1],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = ""
     )
mapply(function(x, c) points(heltonjan$rt*0.6, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonjan),
       helcol[1])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_jan_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
axis(1, at = seq(0,305, length.out = 6)*0.6, labels = seq(0,305, length.out = 6), line = 3)
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)
legend("topright", c("Jan-18", "Apr-20", "Jul-20", "Oct-19"),
       fill = helcol)

plot(smoothrt/86400, highsmooth$apr-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,182),
     col = helcol[2],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
    xlab = ""
     )
mapply(function(x, c) points(heltonjan$rt*0.6, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonapr),
       helcol[2])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_apr_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
axis(1, at = seq(0,305, length.out = 6)*0.6, labels = seq(0,305, length.out = 6), line = 3)
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)

plot(smoothrt/86400, highsmooth$jul-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,182),
     col = helcol[3],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = ""
     )
mapply(function(x, c) points(heltonjan$rt*0.6, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonjul),
       helcol[3])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_apr_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
axis(1, at = seq(0,305, length.out = 6)*0.6, labels = seq(0,305, length.out = 6), line = 3)
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)

plot(smoothrt/86400, highsmooth$oct-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,182),
     col = helcol[4],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = ""
     )
mapply(function(x, c) points(heltonjan$rt*0.6, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonjul),
       helcol[4])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_apr_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
axis(1, at = seq(0,305, length.out = 6)*0.6, labels = seq(0,305, length.out = 6), line = 3)
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)
mtext("Residence Time (days)", side = 1, line = 2, cex=1.1, outer = T)
dev.off()


###################################################
#### Facet by date; full length of helton data ####
###################################################
png("plots/temptool_hypo_temps_helton_dots_full_length.png",
    width = 700*5,
    height = 1200*5,
    res = 72*5)
par(mar = c(5,5,2,5),
    cex = 1.3,
    cex.axis = 1.3,
    cex.lab = 1.3,
    mfrow = c(4,1))
plot(smoothrt/86400, highsmooth$jan-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,305),
     col = helcol[1],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = "Residence Time (days)")
mapply(function(x, c) points(heltonjan$rt, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonjan),
       helcol[1])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_jan_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)
legend("topright", c("Jan-18", "Apr-20", "Jul-20", "Oct-19"),
       fill = helcol)

plot(smoothrt/86400, highsmooth$apr-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,305),
     col = helcol[2],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = "Residence Time (days)")
mapply(function(x, c) points(heltonjan$rt, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonapr),
       helcol[2])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_apr_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)

plot(smoothrt/86400, highsmooth$jul-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,305),
     col = helcol[3],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = "Residence Time (days)")
mapply(function(x, c) points(heltonjan$rt, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonjul),
       helcol[3])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_apr_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)

plot(smoothrt/86400, highsmooth$oct-modelmean, type = "l",
     ylim = c(-11,11),
     xlim = c(0,305),
     col = helcol[4],
     pch = 20,
     lwd = 3,
     lty = lty_model,
     yaxt = "n",
     ylab = expression(paste("TempTool Hyporheic Temperature (", degree, "C)")),
     xlab = "Residence Time (days)")
mapply(function(x, c) points(heltonjan$rt, x$temp*scalehelton - heltonmean, col = c, pch = c(rep(15,3), rep(19,16)), cex = 2),
       list(heltonjul),
       helcol[4])
# mapply(function(x, c) lines(seq(0, 305, length.out = 400)*0.6, x*scalehelton-heltonmean, col = c, lwd = 2, lty = lty_helton),
#        list(p_apr_helton),
#        helcol[1])
axis(2, at = seq(-10, 10, by = 5), labels = round(seq(-10, 10, by = 5)+modelmean, 1))
axis(4, at = seq(-10, 10, by = 5), labels = round((seq(-10, 10, by = 5)/scalehelton)+heltonmean, 1))
mtext(expression(paste("Nyack Hyporheic Temperature (", degree, "C)")),
      side = 4, line = 3, cex = 1.1)
dev.off()


