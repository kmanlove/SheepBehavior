PlotContactsByTimeAge <- function(data.in) {
  # Qu 2: How do contact patterns change with time/age?
  #
  # Args
  # 
  # data.in = session-level focal follow data
  #
  # Return
  #
  #
  # group sizes
#   par(mfrow = c(1, 2))
#   plot(bb.follows$Grpsz ~ bb.follows$JulianDate, ylim = c(0, 40), xlim = c(120, 200), xlab = "Time", ylab = "Total group size", xaxt = "n", col = rgb(0, 0, 0, .1, names = NULL, maxColorValue = 1))
#   lines(lowess(bb.follows$Grpsz ~ bb.follows$JulianDate), lwd = 2)
#   points(aso.follows$Grpsz ~ aso.follows$JulianDate, col = rgb(1, 0, 0, .1, names = NULL, maxColorValue = 1), pch = 16, cex = .6)
#   lines(lowess(aso.follows$Grpsz ~ aso.follows$JulianDate), col = "red", lwd = 3, lty = 2)
#   axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))
#   leg.text <- c("Black Butte", "Asotin")
#   legend("topleft", bty = "n", leg.text, col = c("black", "red"), pch = c(1, 16), lty = c(1, 2), lwd = c(2, 2))
#   
#   plot((bb.follows$Ewes + bb.follows$YrEwes) ~ bb.follows$JulianDate, ylim = c(0, 25), xlim = c(120, 200), xlab = "Time", ylab = "Group size without lambs", xaxt = "n", col = rgb(0, 0, 0, .1, names = NULL, maxColorValue = 1))
#   lines(lowess((bb.follows$Ewes + bb.follows$YrEwes) ~ bb.follows$JulianDate), lwd = 2)
#   points((aso.follows$Ewes + aso.follows$YrEwes) ~ aso.follows$JulianDate, col = rgb(1, 0, 0, .1, names = NULL, maxColorValue = 1), pch = 16, cex = .6)
#   lines(lowess((aso.follows$Ewes + aso.follows$YrEwes) ~ aso.follows$JulianDate), lty = 2, col = "red", lwd = 3)
#   axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))
#   legend("topleft", bty = "n", leg.text, col = c("black", "red"), pch = c(1, 16), lty = c(1, 2), lwd = c(2, 2))
  
  par(mfrow = c(1, 4))
  plot((data.in$TotContactDur + 1) ~ data.in$sessionDate, ylab = "Total contact duration", 
       xlab = "Julian date", log = "y",  ylim = c(1, max(data.in$TotContactDur)))
#  lines(lowess((data.in$TotContactDur + 1) ~ data.in$sessionDate))
  plot((data.in$ELContactDur + 1) ~ data.in$sessionDate, ylab = "EL contact duration", 
       xlab = "Julian date", log = "y",  ylim = c(1, max(data.in$TotContactDur)))
#  lines(lowess((data.in$ELContactDur + 1) ~ data.in$sessionDate))
  plot((data.in$ELContactNotBedded + 1) ~ data.in$sessionDate, ylab = "EL contact duration (not bedded)", 
       xlab = "Julian date", log = "y",  ylim = c(1, max(data.in$TotContactDur)))
#  lines(lowess((data.in$ELContactNotBedded + 1) ~ data.in$sessionDate))
  plot((data.in$LLContactDur + 1) ~ data.in$sessionDate, ylab = "LL contact duration", xlab = "Julian date", 
       log = "y",  ylim = c(1, max(data.in$TotContactDur)))
#  lines(lowess((data.in$LLContactDur + 1) ~ data.in$sessionDate))
  
  
}