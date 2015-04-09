PlotActivityByDemogGrp <- function(data.in) {
  # function builds boxplots of activity patterns by demographic group
  #
  # Args:
  #
  # data.in = cleaned scans data
  #
  # Return:
  #
  # boxplots
  
  lambs.bb <- subset(data.in, Pop == "BB" & IndividID == "l")
  lambs.aso <- subset(data.in, Pop == "Aso" & IndividID == "l")
  
  lambs.bb.may <- subset(lambs.bb, JulianDate <= 152)
  lambs.bb.june <- subset(lambs.bb, JulianDate >= 153 & JulianDate <= 182)
  lambs.bb.july <- subset(lambs.bb, JulianDate >= 183)
  lambs.aso.may <- subset(lambs.aso, JulianDate <= 152)
  lambs.aso.june <- subset(lambs.aso, JulianDate >= 153 & JulianDate <= 182)
  lambs.aso.july <- subset(lambs.aso, JulianDate >= 183)
  
  may.lambs <- subset(data.in, IndividID == "l" & JulianDate <= 152)
  june.lambs <- subset(data.in, IndividID == "l" & JulianDate >= 153 & JulianDate <= 182)
  july.lambs <- subset(data.in, IndividID == "l" & JulianDate >= 183)
  
  may.lambs <- subset(data.in, IndividID == "l" & JulianDate <= 152)
  june.lambs <- subset(data.in, IndividID == "l" & JulianDate >= 153 & JulianDate <= 182)
  july.lambs <- subset(data.in, IndividID == "l" & JulianDate >= 183)
  
  may.yr <- subset(data.in, DemogGrp == "Y" & JulianDate <= 152)
  june.yr <- subset(data.in, DemogGrp == "Y" & JulianDate >= 153 & JulianDate <= 182)
  july.yr <- subset(data.in, DemogGrp == "Y" & JulianDate >= 183)
  
  may.ewewithlamb <- subset(data.in, DemogGrp == "EweWithLamb" & JulianDate <= 152)
  june.ewewithlamb <- subset(data.in, DemogGrp == "EweWithLamb" & JulianDate >= 153 & JulianDate <= 182)
  july.ewewithlamb <- subset(data.in, DemogGrp == "EweWithLamb" & JulianDate >= 183)
  
  may.nolamb <- subset(data.in, DemogGrp == "NoLamb" & JulianDate <= 152)
  june.nolamb <- subset(data.in, DemogGrp == "NoLamb" & JulianDate >= 153 & JulianDate <= 182)
  july.nolamb <- subset(data.in, DemogGrp == "NoLamb" & JulianDate >= 183)
  
  par(mfrow = c(4, 3), oma = c(3, 2, 0, 0), mar = c(2, 5, 2, 2))
  boxplot(may.lambs$ActivityScore ~ may.lambs$Pop, col = c("grey70", "red"), main = "May", ylab = "Lamb Activity Scores")
  boxplot(june.lambs$ActivityScore ~ june.lambs$Pop, col = c("grey70", "red"), main = "June")
  boxplot(july.lambs$ActivityScore ~ july.lambs$Pop, col = c("grey70", "red"), main = "July")
  
  boxplot(may.yr$ActivityScore ~ may.yr$Pop, col = c("grey70", "red"), ylab = "Yearling Activity Scores")
  boxplot(june.yr$ActivityScore ~ june.yr$Pop, col = c("grey70", "red"))
  boxplot(july.yr$ActivityScore ~ july.yr$Pop, col = c("grey70", "red"))
  
  boxplot(may.nolamb$ActivityScore ~ may.nolamb$Pop, col = c("grey70", "red"), ylab = "Ewe-No-Lamb \n Activity Scores")
  boxplot(june.nolamb$ActivityScore ~ june.nolamb$Pop, col = c("grey70", "red"))
  boxplot(july.nolamb$ActivityScore ~ july.nolamb$Pop, col = c("grey70", "red"))
  
  boxplot(may.ewewithlamb$ActivityScore ~ may.ewewithlamb$Pop, col = c("grey70", "red"), ylab = "Ewe-With-Lamb \n Activity Scores")
  boxplot(june.ewewithlamb$ActivityScore ~ june.ewewithlamb$Pop, col = c("grey70", "red"))
  boxplot(july.ewewithlamb$ActivityScore ~ july.ewewithlamb$Pop, col = c("grey70", "red"))
  
  
}