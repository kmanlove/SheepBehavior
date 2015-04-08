PlotScanTimeByDateByPop <- function(data.in) {
  # Plots when scan samples occurred: time by date by population 
  #
  # Args
  #
  # data.in = scan sample dataset
  #
  # Returns
  # 
  # plot
  
  TimesToLabel <- c("07:00:00 AM", "11:00:00 AM", "03:00:00 PM", "07:00:00 PM") 
  PosixLabelTimes <- as.numeric(strptime(TimesToLabel, "%I:%M:%S %p", tz = ""))
  
  par(oma = c(2, 4, 0, 0))
#  plot(data.in$TimePosix ~ as.numeric(as.character(data.in$Julian.Date)), xlim = c(120, 200), xaxt = "n", yaxt = "n", ylab = "", xlab = "Date", col = data.in$Population)
  plot(data.in$SessionTime ~ as.numeric(as.character(data.in$JulianDate)), xlim = c(120, 200), xaxt = "n", yaxt = "n", ylab = "", xlab = "Date", col = data.in$Pop)
  for(i in 1:4){
    abline(h = PosixLabelTimes[i], col = "grey50", lty = 2)
  }
  axis(side = 2, at = PosixLabelTimes, labels = c("7:00 AM", "11:00 AM", "3:00 PM", "7:00 PM"), las = 2)
  mtext(side = 2, line = 2, outer = T, "Time")
  axis(side = 1, at = c(121, 152, 182), labels = c("May 1", "June 1", "July 1"))
}