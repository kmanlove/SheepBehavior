PlotActivityByContacts <- function(follows.in, scans.in) {
  # function to plot mean activity score against mean contact time
  #
  # Args
  # 
  # follows.in = clean follows dataframe
  # scans.in = clean scans dataframe
  #
  mean.contact.dur <- mean.activity.score <- rep(NA, length(levels(follows.in$ID)))
  for(i in 1:length(levels(follows.in$ID))){
    k <- subset(follows.in, ID == levels(follows.in$ID)[i])
    j <- subset(scans.in, IndividID == levels(follows.in$ID)[i])
    
    if(dim(k)[1] > 0 & dim(j)[1] > 0){
      mean.activity.score[i] <- mean(j$ActivityScore)
      mean.contact.dur[i] <- mean(k$TotContactDurNoBedded)
    } else {
      mean.activity.score[i] <- NA
      mean.contact.dur[i] <- NA
    }
  }
  
  score.dat <- cbind(levels(follows.in$ID), mean.activity.score, mean.contact.dur)
  score.dat <- score.dat[complete.cases(score.dat), ]
#  cor(score.dat[ ,2], score.dat[, 3])
  
  par(mfrow = c(1, 1), oma = c(3, 3, 0, 0), mar = c(4, 4, 2, 2))
  plot(mean.contact.dur ~ mean.activity.score, xlab = "Mean activity score", ylab = "Mean contact duration", pch = 16)
  
}