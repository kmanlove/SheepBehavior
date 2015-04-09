#-- script to parse scan samples --#
# data <- read.csv("~/Dropbox/HCSummer2014/ScanSamples/ScansToParse_150324.csv", header = T)

#----------------------------------#
#-- look at time by date by pop ---#
#----------------------------------#
# data$TimePosix0 <- strptime(data$Time, "%H:%M:%S %p", tz = "")
# TimesToLabel <- c("07:00:00 AM", "11:00:00 AM", "03:00:00 PM", "07:00:00 PM") 
# PosixLabelTimes <- as.numeric(strptime(TimesToLabel, "%I:%M:%S %p", tz = ""))
# data$TimePosix <- as.numeric(strptime(data$Time, "%I:%M:%S %p", tz = ""))
# data$TimePosix2 <- as.numeric(strptime(data$Time, "%l"))
# 
# par(oma = c(2, 4, 0, 0))
# plot(data$TimePosix ~ as.numeric(as.character(data$Julian.Date)), xlim = c(120, 200), xaxt = "n", yaxt = "n", ylab = "", xlab = "Date", col = data$Population)
# for(i in 1:4){
#   abline(h = PosixLabelTimes[i], col = "grey50", lty = 2)
# }
# axis(side = 2, at = PosixLabelTimes, labels = c("7:00 AM", "11:00 AM", "3:00 PM", "7:00 PM"), las = 2)
# mtext(side = 2, line = 2, outer = T, "Time")
# axis(side = 1, at = c(121, 152, 182), labels = c("May 1", "June 1", "July 1"))

# #-- loop over ScanSessionIDs.
# out <- vector("list", length(levels(data$ScanSessionID)))
# SessionTime <- Pop <- Date <- rep(NA, length(levels(data$ScanSessionID)))
# 
# for(i in 1:length(levels(data$ScanSessionID))){
#   k <- subset(data, ScanSessionID == levels(data$ScanSessionID)[i])
#   SessionTime[i] <- k$TimePosix[1]
#   Pop[i] <- as.character(k$Population[1])
#   Date[i] <- k$Julian.Date[1]
#   responses.by.scan <- vector("list", dim(k)[1])
#   for(j in 1:dim(k)[1]){
#     responses.by.scan[[j]] <- strsplit(as.character(k$Responses[j]), split = "\ ")[[1]]
#   }
#   session.responses <- unlist(responses.by.scan)
#   
#   # split session responses into two vectors; 1st col is id; 2nd col is activity
#   individ <- activity <- rep(NA, length(session.responses))
#   for(k in 1:length(session.responses)){
#     individ[k] <- substr(session.responses[k], start = 1, stop = nchar(session.responses[k]) - 1)
#     activity[k] <- substr(session.responses[k], start = nchar(session.responses[k]), stop = nchar(session.responses[k]))
#   }
#   
#   session.dat <- as.data.frame(cbind(individ, activity))
#   
#   # which animals were present in this ScanSession?
#   individ.marker <- rep(NA, length(levels(factor(session.responses))))
#   for(j in 1:length(individ.marker)){
#     individ.marker[j] <- substr(levels(factor(session.responses))[j], start = 1, stop = nchar(levels(factor(session.responses))[j]) - 1)
#   }
#   individs <- levels(factor(individ.marker))
# 
#   # loop over individs
#   individ.dat <- matrix(NA, nrow = length(individs), ncol = 13)
#   individ.data <- vector("list", length(individs))
#   for(k in 1:length(individs))
#     {
#       individ.data[[k]] <- subset(session.dat, as.character(individ) == individs[k])
#       individ.dat[k, ] <- c(levels(data$ScanSessionID)[i], Date[i], SessionTime[i], Pop[i], individs[k], as.vector(table(factor(individ.data[[k]]$activity, 
#                                                  levels = c("u", "d", "g", "w", "s", "p", "n", "v")))))
#     }
#   
#   out[[i]] <- as.data.frame(individ.dat)
#   names(out[[i]]) <- c("ScanSessionID", "JulianDate", "SessionTime", "Pop", "IndividID", "u", "d", "g", "w", "s", "p", "n", "v")
#   print(i)
# }
# 
# out.full <- do.call("rbind", out)
# levels(out.full$IndividID)
# 
# yearling.ids <- c("orange1", "orange15", "yellow41", "y", "yr", "oranger", "floppy",
#                   "orange21", "orangel", "white45", "white52", "yellowl", "yellowL", 
#                   "o18", "Or", "Yellow41", "white51", "o15", "whiter")
# 
# nolambewe.ids <- c("1871", "020", "1220", "1210", "180")
# 
# out.full$DemogGrp <- ifelse(out.full$IndividID == "l", "L",
#                             ifelse(out.full$IndividID %in% yearling.ids, "Y", 
#                             ifelse(out.full$IndividID %in% nolambewe.ids, "NoLamb", "EweWithLamb")))
# 
# # match IDs to follow IDs
# out.full$IndividID <- as.character(out.full$IndividID)
#   for(i in 1:dim(out.full)[1]){
#     out.full$IndividID[i] <- ifelse(out.full$IndividID[i] == "1389", "151.389",
#                     ifelse(out.full$IndividID[i] == "149", "149.849",
#                     ifelse(out.full$IndividID[i] == "1851", "151.851",
#                     ifelse(out.full$IndividID[i] == "1871", "151.871",
#                     ifelse(out.full$IndividID[i] == "1891", "151.891",
#                     ifelse(out.full$IndividID[i] == "190", "150.19",
#                     ifelse(out.full$IndividID[i] == "650", "150.65",
#                     ifelse(out.full$IndividID[i] == "145", "150.145",
#                     ifelse(out.full$IndividID[i] == "1561", "151.561",
#                     ifelse(out.full$IndividID[i] == "1832", "151.832",
#                     ifelse(out.full$IndividID[i] == "1220", "151.22",
#                     ifelse(out.full$IndividID[i] == "1600", "151.6",
#                     ifelse(out.full$IndividID[i] == "360", "150.36",
#                     ifelse(out.full$IndividID[i] == "020", "150.02",
#                     ifelse(out.full$IndividID[i] == "1210", "151.21",
#                     ifelse(out.full$IndividID[i] == "180", "150.18",
#                     ifelse(out.full$IndividID[i] == "319", "150.319",
#                     ifelse(out.full$IndividID[i] == "360", "150.36", 
#                     ifelse(out.full$IndividID[i] == "1440", "151.44", 
#                     ifelse(out.full$IndividID[i] == "1481", "151.481", 
#                     ifelse(out.full$IndividID[i] == "1321", "151.321",
#                     ifelse(out.full$IndividID[i] == "1888", "151.888",  
#                     ifelse(out.full$IndividID[i] == "1860", "151.86", 
#                     ifelse(out.full$IndividID[i] == "1281", "151.281", 
#                     ifelse(out.full$IndividID[i] == "1291", "151.291", 
#                     ifelse(out.full$IndividID[i] == "050", "150.05", 
#                     ifelse(out.full$IndividID[i] == "819", "150.819", 
#                     ifelse(out.full$IndividID[i] == "411", "150.411", 
#                     out.full$IndividID[i]
#                     ))))))))))))))))))))))))))))
#   }
# 
# table(factor(out.full$IndividID))
# 
# #-- score activity on a continuum. 
# # d = 1
# # u = 2
# # s = 3
# # g = 4
# # n = 4
# # v = 4
# # w = 5
# 
# out.full$u <- as.numeric(as.character(out.full$u))
# out.full$d <- as.numeric(as.character(out.full$d))
# out.full$s <- as.numeric(as.character(out.full$s))
# out.full$g <- as.numeric(as.character(out.full$g))
# out.full$n <- as.numeric(as.character(out.full$n))
# out.full$v <- as.numeric(as.character(out.full$v))
# out.full$w <- as.numeric(as.character(out.full$w))
# out.full$p <- as.numeric(as.character(out.full$p))
# out.full$SessionTime <- as.numeric(as.character(out.full$SessionTime))
# out.full$JulianDate <- as.numeric(as.character(out.full$JulianDate))
# 
# out.full$TotObs <- out.full$u + out.full$d + out.full$g + out.full$w + out.full$s +
#   out.full$p + out.full$n + out.full$v
# 
# out.full$ActivityScore <- (out.full$d * 1 + 
#   out.full$u * 2 + 
#   out.full$s * 3 +
#   out.full$g * 4 +
#   out.full$n * 4 +
#   out.full$v * 4 + 
#   out.full$w * 5) / out.full$TotObs

hist(out.full$ActivityScore, main = "", xlab = "Activity Score", col = "grey70")

bb.scans <- subset(out.full, Pop == "BB")
aso.scans <- subset(out.full, Pop == "Aso")
plot(out.full$ActivityScore ~ out.full$SessionTime, col = out.full$Pop, xaxt = "n", xlab = "", ylab = "Activity Score")
lines(lowess(as.numeric(as.character(bb.scans$ActivityScore)) ~ as.numeric(as.character(bb.scans$SessionTime))), col = "red", lty = 1, lwd = 2)
axis(side = 1, at = PosixLabelTimes, labels = c("7:00 AM", "11:00 AM", "3:00 PM", "7:00 PM"), las = 2)

boxplot(out.full$ActivityScore ~ out.full$Pop)

# by month
may.full <- subset(out.full, JulianDate <= 152)
june.full <- subset(out.full, JulianDate >= 153 & JulianDate <= 182)
july.full <- subset(out.full, JulianDate >= 183)

par(mfrow = c(1, 3))
boxplot(may.full$ActivityScore ~ may.full$Pop, col = "grey70", main = "May", ylab = "Activity Score")
boxplot(june.full$ActivityScore ~ june.full$Pop, col = "grey70", main = "June")
boxplot(july.full$ActivityScore ~ july.full$Pop, col = "grey70", main = "July")

# # by demog grp
# lambs.bb <- subset(out.full, Pop == "BB" & IndividID == "l")
# lambs.aso <- subset(out.full, Pop == "Aso" & IndividID == "l")
# 
# lambs.bb.may <- subset(lambs.bb, JulianDate <= 152)
# lambs.bb.june <- subset(lambs.bb, JulianDate >= 153 & JulianDate <= 182)
# lambs.bb.july <- subset(lambs.bb, JulianDate >= 183)
# lambs.aso.may <- subset(lambs.aso, JulianDate <= 152)
# lambs.aso.june <- subset(lambs.aso, JulianDate >= 153 & JulianDate <= 182)
# lambs.aso.july <- subset(lambs.aso, JulianDate >= 183)
# 
# may.lambs <- subset(out.full, IndividID == "l" & JulianDate <= 152)
# june.lambs <- subset(out.full, IndividID == "l" & JulianDate >= 153 & JulianDate <= 182)
# july.lambs <- subset(out.full, IndividID == "l" & JulianDate >= 183)
# 
# may.lambs <- subset(out.full, IndividID == "l" & JulianDate <= 152)
# june.lambs <- subset(out.full, IndividID == "l" & JulianDate >= 153 & JulianDate <= 182)
# july.lambs <- subset(out.full, IndividID == "l" & JulianDate >= 183)
# 
# may.yr <- subset(out.full, DemogGrp == "Y" & JulianDate <= 152)
# june.yr <- subset(out.full, DemogGrp == "Y" & JulianDate >= 153 & JulianDate <= 182)
# july.yr <- subset(out.full, DemogGrp == "Y" & JulianDate >= 183)
# 
# may.ewewithlamb <- subset(out.full, DemogGrp == "EweWithLamb" & JulianDate <= 152)
# june.ewewithlamb <- subset(out.full, DemogGrp == "EweWithLamb" & JulianDate >= 153 & JulianDate <= 182)
# july.ewewithlamb <- subset(out.full, DemogGrp == "EweWithLamb" & JulianDate >= 183)
# 
# may.nolamb <- subset(out.full, DemogGrp == "NoLamb" & JulianDate <= 152)
# june.nolamb <- subset(out.full, DemogGrp == "NoLamb" & JulianDate >= 153 & JulianDate <= 182)
# july.nolamb <- subset(out.full, DemogGrp == "NoLamb" & JulianDate >= 183)
# 
# par(mfrow = c(4, 3), oma = c(3, 2, 0, 0), mar = c(2, 5, 2, 2))
# boxplot(may.lambs$ActivityScore ~ may.lambs$Pop, col = c("grey70", "red"), main = "May", ylab = "Lamb Activity Scores")
# boxplot(june.lambs$ActivityScore ~ june.lambs$Pop, col = c("grey70", "red"), main = "June")
# boxplot(july.lambs$ActivityScore ~ july.lambs$Pop, col = c("grey70", "red"), main = "July")
# 
# boxplot(may.yr$ActivityScore ~ may.yr$Pop, col = c("grey70", "red"), ylab = "Yearling Activity Scores")
# boxplot(june.yr$ActivityScore ~ june.yr$Pop, col = c("grey70", "red"))
# boxplot(july.yr$ActivityScore ~ july.yr$Pop, col = c("grey70", "red"))
# 
# boxplot(may.nolamb$ActivityScore ~ may.nolamb$Pop, col = c("grey70", "red"), ylab = "Ewe-No-Lamb \n Activity Scores")
# boxplot(june.nolamb$ActivityScore ~ june.nolamb$Pop, col = c("grey70", "red"))
# boxplot(july.nolamb$ActivityScore ~ july.nolamb$Pop, col = c("grey70", "red"))
# 
# boxplot(may.ewewithlamb$ActivityScore ~ may.ewewithlamb$Pop, col = c("grey70", "red"), ylab = "Ewe-With-Lamb \n Activity Scores")
# boxplot(june.ewewithlamb$ActivityScore ~ june.ewewithlamb$Pop, col = c("grey70", "red"))
# boxplot(july.ewewithlamb$ActivityScore ~ july.ewewithlamb$Pop, col = c("grey70", "red"))

# just ewes with lambs and lambs
par(mfrow = c(2, 3), oma = c(3, 2, 0, 0), mar = c(2, 5, 2, 2))
boxplot(may.lambs$ActivityScore ~ may.lambs$Pop, col = c("grey70", "red"), main = "May", ylab = "Lamb Activity Scores")
boxplot(june.lambs$ActivityScore ~ june.lambs$Pop, col = c("grey70", "red"), main = "June")
boxplot(july.lambs$ActivityScore ~ july.lambs$Pop, col = c("grey70", "red"), main = "July")

boxplot(may.ewewithlamb$ActivityScore ~ may.ewewithlamb$Pop, col = c("grey70", "red"), ylab = "Ewe-With-Lamb \n Activity Scores")
boxplot(june.ewewithlamb$ActivityScore ~ june.ewewithlamb$Pop, col = c("grey70", "red"))
boxplot(july.ewewithlamb$ActivityScore ~ july.ewewithlamb$Pop, col = c("grey70", "red"))


#--------------------------------------------------------------------#
#-- relationship between avg. animal activity score and # contacts --#
#--------------------------------------------------------------------#
# follows <- read.csv("~/Dropbox/HCSummer2014/FocalFollows/FullCleanFollows_2014.csv", header = T)
#
# follows$EL.Nose.to.nose.Duration <- ifelse(is.na(follows$EL.Nose.to.nose.Duration), 0, follows$EL.Nose.to.nose.Duration) 
# follows$EL.Body.Contact.Duration <- ifelse(is.na(follows$EL.Body.Contact.Duration), 0, follows$EL.Body.Contact.Duration)    
# follows$EL.Bedded.Duration <- ifelse(is.na(follows$EL.Bedded.Duration), 0, follows$EL.Bedded.Duration)  
# follows$LL.Body.Duration <- ifelse(is.na(follows$LL.Body.Duration), 0, follows$LL.Body.Duration)   
# follows$LL.Nose.to.nose.Duration <- ifelse(is.na(follows$LL.Nose.to.nose.Duration), 0, follows$LL.Nose.to.nose.Duration)     
# follows$EE.Body.Duration <- ifelse(is.na(follows$EE.Body.Duration), 0, follows$EE.Body.Duration)      
# follows$YY.BC.Duration <- ifelse(is.na(follows$YY.BC.Duration), 0, follows$YY.BC.Duration)        
# follows$YY.N.to.N.Duration <- ifelse(is.na(follows$YY.N.to.N.Duration), 0, follows$YY.N.to.N.Duration)        
# follows$EY.BC.Duration <- ifelse(is.na(follows$EY.BC.Duration), 0, follows$EY.BC.Duration)        
# follows$LY.BC.Duration <- ifelse(is.na(follows$LY.BC.Duration), 0, follows$LY.BC.Duration)       
# follows$LY.N.to.N.Duration <- ifelse(is.na(follows$LY.N.to.N.Duration), 0, follows$LY.N.to.N.Duration)        
# 
# 
# follows$TotContactDur <- (follows$ELNurseDuration + 
#   follows$EL.Nose.to.nose.Duration +   
#   follows$EL.Body.Contact.Duration +   
#   follows$EL.Bedded.Duration +   
#   follows$LL.Body.Duration +   
#   follows$LL.Nose.to.nose.Duration +   
#   follows$EE.Body.Duration +   
#   follows$YY.BC.Duration +   
#   follows$YY.N.to.N.Duration +   
#   follows$EY.BC.Duration +   
#   follows$LY.BC.Duration +   
#   follows$LY.N.to.N.Duration)  
#   
# follows$TotContactDurNoBedded <-(follows$ELNurseDuration + 
#                                    follows$EL.Nose.to.nose.Duration +   
#                                    follows$EL.Body.Contact.Duration + 
#                                    follows$LL.Body.Duration +   
#                                    follows$LL.Nose.to.nose.Duration +   
#                                    follows$EE.Body.Duration +   
#                                    follows$YY.BC.Duration +   
#                                    follows$YY.N.to.N.Duration +   
#                                    follows$EY.BC.Duration +   
#                                    follows$LY.BC.Duration +   
#                                    follows$LY.N.to.N.Duration)  
#
# mean.contact.dur <- mean.activity.score <- rep(NA, length(levels(follows$ID)))
# for(i in 1:length(levels(follows$ID))){
#   k <- subset(follows, ID == levels(follows$ID)[i])
#   j <- subset(out.full, IndividID == levels(follows$ID)[i])
#   
#   if(dim(k)[1] > 0 & dim(j)[1] > 0){
#     mean.activity.score[i] <- mean(j$ActivityScore)
#     mean.contact.dur[i] <- mean(k$TotContactDurNoBedded)
#   } else {
#     mean.activity.score[i] <- NA
#     mean.contact.dur[i] <- NA
#   }
# }
# 
# score.dat <- cbind(levels(follows$ID), mean.activity.score, mean.contact.dur)
# score.dat <- score.dat[complete.cases(score.dat), ]
# cor(score.dat[ ,2], score.dat[, 3])
# 
# par(mfrow = c(1, 1), oma = c(3, 3, 0, 0), mar = c(4, 4, 2, 2))
# plot(mean.contact.dur ~ mean.activity.score, xlab = "Mean activity score", ylab = "Mean contact duration", pch = 16)

# cor(mean.activity.score, mean.contact.dur)
