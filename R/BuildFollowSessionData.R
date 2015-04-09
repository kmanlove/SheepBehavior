BuildFollowSessionData <- function(data.in) {
  # builds session-level focal follow data
  #
  # Args
  # 
  # data.in = clean follow data
  #
  # Returns
  # 
  # follow.sess.data = data frame of session-level focal follow data
  #
  sessionID <- sessionGrpSz <- sessionDate <- TotContactDur <- rep(NA, length(levels(factor(data.in$SessionID))))
  LLContactDur <- ELContactDur <- ELContactNotBedded <- pop <- rep(NA, length(levels(factor(data.in$SessionID))))
  
  for(i in 1:length(sessionID)) # loop over session IDs
  {
    k <- subset(full.follows, as.character(SessionID) == as.character(levels(factor(data.in$SessionID)))[i])
    sessionID[i] <- as.character(levels(factor(data.in$SessionID)))[i]
    sessionGrpSz[i] <- k$Grpsz[1]
    sessionDate[i] <- k$JulianDate[1]
    pop[i] <- k$Population[1]
    TotContactDur[i] <- sum(na.omit(c(k$ELNurseDuration,  
                                      k$EL.Nose.to.nose.Duration,
                                      k$EL.Body.Contact.Duration,
                                      k$EL.Bedded.Duration,
                                      k$LL.Body.Duration,
                                      k$LL.Nose.to.nose.Duration,
                                      k$EE.Body.Duration,
                                      k$YY.BC.Duration,
                                      k$YY.N.to.N.Duration,
                                      k$EY.BC.Duration,
                                      k$LY.BC.Duration,
                                      k$LY.N.to.N.Duration
    )))
    LLContactDur[i] <- sum(na.omit(c(k$LL.Body.Duration,
                                     k$LL.Nose.to.nose.Duration
    )))
    ELContactDur[i] <- sum(na.omit(c(k$ELNurseDuration,  
                                     k$EL.Nose.to.nose.Duration,
                                     k$EL.Body.Contact.Duration,
                                     k$EL.Bedded.Duration
    )))
    ELContactNotBedded[i] <- sum(na.omit(c(k$ELNurseDuration,  
                                           k$EL.Nose.to.nose.Duration,
                                           k$EL.Body.Contact.Duration
    )))
    
  }
  follow.sess.data <- as.data.frame(cbind(sessionID, sessionGrpSz, sessionDate, TotContactDur,
                                          LLContactDur, ELContactDur, ELContactNotBedded, pop))
  names(follow.sess.data) <- c("sessionID", "sessionGrpSz", "sessionDate", "TotContactDur", 
                               "LLContactDur", "ELContactDur", "ELContactNotBedded", "pop")
  
  # reclass variables to numeric
  follow.sess.data$TotContactDur <- as.numeric(as.character(follow.sess.data$TotContactDur))
  follow.sess.data$LLContactDur <- as.numeric(as.character(follow.sess.data$LLContactDur))
  follow.sess.data$ELContactDur <- as.numeric(as.character(follow.sess.data$ELContactDur))
  follow.sess.data$ELContactNotBedded <- as.numeric(as.character(follow.sess.data$ELContactNotBedded))
  follow.sess.data$sessionGrpSz <- as.numeric(as.character(follow.sess.data$sessionGrpSz))
  
  return(follow.sess.data = follow.sess.data)
}