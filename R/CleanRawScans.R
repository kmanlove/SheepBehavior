CleanRawScans <- function(data.in) {
  # parse raw scans by individuals, affix additionl individual data and return
  # dataframe
  #
  # Args
  #
  # data.in = raw scans dataframe
  #
  # Return
  # cleaned scans dataframe
  #
  #-- loop over ScanSessionIDs.
  out <- vector("list", length(levels(data.in$ScanSessionID)))
  SessionTime <- Pop <- Date <- rep(NA, length(levels(data.in$ScanSessionID)))
  
  for(i in 1:length(levels(data.in$ScanSessionID))){
    k <- subset(data.in, ScanSessionID == levels(data.in$ScanSessionID)[i])
    SessionTime[i] <- k$TimePosix[1]
    Pop[i] <- as.character(k$Population[1])
    Date[i] <- k$Julian.Date[1]
    responses.by.scan <- vector("list", dim(k)[1])
    for(j in 1:dim(k)[1]){
      responses.by.scan[[j]] <- strsplit(as.character(k$Responses[j]), split = "\ ")[[1]]
    }
    session.responses <- unlist(responses.by.scan)
    
    # split session responses into two vectors; 1st col is id; 2nd col is activity
    individ <- activity <- rep(NA, length(session.responses))
    for(k in 1:length(session.responses)){
      individ[k] <- substr(session.responses[k], start = 1, stop = nchar(session.responses[k]) - 1)
      activity[k] <- substr(session.responses[k], start = nchar(session.responses[k]), stop = nchar(session.responses[k]))
    }
    
    session.dat <- as.data.frame(cbind(individ, activity))
    
    # which animals were present in this ScanSession?
    individ.marker <- rep(NA, length(levels(factor(session.responses))))
    for(j in 1:length(individ.marker)){
      individ.marker[j] <- substr(levels(factor(session.responses))[j], start = 1, stop = nchar(levels(factor(session.responses))[j]) - 1)
    }
    individs <- levels(factor(individ.marker))
    
    # loop over individs
    individ.dat <- matrix(NA, nrow = length(individs), ncol = 13)
    individ.data <- vector("list", length(individs))
    for(k in 1:length(individs))
    {
      individ.data[[k]] <- subset(session.dat, as.character(individ) == individs[k])
      individ.dat[k, ] <- c(levels(data.in$ScanSessionID)[i], Date[i], SessionTime[i], Pop[i], individs[k], as.vector(table(factor(individ.data[[k]]$activity, 
                                                                                                                                levels = c("u", "d", "g", "w", "s", "p", "n", "v")))))
    }
    
    out[[i]] <- as.data.frame(individ.dat)
    names(out[[i]]) <- c("ScanSessionID", "JulianDate", "SessionTime", "Pop", "IndividID", "u", "d", "g", "w", "s", "p", "n", "v")
    print(i)
  }
  
  
  out.full <- do.call("rbind", out)
  levels(out.full$IndividID)
  
  yearling.ids <- c("orange1", "orange15", "yellow41", "y", "yr", "oranger", "floppy",
                    "orange21", "orangel", "white45", "white52", "yellowl", "yellowL", 
                    "o18", "Or", "Yellow41", "white51", "o15", "whiter")
  
  nolambewe.ids <- c("1871", "020", "1220", "1210", "180")
  
  out.full$DemogGrp <- ifelse(out.full$IndividID == "l", "L",
                              ifelse(out.full$IndividID %in% yearling.ids, "Y", 
                                     ifelse(out.full$IndividID %in% nolambewe.ids, "NoLamb", "EweWithLamb")))
  
  # match IDs to follow IDs
  out.full$IndividID <- as.character(out.full$IndividID)
  for(i in 1:dim(out.full)[1]){
    out.full$IndividID[i] <- ifelse(out.full$IndividID[i] == "1389", "151.389",
                                    ifelse(out.full$IndividID[i] == "149", "149.849",
                                           ifelse(out.full$IndividID[i] == "1851", "151.851",
                                                  ifelse(out.full$IndividID[i] == "1871", "151.871",
                                                         ifelse(out.full$IndividID[i] == "1891", "151.891",
                                                                ifelse(out.full$IndividID[i] == "190", "150.19",
                                                                       ifelse(out.full$IndividID[i] == "650", "150.65",
                                                                              ifelse(out.full$IndividID[i] == "145", "150.145",
                                                                                     ifelse(out.full$IndividID[i] == "1561", "151.561",
                                                                                            ifelse(out.full$IndividID[i] == "1832", "151.832",
                                                                                                   ifelse(out.full$IndividID[i] == "1220", "151.22",
                                                                                                          ifelse(out.full$IndividID[i] == "1600", "151.6",
                                                                                                                 ifelse(out.full$IndividID[i] == "360", "150.36",
                                                                                                                        ifelse(out.full$IndividID[i] == "020", "150.02",
                                                                                                                               ifelse(out.full$IndividID[i] == "1210", "151.21",
                                                                                                                                      ifelse(out.full$IndividID[i] == "180", "150.18",
                                                                                                                                             ifelse(out.full$IndividID[i] == "319", "150.319",
                                                                                                                                                    ifelse(out.full$IndividID[i] == "360", "150.36", 
                                                                                                                                                           ifelse(out.full$IndividID[i] == "1440", "151.44", 
                                                                                                                                                                  ifelse(out.full$IndividID[i] == "1481", "151.481", 
                                                                                                                                                                         ifelse(out.full$IndividID[i] == "1321", "151.321",
                                                                                                                                                                                ifelse(out.full$IndividID[i] == "1888", "151.888",  
                                                                                                                                                                                       ifelse(out.full$IndividID[i] == "1860", "151.86", 
                                                                                                                                                                                              ifelse(out.full$IndividID[i] == "1281", "151.281", 
                                                                                                                                                                                                     ifelse(out.full$IndividID[i] == "1291", "151.291", 
                                                                                                                                                                                                            ifelse(out.full$IndividID[i] == "050", "150.05", 
                                                                                                                                                                                                                   ifelse(out.full$IndividID[i] == "819", "150.819", 
                                                                                                                                                                                                                          ifelse(out.full$IndividID[i] == "411", "150.411", 
                                                                                                                                                                                                                                 out.full$IndividID[i]
                                                                                                                                                                                                                          ))))))))))))))))))))))))))))
  }
  
  table(factor(out.full$IndividID))
  
  #-- score activity on a continuum. 
  # d = 1
  # u = 2
  # s = 3
  # g = 4
  # n = 4
  # v = 4
  # w = 5
  
  out.full$u <- as.numeric(as.character(out.full$u))
  out.full$d <- as.numeric(as.character(out.full$d))
  out.full$s <- as.numeric(as.character(out.full$s))
  out.full$g <- as.numeric(as.character(out.full$g))
  out.full$n <- as.numeric(as.character(out.full$n))
  out.full$v <- as.numeric(as.character(out.full$v))
  out.full$w <- as.numeric(as.character(out.full$w))
  out.full$p <- as.numeric(as.character(out.full$p))
  out.full$SessionTime <- as.numeric(as.character(out.full$SessionTime))
  out.full$JulianDate <- as.numeric(as.character(out.full$JulianDate))
  
  out.full$TotObs <- out.full$u + out.full$d + out.full$g + out.full$w + out.full$s +
    out.full$p + out.full$n + out.full$v
  
  out.full$ActivityScore <- (out.full$d * 1 + 
                               out.full$u * 2 + 
                               out.full$s * 3 +
                               out.full$g * 4 +
                               out.full$n * 4 +
                               out.full$v * 4 + 
                               out.full$w * 5) / out.full$TotObs
  
  return(clean.scans = out.full)
}