CleanRawFollows <- function(data.in) {
  # function that cleans raw focal follows dataset
  #
  # Args
  # 
  # data.in = Focal follow dataset
  #
  # Returns
  # clean follows dataframe
  #
  data.in$EL.Nose.to.nose.Duration <- ifelse(is.na(data.in$EL.Nose.to.nose.Duration), 0, data.in$EL.Nose.to.nose.Duration) 
  data.in$EL.Body.Contact.Duration <- ifelse(is.na(data.in$EL.Body.Contact.Duration), 0, data.in$EL.Body.Contact.Duration)    
  data.in$EL.Bedded.Duration <- ifelse(is.na(data.in$EL.Bedded.Duration), 0, data.in$EL.Bedded.Duration)  
  data.in$LL.Body.Duration <- ifelse(is.na(data.in$LL.Body.Duration), 0, data.in$LL.Body.Duration)   
  data.in$LL.Nose.to.nose.Duration <- ifelse(is.na(data.in$LL.Nose.to.nose.Duration), 0, data.in$LL.Nose.to.nose.Duration)     
  data.in$EE.Body.Duration <- ifelse(is.na(data.in$EE.Body.Duration), 0, data.in$EE.Body.Duration)      
  data.in$YY.BC.Duration <- ifelse(is.na(data.in$YY.BC.Duration), 0, data.in$YY.BC.Duration)        
  data.in$YY.N.to.N.Duration <- ifelse(is.na(data.in$YY.N.to.N.Duration), 0, data.in$YY.N.to.N.Duration)        
  data.in$EY.BC.Duration <- ifelse(is.na(data.in$EY.BC.Duration), 0, data.in$EY.BC.Duration)        
  data.in$LY.BC.Duration <- ifelse(is.na(data.in$LY.BC.Duration), 0, data.in$LY.BC.Duration)       
  data.in$LY.N.to.N.Duration <- ifelse(is.na(data.in$LY.N.to.N.Duration), 0, data.in$LY.N.to.N.Duration)        
  
  
  data.in$TotContactDur <- (data.in$ELNurseDuration + 
                              data.in$EL.Nose.to.nose.Duration +   
                              data.in$EL.Body.Contact.Duration +   
                              data.in$EL.Bedded.Duration +   
                              data.in$LL.Body.Duration +   
                              data.in$LL.Nose.to.nose.Duration +   
                              data.in$EE.Body.Duration +   
                              data.in$YY.BC.Duration +   
                              data.in$YY.N.to.N.Duration +   
                              data.in$EY.BC.Duration +   
                              data.in$LY.BC.Duration +   
                              data.in$LY.N.to.N.Duration)  
  
  data.in$TotContactDurNoBedded <-(data.in$ELNurseDuration + 
                                     data.in$EL.Nose.to.nose.Duration +   
                                     data.in$EL.Body.Contact.Duration + 
                                     data.in$LL.Body.Duration +   
                                     data.in$LL.Nose.to.nose.Duration +   
                                     data.in$EE.Body.Duration +   
                                     data.in$YY.BC.Duration +   
                                     data.in$YY.N.to.N.Duration +   
                                     data.in$EY.BC.Duration +   
                                     data.in$LY.BC.Duration +   
                                     data.in$LY.N.to.N.Duration)  
  
  return(follows = data.in)
}