#------------------------------------------------------#
#-- import data and require necessary packages --------#
#------------------------------------------------------#

# Absolute path (Bad !!)
aso.follows <- read.csv("~/work/Kezia/Research/EcologyPapers/SheepBehavior/data/CleanAsotinFollows_090214.csv", header = T)

# Relative path (Good!!)
aso.follows <- read.csv("./data/CleanAsotinFollows_090214.csv", header = T)




bb.follows <- read.csv("./data/CleanBlackButteFollows_090214.csv", header = T)
names(bb.follows)[40] <- "YY.N.to.N.Count"
full.follows <- as.data.frame(rbind(aso.follows, bb.follows))
names(full.follows) <- names(aso.follows)

aso.relocs <- read.csv("./data/Asotin_Locations_Clean.csv", header = T, sep = "\t")
bb.relocs <- read.csv("./data/BlackButte_Locations_Clean.csv", header = T, sep = "\t")

raw.scans <- read.csv("./data/AnonymousCompiledScans.csv", header = T)

#--------------------------------------------------------------#
#-- Qu 1: Does direct contact pattern scale with group size? --#
#--------------------------------------------------------------#
# build session-level data.
sessionID <- sessionGrpSz <- sessionDate <- TotContactDur <- LLContactDur <- ELContactDur <- ELContactNotBedded <- pop <- rep(NA, length(levels(factor(aso.follows$SessionID))))
for(i in 1:length(sessionID))
{
#  k <- subset(full.follows, as.character(SessionID) == as.character(levels(factor(aso.follows$SessionID)))[i])
#  sessionID[i] <- as.character(levels(factor(aso.follows$SessionID)))[i]
  k <- subset(full.follows, as.character(SessionID) == as.character(levels(factor(full.follows$SessionID)))[i])
  sessionID[i] <- as.character(levels(factor(full.follows$SessionID)))[i]
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

par(mfrow = c(1, 3))
plot(log(TotContactDur + 1) ~ sessionGrpSz, ylab = "Log(Total contact duration + 1)", xlab = "Group size")
lines(lowess(log(TotContactDur + 1) ~ sessionGrpSz))
plot(log(ELContactDur + 1) ~ sessionGrpSz, ylab = "Log(EL contact duration + 1)", xlab = "Group size")
lines(lowess(log(ELContactDur + 1) ~ sessionGrpSz))
plot(log(LLContactDur + 1) ~ sessionGrpSz, ylab = "Log(LL contact duration + 1)", xlab = "Group size")
lines(lowess(log(LLContactDur + 1) ~ sessionGrpSz))
test.fit.linear <- glm(TotContactDur ~ sessionGrpSz, family = "poisson")
test.fit.quad <- glm(TotContactDur ~ sessionGrpSz + I(sessionGrpSz ^ 2), family = "poisson")
anova(test.fit.linear, test.fit.quad, test = "F")
  # highly significant improvement when including quadratic term. 
  # probably should be a zip...
  # need to check on Black Butte as well. 

#  now look at non-zero lamb contacts. 
aso.nonzerolambs <- subset(aso.follows, !(LL.Body.Duration == 0 & EL.Body.Contact.Duration == 0))
plot(log(aso.nonzerolambs$LL.Body.Duration + aso.nonzerolambs$EL.Body.Contact.Duration + 1) ~ aso.nonzerolambs$Grpsz, ylab = "Log(Lamb contact duration + 1)", xlab = "Group size")
lines(lowess(log(aso.nonzerolambs$LL.Body.Duration + aso.nonzerolambs$EL.Body.Contact.Duration + 1) ~ aso.nonzerolambs$Grpsz))
  # contact duration has no apparent relationship to groupsize for lambs....

# what about degree -- does degree (in terms of # different animals contacted) change with groupsize?

#---------------------------------------------------------#
#-- Qu 2: How do contact patterns change with time/age? --#
#---------------------------------------------------------#
# group sizes
par(mfrow = c(1, 2))
plot(bb.follows$Grpsz ~ bb.follows$JulianDate, ylim = c(0, 40), xlim = c(120, 200), xlab = "Time", ylab = "Total group size", xaxt = "n", col = rgb(0, 0, 0, .1, names = NULL, maxColorValue = 1))
lines(lowess(bb.follows$Grpsz ~ bb.follows$JulianDate), lwd = 2)
points(aso.follows$Grpsz ~ aso.follows$JulianDate, col = rgb(1, 0, 0, .1, names = NULL, maxColorValue = 1), pch = 16, cex = .6)
lines(lowess(aso.follows$Grpsz ~ aso.follows$JulianDate), col = "red", lwd = 3, lty = 2)
axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))
leg.text <- c("Black Butte", "Asotin")
legend("topleft", bty = "n", leg.text, col = c("black", "red"), pch = c(1, 16), lty = c(1, 2), lwd = c(2, 2))

plot((bb.follows$Ewes + bb.follows$YrEwes) ~ bb.follows$JulianDate, ylim = c(0, 25), xlim = c(120, 200), xlab = "Time", ylab = "Group size without lambs", xaxt = "n", col = rgb(0, 0, 0, .1, names = NULL, maxColorValue = 1))
lines(lowess((bb.follows$Ewes + bb.follows$YrEwes) ~ bb.follows$JulianDate), lwd = 2)
points((aso.follows$Ewes + aso.follows$YrEwes) ~ aso.follows$JulianDate, col = rgb(1, 0, 0, .1, names = NULL, maxColorValue = 1), pch = 16, cex = .6)
lines(lowess((aso.follows$Ewes + aso.follows$YrEwes) ~ aso.follows$JulianDate), lty = 2, col = "red", lwd = 3)
axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))
legend("topleft", bty = "n", leg.text, col = c("black", "red"), pch = c(1, 16), lty = c(1, 2), lwd = c(2, 2))

par(mfrow = c(1, 4))
plot((TotContactDur + 1) ~ sessionDate, ylab = "Total contact duration", xlab = "Julian date", log = "y",  ylim = c(1, max(TotContactDur)))
lines(lowess((TotContactDur + 1) ~ sessionDate))
plot((ELContactDur + 1) ~ sessionDate, ylab = "EL contact duration", xlab = "Julian date", log = "y",  ylim = c(1, max(TotContactDur)))
lines(lowess((ELContactDur + 1) ~ sessionDate))
plot((ELContactNotBedded + 1) ~ sessionDate, ylab = "EL contact duration (not bedded)", xlab = "Julian date", log = "y",  ylim = c(1, max(TotContactDur)))
lines(lowess((ELContactNotBedded + 1) ~ sessionDate))
plot((LLContactDur + 1) ~ sessionDate, ylab = "LL contact duration", xlab = "Julian date", log = "y",  ylim = c(1, max(TotContactDur)))
lines(lowess((LLContactDur + 1) ~ sessionDate))



#---------------------------------------------------------------------------#
#-- Qu 3: Do activity/contact patterns depend on age/reproductive status? --#
#---------------------------------------------------------------------------#
# time spend in ee/el/ey ll/ly / yy contacts (divided by obs time and number)
sessionID <- sessionGrpSz <- TotEEContactDur <- rep(NA, length(levels(factor(full.follows$SessionID))))
TotELContactDur <- TotEYContactDur <- TotLLContactDur <- rep(NA, length(levels(factor(full.follows$SessionID))))
TotLYContactDur <- TotYYContactDur <- pop <- rep(NA, length(levels(factor(full.follows$SessionID))))

for(i in 1:length(sessionID))
{
  k <- subset(full.follows, as.character(SessionID) == as.character(levels(factor(full.follows$SessionID)))[i])
  sessionID[i] <- as.character(levels(factor(full.follows$SessionID)))[i]
  sessionGrpSz[i] <- k$Grpsz[1]
  pop[i] <- as.character(k$Population)[1]
  TotEEContactDur[i] <- sum(na.omit(c(
    k$EE.Body.Duration
  )))
  TotELContactDur[i] <- sum(na.omit(c(k$ELNurseDuration,  
                                    k$EL.Nose.to.nose.Duration,
                                    k$EL.Body.Contact.Duration,
                                    k$EL.Bedded.Duration
  )))
  TotEYContactDur[i] <- sum(na.omit(c(
    k$EY.BC.Duration
  )))
  TotLLContactDur[i] <- sum(na.omit(c(
    k$LL.Body.Duration,
    k$LL.Nose.to.nose.Duration
  )))
  TotLYContactDur[i] <- sum(na.omit(c(
                                      k$LY.BC.Duration,
                                      k$LY.N.to.N.Duration
  )))
  TotYYContactDur[i] <- sum(na.omit(c(
                                    k$YY.BC.Duration,
                                    k$YY.N.to.N.Duration
  )))
}

layout(matrix(c(1, 2, 3, 7, 4, 5, 8, 9, 6), nrow = 3, byrow = T))
par(oma = c(4, 2, 2, 2))
plot(TotEEContactDur + 1 ~ factor(pop), main = "Ewes", ylab = "EE contact duration", xlab = "", log = "y", ylim = c(1, 600), col = "grey70")
mtext(side = 2, line = 4, outer = F, "Ewes")
plot(TotELContactDur + 1 ~ factor(pop), main = "Lambs", ylab = "EL contact duration",  xlab = "", log = "y", ylim = c(1, 600), col = "grey70")
plot(TotEYContactDur + 1 ~ factor(pop), main = "Yearlings", ylab = "EY contact duration", xlab = "", log = "y", ylim = c(1, 600), col = "grey70")
mtext(side = 2, line = 4, outer = T, "Lambs")
plot(TotLLContactDur + 1 ~ factor(pop), xlab = "", ylab = "LL contact duration", log = "y", ylim = c(1, 600), col = "grey70")
plot(TotLYContactDur + 1 ~ factor(pop), xlab = "", ylab = "LY contact duration", log = "y", ylim = c(1, 600), col = "grey70")
mtext(side = 2, line = 4, outer = T, "Yearlings")
plot(TotYYContactDur + 1 ~ factor(pop), xlab = "", ylab = "YY contact duration", log = "y", ylim = c(1, 600), col = "grey70")
mtext(side = 1, outer = T, line = 2, "Contact durations between demographic groups")


#------------------------------------------------------------------------------------------------#
#-- Qu 4: Do activity/contact patterns depend on health status (individual's or population's)? --#
#------------------------------------------------------------------------------------------------#
activity.by.day <- matrix(NA, nrow = 143 - 122, ncol = 7)
for(i in 1:(143 - 122)){
  aso.k <- subset(raw.scans, JulianDate == i + 122 & Population == "Aso")
  if(dim(aso.k)[1] != 0){
  activity.by.day[i, ] <- c(sum(aso.k[ ,18]), sum(aso.k[ ,19]), sum(aso.k[ ,20]), 
                            sum(aso.k[ ,21]), sum(aso.k[ ,22]), sum(aso.k[ ,23]), 
                            sum(aso.k[ ,24])) / (sum(aso.k[, 18:24]))
  } else{
    activity.by.day[i, ] <- rep(NA, 7)
  }
}

activity.by.day <- cbind(activity.by.day, seq(123:143))
par(mfrow = c(1, 1))
plot(activity.by.day[, 1] ~ activity.by.day[, 8], ylab = "Proportion of time spent in activity", xlab = "Day past May 1", ylim = c(0, 1), type = "l")
for(i in 2:7){
  lines(activity.by.day[, i] ~ activity.by.day[, 8], col = i)
}
leg.text <- names(raw.scans)[19:24]
legend("topright", bty = "n", leg.text, col = c(1:7), lty = rep(1, 7))

#---------------------------------------------------#
#-- Q5: How stable are social bonds through time? --#
#---------------------------------------------------#
data <- read.csv("~/work/Kezia/Research/FieldStudies/HCBighornContactStudies/Data/RelocationData/AsotinLocationsFinal_20July2014.csv", header = T, sep = "\t")

data$UTMN <- data$Longitude
data$UTME <- data$Latitude
data$DATE <- data$Date

# look at probability of two animals being in same place at 1-day lag, 2-day lag, etc.
ind <- unique(factor(data$EWEID))
n <-  length(ind) # number of individuals in the dataset

#dyads <-  n * (n - 1) / 2
dyads <- combn(as.character(ind), m = 2)
# TimesTogether <- DyadID <- Ind1 <- Ind2 <- TimesInd1Separate <- TimesInd2Separate <- TotalInd1 <- TotalInd2 <- AssociationIndex <- rep(NA,length(dyads))
# Avec <- rep(NA, dyads * 5)
# dim(Avec) <- c(dyads, 5)
#cooccur <- vector("list", dyads)

dyad.list <- dates.list <- cooccur.list<- vector("list", dim(dyads)[2])
for(k in 1:dim(dyads)[2]){
  #      Avec[k,1] <- ind[i]  # individual 1
  #      Avec[k,2] <- ind[j]  # individual 2
  #      ind1<-ind[i]
  #      ind2<-ind[j]
  
  #-- subset observations for each individual in dyad --#
  
  temp.a<-subset(data,as.character(data$EWEID) == as.character(dyads[1, k]))
  temp.2a<-subset(data,as.character(data$EWEID) == as.character(dyads[2, k]))
  
  #-- dates of all observations on either individual --#
  dates.list[[k]]$dates <- unique(c(as.character(temp.a$Date), as.character(temp.2a$Date)))
  cooccur.list[[k]]$cooccur <- rep(NA, length(dates.list[[k]]$dates))
  dyad.list[[k]]<- data.frame(matrix(nrow = length(dates.list[[k]]$dates), ncol = 14))
  names(dyad.list[[k]]) <- c("Together", "Ind1", "Ind2", "Ind1Repro", "Ind2Repro", "Ind1Hotspot", "Ind2Hotspot", "Ind1Grpsz", "Ind2Grpsz", "DyadInd", "Ind1x", "Ind1y", "Ind2x", "Ind2y")
  dyad.list[[k]]$Ind1 <- rep(as.character(dyads[1, k]), dim(dyad.list[[k]])[1])
  dyad.list[[k]]$Ind2 <- rep(as.character(dyads[2, k]), dim(dyad.list[[k]])[1])
  temp.a.subs <- temp.2a.subs <- vector("list", length(dates.list[[k]]$dates))
  
  #-- on each date, was dyad together? --#
  for(d in 1:length( dates.list[[k]]$dates)){
    temp.a.subs[[d]] <- subset(temp.a, as.character(Date) == dates.list[[k]]$dates[d])
    temp.2a.subs[[d]] <- subset(temp.2a, as.character(Date) == dates.list[[k]]$dates[d])
    temp.a.d <- temp.a.subs[[d]]
    temp.2a.d <- temp.2a.subs[[d]]
    
    dyad.list[[k]]$Together[d] <- ifelse(dim(temp.a.d)[1] == 0 | dim(temp.2a.d)[1] == 0, 0, 
                                         ifelse(temp.a.d$Latitude == temp.2a.d$Latitude & temp.2a.d$Longitude == temp.2a.d$Longitude, 1, 0
                                         ))
    dyad.list[[k]]$Ind1Grpsz[d] <- ifelse(dim(temp.a.d)[1] == 0, NA, temp.a.d$Grpsz[1])
    dyad.list[[k]]$Ind2Grpsz[d] <- ifelse(dim(temp.2a.d)[1] == 0, NA, temp.2a.d$Grpsz[1])
    dyad.list[[k]]$Ind1Repro[d] <- ifelse(dim(temp.a.d)[1] == 0, NA, as.character(temp.a.d$Lamb.Status[1]))
    dyad.list[[k]]$Ind2Repro[d] <- ifelse(dim(temp.2a.d)[1] == 0, NA, as.character(temp.2a.d$Lamb.Status[1]))
    dyad.list[[k]]$Ind1x[d] <- ifelse(dim(temp.a.d)[1] == 0, NA, temp.a.d$Latitude)
    dyad.list[[k]]$Ind1y[d] <- ifelse(dim(temp.a.d)[1] == 0, NA, temp.a.d$Longitude)
    dyad.list[[k]]$Ind2x[d] <- ifelse(dim(temp.2a.d)[1] == 0, NA, temp.2a.d$Latitude)
    dyad.list[[k]]$Ind2y[d] <- ifelse(dim(temp.2a.d)[1] == 0, NA, temp.2a.d$Longitude)
#    dyad.list[[k]]$Ind1Hotspot[d] <- ifelse(dim(temp.a.d)[1] == 0, NA, contour.ind[min(which(contour.density$x >= dyad.list[[k]]$Ind1x[d])), min(which(contour.density$y >= dyad.list[[k]]$Ind1y[d]))])
#    dyad.list[[k]]$Ind2Hotspot[d] <- ifelse(dim(temp.2a.d)[1] == 0, NA, contour.ind[min(which(contour.density$x >= dyad.list[[k]]$Ind2x[d])), min(which(contour.density$y >= dyad.list[[k]]$Ind2y[d]))])
    dyad.list[[k]]$DyadInd[d] <- k
  }
}  

full.data <- do.call("rbind", dyad.list)
head(full.data)
dim(full.data)
full.data$Ind1 <- as.factor(full.data$Ind1)
full.data$Ind2 <- as.factor(full.data$Ind2)

require(lme4)

together1 <- glmer(Together ~ Ind1Grpsz * Ind2Grpsz + (1 | Ind1) + (1 | Ind2) + (1 |DyadInd), family = "binomial", data = full.data)
summary(together1)

#--- time-lags on bond breaks --#
AssociationData <- out[[2]]
AssociationData$TimesTogether <- as.numeric(as.character(AssociationData$TimesTogether))
AssociationData$TotalInd1 <- as.numeric(as.character(AssociationData$TotalInd1))
AssociationData$TotalInd2 <- as.numeric(as.character(AssociationData$TotalInd2))
AssociationData$AssociationIndex <- AssociationData$TimesTogether / (AssociationData$TotalInd1 + AssociationData$TotalInd2 - AssociationData$TimesTogether)
par(mfrow = c(1, 1))
hist(AssociationData$AssociationIndex, col = "grey90", xlab = "Association Index for all Asotin dyads", ylab = "Frequency", main = "")

#-- autocorrelation in dyad-togetherness --#
#-- for each dyad, need longitudinal sequence of together/not --#
#-- then, look at variation in autocorrelation functions --#
#-- then, get mean-field autocorrelation function --#

#-- consider regressing dyad autocorrelation on association (or model fission separate from fusion?) --#
#-- consider imputing days when neither ewe was observed --#

#-- build two matrices that are dyads by days --#
together.mat <- matrix(NA, ncol = dim(AssociationData)[1], nrow = max(data$JulianDate) - min(data$JulianDate))
for(j in 1:dim(together.mat)[1]){
  today <- subset(data, JulianDate == (j + 120))
  for(i in 1:dim(AssociationData)[1]){
    ind1sub <- subset(today, as.character(EWEID) == as.character(AssociationData$Ind1[i]))
    ind2sub <- subset(today, as.character(EWEID) == as.character(AssociationData$Ind2[i]))
    if(dim(ind1sub)[1] == 0){
      if(dim(ind2sub)[1] == 0){
        together.mat[j, i] <- NA
      } else{
        together.mat[j, i] <- 0
      }
    } else{
      if(dim(ind2sub)[1] == 0){
        together.mat[j, i] <- NA
      } else{
        together.mat[j, i] <- ifelse(ind1sub$Latitude[1] == ind2sub$Latitude[1] & ind1sub$Longitude[1] == ind2sub$Longitude[1], 1, 0)
      }
    }
  }  
}

change.mat <- fiz.mat <- fuz.mat <- matrix(NA, ncol = dim(AssociationData)[1], nrow = max(data$JulianDate) - min(data$JulianDate))
for(i in 1:dim(AssociationData)[1]){
  for(j in 2:dim(together.mat)[1]){
    change.mat[j, i] <- ifelse(together.mat[j, i] != together.mat[j - 1, i], 1, 0)
  }
}

#-- ratio of 0's to 1's throughout whole change.mat should give me probability of fiz/fuz per day --#
table(change.mat)
prob.change <- 1817 / (8308 + 1817)
  # prob.change = .179
  # 1 / prob.change = 5.57

prob.change <- 260 / (917 + 260)
#-- 1/.221  = 4.5249 --#


#------------------------------------------#
#-- Q6: How variable is sheep sociality? --#
#------------------------------------------#


#--------------------------------------------------------------------------------#
#-- Q7: What proportion of var in epi size could be accounted for by behavior? --#
#--------------------------------------------------------------------------------#
