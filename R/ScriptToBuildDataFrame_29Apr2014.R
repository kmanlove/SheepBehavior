#-- Asotin spatial node identification --#
filepath <- "~/work/Kezia/Research/EcologyPapers/ClustersAssocations_V2/ClustersAssociations/Data/"

read.path <- "~/work/Kezia/Research/FieldStudies/ContactStudy/Analysis/Data/Asotin_Locations_2013_Clean.csv"
data <- read.csv(paste(read.path), header = T, sep = ",")
data$UTMN <- data$Longitude
data$UTME <- data$Latitude
data$DATE <- data$Date

#-- pull out three ram records from Asotin, as well as uncollareds --#
ram.ids <- c("08AS31", "12AS19", "12AS23", "12AS22", "11AS06", "08AS34", "12AS21", "")
data <- subset(data, !(EWEID %in% ram.ids))

sumimvewes <- eweids <- levels(factor(data$EWEID))

names(data)

plot(data$UTMN, data$UTME, pch = 16)
ewe150.260 <- subset(data, ID == "150.260_11AS13")
require(MASS)
contour.density <- kde2d(data$UTME, data$UTMN, n = 25, lims = c(range(data$UTME), range(data$UTMN)))
plot(data$UTMN, data$UTME)
contour(contour.density, levels = quantile(contour.density$z, 0.9))
points(ewe150.260$UTMN ~ ewe150.260$UTME)

#-- Month-by-month --#
biwk1.data <- subset(data, JulianDate <= 138)
biwk2.data <- subset(data, JulianDate <= 152 & JulianDate >= 138)
biwk3.data <- subset(data, JulianDate <= 166 & JulianDate >= 152)
biwk4.data <- subset(data, JulianDate <= 180 & JulianDate >= 166)
biwk5.data <- subset(data, JulianDate <= 194 & JulianDate >= 180)
biwk6.data <- subset(data, JulianDate <= 208 & JulianDate >= 194)

biwk1.kde <- kde2d(biwk1.data$UTME, biwk1.data$UTMN, n = 25, lims = c(range(data$UTME), range(data$UTMN)))
biwk2.kde <- kde2d(biwk2.data$UTME, biwk2.data$UTMN, n = 25, lims = c(range(data$UTME), range(data$UTMN)))
biwk3.kde <- kde2d(biwk3.data$UTME, biwk3.data$UTMN, n = 25, lims = c(range(data$UTME), range(data$UTMN)))
biwk4.kde <- kde2d(biwk4.data$UTME, biwk4.data$UTMN, n = 25, lims = c(range(data$UTME), range(data$UTMN)))
biwk5.kde <- kde2d(biwk5.data$UTME, biwk5.data$UTMN, n = 25, lims = c(range(data$UTME), range(data$UTMN)))
biwk6.kde <- kde2d(biwk6.data$UTME, biwk6.data$UTMN, n = 25, lims = c(range(data$UTME), range(data$UTMN)))


par(mfrow = c(2, 3))
contour(biwk1.kde, levels = quantile(biwk1.kde$z, 0.9))
points(biwk1.data$UTMN ~ biwk1.data$UTME)
contour(biwk2.kde, levels = quantile(biwk2.kde$z, 0.9))
points(biwk2.data$UTMN ~ biwk2.data$UTME)
contour(biwk3.kde, levels = quantile(biwk3.kde$z, 0.9))
points(biwk3.data$UTMN ~ biwk3.data$UTME)
contour(biwk4.kde, levels = quantile(biwk4.kde$z, 0.9))
points(biwk4.data$UTMN ~ biwk4.data$UTME)
contour(biwk5.kde, levels = quantile(biwk5.kde$z, 0.9))
points(biwk5.data$UTMN ~ biwk5.data$UTME)
contour(biwk6.kde, levels = quantile(biwk6.kde$z, 0.9))
points(biwk6.data$UTMN ~ biwk6.data$UTME)

plot(biwk2.data$UTMN, biwk2.data$UTME, pch = 16)
plot(biwk3.data$UTMN, biwk3.data$UTME, pch = 16)
plot(biwk4.data$UTMN, biwk4.data$UTME, pch = 16)
plot(biwk5.data$UTMN, biwk5.data$UTME, pch = 16)
plot(biwk6.data$UTMN, biwk6.data$UTME, pch = 16)


#-- build contour indicator map --#
contour.ind <- matrix(NA, nrow = 25, ncol = 25)
for(i in 1:25){
  for(j in 1:25){
    if(contour.density$z[i, j] < quantile(contour.density$z, 0.9)){
      contour.ind[i, j] <- 0
    } else if(contour.density$x[i] <= 46.25 & contour.density$y[j] <= -117.36){
      contour.ind[i, j] <- 1
    } else if(contour.density$x[i] <= 46.285 & contour.density$y[j] <= -117.28){
      contour.ind[i, j] <- 2
    } else{
      contour.ind[i, j] <- 3
    }
  }
}

contour.ind

data$Hotspot <- rep(NA, dim(data)[1])
for(d in 1:dim(data)[1]){
  data$Hotspot[d] <- contour.ind[min(which(contour.density$x >= data$Latitude[d])), min(which(contour.density$y >= data$Longitude[d]))]
}

#-- build dataset --------------------------------------------------------------#
#-- responses are Bernouli: Was each dyad together on each observation event? --#

ind <- unique(factor(data$ID))
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
  
  temp.a<-subset(data,as.character(data$ID) == as.character(dyads[1, k]))
  temp.2a<-subset(data,as.character(data$ID) == as.character(dyads[2, k]))
  
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
    dyad.list[[k]]$Ind1Hotspot[d] <- ifelse(dim(temp.a.d)[1] == 0, NA, contour.ind[min(which(contour.density$x >= dyad.list[[k]]$Ind1x[d])), min(which(contour.density$y >= dyad.list[[k]]$Ind1y[d]))])
    dyad.list[[k]]$Ind2Hotspot[d] <- ifelse(dim(temp.2a.d)[1] == 0, NA, contour.ind[min(which(contour.density$x >= dyad.list[[k]]$Ind2x[d])), min(which(contour.density$y >= dyad.list[[k]]$Ind2y[d]))])
    dyad.list[[k]]$DyadInd[d] <- k
  }
}  

full.data <- do.call(rbind, dyad.list)
head(full.data)
dim(full.data)

#----------------------------------------#
#-- Initial logistic regression models --#
#----------------------------------------#
require(lme4)
together1 <- glmer(Together ~ Ind1Grpsz * Ind2Grpsz + Ind1Hotspot + Ind2Hotspot + (1 | Ind1) + (1 | Ind2) + (1 |DyadInd), family = "binomial", data = full.data)
summary(together1)


#-- which is most important in determining dyad togetherness: propensity of individuals to be 
#-- in hotspots or propensity of dyad to be together? --#

#-- get a hotspot preference score for each individual --#
data$IndHotspotPref <- rep(NA, dim(data)[1])
for(i in 1:dim(data)[1]){
  k <- subset(data, as.character(ID) == as.character(data$ID)[i])
  data$IndHotspotPref[i] <- length(which(k$Hotspot >= 1)) / dim(k)[1]
}

full.data$Ind1HotspotPref <- full.data$Ind2HotspotPref <- rep(NA, dim(full.data)[1])
for(i in 1:dim(full.data)[1]){
  ind1 <- subset(data, as.character(ID) == as.character(full.data$Ind1)[i])
  full.data$Ind1HotspotPref[i] <- ind1$IndHotspotPref[1]
  ind2 <- subset(data, as.character(ID) == as.character(full.data$Ind1)[i])
  full.data$Ind2HotspotPref[i] <- ind2$IndHotspotPref[1]
}

full.data$DyadPref <- rep(NA, dim(full.data)[1])
for(i in 1:dim(full.data)[1]){
  k <- subset(full.data, DyadInd == full.data$DyadInd[i])
  full.data$DyadPref[i] <- length(which(k$Together == 1)) / dim(k)[1]
}

data$Individ.Grpsz <- rep(NA, dim(data)[1])
for(i in 1:dim(data)[1]){
  k <- subset(data, as.character(ID) == as.character(data$ID)[i])
  data$Individ.Grpsz[i] <- mean(na.omit(k$Grpsz))
}

full.data$Ind1Grpsz <- full.data$Ind2Grpsz <- rep(NA, dim(full.data)[1])
for(i in 1:dim(full.data)[1]){
  ind1 <- subset(data, as.character(ID) == as.character(full.data$Ind1)[i])
  full.data$Ind1Grpsz[i] <- ind1$Individ.Grpsz[1]
  ind2 <- subset(data, as.character(ID) == as.character(full.data$Ind1)[i])
  full.data$Ind2Grpsz[i] <- ind2$Individ.Grpsz[1]
}

hotspot.only <- glm(Together ~ as.numeric(Ind1HotspotPref) * as.numeric(Ind2HotspotPref) , family = "binomial", data = full.data)
dyad.only <- glm(Together ~ as.numeric(DyadPref) , family = "binomial", data = full.data)
hotspot.dyad <- glm(Together ~ as.numeric(Ind1HotspotPref) * as.numeric(Ind2HotspotPref) + as.numeric(DyadPref), family = "binomial", data = full.data)
hotspot.dyad.grpsz <- glm(Together ~ as.numeric(Ind1HotspotPref) * as.numeric(Ind2HotspotPref) + as.numeric(DyadPref) + as.numeric(Ind1Grpsz) * as.numeric(Ind2Grpsz), family = "binomial", data = full.data)
dyad.grpsz <- glm(Together ~ as.numeric(DyadPref) + as.numeric(Ind1Grpsz) * as.numeric(Ind2Grpsz), family = "binomial", data = full.data)
summary(hotspot.only)
summary(dyad.only)
summary(hotspot.dyad)
summary(hotspot.dyad.grpsz)
summary(dyad.grpsz)


#-- are group sizes different in hotspots than out of them?  
boxplot(data$Grpsz ~ data$Hotspot)

#--------------------------#
#-- JAGS set-up and call --#
#--------------------------#
n.individs <- length(levels(unique(factor(as.character(full.data$Ind1)), factor(as.character(full.data$Ind2)))))
n.hotspots <- length(levels(factor(full.data$Ind1Hotspot)))

