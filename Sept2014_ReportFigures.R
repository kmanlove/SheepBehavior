#---------------------------------------------------------------------#
#-- code for preliminary analysis of bhs contact patterns from 2014 --#
#---------------------------------------------------------------------#

bb <- read.csv("./data/CleanBlackButteFollows_090214.csv", header = T)
aso <- read.csv("./data/CleanAsotinFollows_090214.csv", header = T)

length(levels(bb$SessionID))
length(levels(aso$SessionID))
length(levels(aso$ID))

aso.relocs <- read.csv("./data/Asotin_Locations_Clean.csv", header = T, sep = "\t")
bb.relocs <- read.csv("./data/BlackButte_Locations_Clean.csv", header = T, sep = "\t")
dim(aso.relocs)
dim(bb.relocs)

#--------------------------------------------------------------------------------#
#-- Q0: How do contact patterns and group sizes change over the timescale of a --#
#-- lamb pneumonia event? -------------------------------------------------------#
#--------------------------------------------------------------------------------#

# group sizes
par(mfrow = c(1, 2))
plot(bb$Grpsz ~ bb$JulianDate, ylim = c(0, 40), xlim = c(120, 200), xlab = "Time", ylab = "Total group size", xaxt = "n")
lines(lowess(bb$Grpsz ~ bb$JulianDate), lwd = 2)
points(aso$Grpsz ~ aso$JulianDate, col = "red")
lines(lowess(aso$Grpsz ~ aso$JulianDate), col = "red", lwd = 2)
axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))
leg.text <- c("Black Butte", "Asotin")
legend("topleft", bty = "n", leg.text, col = c("black", "red"), pch = c(1, 1), lwd = c(2, 2))

plot((bb$Ewes + bb$YrEwes) ~ bb$JulianDate, ylim = c(0, 25), xlim = c(120, 200), xlab = "Time", ylab = "Group size without lambs", xaxt = "n")
lines(lowess((bb$Ewes + bb$YrEwes) ~ bb$JulianDate), lwd = 2)
points((aso$Ewes + aso$YrEwes) ~ aso$JulianDate, col = "red")
lines(lowess((aso$Ewes + aso$YrEwes) ~ aso$JulianDate), col = "red", lwd = 2)
axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))

# with boxplots
par(mfrow = c(2, 2))
boxplot(bb$Grpsz ~ bb$JulianDate, main = "Black Butte", ylab = "Total group size", xlab = "Time (Julian date: 120 = May 01, 150 = June 01, 181 = July 01)")
boxplot(aso$Grpsz ~ aso$JulianDate, main = "Asotin", ylab = "Total group size", xlab = "Time (Julian date: 120 = May 01, 150 = June 01, 181 = July 01)")

boxplot((bb$Ewes + bb$YrEwes) ~ bb$JulianDate, main = "Black Butte", ylab = "Group size without lambs", xlab = "Time (Julian date: 120 = May 01, 150 = June 01, 181 = July 01)")
boxplot((aso$Ewes + aso$YrEwes) ~ aso$JulianDate, main = "Asotin", ylab = "Group size without lambs", xlab = "Time (Julian date: 120 = May 01, 150 = June 01, 181 = July 01)")

# within-group contacts
# dams and lambs
aso.dams <- subset(aso, !(ID %in% c("150.02", "150.07", "150.36", "151.21", "151.22", "151.232", "151.871", "Floppy", "Orange15", "Orange18", "Orange21", "Yellow41", "White45", "White51", "White52", "White left")))
aso.elcontacts <- subset(aso.dams, EL.Body.Contact.Duration >= 0.1 | ELNurseDuration >= 0.1)
aso.llcontacts <- subset(aso.dams, LL.Body.Duration >= 0.1)
bb.elcontacts <- subset(bb, EL.Body.Contact.Duration >= 0.1 | ELNurseDuration >= 0.1)
bb.llcontacts <- subset(bb, LL.Body.Duration >= 0.1)

par(mfrow = c(1, 2))
plot(log(bb$EL.Body.Contact.Duration + bb$ELNurseDuration + 1) ~ bb$JulianDate, ylim = c(0, 5.5), xlim = c(120, 200), pch = 16, xlab = "Time", ylab = "log(Ewe-lamb body + nursing contacts)", xaxt = "n")
lines(lowess(log(bb.elcontacts$EL.Body.Contact.Duration + bb.elcontacts$ELNurseDuration + 1) ~ bb.elcontacts$JulianDate), col = "black", lwd = 2)
points(log(aso.dams$EL.Body.Contact.Duration + aso.dams$ELNurseDuration + 1) ~ aso.dams$JulianDate, col = "red")
lines(lowess(log(aso.elcontacts$EL.Body.Contact.Duration + aso.elcontacts$ELNurseDuration + 1) ~ aso.elcontacts$JulianDate), col = "red", lwd = 2)
axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))
leg.text <- c("Black Butte", "Asotin")
legend("topright", leg.text, col = c("black", "red"), pch = c(16, 1), bty = "n")

plot(log(bb$LL.Body.Duration + 1) ~ bb$JulianDate, ylim = c(0, 5.5), xlim = c(120, 200), pch = 16, xlab = "time", ylab = "log(Lamb-lamb contacts + 1)", xaxt = "n")
lines(lowess(log(bb.llcontacts$LL.Body.Duration) ~ bb.llcontacts$JulianDate), col = "black", lwd = 2)
points(log(aso.dams$LL.Body.Duration + 1) ~ aso.dams$JulianDate, col = "red")
lines(lowess(log(aso.llcontacts$LL.Body.Duration) ~ aso.llcontacts$JulianDate), col = "red", lwd = 2)
axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))

# nursing through time 
bb.nurse <- subset(bb, ELNurseDuration >= 0.1)
aso.nurse <- subset(aso, ELNurseDuration >= 0.1)
par(mfrow = c(1, 1))
plot(log(bb$ELNurseDuration + 1) ~ bb$JulianDate, ylim = c(0, 5.5), xlim = c(120, 200), pch = 16, xlab = "Time", ylab = "log(nursing time)", xaxt = "n")
points(log(aso$ELNurseDuration + 1) ~ aso$JulianDate, col = "red")
lines(lowess(log(aso.nurse$ELNurseDuration + 1) ~ aso.nurse$JulianDate), col = "red")
lines(lowess(log(bb.nurse$ELNurseDuration + 1) ~ bb.nurse$JulianDate), col = "black")
axis(side = 1, at = c(120, 150, 181), labels = c("May 01", "June 01", "July 01"))
leg.text <- c("Black Butte", "Asotin")
legend("topright", leg.text, col = c("black", "red"), pch = c(16, 1), bty = "n")


