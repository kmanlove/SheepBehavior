# cleaning

# source cleaning functions

# relocations to associations
source("./R/StaticNetworkAssocMat.R")

# follows
source("./R/CleanRawFollows.R")

# scans
source("./R/CleanRawScans.R")

# clean relocations

# clean follows
clean.follows <- CleanRawFollows(follows)

# clean scans
# add time fields to raw scans
scans$TimePosix0 <- strptime(scans$Time, "%H:%M:%S %p", tz = "")
scans$TimePosix <- as.numeric(strptime(scans$Time, "%I:%M:%S %p", tz = ""))
scans$TimePosix2 <- as.numeric(strptime(scans$Time, "%l"))

# convert raw scans to individual compiled scans
clean.scans <- CleanRawScans(scans)
