# cleaning

# source cleaning functions

# relocations to associations
source("./R/StaticNetworkAssocMat.R")

# scans
source("./R/CleanRawScans.R")

# clean scans
# add time fields to raw scans
scans$TimePosix0 <- strptime(scans$Time, "%H:%M:%S %p", tz = "")
scans$TimePosix <- as.numeric(strptime(scans$Time, "%I:%M:%S %p", tz = ""))
scans$TimePosix2 <- as.numeric(strptime(scans$Time, "%l"))

# convert raw scans to individual compiled scans
clean.scans <- CleanRawScans(scans)
