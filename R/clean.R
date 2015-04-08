# cleaning

# source functions to covert relocations to associations

source("./R/StaticNetworkAssocMat.R")

# clean scans
scans$TimePosix0 <- strptime(scans$Time, "%H:%M:%S %p", tz = "")
scans$TimePosix <- as.numeric(strptime(scans$Time, "%I:%M:%S %p", tz = ""))
scans$TimePosix2 <- as.numeric(strptime(scans$Time, "%l"))
