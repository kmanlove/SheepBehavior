# run sheep behavior analyses

source("./R/load.R")
# loads all required datasets and packages

source("./R/clean.R")
# cleans all datasets
# datasets named: clean.scans, clean.follows, clean.relocs

source("./R/sourcefuns.R")
# sources in all analysis functions

# Build Aso network from 2014
ewes.without.lambs.2014 <- c("14AS01", "13AS05", "14AS44", "14AS25", "14AS22")
yearlings.2014 <- c("14AS51", "14AS15", "14AS18", "14AS20", "14AS21", "14AS45")
aso.2014.graph <- BuildAsoRelocNetworks(clean.relocs, ewes.without.lambs = ewes.without.lambs.2014, yearlings = yearlings.2014)
par(mfrow = c(1, 1))
plot(aso.2014.graph[[1]])

# Build Scan plots
PlotScanTimeByDateByPop(data.in = clean.scans)
PlotActivityByContacts(follows.in = clean.follows, scans.in = clean.scans)
PlotActivityByDemogGrp(data.in = clean.scans)
