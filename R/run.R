# run sheep behavior analyses

source("./R/load.R")
# loads all required datasets and packages

source("./R/clean.R")
# cleans all datasets
# datasets named: clean.scans, clean.follows

source("./R/sourcefuns.R")
# sources in all analysis functions

# Build plots
PlotScanTimeByDateByPop(data.in = clean.scans)
PlotActivityByContacts(follows.in = clean.follows, scans.in = clean.scans)

