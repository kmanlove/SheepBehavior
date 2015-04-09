# load datasets

# Relocations
aso.locations <- read.csv("./data/Asotin_Locations_Clean.csv", header = T, sep = "\t")

# Follows
aso.follows <- read.csv("./data/CleanAsotinFollows_090214.csv", header = T)
follows <- read.csv("~/Dropbox/HCSummer2014/FocalFollows/FullCleanFollows_2014.csv", header = T)

# Scans
scans <- read.csv("~/Dropbox/HCSummer2014/ScanSamples/ScansToParse_150324.csv", header = T)

# require necessary packages
require(igraph)
require(assortnet)
