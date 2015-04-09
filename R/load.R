# load datasets

# Relocations
aso.locations <- read.csv("./data/Asotin_Locations_Clean.csv", header = T, sep = "\t")
bb.relocs <- read.csv("./data/BlackButte_Locations_Clean.csv", header = T, sep = "\t")

# Follows
aso.follows <- read.csv("./data/CleanAsotinFollows_090214.csv", header = T)
follows <- read.csv("~/Dropbox/HCSummer2014/FocalFollows/FullCleanFollows_2014.csv", header = T)
bb.follows <- read.csv("./data/CleanBlackButteFollows_090214.csv", header = T)
names(bb.follows)[40] <- "YY.N.to.N.Count"
full.follows <- as.data.frame(rbind(aso.follows, bb.follows))
names(full.follows) <- names(aso.follows)

# Scans
scans <- read.csv("~/Dropbox/HCSummer2014/ScanSamples/ScansToParse_150324.csv", header = T)

# require necessary packages
require(igraph)
require(assortnet)
