#-- follow data prep --#

FollowsDataPrep <- function(data.in)
{
  NumericTime <- rep(NA, dim(data.in)[1])
  for(i in 1:dim(data.in)[1]){
    NumericTime[i] <- as.POSIXlt(strptime(as.character(data.in$Time[i]), format = "%H:%M %p"))
  }
  
}

#-- scan data prep --#

scans <- read.csv("~/Dropbox/HCSummer2014/ScanSamples/ScansToParse_Unformatted.csv", header = T)

# separate final column on spaces

# specify all levels for group/activity
levels.in <- c("eu", "ed", "es", "eg", "ew", "ev", "en", 
               "lu", "ld", "ls", "lg", "lw", "lv", "ln", "lp", 
               "yu", "yd", "ys", "yg", "yw", "yv",
)

i <- 1

scans[i, ]$Responses
table(factor(strsplit(x = as.character(scans[i, ]$Responses), split = " ")[[1]], levels = levels.in))
