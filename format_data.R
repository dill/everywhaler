# format the data

library(lubridate)
library(tools)

# data downloaded from
#  https://www.whalingmuseum.org/online_exhibits/crewlist/crewlist.csv
# Sat 13 Aug 2016 20:11:41 EDT

## load the data
whalers <- read.csv("crewlist.csv", stringsAsFactors=FALSE)



## deal with some dates
whalers$ApproximateDeparture[whalers$ApproximateDeparture==""] <- NA
# what doesn't get parsed
na_ind <- is.na(mdy(whalers$ApproximateDeparture))
# some dates are of the form 4/0/1840, make those be first of month
zero_date <- grepl("\\d+/0/\\d{4}", whalers$ApproximateDeparture)
whalers$ApproximateDeparture[zero_date] <-
  sub("(\\d+)/0/(\\d{4})", "\\1/1/\\2", whalers$ApproximateDeparture[zero_date])
# others have missing slashes e.g. 10/71829
noslash_date <- grepl("\\d+/\\d+\\d{4}", whalers$ApproximateDeparture)
whalers$ApproximateDeparture[noslash_date] <-
  sub("(\\d+)/(\\d+)(\\d{4})", "\\1/\\2/\\3",
      whalers$ApproximateDeparture[noslash_date])
# now we can parse the dates
whalers$ApproximateDeparture <- mdy(whalers$ApproximateDeparture)


## make the vessel numbers numeric
whalers$VesselNumber[whalers$VesselNumber==""] <- NA
whalers$VesselNumber <- as.numeric(whalers$VesselNumber)

## make ages numeric
whalers$Age[whalers$Age==""] <- NA
whalers$Age <- as.numeric(whalers$Age)

## tidy up the rig field
whalers$Rig <- toTitleCase(trimws(whalers$Rig))
whalers$Rig[whalers$Rig == ""] <- NA
whalers$Rig <- as.factor(whalers$Rig)


## parse the insane heights
# thanks to James Curran for this code
source("heights.R")
# returns a vector hh that has the heights in it
# second column is height in mm, convert to m
whalers$Height <- hh[,2]/1000
rm(hh)

save(whalers, file="whalers.RData")

