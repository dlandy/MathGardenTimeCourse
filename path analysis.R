# requires ########################
require(plyr)
require(ggplot2)

# Load MathsGarden Data ###########

require(data.table)
wd <- "/Users/dlandy/Dropbox/Projects/Math\ Garden/"
setwd(wd)

#source("/Users/dlandy/Dropbox/Operation Order (shared)/rekenvolgorde.r")
source(paste(wd, "buildItems.R", sep=""))
source(paste(wd, "loadMathGardenDataSets.R", sep="/"))

# Load Data:
outcome = loadMathGardenDataSets(0.0001)

log27Reduced = outcome[[1]]






# Reduce to spacing cases

