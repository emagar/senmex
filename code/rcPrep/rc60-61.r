rm(list = ls())
#
## SET YOUR WORKING DIRECTORY HERE (SAVE DATA FILES IN THIS DIRECTORY)
workdir <- c("~/Dropbox/data/rollcall/senMex")
setwd(workdir)

## # if only loading saved data
## load(file=paste(workdir, "data/votesForWeb", "rc60.RData", sep="/"))

## READ csv export with Cantú's Diario information (cleaned separately in excel)
d <- read.csv("data/raw/all.work.csv")
d$leg <- sub("^([0-9]+)_[0-9]", "\\1", d$ses)
d$leg <- as.numeric(d$leg)

## Restrict to 60 and 61 legs
d <- d[d$leg %in% c(60,61),]

## READ senator info
sendat <- read.csv("data/senadores/sen60-61.csv")
head(sendat)

## Included votes
votdat <- d[d$info=="record",]
votdat$info <- NULL
votdat$txt <- NULL

## summarize senator's names
n <- d[-which(d$info %in% c("record","VOT"))]
n <- unique(d$txt)
n <- n[order(n)] ## alphabetize
write.csv(n, file = "tmp.csv") ## 

