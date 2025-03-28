## clean mem
rm(list=ls())

## loads and explores Senate roll call votes
workdir <- c("~/Dropbox/data/rollcall/senMex")
setwd(workdir)

## get 58-59 and rename objects
load(file=paste(workdir, "data/votes-for-web", "rc58-59.RData", sep="/"))
sendat58.59 <- sendat
votdat58.59 <- votdat
rc58.59 <- rc

## get 60-61 and rename objects
load(file=paste(workdir, "data/votes-for-web", "rc60-61.RData", sep="/"))
sendat60.61 <- sendat
votdat60.61 <- votdat
rc60.61 <- rc

rm(raw, raw.long, rc, sendat, votdat)

## rice cohesion
sel <- which(sendat58.59$part=="pan")
tmp <- rc58.59[,sel]
yes <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.coh.pan.58.59 <- mean(abs(yes-no)/(yes+no))
##
sel <- which(sendat60.61$part=="pan")
tmp <- rc60.61[,sel]
yes <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.coh.pan.60.61 <- mean(abs(yes-no)/(yes+no))

sel <- which(sendat58.59$part=="pri")
tmp <- rc58.59[,sel]
yes <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.coh.pri.58.59 <- mean(abs(yes-no)/(yes+no))
##
sel <- which(sendat60.61$part=="pri")
tmp <- rc60.61[,sel]
yes <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.coh.pri.60.61 <- mean(abs(yes-no)/(yes+no))

sel <- which(sendat58.59$part=="prd")
tmp <- rc58.59[,sel]
yes <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.coh.prd.58.59 <- mean(abs(yes-no)/(yes+no))
##
sel <- which(sendat60.61$part=="prd")
tmp <- rc60.61[,sel]
yes <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
sel2 <- which(yes+no>0)
rice.coh.prd.60.61 <- mean(abs(yes[sel2]-no[sel2])/(yes[sel2]+no[sel2]))
##
round(rice.coh.pan.58.59, 2)
round(rice.coh.pan.60.61, 2)
round(rice.coh.pri.58.59, 2)
round(rice.coh.pri.60.61, 2)
round(rice.coh.prd.58.59, 2)
round(rice.coh.prd.60.61, 2)

## rice dissimilarity
sel1 <- which(sendat58.59$part=="pan")
sel2 <- which(sendat58.59$part=="pri")
tmp <- rc58.59[,sel1]
yes1 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no1 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
tmp <- rc58.59[,sel2]
yes2 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no2 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.dis.pan.pri.58.59 <- mean(abs(yes1/(yes1+no1) - yes2/(yes2+no2)))

sel1 <- which(sendat60.61$part=="pan")
sel2 <- which(sendat60.61$part=="pri")
tmp <- rc60.61[,sel1]
yes1 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no1 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
tmp <- rc60.61[,sel2]
yes2 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no2 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.dis.pan.pri.60.61 <- mean(abs(yes1/(yes1+no1) - yes2/(yes2+no2)))

sel1 <- which(sendat58.59$part=="pan")
sel2 <- which(sendat58.59$part=="prd")
tmp <- rc58.59[,sel1]
yes1 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no1 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
tmp <- rc58.59[,sel2]
yes2 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no2 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
rice.dis.pan.prd.58.59 <- mean(abs(yes1/(yes1+no1) - yes2/(yes2+no2)))

sel1 <- which(sendat60.61$part=="pan")
sel2 <- which(sendat60.61$part=="prd")
tmp <- rc60.61[,sel1]
yes1 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no1 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
tmp <- rc60.61[,sel2]
yes2 <- apply(tmp, 1, function(x) sum(x==1, na.rm = TRUE))
no2 <- apply(tmp, 1, function(x) sum(x==-1, na.rm = TRUE))
sel3 <- which(yes2+no2>0)
rice.dis.pan.prd.60.61 <- mean(abs(yes1[sel3]/(yes1[sel3]+no1[sel3]) - yes2[sel3]/(yes2[sel3]+no2[sel3])))


sel1 <- which(sendat60.61$part=="pan")
sel2 <- which(sendat60.61$part=="pri")



ls()


