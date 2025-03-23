rm(list = ls())
#
## SET YOUR WORKING DIRECTORY HERE (SAVE DATA FILES IN THIS DIRECTORY)
workdir <- c("~/Dropbox/data/rollcall/senMex")
setwd(workdir)

## # if only loading saved data
## load(file=paste(workdir, "data/votesForWeb", "rc60.RData", sep="/"))

## READ csv export with Cantú's Diario information (cleaned separately in excel)
d <- read.csv("data/raw/all.work.csv")
d <- d[order(d$drop, d$ord),] ## sort
d$leg <- sub("^([0-9]+)_[0-9]", "\\1", d$ses)
d$leg <- as.numeric(d$leg)

## Restrict to 58 and 59 legs
d <- d[d$leg %in% c(58,59),]

## info selector
d$vot <- d$pty <- d$sen <- NA
tmp <- d[-which(d$info=="record"),] # pick rows other than records
tmp$info[seq(from=1, to=nrow(tmp), by=3)] <- "SEN" # every third row starting in 1 should only have senators 
tmp$info[seq(from=2, to=nrow(tmp), by=3)] <- "PTY" # every third row starting in 2 should only have parties 
table(tmp$txt[tmp$info=="PTY"]) # verify
tmp$sen[tmp$info=="SEN"] <- tmp$txt[tmp$info=="SEN"]
tmp$pty[tmp$info=="SEN"] <- tmp$txt[tmp$info=="PTY"] ## lag 1
tmp$vot[tmp$info=="SEN"] <- tmp$txt[tmp$info=="VOT"] ## lag 2
## keep only obs with record
tmp <- tmp[tmp$info=="SEN",]
## clean
tmp$ses <- tmp$txt <- tmp$drop <- tmp$file <- tmp$fch <- tmp$tit <- tmp$info <- tmp$check <- tmp$leg <- NULL
tmp[1,]
## rename: raw will be where votes are extracted from
raw <- tmp
head(raw)
rm(tmp)

## READ senator info
sendat <- read.csv("data/senadores/sen58-59.csv")
sendat$pila   <- sub("^ +| +$", "", sendat$pila)   ## drop heading and trailing spaces
sendat$pila   <- sub(" +", " ", sendat$pila)       ## drop double+ spaces
sendat$patmat <- sub("^ +| +$", "", sendat$patmat) ## drop heading and trailing spaces
sendat$patmat <- sub(" +", " ", sendat$patmat)     ## drop double+ spaces
sendat$nom <- with(sendat, paste(patmat, pila))
head(sendat)

## Included votes
votdat <- d[d$info=="record",]
votdat$info <- NULL
votdat$txt <- NULL
dim(votdat)

## prep object that will receive roll call votes
rc <- matrix(NA, nrow = nrow(votdat), ncol = nrow(sendat))
rc <- as.data.frame(rc)
colnames(rc) <- sendat$id

## allocate votes into rc
raw$dhit <- 0 ## will record that vote was taken
##
for (n in 1:nrow(votdat)) {
    ##n <- 1 ## debug
    one.v <- raw[which(raw$n==n),] ## subset one roll call vote
    for (s in 1:nrow(sendat)) {
        ##s <- 1 ## debug
        sel.r <- which(one.v$sen==sendat$nom[s])
        ##sel.r <- grep(sendat$nomregexp[s], one.v$sen)
        if (length(sel.r)==1) {
            rc[n,s] <- one.v$vot[sel.r]
            one.v$dhit[sel.r] <- 1 ## indicate hit
        }
        if (length(sel.r)>1) {
            one.v$dhit[sel.r] <- 2 ## indicate hit
        }
    one.v -> raw[which(raw$n==n),] ## return manip
    }
}

## Recode
rc[rc=="NO"]         <- -1
rc[rc=="ABSTENCION"] <-  0
rc[rc=="SI"]         <-  1
## To numeric
rc <- data.frame(apply(rc, 2, function(x) as.numeric(x)))

rc[1,]
table(raw$dhit)
table(one.v$dhit)
one.v[one.v$dhit==0,]
raw[raw$dhit==2,]



