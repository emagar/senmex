rm(list = ls())
setwd("/home/eric/Downloads/Desktop/data/rollcall/senMex/data/raw/")

all.v <- "Text of Senate roll call votes sent by Francisco" ## empty vector to append each period's votes

per <- "59_2" ## periodo manipulated
##
## all the files that Francisco sent
fls <- c("832.html", "847.html", "848.html", "869.html", "870.html", "871.html", "872.html", "873.html", "904.html", "931.html", "932.html", "980.html", "982.html", "983.html", "984.html", "1016.html", "1019.html", "1020.html", "1021.html", "1022.html", "1041.html", "1042.html", "1064.html", "1065.html", "1066.html", "1085.html", "1094.html", "1104.html", "1143.html", "1144.html", "1173.html", "1174.html", "1175.html", "1180.html", "1205.html", "1208.html", "1209.html", "1220.html", "1228.html", "1229.html", "1230.html", "1231.html", "1232.html", "1233.html", "1234.html", "1236.html", "1283.html", "1284.html", "1286.html", "1287.html", "1288.html", "1289.html", "1290.html", "1291.html", "1292.html", "1326.html", "1327.html", "1328.html", "1329.html", "1330.html", "1331.html", "1332.html", "1333.html", "1335.html", "1346.html", "1347.html", "1348.html", "1349.html", "1350.html", "1351.html", "1352.html", "1353.html", "1354.html", "1384.html", "1385.html", "1386.html", "1387.html", "1388.html", "1389.html", "1392.html", "1393.html", "1395.html", "1396.html", "1397.html", "1398.html", "1399.html", "1400.html", "1401.html", "1402.html", "1448.html", "1449.html", "1450.html", "1500.html", "1589.html", "1590.html", "1591.html", "1592.html", "1593.html")

## will receive period's roll call votes
per.v <- c("Periodo de sesiones:", per) ## prep vecto that will receive text of all roll calls
##
for (i in 1:length(fls)){
    ##i <- 54 ## debug
    t <- readLines(paste0(per, "/", fls[i], ".txt")) ## make path to file
    t <- gsub(" ", "", t)
    t <- gsub("[ ]{2}", " ", t)  ## drop double spaces
    t <- gsub("^[ ]+", "", t)    ## drop leading spaces
    t <- gsub("[ ]+$", "", t)    ## drop trailing spaces
    t <- gsub("[\"]", "", t)     ## drop quotes
    t <- t[-which(t=="")]        ## drop empty lines
    ##
    ##fls[i]; t[141:375]  ## debug
    ##
    if (i %in% c(1:3,54)){ ## these I did by hand, extract roll call on top
        t.sub <- t[1:c(grep("----end----",t)-1)]
        per.v <- c(per.v, t.sub)
        next
    }
    ##
    ## get relevant indexes
    sel.1 <- grep("Diario 1.+Abr 28", t, ignore.case = TRUE)                      ## vote id follows ~12 lines
    sel.2 <- grep("[áa]BRASE.+el SISTEMA.+ELECTR.NICO", t, ignore.case = TRUE)    ## before roll call
    if (length(sel.2)==0) sel.2 <- grep("CONFORME.*SISTEMA.+ELECTR.NICO", t, ignore.case = TRUE) ## attempt #2
    sel.3 <- length(t)-2  ## bring text til end of file (will check if all files end soon after roll call
    ## sel.3 <- grep("(?:recibieron|emitieron) [0-9]+ votos", t, ignore.case = TRUE) ## after roll call
    ## if (length(sel.3)==0){
    ##     sel.3 <- grep("secretari[oa]", t, ignore.case = TRUE)                         ## Secretario's interventions
    ##     sel.3 <- sel.3[which(sel.3 > sel.2)][1]                                       ## The first one after roll call
    ## }
    ##sel.1; sel.2; sel.3 ## debug
    ##
    if (length(sel.1[!is.na(sel.1)])!=1) {
        print(paste("sel.1 not singleton", fls[i]))
        break
    }
    if (length(sel.2[!is.na(sel.2)])!=1) {
        print(paste("sel.2 not singleton", fls[i]))
        break
    }
    if (length(sel.3[!is.na(sel.3)])!=1) {
        print(paste("sel.3 not singleton", fls[i]))
        break
    }
    ## subset vector with relevant info
    t.sub <- c(
        "In remembrance of Francisco Cantu" ## a marker that shows start of the vector (useful when concatenating with next vote) 
      , fls[i]                              ## raw file whence this info derived
      , t[(sel.1+1):(sel.1+13)]             ## vote id
      , t[(sel.2+1):(sel.3+2)]              ## roll call with aye/nay sums at end
    )
    ##
    ## append vote to others
    per.v <- c(per.v, t.sub)
    rm(sel.1, sel.2, sel.3, t, t.sub) ## clean
}
##
## append votes to all.v
all.v <- c(all.v, per.v)
head(all.v)

per <- "59_3" ## periodo manipulated
##
## all the files that Francisco sent
fls <- c("26.html", "27.html", "28.html", "33.html", "49.html", "65.html", "135.html", "138.html", "160.html", "161.html", "187.html", "223.html", "224.html", "225.html", "226.html", "227.html", "241.html", "247.html", "251.html", "252.html", "253.html", "280.html", "282.html", "307.html", "308.html", "309.html", "336.html", "337.html", "338.html", "364.html", "366.html", "422.html", "424.html", "429.html", "451.html", "471.html", "472.html", "473.html", "496.html", "497.html", "498.html", "499.html", "532.html", "533.html", "534.html", "535.html", "553.html", "554.html", "555.html", "568.html", "573.html", "574.html", "575.html", "576.html", "606.html", "607.html", "608.html", "609.html", "610.html", "640.html", "641.html", "642.html", "666.html", "667.html", "668.html", "694.html", "695.html", "696.html", "697.html", "736.html", "739.html", "741.html", "742.html", "743.html", "744.html", "791.html", "792.html", "793.html", "794.html", "795.html", "797.html", "798.html", "799.html", "800.html", "801.html", "843.html", "846.html", "847.html", "848.html", "849.html", "850.html", "851.html", "852.html", "853.html", "855.html", "856.html", "857.html", "878.html", "881.html", "882.html", "883.html", "884.html", "886.html", "887.html", "888.html", "904.html", "905.html", "906.html", "907.html", "908.html", "911.html", "912.html", "914.html", "915.html", "1001.html", "1002.html", "1050.html", "1051.html", "1052.html", "1053.html", "1054.html", "1055.html", "1056.html", "1057.html", "1081.html", "1108.html", "1109.html", "1131.html", "1156.html", "1157.html", "1184.html", "1185.html", "1212.html", "1213.html", "1214.html", "1215.html", "1216.html", "1245.html", "1246.html", "1247.html", "1280.html", "1281.html", "1282.html", "1283.html", "1284.html", "1285.html", "1286.html", "1287.html", "1336.html", "1337.html", "1338.html", "1339.html", "1340.html", "1362.html", "1363.html", "1364.html", "1365.html", "1366.html", "1367.html", "1368.html", "1370.html", "1371.html", "1375.html", "1376.html", "1401.html", "1402.html", "1403.html", "1404.html", "1426.html", "1427.html", "1428.html", "1429.html", "1430.html", "1431.html", "1458.html", "1459.html", "1460.html", "1461.html", "1462.html", "1463.html", "1464.html", "1465.html", "1466.html", "1467.html", "1468.html", "1469.html", "1491.html", "1492.html", "1493.html", "1507.html", "1508.html", "1546.html", "1547.html", "1548.html", "1549.html", "1550.html", "1551.html", "1552.html", "1581.html", "1584.html", "1585.html", "1586.html", "1587.html", "1588.html", "1589.html", "1590.html", "1591.html", "1592.html", "1593.html", "1594.html", "1595.html", "1596.html", "1597.html", "1639.html", "1641.html", "1642.html", "1643.html", "1644.html", "1648.html", "1653.html", "1654.html", "1655.html", "1656.html", "1657.html", "1680.html", "1684.html", "1717.html", "1718.html", "1719.html", "1720.html", "1725.html", "1726.html", "1727.html", "1728.html", "1729.html", "1730.html", "1732.html", "1733.html", "1734.html", "1735.html", "1736.html", "1737.html", "1738.html", "1748.html", "1749.html", "1750.html", "1751.html", "1752.html", "1753.html", "1754.html", "1755.html", "1758.html", "1759.html", "1760.html", "1761.html", "1762.html", "1763.html", "1764.html", "1765.html", "1766.html", "1767.html", "1768.html", "1769.html", "1770.html", "1771.html")

per.v <- c("Periodo de sesiones:", per) ## add new periodo marker
##
for (i in 1:length(fls)){
    ##i <- 1 ## debug
    t <- readLines(paste0(per, "/", fls[i], ".txt")) ## make path to file
    t <- gsub(" ", "", t)
    t <- gsub("[ ]{2}", " ", t)  ## drop double spaces
    t <- gsub("^[ ]+", "", t)    ## drop leading spaces
    t <- gsub("[ ]+$", "", t)    ## drop trailing spaces
    t <- gsub("[\"]", "", t)     ## drop quotes
    t <- t[-which(t=="")]        ## drop empty lines
    ##
    ##fls[i]; t[141:375]  ## debug
    ##
    if (i %in% c(46,86,96,112,139,227)){ ## these I did by hand, extract roll call on top
        t.sub <- t[1:c(grep("----end----",t)-1)]
        per.v <- c(per.v, t.sub)
        next
    }
    ##
    ## get relevant indexes
    sel.1 <- grep("Diario 1.+Abr 28", t, ignore.case = TRUE)                      ## vote id follows ~12 lines
    sel.2 <- grep("[áa]BRAn?SE.+el SISTEMA.+ELECTR.NICO", t, ignore.case = TRUE)    ## before roll call
    if (length(sel.2)==0) sel.2 <- grep("CONFORME.*SISTEMA.+ELECTR.NICO", t, ignore.case = TRUE) ## attempt #2
    sel.3 <- length(t)-2  ## bring text til end of file (will check if all files end soon after roll call
    ## sel.3 <- grep("(?:recibieron|emitieron) [0-9]+ votos", t, ignore.case = TRUE) ## after roll call
    ## if (length(sel.3)==0){
    ##     sel.3 <- grep("secretari[oa]", t, ignore.case = TRUE)                         ## Secretario's interventions
    ##     sel.3 <- sel.3[which(sel.3 > sel.2)][1]                                       ## The first one after roll call
    ## }
    ## sel.1; sel.2; sel.3 ## debug
    ## t[sel.2]
    ##
    if (length(sel.1[!is.na(sel.1)])!=1) {
        print(paste("sel.1 not singleton", fls[i]))
        break
    }
    if (length(sel.2[!is.na(sel.2)])!=1) {
        print(paste("sel.2 not singleton", fls[i]))
        break
    }
    if (length(sel.3[!is.na(sel.3)])!=1) {
        print(paste("sel.3 not singleton", fls[i]))
        break
    }
    ## subset vector with relevant info
    t.sub <- c(
        "In remembrance of Francisco Cantu" ## a marker that shows start of the vector (useful when concatenating with next vote) 
      , fls[i]                              ## raw file whence this info derived
      , t[(sel.1+1):(sel.1+13)]             ## vote id
      , t[(sel.2+1):(sel.3+2)]              ## roll call with aye/nay sums at end
    )
    ##
    ## append vote to others
    per.v <- c(per.v, t.sub)
    rm(sel.1, sel.2, sel.3, t, t.sub) ## clean
}
##
## append votes to all.v
all.v <- c(all.v, per.v)
tail(all.v)


per <- "60_1" ## periodo manipulated
##
## all the files that Francisco sent
fls <- c("12.html", "14.html", "19.html", "34.html", "76.html", "80.html", "133.html", "155.html", "165.html", "179.html", "180.html", "194.html", "209.html", "215.html", "236.html", "238.html", "241.html", "263.html", "264.html", "289.html", "321.html", "404.html", "410.html", "418.html", "435.html", "451.html", "452.html", "453.html", "454.html", "455.html", "456.html", "491.html", "492.html", "503.html", "504.html", "505.html", "506.html", "507.html", "508.html", "509.html", "513.html", "514.html", "571.html", "601.html", "648.html", "657.html", "658.html", "659.html", "674.html", "685.html", "709.html", "720.html", "739.html", "751.html", "752.html", "753.html", "754.html", "755.html", "766.html", "779.html", "780.html", "781.html", "783.html", "808.html", "809.html", "810.html", "817.html", "848.html", "849.html", "850.html", "851.html", "871.html", "873.html", "889.html", "890.html", "891.html", "904.html", "912.html", "913.html", "929.html", "930.html", "931.html", "932.html", "933.html", "948.html", "960.html", "965.html", "998.html", "999.html", "1000.html", "1010.html", "1038.html", "1039.html", "1075.html", "1076.html", "1100.html", "1101.html", "1102.html", "1116.html", "1179.html", "1181.html", "1182.html", "1183.html", "1184.html", "1185.html", "1186.html", "1187.html", "1188.html", "1189.html", "1190.html", "1219.html", "1220.html", "1221.html", "1222.html", "1223.html", "1224.html", "1225.html", "1227.html", "1228.html", "1229.html", "1230.html", "1231.html", "1232.html", "1233.html", "1234.html", "1235.html", "1236.html", "1237.html", "1238.html", "1239.html", "1240.html", "1241.html", "1242.html", "1243.html", "1244.html", "1908.html")

per.v <- c("Periodo de sesiones:", per) ## add new periodo marker
##
for (i in 1:length(fls)){
    ##i <- 1 ## debug
    t <- readLines(paste0(per, "/", fls[i], ".txt")) ## make path to file
    t <- gsub(" ", "", t)
    t <- gsub("[ ]{2}", " ", t)  ## drop double spaces
    t <- gsub("^[ ]+", "", t)    ## drop leading spaces
    t <- gsub("[ ]+$", "", t)    ## drop trailing spaces
    t <- gsub("[\"]", "", t)     ## drop quotes
    t <- t[-which(t=="")]        ## drop empty lines
    ##
    ##fls[i]; t[141:375]  ## debug
    ##
    ## if (i %in% c(46,86,96,112,139,227)){ ## these I did by hand, extract roll call on top
    ##     t.sub <- t[1:c(grep("----end----",t)-1)]
    ##     per.v <- c(per.v, t.sub)
    ##     next
    ## }
    ##
    ## get relevant indexes
    ##sel.1 <- grep("Diario 1.+Abr 28", t, ignore.case = TRUE)                      ## vote id follows ~12 lines
    sel.1 <- grep("Diario 1.+Abr 26", t, ignore.case = TRUE)                      ## vote id follows ~12 lines
    sel.2 <- grep("[áa]BRAn?SE.+el SISTEMA.+ELECTR.NICO", t, ignore.case = TRUE)    ## before roll call
    if (length(sel.2)==0) sel.2 <- grep("ada CONFORME.*SISTEMA.+ELECTR.NICO", t, ignore.case = TRUE) ## attempt #2
    if (length(sel.2)==1) sel.3 <- length(t)-2  ## bring text til end of file (will check if all files end soon after roll call
    if (length(sel.2) >1) sel.3 <- grep("(?:recibieron|emitieron|han emitido) [0-9]+ votos", t, ignore.case = TRUE) ## after roll call
    ##if (length(sel.2) >1) sel.3 <- grep("votos (?:a|en) (?:favor|pro)", t, ignore.case = TRUE) ## after roll call
    ## if (length(sel.3)==0){
    ##     sel.3 <- grep("secretari[oa]", t, ignore.case = TRUE)                         ## Secretario's interventions
    ##     sel.3 <- sel.3[which(sel.3 > sel.2)][1]                                       ## The first one after roll call
    ## }
    sel.1; sel.2; sel.3 ## debug
    t[sel.3]
    ##
    if (length(sel.1[!is.na(sel.1)])==0 | length(sel.2[!is.na(sel.2)])==0 | length(sel.3[!is.na(sel.3)])==0) {
        print(paste("some sel.. empty", fls[i]))
        break
    }
    ##
    if (length(sel.1[!is.na(sel.1)])==1 & length(sel.2[!is.na(sel.2)])==1 & length(sel.3[!is.na(sel.3)])==1) {
        ## subset vector with relevant info
        t.sub <- c(
            "In remembrance of Francisco Cantu" ## a marker that shows start of the vector (useful when concatenating with next vote) 
          , fls[i]                              ## raw file whence this info derived
          , t[(sel.1+1):(sel.1+13)]             ## vote id
          , t[(sel.2+1):(sel.3+2)]              ## roll call with aye/nay sums at end
        )
    }
    ##
    if (length(sel.2[!is.na(sel.2)])>1 & length(sel.2[!is.na(sel.2)])==length(sel.3[!is.na(sel.3)])) {
        sel.4 <- paste(sel.2, sel.3, sep=":")
        t.sub <- vector() ## initialize empty vector
        for (j in 1:length(sel.4)){
            #j <- 1 ## debug
            t.sub <- c(t.sub,
                       "In remembrance of Francisco Cantu" ## a marker that shows start of the vector (useful when concatenating with next vote)
                     , "Multivote"
                     , fls[i]                              ## raw file whence this info derived
                     , t[(sel.1+1):(sel.1+13)]             ## vote id
                     , t[eval(str2expression(sel.4[j]))]   ## roll call with aye/nay sums at end
                       )
        }
    }
    ##
    if (                                 length(sel.2[!is.na(sel.2)])!=length(sel.3[!is.na(sel.3)])) {
        print(paste("Asymmetric sels", fls[i]))
        break
    }
    ##
    ## append vote to others
    per.v <- c(per.v, t.sub)
    rm(sel.1, sel.2, sel.3, t, t.sub) ## clean
}
i

tmp <- c(tmp,i)
fls[tmp]

##
## append votes to all.v
all.v <- c(all.v, per.v)
tail(all.v)

##
## save per.v for exploration
write.csv(all.v, file = paste(per, "all.csv", sep = "/"), row.names = TRUE)

i
head(t)                
t[540]


dim(t)
