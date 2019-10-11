library(stringi)
library(stringdist)
library(raster)

eth.markets <- read.csv("data/ETH_Region_Market_LongLati.csv", as.is = TRUE) 
eth.markets <- eth.markets[, !(names(eth.markets) %in% c("Longitude","Latitude"))]
eth.markets2 <- read.csv("data/ETH_Region_Market_LongLati2.csv", as.is = TRUE) 
eth.markets2 <- eth.markets2[, !(names(eth.markets2) %in% c("Longitude","Latitude"))]
# eth.markets2 <- coordinates(eth.markets2)[,1]

eth3 <- raster::getData("GADM", country = "ETH", level = 3 )
eth.major.town <- shapefile("data/shp/eth_major_town.shp")
eth.pplp.2016 <- shapefile("data/shp/eth_pplp_multiplesources_20160205.shp")

ethpoints <- shapefile("data/shp/eth_point.shp")
ethpoints@data$Longitute <- coordinates(ethpoints)[,1]
ethpoints@data$Latitude <- coordinates(ethpoints)[,2]
eth.over <- data.frame(xx=over(eth3, ethpoints)) 
eth3b <- eth3
eth3b@data <- data.frame(eth3@data, eth.over)
write.csv( eth_tmp@data,
           "output/draft3.csv")


add.closestmatch <- function(origdata, mrktvector, rgnvector, longlatishp, col.id) {
  
  entrys.no <- dim(origdata)[1]
  for ( i in c(1:entrys.no) ) {
    # print(mrkt)
    mrkt <- eth.markets$Market[i]
    rgn1 <- eth.markets$Region1[i]
    rgn2 <- eth.markets$Region2[i]
    
    matchdist1 <- stringdist(tolower(mrkt), tolower(mrktvector))
    mrkt.row.nos <-  which(matchdist1  %in% min(matchdist1))
    
    matchdist2 <- stringdist(tolower(rgn1), tolower(rgnvector))
    rgn1.row.nos <- which(matchdist2 %in% min(matchdist2))
    
    matchdist3 <- stringdist(tolower(rgn2), tolower(rgnvector))
    rgn2.row.nos <- which(matchdist3 %in% min(matchdist3))
    
    bestmatches <- union(intersect(mrkt.row.nos, rgn1.row.nos), intersect(mrkt.row.nos, rgn2.row.nos))
    print("##########################################")
    print(mrkt.row.nos)
    print(rgn1.row.nos)
    print(rgn2.row.nos)
    print(bestmatches)
    if (length(bestmatches) == 1) {
      
      origdata[i, paste0('Region', col.id)] <- paste(unique(rgnvector[bestmatches]), collapse=" ") 
      origdata[i, paste0('Market', col.id)] <- paste(unique(mrktvector[bestmatches]), collapse=" ") 
      origdata[i, paste0('Longitude', col.id)] <- coordinates(longlatishp[bestmatches[1], ])[,1]
      origdata[i, paste0('Latitude',  col.id)] <- coordinates(longlatishp[bestmatches[1], ])[,2]
      }
  }
  
  return(origdata)
  
  }

a <- add.closestmatch(eth.markets, eth3$NAME_3, eth3$NAME_1, eth3, 1)
b <- add.closestmatch(eth.markets, eth.major.town$TOWN_NAME, eth.major.town$REGION, eth.major.town, 2)
c <- add.closestmatch(eth.markets, eth.pplp.2016$admin3Name, eth.pplp.2016$admin1Name, eth.pplp.2016, 3)

a
b
c
write.csv( data.frame(eth.markets[,c(1,3)], 
           a[,c('Region1', 'Market1', 'Longitude1', 'Latitude1')], 
           b[,c('Region2', 'Market2', 'Longitude2', 'Latitude2')], 
           c[,c('Region3', 'Market3', 'Longitude3', 'Latitude3')] ),
            "output/draft2.csv"
)




# add.closestmatch2 <- function(origdata, mrktvector, rgnvector, longlatishp, col.id) {
#   
#   entrys.no <- dim(origdata)[1]
#   for ( i in c(1:entrys.no) ) {
#     # print(mrkt)
#     mrkt <- eth.markets2$Market[i]
#     rgn1 <- eth.markets2$Region1[i]
#     rgn2 <- eth.markets2$Region1.1[i]
#     
#     matchdist1 <- stringdist(tolower(mrkt), tolower(mrktvector))
#     mrkt.row.nos <-  which(matchdist1  %in% min(matchdist1))
#     
#     matchdist2 <- stringdist(tolower(rgn1), tolower(rgnvector))
#     rgn1.row.nos <- which(matchdist2 %in% min(matchdist2))
#     
#     matchdist3 <- stringdist(tolower(rgn2), tolower(rgnvector))
#     rgn2.row.nos <- which(matchdist3 %in% min(matchdist3))
#     
#     bestmatches <- union(intersect(mrkt.row.nos, rgn1.row.nos), intersect(mrkt.row.nos, rgn2.row.nos))
#     print("##########################################")
#     print(mrkt.row.nos)
#     print(rgn1.row.nos)
#     print(rgn2.row.nos)
#     print(bestmatches)
#     if (length(bestmatches)) {
#       
#       origdata[i, paste0('Region', col.id)] <- paste(unique(rgnvector[bestmatches]), collapse=" ") 
#       origdata[i, paste0('Market', col.id)] <- paste(unique(mrktvector[bestmatches]), collapse=" ") 
#       origdata[i, paste0('Longitude', col.id)] <- longlatishp[bestmatches[1], "xx.Longitute"]
#       origdata[i, paste0('Latitude',  col.id)] <- longlatishp[bestmatches[1], "xx.Latitude"]
#     }
#   }
#   
#   return(origdata)
#   
# }
# 
# d <- add.closestmatch2(eth.markets2, eth3b$xx.name, eth3b$NAME_1, eth3b, 4)
# 
# d
# write.csv( data.frame(eth.markets2[,c(1,3)], 
#                       d[,c('Region4', 'Market4', 'Longitude4', 'Latitude4')]
#                       ),
#            "output/draft4.csv"
# )




# warnings()# eth.pplp.2016$admin2Name[grepl("arari", eth.pplp.2016$admin3Name,ignore.case = TRUE)]
# eth.pplp.2016$admin2Name[grepl("silase", eth.pplp.2016$admin2Name)]
# 

# closestmatch <- function(str.pattern, search.values) {
#   matchdist1 <- stringdist(str.pattern, search.values)
#   
#   if(min(matchdist1) > 2) return(NA)
#   return( unique(search.values[ matchdist1 %in% min(matchdist1)]) )
# }

# mrkts.no <- dim(eth.markets)[1]
# for ( i in c(1:mrkts.no) ) {
#   # print(mrkt)
#   mrkt <- eth.markets$Market[i]
#   rgn <- eth.markets$Region[i]
# # a <- eth.pplp.2016$admin2Name[match(mrkt, eth.pplp.2016$admin2Name)]
# # b <- eth.major.town$TOWN_NAME[match(mrkt, eth.major.town$TOWN_NAME)]
#   
#   c <- closestmatch(mrkt, eth.pplp.2016$admin2Name)
#   
#   d <- closestmatch(mrkt, eth.major.town$TOWN_NAME)
#   
#   e <- closestmatch(mrkt, eth3$NAME_3)
#   
#   f <- closestmatch(mrkt, eth3$NAME_3)
#   
#   g <- closestmatch(rgn, eth3$NAME_1)
#   # print(list(rgn, g))
#    print(list(mrkt, c, d, e, f))
# 
# }
