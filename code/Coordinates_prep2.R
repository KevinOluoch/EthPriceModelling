# Extract Maize prices from CSvs with ETH commodity prices

#### Libraries ####
library(stringdist)



#### INput Data ####
data.dir.path <- "../ScrapDoc/output"    # "data/ETHPriceData"
output.dir.path <- "output/ETHPriceData"     
coords.file.path <-  "data/ETH_Region_Market_Coords.csv"



#### Prepare data ####
mrkts.coords <- read.csv(coords.file.path, as.is = TRUE)
price.csvS.path <- list.files(data.dir.path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
dir.create(output.dir.path, recursive = TRUE, showWarnings = FALSE)



##### Create Row data for each price ####
f <- function(maize.data.row, mnth.year, mrkts.coords) {
  rgion <- maize.data.row[1]
  mrkt  <- maize.data.row[2]
  if (grepl("addis", tolower(rgion))) mrkt  <- "addis ababa"
  
  mnth.year <- sub(".csv$", "", mnth.year)
  mnth <- sub("[[:digit:]]+", "", mnth.year)
  yr <- sub("[[:alpha:]]+", "", mnth.year)
  
  for (market.names in 3:6) {
    print(market.names)
    matchdist1 <- stringdist(tolower(mrkt), tolower(mrkts.coords[,market.names]))
    mrkt.row.nos <-  which(matchdist1  %in% min(matchdist1))
    if (min(matchdist1) == 0) {
      print("AAAAAAAAAAAAAAAAAAAAAAAAA")
      print(mnth.year)
      print(rgion)
      print(mrkt)
      print(market.names)
      print("BBBBBBBBBBBBBBBBBBBBBBBBB")
      
      break()
      }
    mrkt.row.nos <- 999
  }
  
  mrkt.long <- mrkts.coords$Longitude[mrkt.row.nos]
  mrkt.lati <- mrkts.coords$Latitude[mrkt.row.nos]
  
  price_UnmilledMaize <- maize.data.row[3]
  price_MilledMaize <- maize.data.row[4]
  return(c(rgion, mrkt, mnth, yr, mrkt.long, mrkt.lati, price_UnmilledMaize, price_MilledMaize))
}



#### Extract Maize information in data files and store in one File  ####
i = 0
maize.info.all <- data.frame()
output.csv <- ""
for (price.csv.path in price.csvS.path) {
  region.names <- read.csv(price.csv.path, as.is = TRUE, header = FALSE)[1:2,]
  columns.error <- try(read.csv(price.csv.path, as.is = TRUE, skip = 1, ))
  if (class(columns.error) == "try-error"){
  price.csv1 <- read.csv(price.csv.path, as.is = TRUE, skip = 2, header = FALSE )
  markts.available1 <- replace(region.names[2,], region.names[2,]=="", NA)
  markts.available <- markts.available1[!is.na(markts.available1)]
  price.csv <- price.csv1[,1:length(markts.available)]
  names(price.csv) <- markts.available
  
  # Get rows with maize
  maize.rows <- grepl("maize", price.csv$ITEM, ignore.case = TRUE)
  maize.data <- data.frame(t(region.names[,1:length(markts.available)]), 
                           t(price.csv[maize.rows, ])
  )
  }
  if (class(columns.error) != "try-error"){
    price.csv <- read.csv(price.csv.path, as.is = TRUE, skip = 1, )
  
  head(price.csv)
  # Get rows with maize
  maize.rows <- grepl("maize", price.csv$ITEM, ignore.case = TRUE)
  maize.data <- data.frame(t(region.names), 
                           t(price.csv[maize.rows, ])
                           )
  }

  
  
  
  mnth.year <- sub("^.+[_][[:digit:]]+[_]", "", basename(price.csv.path))
  
  maize.info <- apply(tail(maize.data, -4), 1, f, mnth.year = mnth.year, mrkts.coords = mrkts.coords )
  maize.info <- t(maize.info)
  
  ##### Get Medium for Addis Ababa ###
  if (grepl("addis", tolower(maize.info[1]))) {
    # Replace "-" with NA
    maize.info[,7][maize.info[,7] == "-"] <- NA
    maize.info[,8][maize.info[,8] == "-"] <- NA
    
    unmilled.median.price <- median(as.numeric(maize.info[,7]), na.rm = TRUE)
    milled.median.price <- median(as.numeric(maize.info[,8]), na.rm = TRUE)
    
    maize.info <- c(maize.info[1,1:6], unmilled.median.price, milled.median.price)
  }
  
  #### Append data ###
  # First Check the source directory; Create output CSV per Directory
  if (output.csv != paste0(basename(dirname(price.csv.path)), ".csv")) maize.info.all <- data.frame()
  
  if(length(maize.info.all) > 0){
      maize.info.all <- rbind(maize.info.all, maize.info)
      
    colnames(maize.info.all) <- c("Region", "Market", "Month", "Year", "Longitude", "Latitude", 
                        "Unmilled Maize Price Birr.KG", "Milled Maize Price Birr.KG")
    rownames(maize.info.all) <- 1:dim(maize.info.all)[1]
  }
  
  if (length(maize.info.all) == 0){
    maize.info.all <- maize.info
    }
  

  
  ##### Feedback ####
  print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
  
  # print(dirname(price.csv.path))
  # print(basename(price.csv.path))
  # print(maize.data)
  # print(data.frame(maize.info))
  print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")

    
  #### Save Data ###
  output.csv <- paste0(basename(dirname(price.csv.path)), ".csv")
  write.csv( maize.info.all, file.path(output.dir.path, output.csv))
  
  # if (i == 12)break()
  # i = i +1 
  # break() 
  
}

