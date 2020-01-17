# Extract Maize prices from CSvs with ETH commodity prices

#### Libraries ####
library(stringdist)



#### INput Data ####
data.dir.path <- "../ScrapDoc/output"    # "data/ETHPriceData"
output.dir.path <- "output/ETHBarleyPriceData"     
coords.file.path <-  "data/ETH_Region_Market_Coords.csv"
search.term <- "barley" # "wheat" 



#### Prepare data ####
mrkts.coords <- read.csv(coords.file.path, as.is = TRUE)
price.csvS.path <- list.files(data.dir.path, full.names = TRUE, recursive = TRUE, pattern = "*.csv")
dir.create(output.dir.path, recursive = TRUE, showWarnings = FALSE)



##### Create Row data for each price ####
f <- function(maize.data.row, mnth.year, mrkts.coords, no.of.prdct.types) {
  # Function to get market metadata and attach it to price data
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
  
  # price_UnmilledMaize <- maize.data.row[3]
  # price_MilledMaize <- maize.data.row[4]
  return.value <- c(rgion, mrkt, mnth, yr, mrkt.long, mrkt.lati)
  for (prdct.no in 1:no.of.prdct.types  ) {
    return.value <- c(return.value, maize.data.row[2 + prdct.no])
  }
  return(return.value)
}



#### Extract Maize information in data files and store in one File  ####
ClosestMatch2 = function(string, stringVector){
  # Fuction that returns the closest matching string in vector
  stringVector[amatch(string, stringVector, maxDist=Inf)]
}
i = 0
maize.info.all <- data.frame()
first <- TRUE
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
    maize.rows <- grepl(search.term, price.csv$ITEM, ignore.case = TRUE)
    maize.data <- data.frame(t(region.names[,1:length(markts.available)]), 
                             t(price.csv[maize.rows, ])
    )
  }
  if (class(columns.error) != "try-error"){
    price.csv <- read.csv(price.csv.path, as.is = TRUE, skip = 1, )
    
    head(price.csv)
    # Get rows with maize
    maize.rows <- grepl(search.term, price.csv$ITEM, ignore.case = TRUE)
    maize.data <- data.frame(t(region.names), 
                             t(price.csv[maize.rows, ])
    )
  }
  
  if(all(!maize.rows)) next() # Skip to next file if no data found
  
  
  
  mnth.year <- sub("^.+[_][[:digit:]]+[_]", "", basename(price.csv.path))
  
  maize.info <- apply(tail(maize.data, -4), 1, f, 
                      mnth.year = mnth.year, 
                      mrkts.coords = mrkts.coords, 
                      no.of.prdct.types = dim(maize.data)[2] - 2)
  maize.info22 <- maize.info
  maize.info <- t(maize.info)
  
  ##### Get Medium for Addis Ababa ####
  if (grepl("addis", tolower(maize.info[1]))) {
    # Replace "-" with NA
    maize.info[,-1:-6][maize.info[,-1:-6] == "-"] <- NA
    
    apply(maize.info[,-1:-6], 2, function(x) median(as.numeric(x),na.rm = TRUE))
    
    maize.info <- c(maize.info[1,1:6], apply(maize.info[,-1:-6], 2, function(x) median(as.numeric(x),na.rm = TRUE)))
  }
  
  #### Append data ####
  # First Check the source directory; Create output CSV per Directory
  if (output.csv != paste0(basename(dirname(price.csv.path)), "_", search.term, ".csv")) {
    maize.info.all <- data.frame()
    first <- TRUE
  }
    
  
  
  
  
  if (length(maize.info.all) == 0 & first){
    # For Every new file processesed 
    maize.info <- rbind(c("Region", "Market", "Month", "Year", "Longitude", "Latitude",
                          as.character(unname(unlist(maize.data[2,-1:-2]))) ), 
                        maize.info
    )
    maize.info.all <- maize.info
    
     # Add row and column names
    colnames(maize.info.all) <- c("Region", "Market", "Month", "Year", "Longitude", "Latitude",
                                  paste0(as.character(unname(unlist(maize.data[2,-1:-2]))), "_", as.character(unname(unlist(maize.data[3,-1:-2]))) ))
    rownames(maize.info.all) <- 1:dim(maize.info.all)[1]
    
    first <- FALSE
  }
  
 
  
  if(length(maize.info.all) > 0 & !first){
    # For every subsequent file processed
    old.names <- colnames(maize.info.all)[-1:-6]
    old.sections <- as.character(unname(unlist(maize.info.all[1,-1:-6]))) 
    new.names <- as.character(unname(unlist(maize.data[3,-1:-2])))
    new.sections <- as.character(unname(unlist(maize.data[2,-1:-2]))) 
    
    maize.info <- rbind(data.frame(), maize.info)
    maize.info.tmp <- data.frame(matrix(NA, ncol = dim( maize.info.all)[2], nrow = dim(maize.info)[1] ) )
    maize.info.tmp[,1:6] <- maize.info[,1:6]
    
    # Match the columns or create new columns, then populate data in tmp dataframe
    for (i in 1:length(new.names)) {
      new.name.section <- paste0(new.sections[i], "_", new.names[i])
      distsnew <- stringdist(new.name.section, old.names)
      # distsnew2 <- stringdist(new.sections[i], old.sections)
      
      min.distsnew <- which(distsnew %in% 0)
      # if (!length(min.distsnew)) min.distsnew <- 999
      # min.distsnew2 <- which(distsnew2 %in% 0)
      # if (!length(min.distsnew2)) min.distsnew2 <- 888
      # 
      # print(intersect(min.distsnew, min.distsnew2))
      # intersect.col <- intersect(min.distsnew, min.distsnew2)
      print(length(min.distsnew))
      if (length(min.distsnew) > 0){
        maize.info.tmp[,6+min.distsnew] <- maize.info[,6+i]
      }
      
      if (length(min.distsnew) == 0){
        maize.info.tmp[,new.name.section] <- maize.info[,6+i]
        maize.info.all[,new.name.section] <- rep(NA, dim(maize.info.all)[1])
        }
      }
    
    # Bind the data to other data
    colnames.all <- colnames(maize.info.all)
    colnames(maize.info.tmp) <- colnames.all
    maize.info.all <- rbind(maize.info.all, maize.info.tmp)
    # Name Rows and Columns
    colnames(maize.info.all) <- colnames.all
    rownames(maize.info.all) <- 1:dim(maize.info.all)[1]
    
  }
 
  
  ##### Feedback ####
  print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
  
  # print(dirname(price.csv.path))
  # print(basename(price.csv.path))
  # print(maize.data)
  # print(data.frame(maize.info))
  print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
  
  
  #### Save Data ###
  output.csv <- paste0(basename(dirname(price.csv.path)), "_", search.term, ".csv")
  write.csv( maize.info.all, file.path(output.dir.path, output.csv))
  
  # if (i == 12)break()
  # i = i +1 
  # break() 
  
}

