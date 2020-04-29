
input.folder <- "../output/ETHPriceData" #"data/ETHPriceData"
output.filename <- "output/CSA_RetailPrice_BarleyMaizeWheat_2013_19.csv"

missing.coords.path <- "data/missing_market_coordinates.csv"
corrected.names.path <- "data/missing_market_names.csv"

# find price CSV files
price.files <- list.files(input.folder, pattern = "*.csv", full.names = TRUE)

# Load files and make all NA to a period (.), and starndardize column names
# Save files as dataframes in a list
output.dataframes.list <- list()
for (file.name in price.files) {
  print(file.name)
  # Fix NAs
  raw.file <- read.csv(file.name, as.is = TRUE)
  raw.file[raw.file == "-" ]  <- "."
  raw.file[is.na(raw.file)] <- "."
  raw.file
  
  
  # Fix column names
  milled <- unmilled <- coffee.tea <- food <- grain <- ""
  if (grepl("_barley", file.name) | grepl("_wheat", file.name)) {
    raw.file <- raw.file[-1:-3,]
    }
    
  file.col.names <- gsub("[A-Z][0-9][0-9 .]*", "", names(raw.file))
  file.col.names <- gsub("Price.Birr.KG", "", file.col.names)
  if (!grepl("_barley", file.name) | !grepl("_wheat", file.name)) {
    grain <- "maize"
  }
  if (grepl("_barley", file.name)) {  
    grain <- "barley"
    file.col.names <- gsub("Price.Birr.KG", "", file.col.names)
    }
  if (grepl("_wheat", file.name)) {
    grain <- "wheat"
  }
  
  # print(names(raw.file))
  # print(file.col.names)
  file.col.names.new <- list()
  for (col.name in file.col.names) {
    milled <- coffee.tea <- food  <- ""
    if (grepl("UNMILLED.CEREALS.GRAINS_", col.name)) milled  <- "unmilled"
    if (grepl("CEREALS.UNMILLED_", col.name)) milled  <- "unmilled"
    if (grepl("CEREALS.MILLED_", col.name)) milled <- "milled"
    
    if (grepl("COFFEE.AND.TEA_", col.name)) coffee.tea <- ""
    if (grepl("FOOD_", col.name)) food <- ""

    
    otherinfor <- gsub(".*[_]", "", col.name)
    file.col.names.new[[col.name]] <- col.name
    if (!col.name %in% c("X", "Region", "Market", "Month", "Year", "Longitude", "Latitude")){
      # print(grain)
      otherinfor <- gsub(grain, "", otherinfor, ignore.case = TRUE)
      # print(otherinfor)
      # print(paste(grain, milled, otherinfor, sep = "_"))
      file.col.names.new[[col.name]] <- paste(grain, milled, otherinfor, sep = "_")
    }
  names(raw.file) <- unlist(file.col.names.new)
  output.dataframes.list[[gsub(" ", "", basename(file.name))]] <- raw.file
  }
}

# Merge all the dataframes into 1
# First get the unique column names
all.colnames <- c()
for (infor.filename in names(output.dataframes.list)) {
  all.colnames <- c( all.colnames, names(output.dataframes.list[[infor.filename]]) )
  
}

# Expand all dataframes to have the unique columns
first <- TRUE
for (infor.filename in names(output.dataframes.list)) {
  tmp.df <- data.frame(id = character(dim(output.dataframes.list[[infor.filename]])[1]))
  
  for (infor.col.name in unique(all.colnames)) {
    if (infor.col.name %in% names(output.dataframes.list[[infor.filename]])){
      tmp.df[,infor.col.name] <- output.dataframes.list[[infor.filename]][,infor.col.name]
    }
    
    if (!infor.col.name %in% names(output.dataframes.list[[infor.filename]])){
      tmp.df[,infor.col.name] <- rep(".", times = dim(output.dataframes.list[[infor.filename]])[1])
    }
  }
  
  if (first) {output.dataframes <- tmp.df; first <- FALSE; next}
  
  output.dataframes <- rbind(output.dataframes, tmp.df)
}


# Correct colnames 
id.columns <- c("X", "Region", "Market", "Month", "Year", "Longitude", "Latitude")
output.dataframes.names_ <- 
  c(names(output.dataframes)[which(names(output.dataframes) %in% id.columns)], 
  sort(names(output.dataframes)[which(!names(output.dataframes) %in% id.columns)][-1] ) )


output.dataframes.names <- gsub("[_or]*_$", "", 
                                gsub("[_]+", "_", 
                                     gsub("[.]", "_",output.dataframes.names_)))

output.dataframes.final <- output.dataframes[, output.dataframes.names_]
names(output.dataframes.final) <- output.dataframes.names



# Arrange month year data in one line.
missing.coords <- read.csv(missing.coords.path, as.is = TRUE)
corrected.names <- read.csv(corrected.names.path, as.is = TRUE)
unique(output.dataframes.final$Region)
regions <- unique(output.dataframes.final$Region)
markets <- unique(output.dataframes.final$Market)
months_ <- unique(output.dataframes.final$Month)
years_ <- unique(output.dataframes.final$Year)
first <- TRUE
for (region in regions) {
  byregion <- output.dataframes.final[which(output.dataframes.final$Region %in% region),]
  for (market in markets) {
    bymarket <- byregion[which(byregion$Market %in% market),]
    # print(bymarket[which(bymarket$Year %in%  "2015"),])
    for (month_ in months_) {
      bymonth <- bymarket[which(bymarket$Month %in% month_),]
      
      for (year_ in years_) {
        if (year_ %in% "2014.doc") next()
        if (dim(bymonth[which(bymonth$Year %in% year_),])[1] == 0) next()
        byyear <- bymonth[which(bymonth$Year %in% year_),]
        data.byyear <- byyear
        data.byyear[data.byyear == "."] <- NA
        
        for (datacol in names(data.byyear[,-1:-7])) {
          data.byyear[1, datacol] <- mean(as.numeric(data.byyear[,-1:-7][,datacol]), na.rm = TRUE)
        }
        
        data.byyear[is.na(data.byyear)] <- "."
        data.byyear[as.data.frame(data.byyear) == NaN] <- "."
        
        if ( market %in% corrected.names$Market1 ) {
          data.byyear[1, "Market"] <- 
            corrected.names$Market2[which(corrected.names$Market1 %in%  market )]
          
        }
        if ( is.na(as.numeric(data.byyear[1,"Longitude"]))  &  market %in% missing.coords$Market ) {
          data.byyear[1,"Longitude"] <- 
            missing.coords$Longitude[which(missing.coords$Market %in%  market )]
          data.byyear[1, "Latitude"] <- 
            missing.coords$Latitude[which(missing.coords$Market %in%  market )]
        }
        if ( is.na(as.numeric(data.byyear[1,"Longitude"]))  | as.numeric(data.byyear[1,"Longitude"]) < 30) {
          data.byyear[1, "Longitude"] <- as.numeric(byyear[1, "Latitude"])
          data.byyear[1, "Latitude"] <- as.numeric(byyear[1, "Longitude"])
        }
        if (year_ %in% "2013(1)"){
          data.byyear[1, "Year"] <- gsub("[.]doc$", "", gsub("[(]1[)]$", "", byyear[1, "Year"]))
          data.byyear[1, "Month"] <- gsub("[.]doc$", "", gsub("[(]1[)]$", "", byyear[1, "Month"]))
        }
        data.byyear[1, "Month"] <- tolower(data.byyear[1, "Month"])
        data.byyear[1, "Market"] <- tolower(data.byyear[1, "Market"])
        data.byyear[1, "Region"] <- tolower(data.byyear[1, "Region"])
        data.byyear[is.na(data.byyear)] <- "."
        if (first) {output.dataframes.compact <- data.byyear[1,]; first <- FALSE; next}
        output.dataframes.compact <- rbind(output.dataframes.compact, data.byyear[1,])
      }
      
    }
    
  }
  
}

output.dataframes.compact[, "Month"] <- factor(tolower(output.dataframes.compact[, "Month"]),
       levels = tolower(c("January", "February", 
                          "March", "April", "May",
                          "June", "July", "August", 
                          "September", "October", 
                          "November", "December")))
# Save
write.csv(output.dataframes.compact[,-1][with(output.dataframes.compact, 
                                         order(Region, Market, Month, Year)), ] ,
          output.filename, row.names = FALSE, )
print("Saved")




