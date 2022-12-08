## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
#download.file(url="http://api.gbif.org/v1/occurrence/download/request/0000275-171219132708484.zip",
#              destfile="/home/mbiber/data/0000275-171219132708484.zip")
#unzip()

## ---- eval=F-------------------------------------------------------------
#  #' Load data.table library
#  library(data.table)
#  
#  #' Set file directory
#  #filedir <- "E:/Data"
#  filedir <- "/home/mbiber/data"
#  
#  # Specify file path
#  #gbif_file <- paste0(filedir, "/0002839-170627171947987.csv") #Old version
#  gbif_file <- paste0(filedir, "/0000275-171219132708484.csv")
#  
#  #' Read GBIF Data into R, has 437 GB of size, so only read specify subset of rows
#  gbif_data <- data.table::fread(gbif_file, nrow=10)
#  
#  #' See first 5 data entries
#  head(gbif_data)
#  #' See column names
#  colnames(gbif_data)
#  
#  #' Get colClasses
#  (colClass_gbif <- as.character(rmngb::colClasses(gbif_data)))
#  #' Define eventdate as date class
#  colClass_gbif[25] <- "date"
#  
#  #' Only select certain columns
#  gbif_data <- data.table::fread(gbif_file,
#                      select=c(1:10,14,15,17:31,40,44), nrow=10)
#  
#  #' Get column names
#  colnames_gbif <- colnames(gbif_data)
#  
#  #' Read only 1000 entries at a time and only required columns
#  gbif_data <- data.table::fread(gbif_file,
#                      select=c(1:10,14,15,17:31,40,44),
#                      col.names=colnames_gbif,
#                      colClasses=colClass_gbif,
#                      skip=0, nrow=1000, data.table=FALSE)
#  
#  #' Turn Date column into date format
#  gbif_data$eventdate <- as.Date(gbif_data$eventdate)
#  
#  #' Specify date and datetime columns for Database
#  library(dplyr, quiet=TRUE)
#  date_cols <- gbif_data %>%
#    select_if(lubridate::is.Date) %>%
#    colnames()
#  gbif_data[ , date_cols] <- as.character.Date(gbif_data[ , date_cols])
#  
#  #' Create gbif database (chunk by chunk)
#  con <- src_sqlite(paste0(filedir, "/gbif_database.sqlite"), create=TRUE)

## ---- eval=F-------------------------------------------------------------
#  #' Connect to database
#  library(DBI)
#  con <- dbConnect(RSQLite::SQLite(), dbname = paste0(filedir, "/gbif_database.sqlite"))
#  
#  # write this first batch of lines to SQLITE table,
#  # converting dates to string representation
#  #dbWriteTable(con, "gbif", gbif_data, overwrite=TRUE)
#  
#  # Function that appends new sections to the table
#  append_to_sqlite <- function(x, pos) {
#    x <- as.data.frame(x)
#    #x[ , date_cols] <- as.character.Date(x[ , date_cols])
#    dbWriteTable(con, "gbif", x, append = TRUE)
#  }
#  
#  # readr chunk functionality
#  library(readr)
#  read_delim_chunked(file=gbif_file, skip=377805260, callback=append_to_sqlite, delim=",",
#                   chunk_size = 100000,
#                   col_names = colnames_gbif,
#                     col_types = cols(
#                       gbifid = col_integer(),
#                       datasetkey = col_character(),
#                       occurrenceid = col_logical(),
#                       kingdom = col_character(),
#                       phylum = col_character(),
#                       class = col_character(),
#                       order = col_character(),
#                       family = col_character(),
#                       genus = col_character(),
#                       species = col_character(),
#                       countrycode = col_character(),
#                       locality = col_character(),
#                       decimallatitude = col_character(),
#                       decimallongitude = col_character(),
#                       coordinateuncertaintyinmeters = col_logical(),
#                       coordinateprecision = col_logical(),
#                       elevation = col_logical(),
#                       elevationaccuracy = col_logical(),
#                       depth = col_logical(),
#                       depthaccuracy = col_logical(),
#                       eventdate = col_character(),
#                       day = col_integer(),
#                       month = col_integer(),
#                       year = col_integer(),
#                       taxonkey = col_integer(),
#                       specieskey = col_integer(),
#                       basisofrecord = col_character(),
#                       typestatus = col_logical(),
#                       issue = col_character()
#                     ))
#  
#  #' Disconnect database
#  dbDisconnect(con)

## ---- eval=F-------------------------------------------------------------
#  #' Connect to database
#  con <- dbConnect(RSQLite::SQLite(), dbname = paste0(filedir, "/gbif_database.sqlite"))
#  
#  #' Create indexes
#  db_create_indexes(con=con, table="gbif", indexes = list("gbifid", "species", "decimallatitude",
#                                                "decimallongitude", "eventdate",
#                                                "taxonkey", "specieskey"))
#  
#  #' See if table really exists
#  dbListTables(con)
#  
#  #' Now that we have copied the data, we can use tbl() to take a reference to it:
#  gbif_db <- dplyr::tbl(con, "gbif")
#  head(gbif_db)
#  
#  #' Disconnect from database
#  dbDisconnect(con)

