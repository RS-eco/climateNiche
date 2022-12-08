## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=F-------------------------------------------------------------
#  download.file(url="http://ebirddata.ornith.cornell.edu/downloads/ebd/prepackaged/ebd_relNov-2017.tar",
#                destfile="/home/mbiber/data/ebd_relNov-2017.tar")
#  untar("/home/mbiber/data/ebd_relNov-2017")

## ---- eval=F-------------------------------------------------------------
#  #' Load data.tabe library
#  library(data.table)
#  
#  #' Set file directory
#  #filedir <- "E:/Data"
#  filedir <- "/home/mbiber/data"
#  
#  #' eBird file path
#  #ebird_file <- paste0(filedir, "/ebd_relMay-2017.txt")
#  ebird_file <- paste0(filedir, "/ebd_relNov-2017.txt")
#  
#  #' Read eBird Data into R, has 171 GB of size, so specify subset of rows
#  ebird_data <- fread(ebird_file, nrow=10)
#  
#  #' See first 5 data entries
#  head(ebird_data)
#  colnames(ebird_data)
#  
#  #' Get colClasses
#  library(rmngb)
#  colClass_ebird <- as.character(colClasses(ebird_data))
#  #' Set date colClass
#  colClass_ebird[28] <- "date"
#  
#  #' Only select certain columns
#  ebird_data <- fread(ebird_file,
#                      select=c(5,6,8:18,23:29,33:40,43,44), nrow=10)
#  
#  #' Get column names
#  colnames_ebird <- colnames(ebird_data)
#  
#  #' Read only 1000 entries at a time and only required columns
#  ebird_data <- fread(ebird_file,
#                      select=c(5,6,8:18,23:29,33:40,43,44),
#                      col.names=colnames_ebird,
#                      colClasses=colClass_ebird,
#                      skip=0, nrow=1000, data.table=FALSE)
#  
#  
#  #' Turn Date column into date format
#  ebird_data$`OBSERVATION DATE` <- as.Date(ebird_data$`OBSERVATION DATE`)
#  
#  #' Turn Count column into numeric
#  library(dplyr)
#  ebird_data <- ebird_data %>%
#    mutate(`OBSERVATION COUNT` = as.numeric(replace(`OBSERVATION COUNT`,
#                                                    `OBSERVATION COUNT`=="X", 1)))
#  
#  #' Specify date and datetime columns for Database
#  date_cols <- ebird_data %>%
#    select_if(lubridate::is.Date) %>%
#    colnames()
#  datetime_cols <- ebird_data %>%
#    select_if(lubridate::is.POSIXt) %>%
#    colnames()

## ---- eval=F-------------------------------------------------------------
#  #' Create eBird database (chunk by chunk)
#  con <- src_sqlite(paste0(filedir, "/ebird_database.sqlite"), create=TRUE)

## ---- eval=F-------------------------------------------------------------
#  #' Connect to database
#  library(DBI)
#  con <- dbConnect(RSQLite::SQLite(),
#                   dbname = paste0(filedir, "/ebird_database.sqlite"))
#  
#  # write this first batch of lines to SQLITE table,
#  # converting dates to string representation
#  ebird_data[ , date_cols] <- as.character.Date(ebird_data[ , date_cols])
#  ebird_data[ , datetime_cols] <- as.character.POSIXt(ebird_data[ , datetime_cols])
#  dbWriteTable(con, "ebird", ebird_data, overwrite=TRUE)
#  
#  # Function that appends new sections to the table
#  append_to_sqlite <- function(x, pos) {
#    x <- as.data.frame(x)
#    x[ , date_cols] <- as.character.Date(x[ , date_cols])
#    x[ , datetime_cols] <- as.character.POSIXt(x[ , datetime_cols])
#    x <- x %>% mutate(`OBSERVATION COUNT` = as.numeric(replace(`OBSERVATION COUNT`,
#                                                               `OBSERVATION COUNT`=="X", 1)))
#    dbWriteTable(con, "ebird", x, append = TRUE)
#  }
#  
#  # readr chunk functionality
#  library(readr)
#  read_delim_chunked(file=paste0(filedir, "/ebd_relMay-2017.txt"),
#                     delim="\t", skip=1000, callback=append_to_sqlite, chunk_size = 50000,
#                     col_names = colnames_ebird,
#                     col_types = cols(
#                       `COMMON NAME` = col_character(),
#                       `SCIENTIFIC NAME` = col_character(),
#                       `SUBSPECIES SCIENTIFIC NAME` = col_character(),
#                       `OBSERVATION COUNT` = col_double(),
#                       `BREEDING BIRD ATLAS CODE` = col_logical(),
#                       `BREEDING BIRD ATLAS CATEGORY` = col_logical(),
#                       `AGE/SEX` = col_logical(),
#                       `COUNTRY` = col_character(),
#                       `COUNTRY CODE` = col_character(),
#                       `STATE CODE` = col_character(),
#                       `COUNTY CODE` = col_character(),
#                       `LOCALITY` = col_character(),
#                       `LOCALITY ID` = col_character(),
#                       `LOCALITY TYPE` = col_character(),
#                       LATITUDE = col_double(),
#                       LONGITUDE = col_double(),
#                       `OBSERVATION DATE` = col_date(format = ""),
#                       `TIME OBSERVATIONS STARTED` = col_time(format = ""),
#                       `PROTOCOL TYPE` = col_character(),
#                       `PROJECT CODE` = col_character(),
#                       `SAMPLING EVENT IDENTIFIER` = col_character(),
#                       `DURATION MINUTES` = col_integer(),
#                       `EFFORT DISTANCE KM` = col_double(),
#                       `EFFORT AREA HA` = col_double(),
#                       `NUMBER OBSERVERS` = col_integer(),
#                       `ALL SPECIES REPORTED` = col_integer(),
#                       APPROVED = col_integer(),
#                       REVIEWED = col_integer()
#                     ))
#  
#  #' Disconnect database
#  dbDisconnect(con)

## ---- eval=F-------------------------------------------------------------
#  #' Connect to database
#  con <- dbConnect(RSQLite::SQLite(), dbname = paste0(filedir, "/ebird_database.sqlite"))
#  
#  #' Create indexes
#  db_create_indexes(con, "ebird", indexes = list("SCIENTIFIC NAME", "OBSERVATION COUNT",
#                                                 "LATITUDE", "LONGITUDE", "OBSERVATION DATE"))
#  
#  #' See if table really exists
#  dbListTables(con)
#  
#  #' Now that we have copied the data, we can use tbl() to take a reference to it:
#  ebird_db <- tbl(con, "ebird")
#  ebird_db
#  
#  #' Disconnect from database
#  dbDisconnect(con)

