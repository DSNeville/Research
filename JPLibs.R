#### 
### Try MonetDBLite
# Faster read.csv function
# Faster Aggregation
# Slower than sqldf for joins though

### Try dygraphs
# Interactive time series plotting
# RMD integration with shiny

     ######################################################################
############################ Reading Data ######################################
     ######################################################################

################ readr (part of tidyverse)######################################

# read_csv() for csv files
# read_csv2() for semicolon separated files
# read_tsv() reads tab delimited files
# read_delim() reads any delim
# read_fwf() reads in fixed width files
# read_log() for Apache style log files
# possible arguments:  
#    1. skip = n 
#        To skip lines
#    2. comment = '#'
#        To skip comments
#    3. col_names = FALSE
#        If you have no col names
#    4. col_names = c('other names')
#        If you want to rename some stuff
#    5. na = "what am I looking for?"
#        What the data used for NA values

### DT
# data.table::fread()  #  Really really fast with huge files

     ######################################################################
######################### Saving Data ##########################################
     ######################################################################

######################### Save Big File ########################################


jpSaveBigCSV <- function(data, filepath, filename){
  require('data.table')
  
  f <- paste0(filepath,'/',filename)

  write.table(data[0:1000000,],
              file = f,
              row.names = F,
              col.names = T,
              append = F,
              sep = ',')
  rows <- nrow(data)
  b <- 2000000
  a <- 1000000
  
  while(b < rows){
    write.table(data[a:b,],
                file = f,
                row.names = F,
                col.names = F,
                append = T,
                sep = ',')
    a <- a + 1000000
    b <- b + 1000000
  }
}

######################### Save RMD to HTML #####################################

jpRMDtoHTML <- function(filename) {
  # Converts RMD File to HTML Doc
  #should be RMD file type
  require('rmarkdown')
  options(warn = -1)
  render(
    filename,
    output_format = 'html_document',
    output_file = paste0(filename,
                         Sys.Date(),
                         '.html')
  )
}


######################### Get A Weekday ########################################

jpDextWeekDay <- function(date, wday) {
  date <- as.Date(date)
  diff <- wday - wday(date)
  if( diff < 0 )
    diff <- diff + 7
  return(date + diff)
}

jpSampleFeatures <- function(data, featureList, samplePercent){
  
  require('dplyr')
  
  train<-sample_frac(data, samplePercent)
  sid<-as.numeric(rownames(train)) # because rownames() returns character
  test<-a[-sid,]
}


     ######################################################################
######################### Data Structure #######################################
     ######################################################################

############################### shortcuts ######################################

jpD <- function(dataANDcolname, env = parent.frame()){
  #  Pass as dataframe$columnName

  x <- deparse(substitute(dataANDcolname))
  x <- gsub('\"','',x)
  df <- gsub("\\$.*", "", x)
  col <- gsub(".*\\$", "", x)
  data <- get(df)
  data[[col]] <- as.Date(dataANDcolname)
  assign(df,data, envir = env)
 
}

jpC <- function(dataANDcolname, env = parent.frame()){
  #  Pass as dataframe$columnName
  
  x <- deparse(substitute(dataANDcolname))
  x <- gsub('\"','',x)
  df <- gsub("\\$.*", "", x)
  col <- gsub(".*\\$", "", x)
  data <- get(df)
  data[[col]] <- as.character(dataANDcolname)
  assign(df,data, envir = env)
  
}

jpN <- function(dataANDcolname, env = parent.frame()){
  #  Pass as dataframe$columnName
  
  x <- deparse(substitute(dataANDcolname))
  x <- gsub('\"','',x)
  df <- gsub("\\$.*", "", x)
  col <- gsub(".*\\$", "", x)
  data <- get(df)
  data[[col]] <- as.numeric(dataANDcolname)
  assign(df,data, envir = env)
  
}

AppSummary2 <- AppSummary %>% filter(!is.na(FirstEntry)) %>%
   dcast(kiosk_id+date_scanned+placement_code ~ FirstEntry)

jpF <- function(dataANDcolname, env = parent.frame()){
  #  Pass as dataframe$columnName
  
  x <- deparse(substitute(dataANDcolname))
  x <- gsub('\"','',x)
  df <- gsub("\\$.*", "", x)
  col <- gsub(".*\\$", "", x)
  data <- get(df)
  data[[col]] <- as.factor(dataANDcolname)
  assign(df,data, envir = env)
  
}

############################### dplyr ##########################################

# pipe %>%
# spread:  make a skinny table wide
#
# library(dplyr)
# library(tidyr)
# df1 %>%
#   spread(element,value)
# date year month day gridNumber    PPT    TMAX    TMIN
# 1 1899-12-15 1899    12  15     526228 0.0000 43.4782 21.7403

############################### tidyr ##########################################

# make a skinny table wide
# spread(df1,element,value)

############################### rshape2 ##########################################

# assign <- data %>% 
#    dcast(colA + colB + colC ~ colD(The column to make WIDE))

############################### Rename and Reorder #############################

# reNameReOrder <- function(data, oldNames, newNames, nameOrder){
#   # Dataset (required)
# 
#   # list of new names (optional)
#   # list of order, new or old (optional)
#   
#   # Takes a dataset and renames columns based on two lists
#   #  A list of the order may also be supplied if it is needed
#   
#   if(missing(data)){
#     print('No data supplied')
#     break
#   }
#   
#   if(missing(oldNames) && missing(newNames) && missing(nameOrder)){
#     print('Nothing altered, supply a list of names to reorder or rename')
#     break()
#   }
#   
#   if(!missing(oldNames) && missing(newNames) && missing(nameOrder)){
#     # Re
#     data <- data[,c(oldNames)]
#     return(data)
#   }
#   
#   if(!missing(oldNames) && missing(newNames) && !missing(nameOrder)){
#     data <- data[,c(oldNames)]
#     return(data)
#   }
#   
#   if(missing(oldNames) && !missing(newNames) && !missing(nameOrder)){
#      dname <- names(data)
#      y == z
#      nameOrder %in% intersect(newNames, nameOrder)
#      
#   }
#   
# 
#   
#   if(missing(newNames)){
#     data <- data[,c(nameOrder)]
#     return(data)
#   }
#   
#  nyyy  -
#   nnn  -
#   ynn  -
#   yny  -
#   nyy  -
#   nny
#   nyn
#   yyy
#   
#   if(missing(nameOrder)){
#     
#   }
#   x == y
# 
#   
#   data
#   
# }


