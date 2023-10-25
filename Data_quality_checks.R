## ---------------------------
##
## Author: Dr. Leonidas Liakos
##
## Date Created: 2023-02-17
##
## ---------------------------


library(tidyverse)
library(glue)
library(data.table)
library(jsonlite)
library(lubridate)



# User Defined Variables --------------------------------------------------

# The directory that R script resides. 
ROOT_DIR <- dirname(rstudioapi::getSourceEditorContext()$path) # it works only for Rstudio, set manually in other cases

# directory that contains CSV files - to be defined
CSV_DIR <- file.path(ROOT_DIR, "EUSEDcollab_data_repository", "Q_SSL")

# search pattern for csv that start with ID
PATTERN = "^ID_*.*csv$"


# The OUTPUT Directory - to be defined
OUTPUT_DIR <-file.path(ROOT_DIR, "EUSEDcollab_data_repository", "Q_SSL_QUALITY_CONTROL")

# Metadata file - to be defined
METADATA_FN  <- file.path(ROOT_DIR, "EUSEDcollab_data_repository", "ALL_METADATA.csv")


# just a read_delim option, no need to touch it
SHOW_COL_TYPES =F # If FALSE, do not show the guessed column types.







# Functions ---------------------------------------------------------------

#' Checks if a vector of time objects is sequential based on the diff of first elements defined in length parameter
#' @param x dataframe
#' @param length the length of the first n elements to test
#' @return boolean state, True if is sequential
is_sequential <- function(x, length=20){
  
  state=FALSE
  if (length(table(diff(unclass(x$`Date (DD/MM/YYYY)`)[1:length] %>% as.integer())))==1){
    state =TRUE
  }
  return(state)
} 




# Save a dataframe as Json ------------------------------------------------

SaveAsJson <- function(mydf_, fname){
  
  # save 
  outputfile <- glue("{tools::file_path_sans_ext(basename(fname))}_quality_check.json")
  json <- toJSON(mydf_, digits = NA)
  write(json, file = file.path(OUTPUT_DIR, outputfile))
  
}





# Quality test: Event data - variable timestep -------------------------------
Event_data_variable_timestep <- function(x){
  print(glue("    Event data - variable timestep"))
  tbl1 <- read_csv(x, show_col_types = SHOW_COL_TYPES) %>%  rename(Date=`Date (DD/MM/YYYY)`)
  
  names(tbl1)[names(tbl1) == 'Q m3 s-1'] <- 'Q'
  names(tbl1)[names(tbl1) == 'Q (m3 s-1)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'SSL (kg ts-1)'] <- 'SSL'
  names(tbl1)[names(tbl1) == 'time_interval (hh:mm:ss)'] <- 'time_interval'
  
  
  #date_period : number of days from first timestamp to last timestamp of data series
  date_period <-  difftime( max(tbl1$Date),  min(tbl1$Date), units="days") %>% as.integer()+1 
  
  N_events = tbl1$Event_index %>% unique() %>% length() # number of events - count the number of event indices values
  
  # Eventstart,Eventend = start and End timestamp of each event (eg event #: timestamp : timestamp, .....)
  tbl_B <-tbl1 %>% 
    group_by(Event_index) %>% 
    summarise(Eventstart=min(Date), Eventend=max(Date)) %>% 
    mutate(Event_length =as.integer(difftime(Eventend,Eventstart, units="mins")))  #Event_length = delta time between Eventstart and Eventend (eg event #: deltatime, .....)
  
  # -- , when the event only has one timestep, 'Event_length' is given a 0 value.
  #The code should make an exception if this is the case. 'Event_length' should equal the 'time interval' column (in minutes).
  tbl_B_zero_Event_length <- tbl_B %>% filter(Event_length==0) %>% left_join(tbl1, by="Event_index") %>% mutate(Event_length=hour(time_interval)*60 + minute(time_interval
  ))  %>% select(tbl_B %>% colnames())                                                          
  
  tbl_B <- tbl_B %>% filter(!Event_length==0) %>% rbind(tbl_B_zero_Event_length) %>% arrange(Event_index)
  
  
  tbl_C  <- tbl1 %>%
    group_by(Event_index) %>% 
    summarise(n_timesteps_Q  = sum(!is.na(`Q` )), # n_timesteps_Q= n timestamps of Q existing in each event (eg event #: n_timesteps, .......) - do not include nans
              missing_timesteps_Q = sum(is.na(`Q` )), #missing_timesteps_Q = n timestamps with nan (eg event #: n_timesteps_nan, .......) 
              n_timesteps_SSL  = sum(!is.na(`SSL` )),  # n timestamps of SSL existing in each event (eg event #: n_timesteps, .......) - do not include nans
              missing_timesteps_SSL  = sum(is.na(`SSL` )), #missing_timesteps_SSL = n timestamps with nan (eg event #: n_timesteps_nan, .......)
              Event_SSL_pct = (n_timesteps_SSL/n_timesteps_Q) * 100) %>% 
    left_join(tbl_B, by = "Event_index") 
  
  tbl_C <- tbl_C %>% mutate_at(c('Eventstart', 'Eventend'), as.character)
  
  
  tbl_D <- tibble(filename=basename(x),
                  type="Event data - variable timestep",
                  date_period = date_period,
                  N_events,
                  events=list(tbl_C))
  
  
  return(tbl_D)           
  
}




# Quality test: Event data - fixed timestep -------------------------------

Event_data_fixed_timestep <- function(x){
  print(glue("    Event data - fixed timestep"))
  tbl1 <- read_csv(x, show_col_types=SHOW_COL_TYPES) %>%  rename(Date=`Date (DD/MM/YYYY)`)
  
  if ("Comment" %in% colnames(tbl1)){
    tbl1 <-  subset(tbl1, select = -c(Comment) )
  }
  
  names(tbl1)[names(tbl1) == 'Q (m3 d-1)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'Q (m3 ts-1)'] <- 'Q'
  
  names(tbl1)[names(tbl1) == 'SSL (kg ts-1)'] <- 'SSL'
  names(tbl1)[names(tbl1) == 'SSL (kg d-1)'] <- 'SSL'
  
  N_events <- max(tbl1$Event_index)
  
  # In case I have dates like 2017-02-28 23:59:59
  # I will round for example 2017-02-28 23:59:59 to 2016-08-03 00:00:00.
  
  tbl1 <- tbl1 %>% mutate(Date= case_when(format(Date,'%S')=='59'~ ceiling_date(Date, unit='minute'),
                                          TRUE ~ Date))
  
  # or remove time that ends with 59 seconds
  #tbl1 <-tbl1 %>% filter(format(Date,'%S')=='59')
  
  # Then I will have 2 observations for  2016-08-03 00:00:00 and I will get the mean of the two.
  tbl1 <- tbl1 %>% group_by(Date) %>% 
    summarise(across(everything(), mean))
  
  date_period <-  difftime( max(tbl1$Date),  min(tbl1$Date), units="days") %>% as.integer()+1 #number of days from first timestamp to last timestamp of data series
  
  
  # -- calculate Measurement_timestep
  mytimes <- c(NA, difftime(tbl1$Date[-1],
                            tbl1$Date[-nrow(tbl1)],
                            units="mins"))
  Measurement_timestep <- mytimes%>% table() %>% as_tibble() %>% 
    filter(n == max(n)) %>% select('.') %>% as.integer()
  
  
  
  tbl_B <-tbl1 %>% 
    group_by(Event_index) %>% 
    summarise(Eventstart=min(Date), Eventend=max(Date), Event_timesteps_timeperiod=n()) 
  
  # IF data are in date time format
  if (!Measurement_timestep==1440){
    tbl_B <- tbl_B %>% mutate(Event_timesteps_timeperiod=Event_timesteps_timeperiod-1) }
  
  tbl_B <- tbl_B %>% mutate(Event_length =Measurement_timestep*Event_timesteps_timeperiod) 
  
  if (!Measurement_timestep==1440){
    tbl_B <- tbl_B %>% mutate(Event_timesteps_timeperiod=Event_timesteps_timeperiod+1) }
  
  
  
  tbl_C  <- tbl1 %>%
    group_by(Event_index) %>% 
    summarise(n_timesteps_Q = sum(!is.na(`Q` )),
              missing_timesteps_Q = sum(is.na(`Q` )),
              n_timesteps_SSL = sum(!is.na(`SSL` )),
              missing_timesteps_SSL  = sum(is.na(`SSL` ))
              
    ) %>% left_join(tbl_B, by = "Event_index" ) %>% 
    mutate(Event_SSL_pct=((n_timesteps_SSL/n_timesteps_Q) * 100))
  
  
  tbl_C <- tbl_C %>% mutate_at(c('Eventstart', 'Eventend'), as.character)
  
  
  tbl_D <- tibble(filename=basename(x),
                  type="Event data - fixed timestep",
                  date_period = date_period,
                  Measurement_timestep=Measurement_timestep,
                  N_events=N_events, # number of events - count the number of event indices values
                  events=list(tbl_C))
  
  
  return(tbl_D)
}

# Quality test:  Event data - aggregated ----------------------------------------------

Quality_test_Event_data_aggregated<- function(x){
  print(glue("    Event data - aggregated"))
  
  tbl1 <- read_csv(x, show_col_types=SHOW_COL_TYPES)
  names(tbl1)[names(tbl1) == 'sampling start'] <- 'Start date'
  names(tbl1)[names(tbl1) == 'sampling end'] <- 'End date'
  
  names(tbl1)[names(tbl1) == 'sampled volume (m3)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'sampled volume Q (m3)'] <- 'Q'
  
  
  
  names(tbl1)[names(tbl1) == 'Q (m3 ts-1)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'Q (m3 d-1)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'Q (m3 event-1)'] <- 'Q'
  
  names(tbl1)[names(tbl1) == 'SSL (kg d-1)'] <- 'SSL'
  names(tbl1)[names(tbl1) == 'SSL (kg)'] <- 'SSL'
  names(tbl1)[names(tbl1) == 'SSL (kg event-1)'] <- 'SSL'
  names(tbl1)[names(tbl1) == 'SSL (kg ts-1)'] <- 'SSL'
  
  
  
  
  tbl_A <- tibble(filename=basename(x),
                  type="Event data - aggregated",
                  date_period =  difftime( max(tbl1$`End date`),  min(tbl1$`Start date` ), units="days") %>% as.integer()+1, # number of days from first timestamp to last timestamp of data series
                  N_events = nrow(tbl1), # number of events - count the number of rows
                  
                  missing_events_Q =  tbl1 %>% filter(is.na( `Q`)) %>% nrow(), # dates where Q is NULL
                  missing_events_SSL = tbl1 %>% filter(is.na( `SSL`)) %>% nrow()) # dates where SSL is NULL
  return(tbl_A)
  
}





# Quality test: Monthly data ----------------------------------------------

Quality_test_Monthly <- function(x){
  print("    Monthly data - fixed timestep")

  tbl1 <- read_csv(x, show_col_types=SHOW_COL_TYPES)  %>%
    
    rename(Date=`Date (DD/MM/YYYY)`)
  
  names(tbl1)[names(tbl1) == 'Q (m3 m-1)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'Q (m3 d-1)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'SSL (kg m-1)'] <- 'SSL'
  names(tbl1)[names(tbl1) == 'SSL (kg d-1)'] <- 'SSL'
  
  # drop empty rows
  #tbl1 <- tbl1 %>% drop_na(Date, Q,  SSL)
  
  
  total_months <- tbl1$Date %>% unique() %>% length() # unique months in the data record
  date_period_months  <- length(seq(from=min(tbl1$Date), to=max(tbl1$Date), by='month')) # total months from start to end month
  Missing_months_Q.SSL<- tbl1 %>% filter(is.na( `Q`)& is.na(`SSL`)) %>% nrow() # dates where Q AND SSL are NULL
  missing_months_pct_Q.SSL = Missing_months_Q.SSL*100/total_months %>% as.integer() # % of missing_days_Q.SSL from total_Days
  missing_months_SSL.not.Q = tbl1 %>% filter(!is.na( `Q`)) %>%filter(is.na( `SSL`)) %>%  nrow() # dates where Q is not NULL AND SSL is NULL
  missing_months_Q = tbl1 %>% filter(is.na( `Q`)) %>% nrow() # dates where Q is NULL
  missing_months_SSL = tbl1 %>% filter(is.na( `SSL`)) %>% nrow() # dates where SSL is NULL
  
  
  
  tbl_A <- tibble(filename=basename(x),
                  type="Monthly data",
                  total_months=total_months,
                  date_period_months =date_period_months,
                  Missing_months_Q.SSL=Missing_months_Q.SSL,
                  missing_months_pct_Q.SSL=missing_months_pct_Q.SSL,
                  missing_months_SSL.not.Q=missing_months_SSL.not.Q,
                  missing_months_Q=missing_months_Q,
                  missing_months_SSL=missing_months_SSL
  )
  
  
  
  # number of consecutive NAs for SSL
  
  tbl1 <- tbl1 %>% mutate(NA_Q.SSL=case_when(
    is.na( `Q`)& is.na(`SSL`)~ NA_real_, TRUE ~ 1
  )) %>% as.data.frame()
  
  
  is.na.rle <- rle(is.na(tbl1$NA_Q.SSL)) # https://stackoverflow.com/questions/16842163/consecutive-nas-in-a-column
  tbl1$Null_Q.SSL <- rep(is.na.rle$values*is.na.rle$lengths,is.na.rle$lengths)
  tbl1 <-tbl1 %>% select(-NA_Q.SSL)
  
  # number of consecutive NAs for SSL
  is.na.rle <- rle(is.na(tbl1$`SSL`))
  tbl1$Null_SSL <- rep(is.na.rle$values*is.na.rle$lengths,is.na.rle$lengths)
  
  # length of periods with NAs(values=!0) and with data (values=0)
  r <- rle(tbl1$Null_SSL )
  r <- data.frame(unclass(r))
  
  # count days of periods with maximum consecutive NAs
  max_gap_days <- r %>% filter(!values==0) %>% pull(values) %>% max()
  
  # Grouping by consecutive value occurrences
  setDT(tbl1)[, group_Null_SSL := rleid(Null_SSL)][] # https://stackoverflow.com/questions/55169156/grouping-by-consecutive-value-occurrences
  
  # find start date and End date of max periods with NAs (could be more than one, but with same length):
  tbl_B <-tbl1 %>% 
    filter(Null_SSL==max_gap_days) %>% 
    group_by(group_Null_SSL) %>% 
    summarise(startDate=min(Date), endDate=max(Date)) %>%
    mutate(gap_days=as.integer(1+difftime(endDate,startDate, units="days"))) %>%  
    select(-group_Null_SSL) # finally we dont need gap_days, see email 20.12.2022 Francis
  
  
  
  # tbl_A$gaps_periods <- list(tbl_B) # max periods with NAs for SSL (could be more than one, but with same length), contains: startDate,endDate = start date and End date of each period
  return(tbl_A)
}



# Quality test: Daily data - fixed timestep -------------------------------

Daily_data_fixedtimestep <- function(x){
  #browser()
  print(glue("    Daily data - fixed timestep"))
  
  tbl1 <- read_csv(x, show_col_types=SHOW_COL_TYPES)
  
  names(tbl1)[names(tbl1) == 'Date (DD/MM/YYYY)'] <- 'Date'
  names(tbl1)[names(tbl1) == 'Date (DD/MM/YYYY hh:mm)'] <- 'Date'
  
  names(tbl1)[names(tbl1) == 'Q (m3 d-1)'] <- 'Q'
  names(tbl1)[names(tbl1) == 'Q m3 d-1'] <- 'Q'
  
  names(tbl1)[names(tbl1) == 'SSC (kg m-3)'] <- 'SSC'
  names(tbl1)[names(tbl1) == 'SSC (kg m-3 d-1)'] <- 'SSC'
  names(tbl1)[names(tbl1) == 'SSC kg m-3'] <- 'SSC'
  
  names(tbl1)[names(tbl1) == 'SSL kg d-1'] <- 'SSL'
  names(tbl1)[names(tbl1) == 'SSL (kg d-1)'] <- 'SSL'
  
  
  tbl1 <- tbl1 %>% drop_na(Date)
  
  total_Days <- tbl1$Date %>% unique() %>% length()
  date_period <-  difftime( max(tbl1$Date),  min(tbl1$Date), units="days") %>% as.integer()+1
  
  missing_days_Q.SSL<- tbl1 %>% filter(is.na( `Q`)& is.na(`SSL`)) %>% nrow()
  missing_days_SSL.not.Q<- tbl1 %>% filter(!is.na( `Q`)) %>%filter(is.na( `SSL`)) %>%  nrow()
  missing_days_Q<- tbl1 %>% filter(is.na( `Q`)) %>% nrow()
  missing_days_SSL<- tbl1 %>% filter(is.na( `SSL`)) %>% nrow()
  
  missing_days_pct<-missing_days_Q.SSL*100/total_Days %>% as.integer()
  
  tbl_A <- tibble(filename=basename(x),
                  type="Daily data - fixed timestep",
                  total_Days=total_Days,
                  date_period =date_period,
                  missing_days_Q.SSL=missing_days_Q.SSL,
                  missing_days_pct_Q.SSL=missing_days_pct,
                  missing_days_SSL.not.Q=missing_days_SSL.not.Q,
                  missing_days_Q=missing_days_Q,
                  missing_days_SSL=missing_days_SSL
  )
  
  # number of consecutive NAs for Q.SSL
  
  tbl1 <- tbl1 %>% mutate(NA_Q.SSL=case_when(
    is.na( `Q`)& is.na(`SSL`)~ NA_real_, TRUE ~ 1
  )) %>% as.data.frame()
  
  is.na.rle <- rle(is.na(tbl1$NA_Q.SSL)) # https://stackoverflow.com/questions/16842163/consecutive-nas-in-a-column
  tbl1$Null_Q.SSL <- rep(is.na.rle$values*is.na.rle$lengths,is.na.rle$lengths)
  tbl1 <-tbl1 %>% select(-NA_Q.SSL)
  
  # number of consecutive NAs for SSL
  is.na.rle <- rle(is.na(tbl1$`SSL`))
  tbl1$Null_SSL <- rep(is.na.rle$values*is.na.rle$lengths,is.na.rle$lengths)
  
  # length of periods with NAs(values=!0) and with data (values=0)
  r <- rle(tbl1$Null_SSL )
  r <- data.frame(unclass(r))
  
  # count days of periods with maximum consecutive NAs
  max_gap_days <- r %>% filter(!values==0) %>% pull(values) %>% max()
  
  # Grouping by consecutive value occurrences
  setDT(tbl1)[, group_Null_SSL := rleid(Null_SSL)][] # https://stackoverflow.com/questions/55169156/grouping-by-consecutive-value-occurrences
  
  # find start date and End date of max periods with NAs (could be more than one, but with same length):
  tbl_B <-tbl1 %>% 
    filter(Null_SSL==max_gap_days) %>% 
    group_by(group_Null_SSL) %>% 
    summarise(startDate=min(Date), endDate=max(Date)) %>%
    mutate(gap_days=as.integer(1+difftime(endDate,startDate, units="days"))) %>% 
    select(-group_Null_SSL) # finally we dont need gap_days, see email 20.12.2022 Francis
  
  
  #tbl_A$gaps_periods <- list(tbl_B)
  return(tbl_A)
  
  
}



# QualityCheck ------------------------------------------------------------

QualityCheck <- function(fname){
  counter <<- counter + 1
  
  ## Check file type 
  mycsv <- read_csv(fname, show_col_types=SHOW_COL_TYPES)
  print(sprintf("%s: %s", counter, fname))
  
  datatype <- METADATA %>% filter(`File name`== basename(fname)) %>% pull(`Data type`)
  
  
  if (datatype == "Event data - fixed timestep"){
    tbl_quality_check <-Event_data_fixed_timestep(fname)
    SaveAsJson(tbl_quality_check,fname)
  }
  
  if (datatype == "Event data - variable timestep"){
    tbl_quality_check <-Event_data_variable_timestep(fname)
    SaveAsJson(tbl_quality_check, fname)
  }
  
  if (datatype == "Daily data - fixed timestep"){
    tbl_quality_check <- Daily_data_fixedtimestep(fname)
    SaveAsJson(tbl_quality_check, fname)
  }
  
  if (datatype == "Event data - aggregated"){
    tbl_quality_check <- Quality_test_Event_data_aggregated(fname) # do quality test
    SaveAsJson(tbl_quality_check,fname)
  }
  
  if (datatype == "Monthly data"){
    tbl_quality_check <- Quality_test_Monthly(fname) # do quality test
    SaveAsJson(tbl_quality_check, fname)
    
  }
  
}



# list csv files
csv_files = list.files(path = CSV_DIR, pattern=PATTERN, full.names =T)

# read metadata file
METADATA <- read_csv(METADATA_FN) %>% 
  select("Data type", "File name")

# loop in files and apply quality check ---------------------------------
counter <- 0
lapply(csv_files, QualityCheck)

