library(httr)
library(keyring)
library(lubridate)
library(tidyverse)
library(pins)

get.RT.api <- function(key, perspective, taxonomy, interval, startdate, enddate, 
                       format = "csv", device, username, password){
  
  url <- "https://www.rescuetime.com/anapi/data"
  get <- GET(url, authenticate(username, password, type = "basic"),
                  query = list(key = key,
                               perspective = perspective,
                               restrict_kind = taxonomy,
                               interval = interval,
                               restrict_begin = startdate,
                               restrict_end = enddate,
                               format = format,
                               restrict_source_type = device))
  content <- content(get, as = "parsed")
}

# limit for 1 min activity data is 1 month - update per month max and repeat update until Sys.Date()
# with free account limited to 3 month history: need to update at least every ~ 3month!
# Note: Will not include most recent data from today (to avoid overlap or storage of 1st day of month)
update.RT <- function(){
  load("data/RescueTime_all.RData")
  
  last_date <- as_date(rev(rev(RescueTime_all[["pc"]])[[1]]$Date)[1]) # will give duplicates for that day
  
  done <- FALSE
  while(!done){
    # set end date to last day of month or to yesterday (Sys.Date - days(1))
    enddate <- ymd(paste(year(last_date), month(last_date), days_in_month(last_date)))
    if(enddate >= Sys.Date() - days(1)){
      enddate <- Sys.Date() - days(1)
    }
    
    new_data <- map(c("computers", "mobile"), get.RT.api,
                    key = key_get("key", keyring= "RescueTime"),
                    perspective = "interval",
                    taxonomy = "activity",
                    interval = "minute",
                    startdate = last_date,
                    enddate = enddate,
                    format = "csv",
                    username = key_get("username", keyring= "RescueTime"),
                    password = key_get("password", keyring= "RescueTime"))
    
    RescueTime_all[["pc"]][[paste0(last_date,"to", enddate)]] <- new_data[[1]]
    RescueTime_all[["mobile"]][[paste0(last_date,"to", enddate)]] <- new_data[[2]]
    
    if(enddate == Sys.Date() - days(1)){
      done <- TRUE
    } else{
      last_date <- as_date(rev(rev(RescueTime_all[["pc"]])[[1]]$Date)[1]) + days(1)
    }
  }
  save(RescueTime_all, file = "data/RescueTime_all.RData")
}
update.RT()

# Make single df for pc and mobile devices: ---
get.RT.data <- function(){
  load("data/RescueTime_all.RData")
  
  combine_df <- function(mylist){
    df <- bind_rows(mylist[which(map_int(mylist, nrow) > 0)]) %>%
      distinct() %>%
      rename(DateTime = Date) %>%
      mutate(Date = as_date(DateTime)) %>%
      arrange(DateTime)
  }
  
  devices <- c("pc", "mobile")
  
  RescueTime <- map(devices, function(device){
    df <- combine_df(RescueTime_all[[device]])
  }) %>%
    magrittr::set_names(devices)
  
  # for pc activity aggregate website names
  Top10_websites <- combine_df(RescueTime_all[["pc"]]) %>%
    filter(grepl("\\.[a-z]{2,3}$", Activity) == TRUE) %>%
    group_by(Activity) %>%
    summarise(Time = sum(`Time Spent (seconds)`)) %>%
    arrange(desc(Time))
  
  RescueTime[["pc"]] <- RescueTime[["pc"]] %>%
    mutate(Activity = case_when(grepl("github", Activity) == TRUE ~ "github.io",
                                TRUE ~ Activity),
           Activity = ifelse(grepl("\\.[a-z]{2,3}$", Activity) == FALSE |
                               Activity %in% Top10_websites$Activity[1:10] |
                               Activity %in% c("github.io", "rstudio.com", "r-bloggers.com", "stackoverflow.com"),
                             Activity, "Website_other"))
  
  save(RescueTime, file = "data/RescueTime.RData")
  pin(RescueTime, "RescueTime")
  
}
get.RT.data()