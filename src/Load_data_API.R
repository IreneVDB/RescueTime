library(httr)
library(keyring)
library(lubridate)
library(tidyverse)

get.RT.api <- function(key, perspective, taxonomy, interval, startdate, enddate, format, device, username, password){
  url <- paste0("https://www.rescuetime.com/anapi/data?key=", key, "&perspective=", perspective, "&restrict_kind=",
                taxonomy, "&interval=", interval, "&restrict_begin=", startdate, "&restrict_end=", enddate, 
                "&format=", format, "&restrict_source_type=", device)
  get_data <- GET(url, authenticate(username, password, type = "basic"))
  content <- content(get_data, as = "parsed")
}

# reports per minute are limited to time span of 1 month:
get.monthly.report.by.5min <- function(month, year, device){
  startdate <- ym(paste(year, month, "-"))
  enddate <- ymd(paste(year, month, as.numeric(days_in_month(startdate)), sep ="-"))
  
  data <- get.RT.api(key = key_get("key2", keyring= "RescueTime"),
                     perspective = "interval",
                     taxonomy = "activity",
                     interval = "minute",
                     startdate = startdate,
                     enddate = enddate,
                     format = "csv",
                     device = device,
                     username = key_get("username", keyring= "RescueTime"),
                     password = key_get("password", keyring= "RescueTime"))
  }

# get all for longer time span:
dates <- seq(ym("2020-07"), Sys.Date(), by = "month")
RescueTime <- list()

RescueTime[["pc"]] <- map2(month(dates), year(dates), get.monthly.report.by.5min, device = "computers")
names(RescueTime[["pc"]]) <- format(dates, "%Y-%m")

RescueTime[["mobile"]] <- map2(month(dates), year(dates), get.monthly.report.by.5min, device = "mobile")
names(RescueTime[["mobile"]]) <- format(dates, "%Y-%m")

save(RescueTime, file = "data/RescueTime.RData")


# Without Premium: get all data for past month, preferably every 1st day of the month:
load("data/RescueTime.RData")
last_month <- ym(format(Sys.Date(), format = "%Y-%m")) - months(1)

data_last_month <- map(c("computers", "mobile"), get.monthly.report.by.5min, 
                       month = month(last_month), year = year(last_month))

RescueTime[["pc"]][[format(last_month, "%Y-%m")]] <- data_last_month[[1]]
RescueTime[["mobile"]][[format(last_month, "%Y-%m")]] <- data_last_month[[2]]