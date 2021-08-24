library(httr)
library(keyring)
library(lubridate)
library(tidyverse)
library(pins)

get.RT.api <- function(key, perspective, taxonomy, interval, startdate, enddate, format, device, username, password){
  url <- paste0("https://www.rescuetime.com/anapi/data?key=", key, "&perspective=", perspective, "&restrict_kind=",
                taxonomy, "&interval=", interval, "&restrict_begin=", startdate, "&restrict_end=", enddate, 
                "&format=", format, "&restrict_source_type=", device)
  get_data <- GET(url, authenticate(username, password, type = "basic"))
  content <- content(get_data, as = "parsed")
}
update.RT <- function(){
  last_date <- as_date(rev(rev(RescueTime[["pc"]])[[1]]$Date)[1])
  
  new_data <- map(c("computers", "mobile"), get.RT.api,
                  key = key_get("key", keyring= "RescueTime"),
                  perspective = "interval",
                  taxonomy = "activity",
                  interval = "minute",
                  startdate = last_date,
                  enddate = Sys.Date(),
                  format = "csv",
                  username = key_get("username", keyring= "RescueTime"),
                  password = key_get("password", keyring= "RescueTime"))
  
  RescueTime[["pc"]][[paste0(last_date,"to", Sys.Date())]] <- new_data[[1]]
  RescueTime[["mobile"]][[paste0(last_date,"to", Sys.Date())]] <- new_data[[2]]
}

# update rescuetime data with the lastest data per 5 min (only up to 1 month can be obtained with free account)
load("data/RescueTime_all.RData")
update.RT()
save(RescueTime_all, file = "data/RescueTime_all.RData")

# filter Activities to aggregate / anonymize websites:
RescueTime <- list()

Top10_websites <- bind_rows(RescueTime_all[["pc"]][which(map_int(RescueTime_all[["pc"]], nrow) > 0)]) %>%
  distinct() %>%
  filter(grepl("\\.[a-z]{2,3}$", Activity) == TRUE) %>%
  group_by(Activity) %>%
  summarise(Time = sum(`Time Spent (seconds)`)) %>%
  arrange(desc(Time))

RescueTime[["pc_all"]] <- bind_rows(RescueTime_all[["pc"]][which(map_int(RescueTime_all[["pc"]], nrow) > 0)]) %>%
  distinct() %>%
  mutate(Activity = case_when(grepl("github", Activity) == TRUE ~ "github.io",
                              TRUE ~ Activity),
         Activity = ifelse(grepl("\\.[a-z]{2,3}$", Activity) == FALSE |
                             Activity %in% Top10_websites$Activity[1:10] |
                             Activity %in% c("github.io", "rstudio.com", "r-bloggers.com", "stackoverflow.com"),
                           Activity, "Website_other"))

RescueTime[["mobile_all"]] <- bind_rows(RescueTime_all[["mobile"]][which(map_int(RescueTime_all[["mobile"]], nrow) > 0)]) %>%
  distinct()

pin(RescueTime, "RescueTime")


