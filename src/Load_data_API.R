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
    # set end date to the last day of the month for the month in which data was collected for the last time 
    # repeat until last data is from current month: then set date to yesterday
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
      last_date <- enddate + days(1)
    }
  }
  save(RescueTime_all, file = "data/RescueTime_all.RData")
}
update.RT()

# Make single df for pc and mobile devices: ---
get.RT.data <- function(){
  combine_df <- function(mylist){
    df <- bind_rows(mylist[which(map_int(mylist, nrow) > 0)]) %>%
      distinct() %>%
      rename(DateTime = Date,
             Time_sec = `Time Spent (seconds)`) %>%
      mutate(Date = as_date(DateTime),
             Productivity = case_when(Productivity == -2 ~ "Very Distracting",
                                        Productivity == -1 ~ "Distracting",
                                        Productivity == 0 ~ "Neutral",
                                        Productivity == 1 ~ "Productive",
                                        Productivity == 2 ~ "Very Productive"),
              Productivity = factor(Productivity, levels = c("Very Distracting",
                                                              "Distracting", "Neutral",
                                                              "Productive", "Very Productive"))) %>%
      arrange(DateTime)
  }
  
  load("data/RescueTime_all.RData")
  devices <- c("pc", "mobile")
  
  RescueTime <- map(devices, function(device){
    df <- combine_df(RescueTime_all[[device]])
  }) %>%
    magrittr::set_names(devices)
  
  add.overview <- function(df){
    data <- df %>%
      mutate(Overview = case_when(
        Activity == "iOS Device" ~ Activity,
        Category %in% c("Accounting","Administration", "Customer Relations", 
                        "General Business", "Intelligence", "Marketing",
                        "Operations", "Project Management","Sales") ~ 
          "Business",
        Category %in% c("Calendars", "Email", "General Communication & Scheduling", 
                        "Instant Message", "Meetings", "Voice Chat") ~
          "Communication & Scheduling",
        Category %in% c("General Design & Composition", "Graphic Design", 
                        "Presentation", "Writing") ~ 
          "Design & Composition",
        Category %in% c("Games", "Comedy", "General Entertainment", "Music", "Photos", "Video") ~
          "Entertainment",
        Category %in% c("Business", "General News & Opinion", "Regional", "Entertainment",
                        "International", "Science & Technology", "Sports", "Society") ~
          "News & Opinion",
        Category %in% c("Business & Finance", "Employment", "Engineering & Technology", 
                        "Food", "General Reference & Learning", "Health & Medicine", "Home & Garden", 
                        "Legal & Gov't", "Maps & Regional", "Search", "Travel & Outdoors") ~
          "Reference & Learning",
        Category %in% c("Clothes & Personal", "Electronics", "General Shopping", 
                        "Office") ~
          "Shopping",
        Category %in% c("General Social Networking", "Professional Networking") ~
          "Social Networking",
        Category %in% c("Data Modeling & Analysis", "Design & Planning", "Editing & IDEs", 
                        "General Software Development", "Quality Assurance", "Systems Operations") ~
          "Software Development",
        Category %in% c("Anti-Virus & Spyware", "Browsers", "File Sharing", 
                        "General Utilities", "Internet Utilities", "Virtualization", "Other") ~
          "Utilities",
        TRUE ~ "Uncategorized"))
  }

  organize.activity <- function(df) {
    data <- df %>%
      mutate(
        Activity = case_when(
          grepl("github", Activity, ignore.case = TRUE) == TRUE ~ "github",
          grepl("localhost", Activity) == TRUE ~ "localhost",
          grepl("office.com$|office365.com", Activity) == TRUE ~ "microsoft office",
          grepl("MS ", Activity, ignore.case = TRUE) == TRUE ~ gsub("MS ", "microsoft ", Activity),
          grepl("onedrive", Activity, ignore.case = TRUE) == TRUE ~ "microsoft OneDrive",
          grepl("OneNote", Activity, ignore.case = TRUE) == TRUE ~ "microsoft OneNote",
          grepl("^.*\\.microsoft\\..{2,3}$", Activity, ignore.case = TRUE) == TRUE ~ 
            paste("microsoft", str_extract(Activity, "^.*(?=\\.microsoft)")),
          grepl("^([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})",
            Activity) == TRUE ~ "IP address",
          grepl("^.*\\.google\\..{2,3}$", Activity, ignore.case = TRUE) == TRUE ~ 
            paste("google", str_extract(Activity, "^.*(?=\\.google)")),
          grepl("timeout|time out", Activity, ignore.case = TRUE) == TRUE ~ "timeout",
          grepl("sci-hub", Activity, ignore.case = TRUE) == TRUE ~ "Sci-hub",
          grepl("medium.com", Activity, ignore.case = TRUE) == TRUE ~ "Medium",
          grepl("strava.com", Activity, fixed = TRUE) == TRUE ~ "Strava",
          grepl("slack.com", Activity, fixed = TRUE) == TRUE ~ "Slack",
          grepl("facebook.com", Activity, fixed = TRUE) == TRUE ~ "Facebook",
          grepl("instagram.com", Activity, fixed = TRUE) == TRUE ~ "Instagram",
          grepl("twitter.com", Activity, fixed = TRUE) == TRUE ~ "Twitter",
          grepl("dropbox.com", Activity, fixed = TRUE) == TRUE ~ "Dropbox",
          grepl("ikea.com", Activity, fixed = TRUE) == TRUE ~ "Ikea",
          grepl("pinterest", Activity, ignore.case = TRUE) == TRUE ~ "Pinterest",
          grepl("goodreads", Activity, ignore.case = TRUE) == TRUE ~ "Goodreads",
          grepl("meetup", Activity, ignore.case = TRUE) == TRUE ~ "Meetup",
          grepl("wiki", Activity) == TRUE ~ "wikipedia",
          grepl("linkedin", Activity, ignore.case = TRUE) == TRUE ~ "Linkedin",
          grepl("^amazon.", Activity, ignore.case = TRUE) == TRUE ~ "Amazon",
          grepl("(\\.|^)ing\\.nl$", Activity, ignore.case = TRUE) == TRUE ~ "ING bank",
          grepl("endnote", Activity, ignore.case = TRUE) == TRUE ~ "Endnote",
          grepl("dreem", Activity, ignore.case = TRUE) == TRUE ~ "Dreem",
          grepl("ouraring", Activity, ignore.case = TRUE) == TRUE ~ "Oura Ring",
          grepl("netatmo", Activity, ignore.case = TRUE) == TRUE ~ "Netatmo",
          grepl("atmotube", Activity, ignore.case = TRUE) == TRUE ~ "Atmotube",
          grepl("rescuetime", Activity, ignore.case = TRUE) == TRUE ~ "Rescuetime",
          grepl("pubmed", Activity, ignore.case = TRUE) == TRUE ~ "Pubmed",
          grepl("zoom", Activity, ignore.case = TRUE) == TRUE ~ "Zoom",
          grepl("library.uu.nl$", Activity, ignore.case = TRUE) ~ "Utrecht University Library",
          grepl("uu\\.nl$", Activity, ignore.case = TRUE) == TRUE ~ "Utrecht University",
          grepl("jebentwatjemeet", Activity, ignore.case = TRUE) == TRUE ~ "JeBentWatJeMeet",
          grepl("komoot", Activity, ignore.case = TRUE) == TRUE ~ "Komoot",
          grepl("wahoofitness", Activity, ignore.case = TRUE) == TRUE ~ "Wahoofitness",
          grepl("withings", Activity, ignore.case = TRUE) == TRUE ~ "Withings",
          grepl("dhl", Activity, ignore.case = TRUE) == TRUE ~ "DHL",
          grepl("libreview", Activity, ignore.case = TRUE) == TRUE ~ "Abbott freestyle libre",
          grepl("shinyapps.io", Activity, fixed = TRUE) == TRUE ~ "shinyapps.io",
          grepl("rstudio.com", Activity, fixed = TRUE) == TRUE ~ "rstudio.com",
          grepl("rstudio.cloud", Activity, fixed = TRUE) == TRUE ~ "rstudio.cloud",
          grepl("r-project.org", Activity, fixed = TRUE) == TRUE ~ "r-project.org",
          grepl("r-lib.org", Activity, fixed = TRUE) == TRUE ~ "r-lib.org",
          grepl("ifttt.com", Activity, fixed = TRUE) == TRUE ~ "ifttt.com",
          grepl("postnl.nl", Activity, fixed = TRUE) == TRUE ~ "postnl",
          grepl("^cisco", Activity, ignore.case = TRUE) == TRUE ~ "cisco",
          grepl("ebay\\..{2,3}", Activity, ignore.case = TRUE) == TRUE ~ "ebay",
          grepl("safari-resource", Activity) == TRUE ~ "Safari",
          Activity == "wise.com" ~ "transferwise.com",
          Activity == "podcasts.apple.com" ~ "applepodcasts",
          Activity == "irenevdb.netlify.app" ~ "irenevdb.rbind.io",
          Activity == "google.com" ~ "google",
          Activity == "Login Window" ~ "loginwindow",
          Activity == "acrobat reader" ~ "adobe acrobat reader",
          Activity == "berichten" ~ "messages",
          Activity == "notities" ~ "Notes",
          Activity == "archiveringshulpprogramma" ~ "archive utility",
          Activity == "teksteditor" ~ "TextEdit",
          Activity == "systeemvoorkeuren" ~ "System Preferences",
          Activity == "voorvertoning" ~ "Preview",
          Activity == "foto's" ~ "photos",
          Activity %in% c("digitale-kleurenmeter", "digital colour meter") ~ "digital color meter",
          Activity == "lettertypecatalogus" ~ "font book",
          Activity == "installatieprogramma" ~ "installer",
          TRUE ~ Activity
        ),
        Activity = case_when(grepl("microsoft|google", Activity, ignore.case = TRUE) == TRUE ~ str_to_title(Activity),
                              TRUE ~ Activity)
      )
    
    # anonymize websites except when:
    # in top X of websites
    # specified, eg with rstudio, r-bloggers etc
    # a brand that has a simple-icon
    
    Top_websites <- data %>%
      filter(grepl("\\.[a-z]{2,}$", Activity) == TRUE) %>%
      group_by(Activity) %>%
      summarise(Time_sec = sum(Time_sec)) %>%
      arrange(desc(Time_sec))
    
    simple_icons <- pin_get("icons")[["simple-icons"]]
    
    data <- data %>%
      mutate(
        Activity = ifelse(
          grepl("\\.[a-z]{2,}$", Activity) == FALSE |
            tolower(str_extract(Activity, "^.*(?=\\..*$)")) %in% simple_icons |
            tolower(str_extract(Activity, "(?<=\\.).*(?=\\..*$)")) %in% simple_icons |
            tolower(str_extract(Activity, "^\\w+(?=\\s.*$)")) %in% simple_icons |
            Activity %in% Top_websites$Activity[1:10] |
            Activity %in% c(
              "rstudio.com",
              "r-bloggers.com",
              "rstudio.cloud",
              "r-project.org",
              "r-lib.org",
              "shinyapps.io",
              "irenevdb.rbind.io"
            ),
          Activity,
          "Various websites"
        ),
      ) 
  }
  
  RescueTime[["pc"]] <- RescueTime[["pc"]] %>%
    organize.activity() %>%
    add.overview() %>%
    mutate(Overview = fct_reorder(Overview, Time_sec, .fun = sum, .desc = TRUE))
 
  RescueTime[["day"]] <- RescueTime %>%
    bind_rows() %>%
    group_by(Date, Activity, Category, Productivity) %>%
    summarise(Time_sec = sum(Time_sec)) %>%
    ungroup() %>%
    add.overview() %>%
    mutate(
      Overview = fct_reorder(Overview, Time_sec, .fun = sum, .desc = TRUE),
      Category = fct_reorder(Category, Time_sec, .fun = sum, .desc = TRUE),
      Activity = fct_reorder(Activity, Time_sec, .fun = sum, .desc = TRUE),
      Category_lump = fct_other(Category, keep = levels(Category)[1:20]),
      Activity_lump = fct_other(Activity, keep = levels(Activity)[1:30])) 
  
  save(RescueTime, file = "data/RescueTime.RData")
  pin(RescueTime, "RescueTime")
  
}
get.RT.data()

