# Reactable for RescueTime
library(pins)
library(tidyverse)
library(lubridate)
library(png)
library(reactable) 
library(htmltools)
library(sparkline)
library(icons) 
library(htmlwidgets)

RescueTime <- pin_get("RescueTime")

Productivity_col <- rgb(red = c(13, 65, 177, 218, 212, 160),
                        green = c(88, 130, 193, 105, 28, 235),
                        blue = c(193, 221, 191, 93, 21, 142),
                        names = c("Very Productive", "Productive", "Neutral", 
                                  "Distracting", "Very Distracting", "Mobile Phone"),
                        maxColorValue = 255)

icon_col <- c("#e5eeff", "#a6c4ff", "#251a66")

all_dates <- seq(RescueTime[["day"]]$Date[1], rev(RescueTime[["day"]]$Date)[1], by = "day")

# change uncategorized categories:---
uncategorize <- function(df){
  lookup <- df %>%
    group_by(Overview, Category, Activity) %>%
    summarise(Time = sum(Time_sec))%>%
    arrange(Activity, desc(Time)) %>%
    group_by(Activity) %>%
    mutate(Ov1 = first(Overview),
           Cat1 = first(Category)) %>%
    filter(Overview == "Uncategorized",
           Category == "Uncategorized",
           Ov1 != "Uncategorized",
           Cat1 != "Uncategorized") %>%
    mutate(OCA = paste(Overview, Category, Activity))
  
  data <- df %>%
    mutate(OCA = paste(Overview, Category, Activity)) %>%
    rowwise() %>%
    mutate(Overview = ifelse(OCA %in% lookup$OCA,
                             as.character(lookup[which(lookup$OCA == OCA),]$Ov1),
                             as.character(Overview)),
           Category= ifelse(OCA %in% lookup$OCA,
                            as.character(lookup[which(lookup$OCA == OCA),]$Cat1),
                            as.character(Category))) %>%
    ungroup() %>%
    mutate(Time_min = Time_sec / 60,
           Overview = fct_reorder(Overview, Time_min, sum, .desc = TRUE),
           Category = fct_reorder(Category, Time_min, sum, .desc = TRUE))
}
data <- uncategorize(RescueTime[["pc"]])

# create the basis table for Overview, Category, Activity: -----
summarise.table <- function(df, ...){
  
  group_vars <- enquos(...)
  group_syms <- ensyms(...)
  
  ProdPulse <- df %>%
    group_by(!!!group_vars, Productivity) %>%
    summarise(Time_min = sum(Time_min)) %>%
    complete(., Productivity, fill = list(Time_min = 0)) %>%
    summarise(Productivity = list(as.character(Productivity)),
              Pulse_rel = list(Time_min / sum(Time_min) * 100),
              Pulse_abs = list(Time_min),
              Daily_min = sum(Time_min) / length(all_dates),
              Daily_time = case_when(Daily_min < 1 ~ paste(round(Daily_min * 60), "sec"),
                                     TRUE ~ paste(round(Daily_min), "min")),
              .groups = "drop_last") %>%
    mutate(Percentage = Daily_min / sum(Daily_min)) %>%
    select(Percentage, !!!group_vars, everything())
  
  MonthlyTrend <- df %>%
    mutate(ym = factor(format(DateTime, format = "%Y-%m")),
           Productivity = case_when(grepl("Distracting", Productivity) ~ "Distracting",
                                    TRUE ~ "Productive")) %>%
    group_by(!!!group_vars, Productivity, ym) %>%
    summarise(Time_min = sum(Time_min) / days_in_month(ym(first(ym)))[[1]], .groups = "drop") %>%
    # the nesting gave me headaches: !!enquo or {{}} does not work here...
    complete(., expand(., nesting(!!!group_syms), Productivity, ym), 
             fill = list(Time_min = 0)) %>%
    group_by(!!!group_vars, Productivity) %>%
    summarise(MonthlyTrend = list(Time_min),
              MonthLabel = list(format(ym(ym), "%B %Y"))) %>%
    pivot_wider(., names_from = Productivity, values_from = MonthlyTrend,
                names_prefix = "monthly_")
  
  DailyTrend <- df %>%
    mutate(Productivity = case_when(grepl("Distracting", Productivity) ~ "Distracting",
                                    TRUE ~ "Productive")) %>%
    group_by(!!!group_vars, Productivity, Date) %>%
    summarise(Time_min = sum(Time_min), .groups = "drop") %>%
    right_join(., data.frame(Date = all_dates), by = "Date") %>%
    complete(., expand(., nesting(!!!group_syms), Productivity, Date),
             fill = list(Time_min= 0)) %>%
    filter(if_all(all_of(c(map_chr(group_vars, rlang::as_name), "Productivity")), ~ !is.na(.))) %>% # turn the ... into vector with strings
    group_by(!!!group_vars, Productivity) %>%
    summarise(DailyTrend = list(Time_min),
              DateLabel = list(format(Date, format = "%b %d %Y"))) %>%
    pivot_wider(., names_from = Productivity, values_from = DailyTrend,
                names_prefix = "daily_")
  
  Summary <-  ProdPulse %>%
    left_join(., DailyTrend) %>%
    left_join(., MonthlyTrend) %>%
    arrange(Overview, desc(Daily_min))

}
# function to add icon-column:
add.icons <- function(df, value = Activity, path_img = "img/Programs"){
  
  simple_icons <- pin_get("icons")[["simple-icons"]]
  academic_icons <- pin_get("icons")[["academic-icons"]]
  fontawesome_icons <- pin_get("icons")[["fontawesome"]]
  customicons <- pin_get("icons")[["custom_black"]]
  
  df <- df %>%
    mutate(icon = case_when(tolower(gsub("\\s", "", {{value}})) %in% academic_icons ~ 
                              paste("academic-icons", tolower(gsub("\\s", "", {{value}}))),
                            tolower(str_extract({{value}}, "^.*(?=\\..*$)")) %in% academic_icons ~ 
                              paste("academic-icons", tolower(str_extract({{value}}, "^.*(?=\\..*$)"))),
                            tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)")) %in% academic_icons ~ 
                              paste("academic-icons", tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)"))),
                            tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)")) %in% academic_icons ~ 
                              paste("academic-icons", tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)"))),
                            tolower(gsub("\\s*", "", {{value}})) %in% simple_icons ~ 
                              paste("simple-icons", tolower(gsub("\\s", "", {{value}}))), # remove white space (" " does not always work)
                            tolower(str_extract({{value}}, "^.*(?=\\..*$)")) %in% simple_icons ~ 
                              paste("simple-icons", tolower(str_extract({{value}}, "^.*(?=\\..*$)"))), # e.g. ebay.com:look for ebay
                            tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)")) %in% simple_icons ~ 
                              paste("simple-icons", tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)"))), #e.g. account.google.com: look for google
                            tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)")) %in% simple_icons ~ 
                              paste("simple-icons", tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)"))), # get word before white space
                            tolower(gsub("\\s", "", {{value}})) %in% fontawesome_icons ~ 
                              paste("fontawesome", tolower(gsub("\\s", "", {{value}}))),
                            tolower(str_extract({{value}}, "^.*(?=\\..*$)")) %in% fontawesome_icons ~ 
                              paste("fontawesome", tolower(str_extract({{value}}, "^.*(?=\\..*$)"))),
                            tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)")) %in% fontawesome_icons ~ 
                              paste("fontawesome", tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)"))),
                            tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)")) %in% fontawesome_icons ~ 
                              paste("fontawesome", tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)"))),
                            tolower(str_extract({{value}}, "^.*(?=s$)")) %in% fontawesome_icons ~ 
                              paste("fontawesome", tolower(str_extract({{value}}, "^.*(?=s$)"))),
                            tolower(gsub("\\s", "", {{value}})) %in% customicons ~ 
                              paste0(tolower(gsub("\\s", "", {{value}})), ".png"),
                            tolower(str_extract({{value}}, "^.*(?=\\..*$)")) %in% customicons ~ 
                              paste0(tolower(str_extract({{value}}, "^.*(?=\\..*$)")), ".png"),
                            tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)")) %in% customicons ~ 
                              paste0(tolower(str_extract({{value}}, "(?<=\\.).*(?=\\..*$)")), ".png"),
                            tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)")) %in% customicons ~ 
                              paste0(tolower(str_extract({{value}}, "^\\w+(?=\\s.*$)")), ".png"),
                            tolower(gsub("\\s", "", str_extract({{value}}, "^.*(?=\\s.*$)"))) %in% customicons ~ 
                              paste0(gsub("\\s", "",tolower(str_extract({{value}}, "^.*(?=\\s.*$)"))), ".png"),
                            {{value}} == "irenevdb.rbind.io" ~ "jebentwatjemeet.png",
                            {{value}} == "Various websites" ~ "simple-icons safari",
                            {{value}} == "loginwindow" ~ "fontawesome sign-in-alt",
                            {{value}} == "Preview" ~ "fontawesome search",
                            {{value}} == "timeout" ~ "fontawesome hourglass-half",
                            {{value}} == "Mail" ~ "fontawesome envelope",
                            {{value}} == "messages" ~ "fontaweomse comment",
                            grepl("photo", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome image",
                            {{value}} == "iWork Numbers" ~ "fontawesome chart-bar",
                            {{value}} == "onewireviewer" ~ "simple-icons java",
                            {{value}} %in% c("localhost", "IP address") ~ "fontawesome network-wired",
                            {{value}} == "installer" ~ "fontawesome download",
                            {{value}} == "screencastomatic" ~ "video",
                            {{value}} %in% c("reminders", "Notes", "pages", "TextEdit") ~ "fontawesome list",
                            {{value}} == "brackets" ~ "fontawesome code",
                            {{value}} == "System Preferences" ~ "fontawesome cog",
                            grepl("europa", {{value}}, ignore.case = TRUE) ~ "fontawesome globe-europe",
                            grepl("sql", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome database",
                            grepl("^r-|shinyapps", {{value}}, ignore.case = TRUE) == TRUE ~ "simple-icons r",
                            grepl("color", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome eye-dropper",
                            grepl("keychain", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome key",
                            grepl("printer", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome print",
                            grepl("books", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome book-open",
                            grepl("endnote", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome folder-open",
                            grepl("server", {{value}}, ignore.case = TRUE) == TRUE ~ "fontawesome server",
                            TRUE ~ ""))
}

Overview <- summarise.table(data, Overview)
Category <- summarise.table(data, Overview, Category)
Activity<- summarise.table(data, Overview, Category, Activity) %>%
  add.icons()

# Define columns for Overview, Category, Activity ----
set.columns <- function(reactable_df, 
                        colwidth_name = 320, 
                        colwidth_pulse = 280,
                        colwidth_donut = 56,
                        col_height = 52){
  
  columns <- list(
    Overview = colDef(name = "Main Category",
                      width = colwidth_name, 
                      style = list(height = col_height),
                      cell = function(value){
                        img_src <- knitr::image_uri(paste0("img/Categories/",
                                                           gsub(" .*$", "", value), ".png"))
                        image <- img(src = img_src, width = "48px", alt = value)
                        div(style = list(display = "flex", alignItems = "center"),
                            image,
                            div(style = list(marginLeft = "8px"), value))
                      }),
    Pulse_rel = colDef(name = "Average Time per Day",
                       width = colwidth_pulse,
                       cell = function(value, index) {
                         stacked_bar(df = Overview, 
                                     list_of_values = value,
                                     row = index, height = "40px")
                       }),
    daily_Distracting = colDef(name = "Last 30 days",
                               align = "center",
                               cell = function(value, index) {
                                 sparkline(values = cbind(-map(Overview$daily_Distracting, ~round(magrittr::extract(rev(.), 30:1)))[[index]], 
                                                          map(Overview$daily_Productive, ~round(magrittr::extract(rev(.), 30:1)))[[index]]),
                                           type = "bar", 
                                           zeroColor	= icon_col[3],
                                           height = 40,
                                           width = 30 * 4,
                                           stackedBarColor = bar_col,
                                           tooltipFormatter = JS(
                                             sprintf(
                                               "function(sparkline, options, field){
                                                           debugger;
                                                           return('<b>'+ %s[field[0].offset] + '</b><br/>' +
                                                           '<span style=color:' + field[0].color + '> &#9679 </span>' +
                                                           Math.round(field[0].value) + ' min/day' + '<br/>' +
                                                           '<span style=color:' + field[1].color + '> &#9679 </span>' +
                                                           -Math.round(field[1].value) + ' min/day');
                                                           }",
                                               jsonlite::toJSON(rev(Overview$DateLabel[[1]])[30:1])
                                             )
                                           ))
                               }),
    monthly_Distracting = colDef(name = "Monthly Trend",
                                 align = "center",
                                 cell = function(value, index) {
                                   sparkline(values = cbind(-Overview$monthly_Distracting[[index]], 
                                                            Overview$monthly_Productive[[index]]),
                                             type = "bar", 
                                             height = 40,
                                             width = 4 * length(Overview$MonthLabel[[1]]),
                                             zeroColor	= icon_col[3],
                                             stackedBarColor = bar_col,
                                             tooltipFormatter = JS(
                                               sprintf(
                                                 "function(sparkline, options, field){
                                                           debugger;
                                                           return('<b>'+ %s[field[0].offset] + '</b><br/>' +
                                                           '<span style=color:' + field[0].color + '> &#9679 </span>' +
                                                           Math.round(field[0].value) + ' min/day' + '<br/>' +
                                                           '<span style=color:' + field[1].color + '> &#9679 </span>' +
                                                           -Math.round(field[1].value) + ' min/day');
                                                           }",
                                                 jsonlite::toJSON(Overview$MonthLabel[[1]])
                                               )
                                             ))
                                 }),
    Percentage = colDef(show = FALSE),
    Productivity = colDef(show = FALSE),
    Pulse_abs = colDef(show = FALSE),
    MonthLabel = colDef(show = FALSE),
    DateLabel = colDef(show = FALSE),
    Daily_min = colDef(show = FALSE),
    Daily_time = colDef(show = FALSE),
    daily_Productive = colDef(show = FALSE),
    monthly_Productive = colDef(show = FALSE))
  
  if(substitute(reactable_df) == "Cat"){
    columns <- c(list(
      "Category" = colDef(
        name = "Category",
        vAlign = "center",
        width = colwidth_name - 16,
        cell = function(value, index){
          donut <- donut(df = Cat, row = index, width = 48)
          div(style = list(display = "flex", alignItems = "center"),
              donut,
              div(style = list(marginLeft = "8px"), value))
        }
      ),
      "Percentage" = colDef(show = FALSE)),
    columns) %>%
      assign_in(., "Overview", value = colDef(show = FALSE)) %>%
      assign_in(., "monthly_Distracting", value = colDef(show = FALSE)) %>%
      assign_in(., "daily_Distracting", value = colDef(show = FALSE))
  }

  if(substitute(reactable_df) == "Act") {
    columns <- c(list(
      "Activity" = colDef(
        name = "Activity",
        width = colwidth_name,
        vAlign = "center",
        cell = function(value, index) {
            insert.icon(
              df = Act, app = value, row = index, 
              recolor = recolor, fill = icon_col[3])
          }
        ),
      "Category" = colDef(show = FALSE)),
    columns,
    list("icon" = colDef(show = FALSE))) %>%
      assign_in(., "Overview", value = colDef(show = FALSE)) %>%
      assign_in(., "daily_Distracting", value = colDef(show = FALSE)) 
  }
  return(columns)
}

# function to add icon to value: ----
insert.icon <- function(df, app, row, img_path = "img/Programs", 
                        recolor = FALSE, fill = "black"){
  icon <- df[["icon"]][row]
  icon_set <- ifelse(grepl(".png$", icon) == TRUE, "customicons",
                     str_extract(icon, "^.*(?=\\s)"))
  icon_name <- ifelse(grepl(".png$", icon) == TRUE, icon,
                      str_extract(icon, "(?<=\\s).*$"))

  if(is.na(icon_set)) {
    src_icon <- NULL
  }
  else if (icon_set == "customicons") {
    img_file <- file.path(img_path, icon_name)
    img <- readPNG(img_file)
    if(recolor){
      # change all black to icon_color
      col <- col2rgb(fill)
      img[, , 1][img[, , 1] == 0] <- col[1] / 255
      img[, , 2][img[, , 2] == 0] <- col[2] / 255
      img[, , 3][img[, , 3] == 0] <- col[3] / 255
      # and save colored png:
      icon_name <- paste0(str_extract(icon, "^.*(?=\\.png)"), "_colored.png")
      writePNG(img, file.path(img_path, "colored", icon_name))
    }
    if(fill != "black"){
      img_path <- file.path(img_path, "colored")
      icon_name <- paste0(str_extract(icon, "^.*(?=\\.png)"), "_colored.png")
    }
    img <- knitr::image_uri(file.path(img_path, icon_name))
    src_icon <- img(src = img, width = "24px", alt = app)
  } else if (icon_set == "simple-icons") {
    src_icon <- icon_style(simple_icons(icon_name),
                           scale = 1.5, fill = fill)
  } else if (icon_set == "academic-icons") {
    src_icon <-
      icon_style(academicons(icon_name), scale = 1.5, fill = fill)
  } else if (icon_set == "fontawesome") {
    src_icon <-
      icon_style(fontawesome(icon_name), scale = 1.5, fill = fill)
  }
 
  div(style = list(display = "flex", alignItems = "center"),
      div(style = list(width = "40px"), src_icon),
      div(style = list(marginLeft = "8px", width = "300px"), app))
}

# Variables for reactable: ----
stacked_bar <- function(df, list_of_values, row, height = "28px"){
  full_width <- max(df[["Daily_min"]])
  width_bar <- df[["Daily_min"]][row]
  
  label <- div(style = list(flexGrow = 1, marginLeft = "8px", 
                            fontSize = "12px"), df[["Daily_time"]][row])
  
  divs <- imap(list_of_values, function(value, i){
    bar <- div(style = list(background = Productivity_col[[df[["Productivity"]][[row]][i]]],
                            width = paste0(round(value * width_bar / full_width), "%"), 
                            height = height))
  })
  div(style = list(display = "flex", alignItems = "center"), divs, label)
}
bar_col <- unname(Productivity_col[c("Very Distracting", "Very Productive")])

# function to make a donut chart with %: ----
donut <- function(df, row, width = 60){
  percentage <- df[["Percentage"]][row]
  productivity <- Overview[["Productivity"]][[1]]
  colors <- unname(Productivity_col[productivity])
  r <- width / 2.5
  length <- 2 * pi * r
  
  values <- unlist(df[["Pulse_rel"]][row]) * percentage
  slice_end <- cumsum(values) 
  slice_begin <- c(0, slice_end[-5])
  
  label <-  div(style = list(position = "absolute", fontSize = "12px", 
                             lineHeight = paste0(width, "px")),
                paste0(round(percentage * 100), "%"))
  
  circle <- tags$circle(cx = 0.5 * width, cy = 0.5 * width, r = r, fill = "transparent", 
                        stroke = rgb(0,0,0,0.1), strokeWidth = width / 5)
  
  slices <- imap(values, function(value, i){
    array <- c(value, 100 - value) / 100
    offset <- (100 - slice_begin[i] + 25) %% 100
    
    slice <- tags$circle(cx = 0.5 * width, cy = 0.5 * width, r = r, fill = "transparent", 
                         stroke = colors[i], strokeWidth = width / 5, 
                         strokeDasharray = array * length,
                         strokeDashoffset = offset / 100 * length)
  })
  
  donut <- tags$svg(circle, slices)
  div(style = list(width = width, height = width, justifyContent = "center",
                   display = "inline-flex", position = "relative"), donut, label)
}

# make reactable ----
make.reactable.RT <- function(time_limit, recolor) {
  reactable(
    Overview,
    pagination = FALSE,
    sortable = FALSE,
    theme = reactableTheme(
      color = icon_col[3],
      borderColor = icon_col[2],
      cellPadding = "8px 8px",
      style = list(fontFamily = "Comfortaa")
    ),
    columns = set.columns(Overview),
    details = function(index) {
      Cat <- Category[Category$Overview == Overview$Overview[index], ]
      if (length(unique(Cat$Category)) > 1) {
        div(
          style = list(padding = "16px"),
          reactable(
            Cat,
            pagination = FALSE,
            sortable = FALSE,
            outlined = TRUE,
            theme = reactableTheme(
              backgroundColor = icon_col[1],
              color = icon_col[3],
              borderColor = icon_col[2]
            ),
            columns = set.columns(Cat) %>%
              assign_in(
                .,
                list("Category", "cell"),
                value = function(value, index){
                  donut <- donut2(df = Cat, row = index, width = 48)
                  div(style = list(display = "flex", alignItems = "center"),
                      donut,
                      div(style = list(marginLeft = "8px"), value))
                }
              ) %>%
              assign_in(
                .,
                list("Pulse_rel", "cell"),
                value =  function(value, index) {
                  stacked_bar(
                    df = Cat,
                    list_of_values = value,
                    row = index,
                    height = "40px"
                  )
                }
              ),
            details = function(index) {
              Act <- Activity[Activity$Category == Cat$Category[index], ] %>%
                filter(Daily_min > time_limit)
              if (length(unique(Act$Activity)) >= 1) {
                div(
                  style = list(padding = "16px"),
                  reactable(
                    Act,
                    sortable = FALSE,
                    outlined = TRUE,
                    theme = reactableTheme(
                      color = icon_col[3],
                      borderColor = icon_col[2]
                    ),
                    columns = set.columns(Act) %>%
                      assign_in(
                        .,
                        list("Pulse_rel", "cell"),
                        value =  function(value, index) {
                          stacked_bar(
                            df = Act,
                            list_of_values = value,
                            row = index,
                            height = "40px"
                          )
                        }
                      ) %>%
                      assign_in(
                        .,
                        list("Activity", "cell"),
                        value = function(value, index) {
                            insert.icon(
                              df = Act,
                              app = value,
                              row = index,
                              recolor = recolor,
                              fill = icon_col[3]
                            )
                          }
                        ) %>%
                      assign_in(
                        .,
                        list("monthly_Distracting", "cell"),
                        value = function(value, index) {
                          sparkline(
                            values = cbind(
                              -Act$monthly_Distracting[[index]],
                              Act$monthly_Productive[[index]]
                            ),
                            type = "bar",
                            height = 40,
                            width = 4 * length(Overview$MonthLabel[[1]]),
                            stackedBarColor = bar_col,
                            tooltipFormatter = htmlwidgets::JS(
                              sprintf(
                                "function(sparkline, options, field){
                                                           debugger;
                                                           return('<b>'+ %s[field[0].offset] + '</b><br/>' +
                                                           '<span style=color:' + field[0].color + '> &#9679 </span>' +
                                                           Math.round(field[0].value) + ' min/day' + '<br/>' +
                                                           '<span style=color:' + field[1].color + '> &#9679 </span>' +
                                                           -Math.round(field[1].value) + ' min/day');
                                                           }",
                                jsonlite::toJSON(Act$MonthLabel[[1]])
                              )
                            )
                          )
                        }
                      ) %>%
                      assign_in(
                        .,
                        "daily_Distracting",
                        value = colDef(
                          name = "Daily time",
                          align = "center",
                          cell = function(value, index) {
                            sparkline(
                              values = Act$daily_Productive[[index]] - Act$daily_Distracting[[index]],
                              type = "bar",
                              height = 40,
                              width = 150,
                              barColor = 	bar_col[2],
                              negBarColor	= bar_col[1],
                              zeroColor	= icon_col[3],
                              tooltipFormatter = JS(
                                sprintf(
                                  "function(sparkline, options, field){
                                                           debugger;
                                                           return('<b>'+ %s[field[0].offset] + '</b><br/>' +
                                                           '<span style=color:' + field[0].color + '> &#9679 </span>' +
                                                           Math.round(field[0].value) + ' minutes');
                                                           }",
                                  jsonlite::toJSON(Overview$DateLabel[[1]])
                                )
                              )
                            )
                          }
                        )
                      )
                  )
                )
              }
            }
          )
        )
      }
    }
  )
}
make.reactable.RT(time_limit = 1/60, recolor = FALSE)


# sparkline als lijn ----
sparkline(
  values = (Act$daily_Distracting[[index]] + Act$daily_Productive[[index]]) / 60,
  type = "line",
  width = 4 * 30,
  lineColor = icon_col[3],
  height = 40
)

# function to make bar left-right: werkt niet en wordt niet mooi aangezien unproductive time veel minder ----
diverging_bar <- function(df, list_of_values, row, height = "28px"){
  
  full_w_right <- max(df[["Daily_Prod"]])
  full_w_left <- max(df[["Daily_Dist"]])
  total_w <- full_w_right + full_w_left
  
  w_right <- df[["Daily_Prod"]][row]
  w_left <- df[["Daily_Dist"]][row]
  
  label_pos <- div(style = list(flexGrow = 1, marginLeft = "8px"), round(df[["Daily_Prod"]][row]))
  label_neg <- div(style = list(flexGrow = 1, marginRight = "8px"), round(df[["Daily_Dist"]][row]))
  
  divs <- imap(list_of_values * c(-1,-1, 1, 1, 1) / length(all_dates), function(value, i){
    if (value < 0) {
      bar <- div(style = list(background = Productivity_col[[df[["Productivity"]][[row]][i]]],
                              width = paste0(round(-value / total_w * 100), "%"), 
                              height = height))
    } else {
      bar <- div(style = list(background = Productivity_col[[df[["Productivity"]][[row]][i]]],
                              width = paste0(round(value / total_w * 100), "%"), 
                              height = height))
    }
  })
  
  # width is hetzelfde als Pulse_rel * total_w_value/total_w_all_values
  
  neg_chart <- tagAppendChild(div(style = list(flex = "1 1 0")),
                              div(style = list(display = "flex", alignItems = "center", 
                                               justifyContent = "flex-end"), 
                                  label_neg, divs[[1]], divs[[2]]))
  
  pos_chart <- tagAppendChild(div(style = list(flex = "1 1 0")), 
                              div(style = list(display = "flex", alignItems = "center"), 
                                  divs[[3]], divs[[4]], divs[[5]], label_pos))
  
  div(style = list(display = "flex"), neg_chart, pos_chart)
}

# can I change a single thing in the cell with modify_in?
columns_Overview <- Overview %>% set.columns()

# dit werkt, maar of het er makkelijker van wordt....?
body <- body(columns_Overview[["Pulse_rel"]][["cell"]])[2]
body_str <- deparse(body)
new_body <- parse(text = gsub("Overview", "Category", body_str))[[1]]

body(columns_Overview[["Pulse_rel"]][["cell"]])[2] <- parse(text = gsub("Overview", "Category", body_str))[[1]]

test <- columns_Overview %>%
  modify_in(., list("Pulse_rel", "cell"),
            ~ gsub("Overview", "Cat", .x))