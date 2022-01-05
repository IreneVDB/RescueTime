# make Productivity plots:
library(pins)
library(tidyverse)
library(lubridate)
library(ggtext)
library(png)
library(grid)
library(reactable) 
library(htmltools)
library(sparkline)
#library(htmlwidgets)
#library(reactablefmtr)
#library(tippy)

source("src/Load_data_API.R")

RescueTime <- pin_get("RescueTime")

# Productivity colors from Rescue Time app:----
Productivity_col <- rgb(red = c(13, 65, 177, 218, 212, 160),
                        green = c(88, 130, 193, 105, 28, 235),
                        blue = c(193, 221, 191, 93, 21, 142),
                        names = c("Very Productive", "Productive", "Neutral", 
                                  "Distracting", "Very Distracting", "Mobile Phone"),
                        maxColorValue = 255)

all_dates <- seq(RescueTime[["day"]]$Date[1], rev(RescueTime[["day"]]$Date)[1], by = "day")

# function to add logo to plot: ----
add.logo <- function(p, img, plot_w_cm = NA, plot_h_cm = NA, logo_w_mm = 10){
  
  if(is.na(plot_w_cm)){
    plot_w_cm <- dev.size("cm")[1]
  }
  if(is.na(plot_h_cm)){
    plot_h_cm <- dev.size("cm")[2]
  }
  
  gt <- ggplotGrob(p)
  gb <- ggplot_build(p)
  
  panel_pos <- gt$layout[which(gt$layout$name == "panel"), ]
  space_y <- sum(convertUnit(gt$heights[-panel_pos$t], "mm", valueOnly=TRUE))
  space_x <- sum(convertUnit(gt$widths[-panel_pos$l], "mm", valueOnly=TRUE))
  from_top <- sum(convertUnit(gt$heights[seq(panel_pos$t - 1)], "mm", valueOnly=TRUE))
  from_right <- sum(convertUnit(gt$widths[-seq(panel_pos$l)], "mm", valueOnly=TRUE))
  
  panel_w <- 10 * plot_w_cm - space_x
  
  if(!is.null(gb$plot$theme$aspect.ratio)){
    panel_h <- gb$plot$theme$aspect.ratio * panel_w
  } else{
    panel_h <- 10 * plot_h_cm - space_y
  }
  
  top_mar <- convertUnit(gb$plot$theme$plot.margin[1], "mm", valueOnly=TRUE)
  right_mar <- convertUnit(gb$plot$theme$plot.margin[2], "mm", valueOnly=TRUE)
  
  logo <- rasterGrob(readPNG(img), 
                     x = unit(panel_w + from_right - top_mar, "mm"), 
                     y = unit(panel_h + from_top - top_mar, "mm"), 
                     width = unit(logo_w_mm, "mm"),
                     hjust = 1, vjust=1,
                     interpolate=TRUE)
  
  p + annotation_custom(logo) 
}


# Productivity summaries: ----
get.productivity.summary <- function(){
  Productivity <- list()
  
  Productivity[["day"]] <-RescueTime[["day"]] %>%
    filter(Activity != "iOS Device") %>%
    left_join(data.frame(Date = all_dates), ., by = "Date") %>%
    group_by(Date, Productivity) %>%
    summarise(Time_sec = sum(Time_sec)) %>%
    right_join(expand(., Date, Productivity)) %>%
    replace_na(., list(Time_sec = 0)) %>%
    mutate(Time_min = Time_sec / 60,
           label = "Time/day",
           unit = "mins") %>%
    arrange(Date, Productivity)
  
  Productivity[["all_time"]] <- Productivity[["day"]] %>%
    group_by(Productivity) %>%
    summarise(Time_min = mean(Time_min, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Percentage = round(Time_min / sum(Time_min) * 100),
           Time_h = Time_min / 60,
           Mean_chr = case_when(Time_min < 59.5 ~ paste(round(Time_min), "m"),
                                Time_h %% 1 < 59.5 / 60 ~ paste0(
                                  Time_h %/% 1, "h ", str_pad(round(Time_h %% 1 * 60), 2, "left", "0"), "m"),
                                TRUE ~ paste0(ceiling(Time_h), "h 00m")))
  
  Productivity[["hour"]] <- RescueTime[["pc"]] %>%
    mutate(DateTime = floor_date(DateTime, unit="hour")) %>%
    group_by(DateTime, Productivity) %>%
    summarise(Time_sec = sum(Time_sec)) %>%
    mutate(Timebin = data.table::as.ITime(DateTime[1])) %>%
    group_by(Timebin, Productivity) %>%
    summarise(Time_min = sum(Time_sec) / 60) %>%
    ungroup() %>%
    mutate(Timebin = as.numeric(Timebin) / 3600) %>%
    right_join(expand(., Timebin, Productivity)) %>%
    replace_na(., list(Time_min = 0)) %>%
    arrange(Timebin, Productivity) %>%
    mutate(min_day = Time_min / length(all_dates),
           time_posneg = case_when(grepl("Distracting", Productivity) == TRUE ~ -1 * min_day, 
                                      TRUE ~ min_day))
  
  Productivity[["week"]] <- Productivity[["day"]] %>%
    mutate(Weekday = wday(Date, week_start = getOption("lubridate.week.start", 1),
                          label = TRUE)) %>%
    group_by(Weekday, Productivity) %>%
    summarise(Time_min = mean(Time_min, na.rm = TRUE)) %>%
    mutate(time_posneg = case_when(grepl("Distracting", Productivity) == TRUE ~ -1 * Time_min / 60,
                              TRUE ~ Time_min / 60))
  
  return(Productivity)
}
Productivity <- get.productivity.summary()

# Plot 1) All time Productivity pulse - bar chart ----
make.alltime.pulse <- function(df){
  ggplot(df, aes(x = Time_min, y = Productivity)) +
    geom_col(aes(fill = Productivity)) +
    geom_text(aes(label = paste0(Mean_chr, "\n(", Percentage, "%)"),
                  color = Productivity), 
              hjust = -0.1, family = "Comfortaa", size = 2.8) + 
    scale_fill_manual(values=Productivity_col[-6]) +
    scale_color_manual(values = rev(hsv(h =rgb2hsv(col2rgb(Productivity_col[-6]))[1,],
                                        s= rgb2hsv(col2rgb(Productivity_col[-6]))[2,], 
                                        v = 0.5))) +
    scale_x_continuous(breaks=seq(0, max(df$Time_min), by = 30),
                       expand = c(0,0)) +
    coord_cartesian(clip = "off") +
    labs(x = "Time (minutes/day)", y= NULL,
         title = "Mean Productivity Pulse",
         subtitle = paste0("Measured with RescueTime (", length(all_dates), " days)")) +
    theme_minimal(base_family = "Comfortaa") +
    theme(plot.margin = unit(c(2, 15, 2, 2), "mm"),
          panel.grid.major.x = element_line(color = "grey90", size = 0.2),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          plot.title = element_markdown(face = "bold", size = 15, hjust = 0, color="grey10"),
          plot.subtitle = element_text(size = 11, hjust = 0, color="grey20"),
          axis.text.y = element_text(size=10, color = "grey30"),
          axis.title.x = element_text(face = "bold", margin = margin(t = 6), color = "grey40", size = 10),
          axis.text.x = element_text(size = 8, color = "grey50", margin = margin(t = 2)))
}

# Plot 2) Daily Productivity distribution - beeswarm: ----
make.beeswarm <- function(df, name_column = "Productivity", 
                          value_column = "Time_min",
                          label, unit, title, col_vec, 
                          height_per_metric=2){
  
  df <- df %>%
    rename(name = !!as.name(name_column),
           value = !!as.name(value_column))
  
  summ <- df %>%
    group_by(name) %>%
    summarise(median = median(value, na.rm=TRUE),
              Q1 = quantile(value, 0.25, na.rm = TRUE),
              Q3 = quantile(value, 0.75, na.rm = TRUE)) %>%
    mutate(median = ifelse(median >= 100, round(median), signif(median, 2)))
  
  hs <- rgb2hsv(col2rgb(col_vec))[1,][levels(df$name)]
  vs <- rgb2hsv(col2rgb(col_vec))[3,][levels(df$name)]
  vs <- ifelse(vs < 0.85, 0.85, vs)
  cols <- col_vec[levels(df$name)]
  
  poly_df <- rbind(summ, summ) %>%
    pivot_longer(cols = c("Q1", "Q3"), names_to = "Quantile", values_to = "coord_y") %>%
    arrange(name, Quantile) %>%
    mutate(coord_x = rep(1:nrow(summ), each = 4) + c(-0.35, 0.35, 0.35, -0.35))
  
  ggplot(df, aes(x = name, y = value)) +
    geom_violin(scale = "width", fill = "transparent", color = "transparent", size = 0.2, width = 0.6) +
    map(summ$name, function(i){ 
      geom_polygon(data = subset(poly_df, name == i), 
                   aes(x = coord_x, y = coord_y), fill = "grey80")
    }) +
    geom_segment(data = summ, aes(x = as.numeric(name) + 0.45, xend = as.numeric(name) - 0.45,
                                  y = median, yend = median), col = "grey40", lwd = 1) +
    geom_violin(scale = "width", aes(fill = name), color="grey50", size = 0.2, width = 0.6) +
    ggbeeswarm::geom_quasirandom(aes(color=name), shape= 16, size = 1.2, width = 0.25, alpha = 0.5) +
    coord_flip(clip="off") +
    labs(x = NULL, y = paste0(label, " (", unit, ")")) +
    theme_minimal(base_family = "Comfortaa") +
    theme(plot.margin = unit(c(2, 2, 2, 2), "mm"),
          aspect.ratio = nlevels(df$name) * 1.5 / 10,
          panel.grid.major.x = element_line(color = "grey90", size = 0.2),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          plot.title = element_markdown(face = "bold", size = 15, hjust = 0, color="grey10"),
          plot.subtitle = element_text(size = 11, hjust = 0, color="grey20"),
          axis.text.y = element_text(size=10, face="bold", color = "grey40", vjust=-0.2),
          axis.title.x = element_text(face = "bold", margin = margin(t = 6), color = "grey40", size = 10),
          axis.text.x = element_text(size = 8, color = "grey50", margin = margin(t = 2))) +
    geom_text(data = summ, aes(x = name, y = -Inf, 
                               label = paste("median:", median, unit)), 
              vjust = 1.2, hjust = 1.05, color = "grey40", size = 2.8, family = "Comfortaa") +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = hsv(hs, 0.2, vs)) +
    ggtitle(title,
            subtitle = paste0("Measured with RescueTime (", length(all_dates), " days)"))
}

# Plot 3 + 4) Hourly  + Weekly Productivity - stacked bar chart ----
make.stacked.pulse <- function(timebin, xcol){
  label <- sub(",([^,]*)$", " and\\1", paste0("<span style='font-size:8pt;'>",
                                              paste(imap_chr(Productivity_col[-c(4:6)], function(color, name){
                                                paste0("<span style='color:", color, ";'>", name, "</span>")
                                              }), collapse = ", "), " time are shown as positive values. <br>", 
                                              paste(imap_chr(Productivity_col[c(5,4)], function(color, name){
                                                paste0("<span style='color:", color, ";'>", name, "</span>")
                                              }), collapse = " and "), " time are shown as negative values. </span>"))
  
  df <- Productivity[[timebin]]
  
  xlab <- ifelse(timebin == "hour", "Time of day (h)", "Day of week")
  ylab <- ifelse(timebin == "hour", "Time (mins)", "Time (hrs)")
  title <- ifelse(timebin == "hour", "Productivity per hour of day",
                  "Productivity per day of week")
  
  plot <- ggplot(df, aes(x = {{xcol}}, y = time_posneg)) + 
    geom_col(aes(fill = Productivity)) +
    scale_fill_manual(values = Productivity_col[-6]) +
    coord_cartesian(clip = "off") +
    labs(x = xlab, y= ylab, title = title,
         subtitle = paste0("Measured with RescueTime (", length(all_dates), " days) <br>
                         <br>", label)) +
    theme_minimal(base_family = "Comfortaa") +
    theme(plot.margin = unit(c(2, 2, 2, 2), "mm"),
          panel.grid.major.y = element_line(color = "grey90", size = 0.2),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          plot.title = element_markdown(face = "bold", size = 15, hjust = 0, color="grey10"),
          plot.subtitle = element_markdown(size = 11, hjust = 0, color="grey20"),
          axis.text.y = element_text(size=10, color = "grey30"),
          axis.title.x = element_text(face = "bold", margin = margin(t = 6), color = "grey40", size = 10),
          axis.ticks.x = element_line(color = "grey60", size = 0.1),
          axis.title.y = element_text(face = "bold", color = "grey40", size = 10),
          axis.text.x = element_text(size = 8, color = "grey50", margin = margin(t = 2))) 
  
  if(timebin == "hour"){
    plot <- plot +
      scale_x_continuous(breaks= 0:23 - 0.5, labels = paste0(0:23, ":00"),
                         expand = c(0.01, 0.01))
  } else{
      plot <- plot +
        scale_y_continuous(breaks = -1:6)
  }
  return(plot)
}


# add ggplots 1 to 4 to list for easier input in Rmd: ----
RT_plots <- pin_get("RT_plots")
RT_plots[["Prod_pulse"]] <- make.alltime.pulse(Productivity[["all_time"]])
RT_plots[["Prod_beeswarm"]] <- make.beeswarm(df = Productivity[["day"]], 
                                             label = "Time/day", unit = "mins",
                                             title = "Daily Productivity Metrics",
                                             col_vec = Productivity_col[-6])
RT_plots[["Prod_Hourly"]] <- make.stacked.pulse("hour", xcol = Timebin)
RT_plots[["Prod_Weekly"]] <- make.stacked.pulse("week", xcol = Weekday)
pin(RT_plots, "RT_plots")


# Plot 5) Main categories with icons and donut chart: ----
make.donut.RT <- function(plot_h = NA){
  
  Category <- RescueTime[["day"]] %>%
    group_by(Overview, Productivity) %>%
    summarise(Time_min = sum(Time_sec) / length(all_dates) / 60) %>%
    mutate(TotTime = sum(Time_min))
  
  nhours <- length(all_dates) * sum(Category$Time_min) / 60
  
  make.donut <- function(df, category){
    
    subset <- df %>%
      filter(Overview == category) %>%
      mutate(fraction = Time_min / sum(Time_min))
    
    par(mar = c(2, 0, 0, 0), xaxs = "i", yaxs = "i", family = "Comfortaa")
    
    pie(subset$fraction, clockwise = TRUE, 
        col = Productivity_col[-6][unique(as.character(subset$Productivity))], 
        border = "white",labels = NA, xlim = c(-0.7, 0.7), ylim = c(-0.7, 0.7))
    symbols(0,0, circles = 0.35, fg = "white", bg = "white", 
            add = TRUE, inches = FALSE)
    text(0, -0.9, labels = category, font = 2, col = "grey20",
         cex = 1, adj=c(0.5, 1), xpd = TRUE)
    text(0, -1, paste0("\n(", round(unique(subset$TotTime)), " min/day)"), 
         col = "grey20", cex = 1, adj=c(0.5, 1), xpd = TRUE)
  }
  
  if(is.na(plot_h)){plot_h <- dev.size("cm")[2]}
    
  h_title <- 1.5
  h_legend <- 1.8
  
  layout(matrix(c(rep(1, 4), rep(2, 4), 3:14), nrow = 5, ncol =  4, byrow = TRUE),
         heights = lcm(c(h_title, h_legend)))
  
  # first plot is a title + subtitle
  par(mar = c(0, 0, 0, 0), family = "Comfortaa")
  plot.new()
  text(0, 1, "Productivity pulse of the 12 main Categories", 
       adj = c(0, 1), col = "grey20", font = 2, cex = 2, xpd = TRUE)
  text(0, 0, paste0("Measured with RescueTime (", length(all_dates), " days)"), 
       adj = c(0, 0.5), col = "grey30", cex = 1.5, xpd = TRUE)
  
  # second plot is the legend:
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend(0.5, 0.5, legend = levels(Category$Productivity),
         fill = Productivity_col[levels(Category$Productivity)], 
         title.adj = 0, title.col = "grey20",
         y.intersp = 1.2, text.col = "grey30",
         border = "transparent", ncol=2,
         cex = 1.2, xjust = 0.5, yjust = 0.5, bty = "n", xpd = TRUE)
  
  # then add the individual donut plots:
  lapply(levels(Category$Overview), function(Overview){
    image <- readPNG(file.path("img", "Categories", paste0(
      gsub(" .*$", "", Overview), ".png")))
    make.donut(df = Category, category = Overview)
    rasterImage(image, xleft = -0.5, xright = 0.5, ybottom = -0.5, ytop = 0.5)
   })
}
make.donut.RT()
# Table 1: Time in each main category:----

summarise.table <- function(df, ...){
  
  group_vars <- enquos(...)
  group_syms <- ensyms(...)
  
  data <- df %>%
    mutate(Time_min = Time_sec / 60,
           Category = fct_reorder(Category, Time_min, sum, .desc = TRUE),
           Activity = fct_reorder(Activity, Time_min, .fun = sum, .desc = TRUE))
  
  ProdPulse <- data %>%
    group_by(!!!group_vars, Productivity) %>%
    summarise(Time_min = sum(Time_min)) %>%
    complete(., Productivity, fill = list(Time_min = 0)) %>%
    summarise(Productivity = list(as.character(Productivity)),
              Pulse_rel = list(Time_min / sum(Time_min) * 100),
              Pulse_abs = list(Time_min),
              Daily_min = sum(Time_min)/ length(all_dates),
              Daily_time = case_when(Daily_min < 1 ~ paste(round(Daily_min * 60), "sec"),
                                     TRUE ~ paste(round(Daily_min), "min")))

  MonthlyTrend <- data %>%
    mutate(ym = factor(format(DateTime, format = "%Y-%m")),
           Productivity = case_when(grepl("Distracting", Productivity) ~ "Distracting",
                                    TRUE ~ "Productive")) %>%
    group_by(!!!group_vars, Productivity, ym) %>%
    summarise(Time_min = round(sum(Time_min) / days_in_month(ym(first(ym)))[[1]]), .groups = "drop") %>%
    # the nesting gave me headaches: !!enquo or {{}} does not work here...
    complete(., expand(., nesting(!!!group_syms), Productivity, ym), 
             fill = list(Time_min = 0)) %>%
   group_by(!!!group_vars, Productivity) %>%
   summarise(MonthlyTrend = list(Time_min),
             MonthLabel = list(format(ym(ym), "%B %Y"))) %>%
    pivot_wider(., names_from = Productivity, values_from = MonthlyTrend,
                names_prefix = "monthly_")

  DailyTrend <- data %>%
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
    left_join(., MonthlyTrend)
}

Overview <- RescueTime[["pc"]] %>%
  summarise.table(., Overview)

SubCat <- RescueTime[["pc"]] %>%
  summarise.table(., Overview, Category)

Programs <- RescueTime[["pc"]] %>%
  summarise.table(., Overview, Category, Activity)
  

# now make reactable ----

#1) Table of all Categories with icon, stacked bar, time per day, daily trend, monthly trend


# bar_chart <- function(label, width = "100%", height = "20px", fill = "#00bfc4", background = NULL) {
#   bar <- div(style = list(background = fill, width = width, height = height))
#   chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
#   div(style = list(display = "flex", alignItems = "center"), label, chart)
# }

library(reactablefmtr)

make.reactable.RT <- function(df, groupBy = NULL){
  
  months <- df$MonthLabel[[1]]
  distr_m <- df$monthly_Distracting
  prod_m <- df$monthly_Productive
  days_14 <- rev(df$DateLabel[[1]])[14:1]
  distr_14d <- map(df$daily_Distracting, ~round(magrittr::extract(rev(.), 14:1)))
  prod_14d <- map(df$daily_Productive, ~round(magrittr::extract(rev(.), 14:1)))
  
  stacked_bar <- function(df, list_of_values, row, height = "28px", full_width){
    full_width <- max(df[["Daily_min"]])
    width_bar <- df[["Daily_min"]][row]
    
    label <- div(style = list(flexGrow = 1, marginLeft = "8px"), df[["Daily_time"]][row])
    
    divs <- imap(list_of_values, function(value, i){
      bar <- div(style = list(background = Productivity_col[[df[["Productivity"]][[row]][i]]],
                              width = paste0(round(value * width_bar / full_width), "%"), 
                              height = height))
    })
    div(style = list(display = "flex", alignItems = "center"), divs, label)
  }
  
  reactable(df, 
            pagination = FALSE,
            sortable = FALSE,
            groupBy = groupBy,
            columns = list(
              Overview = colDef(name = "Main Category",
                                minWidth = 250, 
                                cell = function(value){
                                  img_src <- knitr::image_uri(paste0("img/Categories/", 
                                                                     gsub(" .*$", "", value), ".png"))
                                  image <- img(src = img_src, width = "48px", alt = value)
                                  div(style = list(display = "flex", alignItems = "center"),
                                      image,
                                      div(style = list(marginLeft = "8px"), value))
                                }),
              Productivity = colDef(show = FALSE),
              Pulse_abs = colDef(show = FALSE),
              MonthLabel = colDef(show = FALSE),
              DateLabel = colDef(show = FALSE),
              Pulse_rel = colDef(name = "Average Time per Day",
                                 cell = function(value, index) {
                                   stacked_bar(df = df,
                                               list_of_values = value,
                                               row = index, height = "36px")
                                 },
                                 minWidth = 200),
              Daily_min = colDef(show = FALSE),
              Daily_time = colDef(show = FALSE),
              daily_Distracting = colDef(name = "Last 14 days", 
                                         cell = function(value, index) {
                                           sparkline(values = cbind(-distr_14d[[index]], prod_14d[[index]]),
                                                     type = "bar", 
                                                     # chartRangeMin = -max(c(distr_m[[index]], distr_14d[[index]]), na.rm = TRUE), 
                                                     # chartRangeMax = max(c(prod_m[[index]], prod_14d[[index]]), na.rm = TRUE),
                                                     height = 48,
                                                     stackedBarColor = unname(Productivity_col[c("Very Distracting", "Very Productive")]),
                                                     tooltipFormatter = htmlwidgets::JS(
                                                       sprintf(
                                                         "function(sparkline, options, field){
                                                             debugger;
                                                             return('<b>'+ %s[field[0].offset] + '</b><br/>' + 
                                                             '<span style=color:' + field[0].color + '> &#9679 </span>' +  
                                                             field[0].value + ' min/day' + '<br/>' +
                                                             '<span style=color:' + field[1].color + '> &#9679 </span>' + 
                                                             -field[1].value + ' min/day');
                                                             }",
                                                         jsonlite::toJSON(rev(Overview$DateLabel[[1]])[14:1])
                                                       )
                                                     ))
                                         }),
              daily_Productive = colDef(show = FALSE),
              monthly_Distracting = colDef(name = "Monthly Trend", 
                                           cell = function(value, index) {
                                             sparkline(values = cbind(-distr_m[[index]], prod_m[[index]]),
                                                       type = "bar", 
                                                       # chartRangeMin = -max(c(distr_m[[index]], distr_14d[[index]]), na.rm = TRUE), 
                                                       # chartRangeMax = max(c(prod_m[[index]], prod_14d[[index]]), na.rm = TRUE), 
                                                       height = 48,
                                                       stackedBarColor = unname(Productivity_col[c("Very Distracting", "Very Productive")]),
                                                       tooltipFormatter = htmlwidgets::JS(
                                                           sprintf(
                                                             "function(sparkline, options, field){
                                                             debugger;
                                                             return('<b>'+ %s[field[0].offset] + '</b><br/>' + 
                                                             '<span style=color:' + field[0].color + '> &#9679 </span>' +  
                                                             field[0].value + ' min/day' + '<br/>' +
                                                             '<span style=color:' + field[1].color + '> &#9679 </span>' + 
                                                             -field[1].value + ' min/day');
                                                             }",
                                                             jsonlite::toJSON(months)
                                                           )
                                                         ))
                                           }),
              monthly_Productive = colDef(show = FALSE)),
            theme = reactableTheme(
              color = "black",
              borderColor = "grey80",
              cellPadding = "8px 8px",
              style = list(fontFamily = "Comfortaa")))
}

make.reactable.RT(df = Overview) %>%
  add_title("Overview of Productive and Distracting time on computer",
            font_family = "Comfortaa", font_color = rgb(t(col2rgb("grey20")/255))) %>%
  add_subtitle(paste0("Measured with Rescue Time (",
                      length(unique(RescueTime[["pc"]]$Date)), 
                      " days; last day = ", 
                      rev(RescueTime[["pc"]]$Date)[1], ")"),
               font_family = "Comfortaa", 
               font_color = rgb(t(col2rgb("grey30")/255)),
               font_weight = "normal",
               font_size = 20,
               margin = margin(5,0,10,0))

# use html to set up table display and add title + logo: this only works in html output (RMd) ----
tbl <- make.reactable.RT(df = Overview)

div(
  div(
  h2("Title"),
  "this is the subtitle"
  ),
  tbl,
  "caption"
)


# test with nested rows: ----

reactable(Overview, 
          pagination = FALSE,
          sortable = FALSE,
          groupBy = NULL,
          columns = list(
            Overview = colDef(name = "Main Category",
                              width = 300, 
                              cell = function(value){
                                img_src <- knitr::image_uri(paste0("img/Categories/", 
                                                                   gsub(" .*$", "", value), ".png"))
                                image <- img(src = img_src, width = "48px", alt = value)
                                div(style = list(display = "flex", alignItems = "center"),
                                    image,
                                    div(style = list(marginLeft = "8px"), value))
                              }),
            Productivity = colDef(show = FALSE),
            Pulse_abs = colDef(show = FALSE),
            MonthLabel = colDef(show = FALSE),
            DateLabel = colDef(show = FALSE),
            Pulse_rel = colDef(name = "Average Time per Day",
                               cell = function(value, index) {
                                 stacked_bar(df = df,
                                             list_of_values = value,
                                             row = index, height = "36px")
                               },
                               width = 300),
            Daily_min = colDef(show = FALSE),
            Daily_time = colDef(show = FALSE),
            daily_Distracting = colDef(name = "Last 14 days", 
                                       cell = function(value, index) {
                                         sparkline(values = cbind(-distr_14d[[index]], prod_14d[[index]]),
                                                   type = "bar", 
                                                   # chartRangeMin = -max(c(distr_m[[index]], distr_14d[[index]]), na.rm = TRUE), 
                                                   # chartRangeMax = max(c(prod_m[[index]], prod_14d[[index]]), na.rm = TRUE),
                                                   height = 48,
                                                   stackedBarColor = unname(Productivity_col[c("Very Distracting", "Very Productive")]),
                                                   tooltipFormatter = htmlwidgets::JS(
                                                     sprintf(
                                                       "function(sparkline, options, field){
                                                             debugger;
                                                             return('<b>'+ %s[field[0].offset] + '</b><br/>' + 
                                                             '<span style=color:' + field[0].color + '> &#9679 </span>' +  
                                                             field[0].value + ' min/day' + '<br/>' +
                                                             '<span style=color:' + field[1].color + '> &#9679 </span>' + 
                                                             -field[1].value + ' min/day');
                                                             }",
                                                       jsonlite::toJSON(rev(Overview$DateLabel[[1]])[14:1])
                                                     )
                                                   ))
                                       }),
            daily_Productive = colDef(show = FALSE),
            monthly_Distracting = colDef(name = "Monthly Trend", 
                                         cell = function(value, index) {
                                           sparkline(values = cbind(-distr_m[[index]], prod_m[[index]]),
                                                     type = "bar", 
                                                     height = 48,
                                                     stackedBarColor = unname(Productivity_col[c("Very Distracting", "Very Productive")]),
                                                     tooltipFormatter = htmlwidgets::JS(
                                                       sprintf(
                                                         "function(sparkline, options, field){
                                                             debugger;
                                                             return('<b>'+ %s[field[0].offset] + '</b><br/>' + 
                                                             '<span style=color:' + field[0].color + '> &#9679 </span>' +  
                                                             field[0].value + ' min/day' + '<br/>' +
                                                             '<span style=color:' + field[1].color + '> &#9679 </span>' + 
                                                             -field[1].value + ' min/day');
                                                             }",
                                                         jsonlite::toJSON(months)
                                                       )
                                                     ))
                                         }),
            monthly_Productive = colDef(show = FALSE)),
          theme = reactableTheme(
            color = "black",
            borderColor = "grey80",
            cellPadding = "8px 8px",
            style = list(fontFamily = "Comfortaa")),
          details = function(index){
            Category <- SubCat[SubCat$Overview == Overview$Overview[index], ]
            if(length(unique(Category$Category)) > 1) {
              htmltools::div(style = "padding: 16px",
                           reactable(Category, 
                                     outlined = TRUE,
                                     columns = list(
                                       Category = colDef(name = "Category",
                                                         width = 300), 
                                       Overview = colDef(show = FALSE),
                                       Productivity = colDef(show = FALSE),
                                       Pulse_abs = colDef(show = FALSE),
                                       MonthLabel = colDef(show = FALSE),
                                       DateLabel = colDef(show = FALSE),
                                       Pulse_rel = colDef(name = "Average Time per Day",
                                                          cell = function(value, index) {
                                                            stacked_bar(df = Category,
                                                                        list_of_values = value,
                                                                        row = index, height = "36px")
                                                          },
                                                          width = 300),
                                       Daily_min = colDef(show = FALSE),
                                       Daily_time = colDef(show = FALSE),
                                       daily_Distracting = colDef(show = FALSE),
                                       daily_Productive = colDef(show = FALSE),
                                       monthly_Distracting = colDef(show = FALSE),
                                       monthly_Productive = colDef(show = FALSE)),
                                     details = function(index){
                                       Activity <- Programs[Programs$Category == Category$Category[index], ] %>%
                                         filter(Daily_min > 1,
                                                Activity != "Website")
                                       if(length(unique(Activity$Activity)) >= 1) {
                                         htmltools::div(style = "padding: 16px",
                                                        reactable(Activity, 
                                                                  outlined = TRUE,
                                                                  columns = list(
                                                                    Activity = colDef(name = "Activity",
                                                                                      width = 325, # nesting adds 25 px (add left margin)
                                                                                      cell = function(value){
                                                                                        img_src <- knitr::image_uri("img/Programs/Jebentwatjemeet.png")
                                                                                        image <- img(src = img_src, width = "48px", alt = value)
                                                                                        div(style = list(display = "flex", alignItems = "center"),
                                                                                            image,
                                                                                            div(style = list(marginLeft = "8px"), value))
                                                                                      }), 
                                                                    Category = colDef(show = FALSE), 
                                                                    Overview = colDef(show = FALSE),
                                                                    Productivity = colDef(show = FALSE),
                                                                    Pulse_abs = colDef(show = FALSE),
                                                                    MonthLabel = colDef(show = FALSE),
                                                                    DateLabel = colDef(show = FALSE),
                                                                    Pulse_rel = colDef(name = "Average Time per Day",
                                                                                       cell = function(value, index) {
                                                                                         stacked_bar(df = Activity,
                                                                                                     list_of_values = value,
                                                                                                     row = index, height = "36px")
                                                                                       },
                                                                                       width = 300),
                                                                    Daily_min = colDef(show = FALSE),
                                                                    Daily_time = colDef(show = FALSE),
                                                                    daily_Distracting = colDef(show = FALSE),
                                                                    daily_Productive = colDef(show = FALSE),
                                                                    monthly_Distracting = colDef(show = FALSE),
                                                                    monthly_Productive = colDef(show = FALSE))
                                                                  ))
                                       }
                                     }
                           ))
            }
          }
)



# add header (with RT icon?)?
# use flex to fit Time per day label within width of column (i.e. bar widths should adjust, label width not)
# more complex: Separate Time per day as Productive and Distracting (pos/neg)
# figure out how to vertically align itens to center: now only works when i set height to 48 but this is very big: want itemslittel smaller and then in the middle, especially the stacked bar
# add column Most used program/app and most used sub category: or mkae it a grouped table and include the subcats in the table?
# make font work in browser
# add column Percentage of all time
# make color scheme: check Rescue Time site use light and dark blue from dashboard
# add last day
# add grouping
# chartRangeMax does not adjust - why?

