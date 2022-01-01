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

RescueTime <- pin_get("RescueTime")

RT_plots <- pin_get("RT_plots")

# Productivity colors from Rescue Time app:----
Productivity_col <- rgb(red = c(13, 65, 177, 218, 212, 160),
                        green = c(88, 130, 193, 105, 28, 235),
                        blue = c(193, 221, 191, 93, 21, 142),
                        names = c("Very Productive", "Productive", "Neutral", 
                                  "Distracting", "Very Distracting", "Mobile Phone"),
                        maxColorValue = 255)

all_dates <- seq(RescueTime[["day"]]$Date[1], rev(RescueTime[["day"]]$Date)[1], by = "day")

Productivity_day <- RescueTime[["day"]] %>%
  filter(Activity != "iOS Device") %>%
  left_join(data.frame(Date = all_dates), ., by = "Date") %>%
  group_by(Date, Productivity) %>%
  summarise(Time = sum(Time)) %>%
  right_join(expand(., Date, Productivity)) %>%
  replace_na(., list(Time = 0)) %>%
  mutate(Time = Time/ 60,
         label = "Time/day",
         unit = "mins") %>%
  arrange(Date, Productivity)

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

# Plot 1) All time Productivity pulse - bar chart ----

Productivity <- Productivity_day %>%
  group_by(Productivity) %>%
  summarise(Time = mean(Time, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Time_hr = Time / 60,
         Percentage = round(Time / sum(Time) * 100),
         Mean_chr = case_when(Time < 59.5 ~ paste(round(Time), "m"),
                              (Time / 60) %% 1 < 59.5 / 60 ~ paste0(
                                floor(Time / 60), "h ", str_pad(round((Time / 60) %% 1 * 60), 2, "left", "0"), "m"),
                              TRUE ~ paste0(ceiling(Time / 60), "h 00m")))

RT_plots[["Prod_pulse"]] <- ggplot(Productivity, aes(x=Time, y=Productivity)) +
  geom_col(aes(fill=Productivity)) +
  geom_text(aes(label = paste0(Mean_chr, "\n(", Percentage, "%)"),
                color = Productivity), 
            hjust = -0.1, family = "Comfortaa", size = 2.8) + 
  scale_fill_manual(values=Productivity_col[-6]) +
  scale_color_manual(values = rev(hsv(h =rgb2hsv(col2rgb(Productivity_col[-6]))[1,],
                                  s= rgb2hsv(col2rgb(Productivity_col[-6]))[2,], 
                                  v = 0.5))) +
  scale_x_continuous(breaks=seq(0, max(Productivity$Time), by = 30),
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

# Plot 2) Daily Productivity distribution - beeswarm: ----

make.beeswarm <- function(df, name_column, value_column, 
                          col_vec, height_per_metric=2){
  
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
    labs(x = NULL, y = paste0(unique(df$label), " (", unique(df$unit), ")")) +
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
                               label = paste("median:", median, unique(df$unit))), 
              vjust = 1.2, hjust = 1.05, color = "grey40", size = 2.8, family = "Comfortaa") +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = hsv(hs, 0.2, vs)) 
}

RT_plots[["Prod_beeswarm"]] <- make.beeswarm(df = Productivity_day,
              name_column = "Productivity", value_column = "Time",
              col_vec = Productivity_col[-6]) +
  ggtitle("Daily Productivity Metrics",
          subtitle = paste0("Measured with RescueTime (", length(seq(Productivity_day$Date[1],
                                                                     rev(Productivity_day$Date)[1],
                                                                     by = "day")), " days)"))

# Plot 3) Hourly Productivity - stacked bar chart ----

label <- sub(",([^,]*)$", " and\\1", paste0("<span style='font-size:8pt;'>",
                                            paste(imap_chr(Productivity_col[-c(4:6)], function(color, name){
                                              paste0("<span style='color:", color, ";'>", name, "</span>")
                                            }), collapse = ", "), " time are shown as positive values. <br>", 
                                            paste(imap_chr(Productivity_col[c(5,4)], function(color, name){
                                              paste0("<span style='color:", color, ";'>", name, "</span>")
                                            }), collapse = " and "), " time are shown as negative values. </span>"))

Productivity_hour <- RescueTime[["pc"]] %>%
  mutate(DateTime = floor_date(DateTime, unit="hour")) %>%
  group_by(DateTime, Productivity) %>%
  summarise(Time = sum(`Time Spent (seconds)`)) %>%
  mutate(Timebin = data.table::as.ITime(DateTime[1])) %>%
  group_by(Timebin, Productivity) %>%
  summarise(Time = sum(Time) / 60) %>%
  ungroup() %>%
  mutate(Timebin = as.numeric(Timebin) / 3600) %>%
  right_join(expand(., Timebin, Productivity)) %>%
  replace_na(., list(Time = 0)) %>%
  arrange(Timebin, Productivity) %>%
  mutate(side = case_when(Productivity < 0 ~ -1, TRUE ~ 1),
         mean_Time = side * Time / length(all_dates),
         Productivity = case_when(Productivity == -2 ~ "Very Distracting",
                                  Productivity == -1 ~ "Distracting",
                                  Productivity == 0 ~ "Neutral",
                                  Productivity == 1 ~ "Productive",
                                  Productivity == 2 ~ "Very Productive"),
         Productivity = factor(Productivity, levels = c("Very Distracting",
                                                        "Distracting", "Neutral",
                                                        "Productive", "Very Productive")))

RT_plots[["Prod_Hourly"]] <- ggplot(Productivity_hour, aes(x=Timebin, y = mean_Time)) + 
  geom_col(aes(fill=Productivity)) +
  scale_fill_manual(values=Productivity_col[-6]) +
  scale_x_continuous(breaks=0:23 - 0.5, labels = paste0(0:23, ":00"),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  labs(x = "Time of day (h)", y= "Time (mins)",
       title = "Productivity per hour of day",
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

# Plot 4) Weekly Productivity - stacked bar chart: ----

Productivity_week <- Productivity_day %>%
  mutate(Weekday = wday(Date, week_start = getOption("lubridate.week.start", 1),
                        label = TRUE)) %>%
  group_by(Weekday, Productivity) %>%
  summarise(Time = mean(Time, na.rm = TRUE)) %>%
  mutate(Time_h = case_when(grepl("Distracting", Productivity) == TRUE ~ -1 * Time / 60,
                            TRUE ~ Time / 60))

RT_plots[["Prod_Weekly"]] <- ggplot(Productivity_week, aes(x = Weekday, y = Time_h)) + 
  geom_col(aes(fill=Productivity)) +
  scale_fill_manual(values=Productivity_col[-6]) +
  scale_y_continuous(breaks = -1:6) +
  coord_cartesian(clip = "off") +
  labs(x = "Day of week", y= "Time (hrs)",
       title = "Productivity per day of week",
       subtitle = paste0("Measured with RescueTime (", length(all_dates), " days) <br>
                         <br>", label)) +
  theme_minimal(base_family = "Comfortaa") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm"),
        panel.grid.major.y = element_line(color = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        legend.position = "none",
        plot.title = element_markdown(face = "bold", size = 15, hjust = 0, color="grey10"),
        plot.subtitle = element_markdown(size = 11, hjust = 0, color="grey20"),
        axis.text.y = element_text(size=10, color = "grey30"),
        axis.title.x = element_text(face = "bold", margin = margin(t = 6), color = "grey40", size = 10),
        axis.title.y = element_text(face = "bold", color = "grey40", size = 10),
        axis.text.x = element_text(size = 8, color = "grey50", margin = margin(t = 2))) 

# Plot 5) Main categories with icons and donut chart: ----

RT.overview <- function(plot_h = NA){
  all_dates <- seq(RescueTime[["day"]]$Date[1], 
                   rev(RescueTime[["day"]]$Date)[1], by = "day")
  
  Category <- RescueTime[["day"]] %>%
    group_by(Overview, Productivity) %>%
    summarise(Time = sum(Time) / length(all_dates) / 60) %>%
    mutate(TotTime = sum(Time))
  
  nhours <- length(all_dates) * sum(Category$Time) / 60
  
  make.donut <- function(df, category){
    
    subset <- df %>%
      filter(Overview == category) %>%
      mutate(fraction = Time / sum(Time))
    
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
  par(mar = c(0, 0, 0, 0))
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

# Table 1: Time in each main category:----

add.overview <- function(df){
 data <- df %>%
   mutate(Overview = case_when(
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
      TRUE ~ "Uncategorized"),
      Overview = fct_reorder(Overview, `Time Spent (seconds)`, sum, .desc = TRUE))
}
summarise.table <- function(df, ...){
  
  group_vars <- enquos(...)
  group_syms <- ensyms(...)
  
  data <- df %>%
    mutate(Time = `Time Spent (seconds)` / 60,
           Category = fct_reorder(Category, Time, sum, .desc = TRUE),
           Productivity = case_when(Productivity == -2 ~ "Very Distracting",
                                    Productivity == -1 ~ "Distracting",
                                    Productivity == 0 ~ "Neutral",
                                    Productivity == 1 ~ "Productive",
                                    Productivity == 2 ~ "Very Productive"),
           Productivity = factor(Productivity, levels = c("Very Distracting",
                                                          "Distracting", "Neutral",
                                                          "Productive", "Very Productive"))) 
  
  ProdPulse <- data %>%
    group_by(!!!group_vars, Productivity) %>%
    summarise(Time = sum(Time)) %>%
    complete(., Productivity, fill = list(Time = 0)) %>%
    summarise(Productivity = list(as.character(Productivity)),
              Pulse_rel = list(Time / sum(Time) * 100),
              Pulse_abs = list(Time),
              Daily_min = sum(Time)/ length(all_dates),
              Daily_time = case_when(Daily_min < 1 ~ paste(round(Daily_min * 60), "sec"),
                                     TRUE ~ paste(round(Daily_min), "min")))

  MonthlyTrend <- data %>%
    mutate(ym = factor(format(DateTime, format = "%Y-%m")),
           Productivity = case_when(grepl("Distracting", Productivity) ~ "Distracting",
                                    TRUE ~ "Productive")) %>%
    group_by(!!!group_vars, Productivity, ym) %>%
    summarise(Time = round(sum(Time) / days_in_month(ym(first(ym)))[[1]]), .groups = "drop") %>%
    # the nesting gave me headaches: !!enquo or {{}} does not work here...
    complete(., expand(., nesting(!!!group_syms), Productivity, ym), 
             fill = list(Time = 0)) %>%
   group_by(!!!group_vars, Productivity) %>%
   summarise(MonthlyTrend = list(Time),
             MonthLabel = list(format(ym(ym), "%B %Y"))) %>%
    pivot_wider(., names_from = Productivity, values_from = MonthlyTrend,
                names_prefix = "monthly_")

  DailyTrend <- data %>%
    mutate(Productivity = case_when(grepl("Distracting", Productivity) ~ "Distracting",
                                    TRUE ~ "Productive")) %>%
    group_by(!!!group_vars, Productivity, Date) %>%
    summarise(Time = sum(Time), .groups = "drop") %>%
    right_join(., data.frame(Date = all_dates), by = "Date") %>%
    complete(., expand(., nesting(!!!group_syms), Productivity, Date),
             fill = list(Time = 0)) %>%
    filter(if_all(all_of(c(map_chr(group_vars, rlang::as_name), "Productivity")), ~ !is.na(.))) %>% # turn the ... into vector with strings
    group_by(!!!group_vars, Productivity) %>%
    summarise(DailyTrend = list(Time),
              DateLabel = list(format(Date, format = "%b %d %Y"))) %>%
    pivot_wider(., names_from = Productivity, values_from = DailyTrend,
                names_prefix = "daily_")

  Summary <-  ProdPulse %>%
    left_join(., DailyTrend) %>%
    left_join(., MonthlyTrend)
}

Overview <- RescueTime[["pc"]] %>%
  add.overview() %>%
  summarise.table(., Overview)

SubCat <- RescueTime[["pc"]] %>%
  add.overview() %>%
  summarise.table(., Overview, Category)

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
  
  # max <- max(c(map_dbl(prod_m, max, na.rm = TRUE), map_dbl(prod_14d, max, na.rm = TRUE)))
  # min <- -max(c(map_dbl(distr_m, max, na.rm = TRUE), map_dbl(distr_14d, max, na.rm = TRUE)))
  
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

# use html to set up table displayand add title + logo: this only works in html output (RMd)
tbl <- make.reactable.RT(df = Overview)


div(
  div(
  h2("Title"),
  "this is the subtitle"
  ),
  tbl,
  "caption"
)

make.reactable.RT(df = SubCat, groupBy = "Overview")

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


# see also : https://stackoverflow.com/questions/16348958/jquery-sparklines-possible-to-have-different-tooltips


# Table 2: Time in each subcategory ----


reactable(SubCat, pagination = FALSE, highlight = TRUE, 
          striped = TRUE, bordered = FALSE,
          borderless = TRUE, outlined  = FALSE,
          groupBy = "Overview",
          columns = list(
            Overview = colDef(name = "Main Category"),
            Category = colDef(name = "Sub Category"),
            Daily_min = colDef(show = FALSE),
            Daily_time = colDef(name = "Time per Day"),
            Productivity = colDef(show = FALSE),
            Total_sec = colDef(show = FALSE),
            Productivity_Pulse = colDef(
                  cell = function(value) {
                    stacked_bar(value)
                  },
                  align = "center",
                  minWidth = 400)),
          theme = reactableTheme(
            color = "black",
            borderColor = "black",
            highlightColor = "grey",
            stripedColor = "lightblue",
            cellPadding = "8px 12px",
            style = list(fontFamily = "Catamaran", align = "right"),
            headerStyle =  list(backgroundColor = "darkblue", color="#fff")
          ))




# Table 2: Top 20 Activities: ----

