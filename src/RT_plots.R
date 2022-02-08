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

#source("src/Load_data_API.R")

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


