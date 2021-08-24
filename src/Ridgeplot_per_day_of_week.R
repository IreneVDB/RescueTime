library(ggridges)
library(ggtext)
library(pins)
library(tidyverse)
library(lubridate)

# functions to summarize data: ----
get.data <- function(my_list){
  df <- bind_rows(my_list[which(map_int(my_list, nrow) > 0)]) %>%
    distinct() %>%
    rename(DateTime = Date) %>%
    mutate(Date = as_date(DateTime)) %>%
    arrange(DateTime)
}
summarise.5mins <- function(df){
  
  Timebin <- data.frame(DateTime = seq(ymd_hm(paste(df$Date[1], "00:00")),
                                       ymd_hm(paste(rev(df$Date)[1], "23:55")), by = "5 min"))
  
  ndays <- length(seq(df$Date[1], rev(df$Date)[1], by = "day"))
  
  df <- df %>%
    group_by(DateTime) %>%
    summarise(Time = sum(`Time Spent (seconds)`)) %>%
    left_join(Timebin, ., by="DateTime") %>%
    replace_na(list(Time = 0)) %>%
    mutate(Date = as_date(DateTime),
           Timebin = substr(DateTime, 12, 16),
           Weekday = fct_rev(wday(Date, week_start=1, label=TRUE))) %>%
    group_by(Weekday, Timebin) %>%
    summarise(Time = mean(Time) / 60 / 5) %>%
    mutate(row = row_number() - 1,
           ndays = ndays) 
}

# function to make ggridge plot: ----
make.ggridge.identity.5min <- function(df, group, hues, starttime=0){
  
  Totals <- df %>%
    group_by(!!as.name(group), Weekday) %>%
    summarise(TotalTime = sum(Time) * 5 /60)
  
  df <- df %>%
    filter(as.numeric(substr(Timebin, 1, 2)) >= starttime) %>%
    mutate(row = row - starttime * 12)
  
  colors <- hsv(hues, 0.8, 0.7)
  
  if(length(unique(df$ndays)) == 1){
    subtitle_add <- unique(df$ndays)
  } else{
    subtitle_add <- paste("<span style = 'color: ", colors[1], ";'>", unique(df$ndays)[1], 
                          "</span> / <span style = 'color: ", colors[2], ";'>", unique(df$ndays)[2], "</span>")
  }
  
  title <- paste0("Time on <span style = 'color: ", colors[1], ";'>", levels(df[[group]])[1], 
                  "</span> and <span style = 'color: ",
                  colors[2], ";'>", levels(df[[group]])[2], "</span> per day of week")
  subtitle <- paste0("Measured with Rescue Time (", subtitle_add, " days)")
  
  breaks <- seq(0, (24 - starttime) * 12 - 1, by = 12)
  
  ggplot(data = df, aes(x = row, y = Weekday)) + 
    geom_density_ridges(aes(fill = !!as.name(group), color = !!as.name(group),  height = Time),
                        stat="identity", alpha = 0.8, size = 0.2, scale = 0.9,
                        show.legend = FALSE) +
    geom_text(data = Totals, aes(x= (24-starttime) * (12 + 1/4), y = c(1:7 + 1, 1:7 + 0.7),
                                 label = paste(format(round(TotalTime, digits = 2), nsmall = 2),  "h/day"),
                                 hjust=0, vjust=1, family = "Comfortaa", color = !!as.name(group)),
              show.legend = FALSE, size = 4.2) +
    coord_cartesian(clip = "off") +
    labs(x = "Time of Day", y = NULL, 
         title = title, subtitle = subtitle) +
    theme_minimal(base_family = "Comfortaa") +
    theme(plot.margin = unit(c(5, 70, 5, 15), "pt"),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid = element_blank(),
          plot.subtitle = element_markdown(size = 14, hjust = 0, color = "grey30", margin = margin(b = 30)),
          plot.title.position = "plot",
          plot.title = element_markdown(face = "bold", size = 20, hjust = 0, color = "grey20"),
          axis.title.x = element_text(face = "bold", margin = margin(t = 6), color = "grey30", size = 12),
          axis.text.x = element_text(size = 10, color = "grey40", margin = margin(t = 5)),
          axis.text.y = element_text(size = 12, color = "grey40", margin = margin(r = 5), vjust = 0),
          axis.ticks.x = element_line(color = "grey60", size = 0.2)) +
    scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = df$Timebin[breaks + 1]) +
    scale_y_discrete(expand = c(0,0)) +
    scale_fill_manual(values = hsv(h=hues, s = 0.6, v = 0.9)) +
    scale_color_manual(values = hsv(h=hues, s = 0.8, v = 0.7)) 
}

# make ggridge plot from time on PC vs time on Mobile:

RescueTime <- pin_get("RescueTime")

pc_Times <- bind_rows("laptop" = summarise.5mins(get.data(RescueTime[["pc"]])), 
                       "mobile device" = summarise.5mins(get.data(RescueTime[["mobile"]])), .id = "device") %>%
    distinct() %>%
    mutate(device = fct_reorder(device, Time, .desc=TRUE))

png(paste0("output/", Sys.Date(), "_pcTimes.png"), 
    width = 13, height =8, units = "in", res = 300)
make.ggridge.identity.5min(df=pc_Times, group="device", hues=c(0.53, 0.78), starttime=5)
dev.off()
