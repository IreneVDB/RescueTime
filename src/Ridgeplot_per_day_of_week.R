library(ggridges)
library(ggtext)
library(pins)
library(tidyverse)
library(lubridate)

# make ggridge plot from time on PC vs time on Mobile:

RescueTime <- pin_get("RescueTime")

pc_Times <- bind_rows("laptop" = RescueTime[["pc"]], 
                      "mobile device" = RescueTime[["mobile"]], .id = "device") 

ggridge.weekday.identity <- function(df, group_by, count_missing_days = NA, tracker, 
                                     time_interval=5, hues, starttime=0){
  
  if(any(is.na(count_missing_days))){
    count_missing_days <- sapply(unique(df[[group_by]]), is.character, 
                                 simplify=FALSE, USE.NAMES = TRUE)
  }
  
  Summary <- df %>%
    rename(group = as.name(!!group_by)) %>%
    group_by(group, DateTime) %>%
    summarise(Time = sum(`Time Spent (seconds)`)) %>%
    mutate(group = fct_reorder(group, Time, .desc=TRUE))
  
  # create long df with ALL times including missing days if count_missing_days == TRUE:
  Timebin <- map_dfr(levels(Summary$group), function(factor){
    
    cmd <- count_missing_days[[factor]]
    
    if(cmd == TRUE){
      DateTimes <- seq(ymd_hm(paste(df[which(df[[group_by]] == factor),]$Date[1], "00:00")),
                       ymd_hm(paste(rev(df[which(df[[group_by]] == factor),]$Date)[1], "23:55")), 
                       by = paste(time_interval, "min"))
    } else{
      DateTimes <- ymd_hm(paste(rep(unique(df[which(df[[group_by]] == factor),]$Date), 
                                    each = 24 * 60 / time_interval),
                                paste(seq(0, 24 - time_interval/60, by = time_interval/60) %/% 1, 
                                      round(seq(0, 24 - time_interval/60, by = time_interval/60) %% 1 * 60), 
                                      sep = ":")))
    }
    
    All_Times <- data.frame(DateTime = DateTimes) %>%
      mutate(group = factor,
             ndays = length(unique(as_date(DateTimes))))
  })
  
  # Add all times to Summary data:
  All_data <- left_join(Timebin, Summary, by=c("DateTime", "group")) %>%
    replace_na(list(Time = 0)) %>%
    mutate(Date = as_date(DateTime),
           Timebin = substr(DateTime, 12, 16),
           Weekday = fct_rev(wday(Date, week_start=1, label=TRUE))) %>%
    group_by(group, Weekday, Timebin) %>%
    summarise(Time = mean(Time) / 60 / time_interval, .groups = "drop_last") %>% # minutes per bin
    mutate(row = row_number() - 1) 
  
  # Summarise total Time based on all days:
  Totals <- All_data %>%
    group_by(group, Weekday) %>%
    summarise(TotalTime = sum(Time) * time_interval /60) %>% # total in hours (~AUC)
    mutate(Total_chr = case_when(TotalTime %/% 1 < 1 ~ paste0(round(TotalTime * 60), " min"),
                       TRUE ~ paste0(TotalTime %/% 1, "h", 
                                     str_pad(round(TotalTime %% 1 * 60), 2, "left", "0"))))
  # Normalize x-axis Time range:
  df_time <- All_data %>%
    filter(as.numeric(substr(Timebin, 1, 2)) >= starttime) %>%
    mutate(row = row - starttime * (60 / time_interval))
  
  colors <- hsv(hues, 0.8, 0.7)
  
  # Define title and subtitle: 
  
  if(length(unique(Timebin$ndays)) == 1){
    subtitle_add <- unique(Timebin$ndays)
  } else{
    subtitle_add <- paste("<span style = 'color: ", colors[1], ";'>", unique(Timebin$ndays)[1], 
                          "</span>/<span style = 'color: ", colors[2], ";'>", unique(Timebin$ndays)[2], "</span>")
  }
  
  title <- paste0("Time on <span style = 'color: ", colors[1], ";'>", levels(Summary$group)[1], 
                  "</span> and <span style = 'color: ",
                  colors[2], ";'>", levels(Summary$group)[2], "</span> per day of week")
  subtitle <- paste0("Measured with ", tracker, " (", subtitle_add, " days)")
  
  # make the plot:
  breaks <- seq(0, (24 - starttime) * (60 / time_interval) - 1, by = 60 / time_interval)
  
  ggplot(data = df_time, aes(x = row, y = Weekday)) + 
    geom_density_ridges(aes(fill = group, color = group,  height = Time),
                        stat="identity", alpha = 0.8, size = 0.2, scale = 0.9, show.legend = FALSE) +
    geom_text(data = Totals, aes(x= (24-starttime) * (12 + 1/4), y = c(1:7 + 1, 1:7 + 0.7),
                                 label = paste(Total_chr,  "/day"),
                                 hjust=0, vjust=1, family = "Comfortaa", color = group),
              show.legend = FALSE, size = 4.2) +
    coord_cartesian(clip = "off") +
    labs(x = "Time of Day", y = NULL, title = title, subtitle = subtitle) +
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
    scale_x_continuous(expand = c(0, 0), breaks = breaks, labels = df_time$Timebin[breaks + 1]) +
    scale_y_discrete(expand = c(0,0)) +
    scale_fill_manual(values = hsv(h=hues, s = 0.6, v = 0.9)) +
    scale_color_manual(values = hsv(h=hues, s = 0.8, v = 0.7)) 
  }
  
png("output/pcTimes.png", width=13, height=8, units="in", res=300)
ggridge.weekday.identity(df=pc_Times, group_by="device", 
                         count_missing_days =  list("laptop" = TRUE, "mobile device" = FALSE),
                         tracker="RescueTime", hues = c(0.53, 0.78), starttime=5)
dev.off()
