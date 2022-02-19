library(tidyverse)
library(lubridate)
library(pins)

# library(berryFunctions)

RescueTime <- pin_get("RescueTime")

make.datetime.matrix <- function(df, time_interval, starttime=3){
  Timebin <- data.frame(DateTime = seq(ymd_hm(paste(df$Date[1], "00:00")),
                                    ymd_hm(paste(rev(df$Date)[1], "23:55")), 
                                     by = paste(time_interval, "min")))
  
  levels <- paste0(str_pad(rep((0:23 + starttime) %% 24, each = 60 / time_interval), 
                           2, "left", "0"), ":", 
                  str_pad(seq(0, 60 - time_interval, by = time_interval), 
                          2, "left", "0"))
  
  matrix <- df %>%
    group_by(DateTime) %>%
    summarise(Time = sum(`Time Spent (seconds)`)) %>%
    left_join(Timebin, ., by="DateTime") %>%
    replace_na(list(Time = 0)) %>%
    mutate(Date = as_date(DateTime),
           Timebin = factor(substr(DateTime, 12, 16), levels=levels)) %>%
    select(-DateTime) %>%
    #pivot_wider(names_from = Date, values_from = Time) %>%
    arrange(Timebin, Date)
  
}

data <- make.datetime.matrix(RescueTime[["pc"]], 5)

if(col=="green"){
  h <- c(150, 125, 85)
} else if(col =="blue"){
  h <- c(225, 200, 180)
}

color_fun <- circlize::colorRamp2(c(0, mean(data$Time[data$Time!=0]), 300), 
                                  hsv(h / 360 , c(0.3, 0.65, 1), c(0.1, 0.8, 1)))

text_color <- hsv(h[3], 1, 1)


setup.plot <- function(data, ma_cm= c(4.2, 2, 2, 0.2), om_cm= c(1, 1, 1, 1), 
                       font="Game Over", starttime=3, time_interval=5){
  
  ndays <- length(unique(data$Date))
  ntimes <- nlevels(data$Timebin)
  
  xrange <- c(0, ndays)
  yrange <- c(-ntimes, 0)
  
  par(mai = ma_cm / 2.54, omi = om_cm / 2.54, family= font, xpd = TRUE)
  plot(xrange, yrange, type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", axes = FALSE)
  
  row_h <- par("pin")[2] * 2.54 / ntimes
  col_w <- par("pin")[1] * 2.54 / ndays
  #asp <- row_h / col_w
  
  # draw the screen layout:
  inner <- par("usr") + ma_cm[c(3, 4, 1, 2)] / c(row_h, col_w, -row_h, -col_w)
  outer <- inner + om_cm[c(3, 4, 1, 2)] / c(row_h, col_w, -row_h, -col_w)
  
  berryFunctions::roundedRect(xleft=outer[4], ybottom = outer[3],
                              xright = outer[2], ytop=outer[1],
                              rounding=.05, col="grey90", border="black", xpd=NA)
  
  rect(xleft = inner[4], ybottom = inner[3],
       xright = inner[2], ytop = inner[1],
       col = "black", xpd=TRUE)
  
  symbols(x = rep(outer[4] + 0.5 * diff(outer[c(4, 2)]), 2),
          y = outer[c(1, 3)] + c(-0.5, 0.5) * om_cm[c(3, 1)] / row_h , 
          circles = c(0.15, 0.3) / col_w,
          fg = "black", bg = c("black", "transparent"),
          asp = 1, add = TRUE, inches = FALSE, xpd = NA)
  
  # add axes + labels:
  
  # ticks per hour:
  segments(x0 = -0.2, y0 = seq(0, -ntimes, by=-12), x1 = -0.2 / col_w, 
           col = adjustcolor("white", alpha.f = 0.5), lwd = 1, xpd = TRUE)
  
  time_labels = levels <- paste0((0:23 + starttime) %% 24, ":00")
  
  mtext(text = time_labels, las = 2, side = 2, line = 0.7, 
        at = seq(0, -ntimes, by= -12), col = text_color, cex = 2)
  
  # add the data:
  symbols(x = rep(1:ndays - 0.5, ntimes),
          y = rep(-1:-ntimes + 0.5, each = ndays),
          rectangles = matrix(rep(c(1,1), ndays * ntimes), byrow = TRUE, ncol = 2),
          bg = color_fun(data$Time), fg = "black", lwd = 0.2,
          add = TRUE, inches = FALSE)

  
}

setup.plot(data, ma_cm=c(1, 1.5, 0.5, 0.5), om_cm=c(0.8, 0.8, 0.8, 0.8))


load("/Users/irenevandenbroek/Documents/R-Projects/Projects/QuantifiedSelf/Sleep/Dreem.RData")




make.bit.heatmap <- function(device, print = NA){
  
  if(is.na(print) == FALSE){
    printname <- paste0("output/RescueTime/Heatmap/", Sys.Date(), "-", device,"_bits.png")
    png(printname, height = cm_per_row * nrow(data) + ma_cm[1] + ma_cm[3] + om_cm[1] + om_cm[3], 
        width = cm_per_col * ncol(data) + ma_cm[2] + ma_cm[4] + om_cm[2] + om_cm[4], 
      units = "cm", res = 500)
  }
  
  
  symbols(x = rep(1:ncol(data) - 0.5, 24),
          y = rep(-1:-24 + 0.5, each = ncol(data)),
          rectangles = matrix(rep(c(1,1), ncol(data) * 24), byrow = TRUE, ncol = 2),
          bg = as.vector(t(color_fun(data))), fg = "black", lwd = 0.2,
          add = TRUE, inches = FALSE)
  
  
  # add binary bits
  text(x = rep(1:ncol(data) - 0.5, 24),
       y = rep(-1:-24 + 0.5, each = ncol(data)), adj = 0.5,
       labels = sample(c(0,1), replace = TRUE, size= 24 * ncol(data)),
       col = adjustcolor("white", alpha.f = 0.8), cex = 1.25)
  
  # title and subtitle:
  mtext(text = paste0("Screen Time on ", device), side = 3, line = 1.8, adj = 1,
        font = 2, col = text_color, cex = 4, at = ncol(data) - 1)
  mtext(text = paste0("RescueTime (", hours, "h on ", device,
                      " over ", ndays, " days)"),
        side = 3, line = 0.5, adj = 1, col = text_color, cex = 3, at = ncol(data) - 1)
  # column labels:
  date_labels <- as_date(substr(colnames(data), 1, 10))
  breaks = unique(c(0, which(day(date_labels) == 1) - 1, ncol(data)))
  segments(x0 = breaks, y0 = -24.2, x1 = breaks, y1 = -25.5, 
           col = adjustcolor("white", alpha.f = 0.75), lwd = 1, xpd = TRUE)
  label.pos <- tapply(rep(breaks, each = 2)[-c(1, 2 *length(breaks))], 
                      rep(1:(length(breaks) - 1), each = 2), mean)
  mtext(text = month(date_labels[breaks], TRUE, TRUE), 
        at = label.pos, cex = 2, 
        side = 1, line = 0.3, col = text_color)
  mtext(text = year(date_labels[breaks]), 
        at = label.pos, cex = 2, 
        side = 1, line = 1, col = text_color)
  mtext(text = "Date", at = 0.5 * ncol(data), side = 1, line = 2,
        cex = 3, col = text_color)
  
  # add the times to bed and wake up from Dreem data:
  get.date.time <- function(column){
    column <- enquo(column)
    name <- paste0("Time_", gsub("_local", "", quo_name(column)))
    df <- Dreem[["Day"]] %>%
      filter(is_nap ==0 | is.na(is_nap) == TRUE) %>%
      select(date, !!column) %>%
      summarise(Date = case_when(grepl("stop", name) == TRUE ~ date, 
                                 TRUE ~ date - days(1)),
                !!name := imputeTS::na_ma((21 + (as.numeric(substr(!!column, 12, 13)) * 3600 +
                             as.numeric(substr(!!column, 15, 16)) * 60 + 
                             as.numeric(substr(!!column, 18, 19))) / 3600) %% 24,k =1, weighting = "linear"))
  }
  # for time to bed use the date of day before (as long as before 3 am)
  sleep_start <- get.date.time(sleep_start_local)
  start <- get.date.time(start_local)
  stop <- get.date.time(stop_local)
  
  datesRT <- data.frame(Date = as_date(substr(colnames(data), 1, 10))) %>%
    left_join(., get.date.time(sleep_start_local), by = "Date") %>%
    left_join(., get.date.time(start_local), by = "Date") %>%
    left_join(., get.date.time(stop_local), by = "Date") %>%
    mutate(row = row_number())
  
  if(is.na(datesRT$Time_sleep_start[nrow(datesRT)]) == TRUE){
    datesRT$Time_sleep_start[nrow(datesRT)] <- datesRT$Time_sleep_start[nrow(datesRT) - 1]
  }
  
  # polygon for asleep till wake up:
  polygon(x = c(0, datesRT$row - 0.5, nrow(datesRT), nrow(datesRT), 0),
          y = c(-datesRT$Time_stop[1], -datesRT$Time_stop, 
                -rev(datesRT$Time_stop)[1], 0, 0),
          col = adjustcolor("white", alpha.f = 0.15), border = NA)
  # polygon for sleep start till 3 am
  polygon(x = c(0, datesRT$row - 0.5, nrow(datesRT), nrow(datesRT), 0),
          y = c(-datesRT$Time_sleep_start[1], -datesRT$Time_sleep_start, 
                -rev(datesRT$Time_sleep_start)[1], -24, -24),
          col = adjustcolor("white", alpha.f = 0.15), border = NA)
  
  # add legend:
  ticks <- seq(0.5 * ncol(data) - 30, 0.5 * ncol(data) + 30, 
               length.out = max(data)/ 10 + 1)[c(1:7, max(data) / 10 + 1)]
  box <- seq(0, max(data), by = 1)
  
  symbols(x = seq(0.5 * ncol(data) - 30, 0.5 * ncol(data) + 30, length.out = max(data) + 1),
          y = rep(-29.5, ceiling(max(data) + 1)),
          rectangles = matrix(rep(c(1,2), ceiling(max(data) + 1)), byrow = TRUE, ncol = 2),
          bg = color_fun(box),
          fg = color_fun(box),
          add = TRUE, inches = FALSE, xpd = TRUE)
  segments(x0 = ticks, y0 = -30.5, x1 = ticks, y1 = -31, 
           col = adjustcolor("white", alpha.f = 0.75), xpd = TRUE)
  text(x = ticks, y = -31.5, col = adjustcolor("white", alpha.f = 0.9), 
       xpd = TRUE, cex = 1.6,
       labels = seq(0, max(data), by = 10)[c(1:7, max(data) / 10 + 1)])
  text(x = 0.5 * ncol(data) - 32, y = -29.5, labels = "Screentime (min/h)",
       xpd = TRUE, col = adjustcolor("white", alpha.f = 0.9), cex = 2.2, adj = c(1, 0.5))
  
  # add legend for asleep
  symbols(x = 0.5 * ncol(data) + 40, y = -29.5, squares = 4,
          bg = adjustcolor("white", alpha.f = 0.2), 
          add = TRUE, inches = FALSE, xpd = TRUE)
  text(x = 0.5 * ncol(data) + 44, y = -29.5, labels = "Asleep",
       xpd = TRUE, col = adjustcolor("white", alpha.f = 0.9),
       cex = 2.2, adj = c(0, 0.5))
  
  
  
  
  if(is.na(print) == FALSE){
    dev.off()
  }
  
  
}

make.bit.heatmap(device = "Mobile", print = "png")
make.bit.heatmap(device = "Computer", print = "png")


# add bedtimes as a polygon:

get.date.time <- function(column){
  column <- enquo(column)
  name <- paste0("Time_", gsub("_local", "", quo_name(column)))
  df <- Dreem[["Day"]] %>%
    filter(is_nap == 0) %>%
    select(date, !!column) %>%
    summarise(Date = case_when(grepl("stop", name) == TRUE ~ date, 
                               TRUE ~ date - days(1)),
           !!name := (as.numeric(substr(!!column, 12, 13)) * 3600 +
                     as.numeric(substr(!!column, 15, 16)) * 60 + 
                     as.numeric(substr(!!column, 18, 19))) / 3600)
}
# for time to bed use the date of day before (as long as before 3 am)
sleep_start <- get.date.time(sleep_start_local)
start <- get.date.time(start_local)
stop <- get.date.time(stop_local)

datesRT <- data.frame(Date = as_date(substr(colnames(data), 1, 10))) %>%
  left_join(., get.date.time(sleep_start_local), by = "Date") %>%
  left_join(., get.date.time(start_local), by = "Date") %>%
  left_join(., get.date.time(stop_local), by = "Date")
  

# get dates from column names and "right_join" so that all dates from rescuetime are use
# give row_number which will be x position (Date)
# time will be y-position: ypos = 3 - Time
