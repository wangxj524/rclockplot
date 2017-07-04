
plot_time <- function(clock.dial, hour = 12, minute = 0,
                      hour.hand.color = "blue", hour.hand.size = 0.7,
                      minute.hand.color = "red", minute.hand.size = 0.7) {
    p <- clock.dial$plot
    radius <- clock.dial$radius
    center <- clock.dial$center
    hour_system <- clock.dial$hour_system

    minute_seq <- c(15:1, 0, 59:16)
    minute_list <- lapply(minute_seq, function(x) 2/60 * which(minute_seq == x))
    time.start <- minute_list[[which(minute_seq == minute)]]
    time.end <- time.start - 2/60
    minute.site.data <- circle_data(center, radius * 2 * 0.7,
                                    npoints = 60, time.end, time.start, FALSE)
    minute.site <- minute.site.data[6 - minute, ]
    minute.site.data <- data.frame(x = center[1], y = center[2],
                                   xend = minute.site[1, 1], yend = minute.site[1, 2])
    p <- p + geom_segment(data = minute.site.data,
                          aes(x = x, y = y, xend = xend, yend = yend),
                          color = minute.hand.color,
                          size = minute.hand.size,
                          arrow = arrow(length = unit(0.2, "cm")))

    if (hour_system == 12) {
        if (hour > 12)
            hour <- hour - 12
        hour_seq <- c(2:1, 12:3)
        time_list <- lapply(hour_seq, function(x) 2/hour_system * which(hour_seq == x))
    } else {
        hour_seq <- c(5:1, 24:6)
        time_list <- lapply(hour_seq, function(x) 2/hour_system * which(hour_seq == x))
    }
    time.start <- time_list[[which(hour_seq == hour)]]
    time.end <- time.start - 2/hour_system
    hour.site.data <- circle_data(center, radius * 2 * 0.55,
                                  npoints = 60, time.end, time.start, FALSE)
    hour.site <- hour.site.data[60 - minute, ]
    hour.site.data <- data.frame(x = center[1], y = center[2],
                                 xend = hour.site[1, 1], yend = hour.site[1, 2])
    p <- p + geom_segment(data = hour.site.data,
                          aes(x = x, y = y, xend = xend, yend = yend),
                          color = hour.hand.color,
                          size = hour.hand.size,
                          arrow = arrow(length = unit(0.2, "cm")))
    retvalue = list(center = center, radius = radius,
                    hour_system = hour_system, plot = p)
    retvalue
}

plot_now <- function(clock.dial,
                     hour.hand.color = "blue", hour.hand.size = 0.7,
                     minute.hand.color = "red", minute.hand.size = 0.7) {
    p <- clock.dial$plot
    radius <- clock.dial$radius
    center <- clock.dial$center
    hour_system <- clock.dial$hour_system

    hour <- as.numeric(format(Sys.time(), format = "%H"))
    minute <- as.numeric(format(Sys.time(), format = "%M"))
    p <- plot_time(clock.dial, hour = hour, minute = minute,
                   hour.hand.color = hour.hand.color,
                   hour.hand.size = hour.hand.size,
                   minute.hand.color = minute.hand.color,
                   minute.hand.size = minute.hand.size)
    retvalue = list(center = center, radius = radius,
                    hour_system = hour_system, plot = p)
    retvalue
}


# code test ---------------------------------------------------------------

clock.dial <- clock_dial()
clock.ticks <- clock_ticks(clock.dial)
plot_now(clock.ticks)
plot_time(clock.ticks)
