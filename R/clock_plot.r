
.onAttach <- function(libname, pkgname){
    packageStartupMessage("Welcome to use rclockplot.")
}

circle_data <- function(center = c(0, 0), diameter = 1, npoints = 300,
                        start = 0, end = 2, filled = TRUE) {
    tt <- seq(start * pi, end * pi, length.out = npoints)
    df <- data.frame(x = center[1] + diameter/2 * cos(tt), y = center[2] +
                         diameter/2 * sin(tt))
    if (filled == TRUE) {
        df <- rbind(df, center)
    }
    return(df)
}

clock_dial <- function(center = c(0, 0), radius = 5, hour_system = 12,
                       xlimit = c(-radius - radius * 0.2, radius + radius * 0.2),
                       ylimit = xlimit, color.dial = "black",
                       color.dial.background = "gray90",
                       edge.width = 1, center.point.size = 3,
                       center.point.shape = 19,
                       color.out.dial = "white", plot.ticks.line = TRUE,
                       ticks.line.width = 0.3,
                       ticks.line.percent = 0.8) {
    diameter <- radius * 2
    quarterCircle <- circle_data(center, diameter, start = 0,
                                 end = 2, filled = TRUE)
    fullCircle <- circle_data(center, diameter, start = 0, end = 2,
                              filled = FALSE)
    p <- ggplot(fullCircle, aes(x = x, y = y)) +
        geom_polygon(data = quarterCircle,
                     aes(x, y), color = color.dial.background,
                     fill = color.dial.background) +
        theme_void() + xlim(xlimit[1], xlimit[2]) + ylim(ylimit[1],
                                                         ylimit[2])
    if (plot.ticks.line == TRUE) {
        ticks.lines.data <- circle_data(center, diameter * ticks.line.percent,
                                        npoints = hour_system + 1, filled = FALSE)
        for (i in 1:(nrow(ticks.lines.data) - 1)) {
            p <- p +
                geom_segment(data = ticks.lines.data,
                             aes(x = 0, y = 0, xend = x, yend = y),
                             color = color.out.dial,
                             size = ticks.line.width)
        }
    }
    p <- p + geom_path(color = color.dial, size = edge.width) +
        geom_point(aes(x = 0, y = 0), color = color.dial, size = center.point.size,
                   shape = center.point.shape)
    retvalue = list(center = center, radius = radius,
                    hour_system = hour_system, plot = p)
    retvalue
}

clock_ticks <- function(clock.dial, edge.distance = 0.03,
                        ticks.type = c('segment', 'point')[1],
                        show.ticks = TRUE, show.time = TRUE,
                        major.ticks.size = 0.9, minor.ticks.size = 0.7,
                        major.ticks.length.percent = 0.1,
                        minor.ticks.length.percent = 0.07,
                        show.minor.ticks = TRUE, show.minor.time = TRUE,
                        minor.time.size = 3.5, major.time.size = 3.8,
                        major.ticks.color = "black", major.time.color = "black",
                        minor.ticks.color = "black", minor.time.color = "black",
                        ticks.position = c("inner", "outer")[1]) {
    p <- clock.dial$plot
    radius <- clock.dial$radius
    center <- clock.dial$center
    hour_system <- clock.dial$hour_system
    diameter = radius * 2

    if (ticks.position == "inner") {
        if (show.ticks) {
            major.ticks.outer <- circle_data(center,
                                             diameter * (1 - edge.distance),
                                             npoints = 5, filled = FALSE)
            major.ticks.inner <- circle_data(center,
                                             diameter * (1 - major.ticks.length.percent),
                                             npoints = 5,
                                             filled = FALSE)
            major.ticks <- cbind(major.ticks.outer, major.ticks.inner)
            colnames(major.ticks) <- c("x1", "y1", "x2", "y2")

            if (show.minor.ticks) {
                minor.ticks.outer <- circle_data(center,
                                                 diameter * (1 - edge.distance),
                                                 npoints = hour_system + 1,
                                                 filled = FALSE)
                minor.ticks.inner <- circle_data(center,
                                                 diameter * (1 - minor.ticks.length.percent),
                                                 npoints = hour_system + 1,
                                                 filled = FALSE)
                minor.ticks <- cbind(minor.ticks.outer, minor.ticks.inner)
                colnames(minor.ticks) <- c("x1", "y1", "x2", "y2")
                if (ticks.type == "segment"){
                    p <- p + geom_segment(data = minor.ticks,
                                          aes(x = x1, y = y1, xend = x2, yend = y2),
                                          colour = minor.ticks.color,
                                          size = minor.ticks.size)
                }else{
                    p <- p + geom_point(data = minor.ticks, aes(x = x1, y = y1),
                                          colour = minor.ticks.color,
                                          size = minor.ticks.size)
                }

            }
            if (ticks.type == "segment"){
                p <- p + geom_segment(data = major.ticks,
                                      aes(x = x1, y = y1, xend = x2, yend = y2),
                                      colour = major.ticks.color,
                                      size = major.ticks.size)
            }else{
                p <- p + geom_point(data = major.ticks, aes(x = x1, y = y1),
                                    colour = major.ticks.color,
                                    size = major.ticks.size)
            }
        }

        if (show.time) {
            if (hour_system == 12) {
                hour_seq <- c("3", "12", "9", "6")
            } else {
                hour_seq <- c("6", "24", "18", "12")
            }
            if (show.ticks) {
                time.site.base <- major.ticks.length.percent +
                    0.05
            } else {
                time.site.base <- major.ticks.length.percent
            }
            major.ticks.inner <- circle_data(center,
                                             diameter * (1 - time.site.base),
                                             npoints = 5, filled = FALSE)
            for (i in 1:4) {
                p <- p + annotate("text", x = major.ticks.inner[i, 1],
                                  y = major.ticks.inner[i, 2],
                                  label = hour_seq[i],
                                  size = major.time.size,
                                  color = major.time.color)
            }
            if (show.minor.time) {
                if (hour_system == 12) {
                    hour_seq <- c("", 2, 1, "", 11, 10, "", 8, 7, "", 5, 4)
                } else {
                    hour_seq <- c("", 5, 4, 3, 2, 1, "", 23, 22, 21, 20, 19, "",
                                  17, 16, 15, 14, 13, "", 11, 10, 9, 8, 7)
                }
                minor.ticks.inner <- circle_data(center,
                                                 diameter * (1 - time.site.base),
                                                 npoints = hour_system + 1,
                                                 filled = FALSE)
                for (i in 1:hour_system) {
                    p <- p + annotate("text", x = minor.ticks.inner[i, 1],
                                      y = minor.ticks.inner[i, 2],
                                      label = hour_seq[i],
                                      size = minor.time.size,
                                      color = minor.time.color)
                }
            }
        }
    }

    if (ticks.position == "outer") {
        if (show.ticks) {
            major.ticks.outer <- circle_data(center,
                                             diameter * (1 + edge.distance),
                                             npoints = 5, filled = FALSE)
            major.ticks.inner <- circle_data(center,
                                             diameter * (1 + major.ticks.length.percent),
                                             npoints = 5, filled = FALSE)
            major.ticks <- cbind(major.ticks.outer, major.ticks.inner)
            colnames(major.ticks) <- c("x1", "y1", "x2", "y2")

            if (show.minor.ticks) {
                minor.ticks.outer <- circle_data(center,
                                                 diameter * (1 + edge.distance),
                                                 npoints = hour_system + 1,
                                                 filled = FALSE)
                minor.ticks.inner <- circle_data(center,
                                                 diameter * (1 + minor.ticks.length.percent),
                                                 npoints = hour_system + 1, filled = FALSE)
                minor.ticks <- cbind(minor.ticks.outer, minor.ticks.inner)
                colnames(minor.ticks) <- c("x1", "y1", "x2", "y2")
                if (ticks.type == "segment"){
                    p <- p + geom_segment(data = minor.ticks,
                                          aes(x = x1, y = y1, xend = x2, yend = y2),
                                          colour = minor.ticks.color,
                                          size = minor.ticks.size)
                }else{
                    p <- p + geom_point(data = minor.ticks, aes(x = x1, y = y1),
                                        colour = minor.ticks.color,
                                        size = minor.ticks.size)
                }
            }
            if (ticks.type == "segment"){
                p <- p + geom_segment(data = major.ticks,
                                      aes(x = x1, y = y1, xend = x2, yend = y2),
                                      colour = major.ticks.color,
                                      size = major.ticks.size)
            }else{
                p <- p + geom_point(data = major.ticks, aes(x = x1, y = y1),
                                    colour = major.ticks.color,
                                    size = major.ticks.size)
            }
        }

        if (show.time) {
            if (hour_system == 12) {
                hour_seq <- c("3", "12", "9", "6")
            } else {
                hour_seq <- c("6", "24", "18", "12")
            }
            if (show.ticks) {
                time.site.base <- major.ticks.length.percent + 0.05
            } else {
                time.site.base <- major.ticks.length.percent
            }
            major.ticks.inner <- circle_data(center,
                                             diameter * (1 + time.site.base),
                                             npoints = 5, filled = FALSE)
            for (i in 1:4) {
                p <- p + annotate("text", x = major.ticks.inner[i, 1],
                                  y = major.ticks.inner[i, 2],
                                  label = hour_seq[i],
                                  size = major.time.size,
                                  color = major.time.color)
            }
            if (show.minor.time) {
                if (hour_system == 12) {
                    hour_seq <- c("", 2, 1, "", 11, 10, "", 8, 7, "", 5, 4)
                } else {
                    hour_seq <- c("", 5, 4, 3, 2, 1, "", 23, 22, 21, 20, 19, "",
                                  17, 16, 15, 14, 13, "", 11, 10, 9, 8, 7)
                }
                minor.ticks.inner <- circle_data(center,
                                                 diameter * (1 + time.site.base),
                                                 npoints = hour_system + 1,
                                                 filled = FALSE)
                for (i in 1:hour_system) {
                    p <- p + annotate("text", x = minor.ticks.inner[i, 1],
                                      y = minor.ticks.inner[i, 2],
                                      label = hour_seq[i],
                                      size = minor.time.size,
                                      color = minor.time.color)
                }
            }
        }
    }
    retvalue = list(center = center, radius = radius,
                    hour_system = hour_system, plot = p)
    retvalue
}

# code test ---------------------------------------------------------------
clock.dial <- clock_dial()
clock.ticks <- clock_ticks(clock.dial)
