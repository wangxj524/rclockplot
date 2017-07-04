
clock.data.24 <- data.frame(hour=1:24,
                            percent=sample(seq(0, 1, 0.01), 24))
clock.data.12 <- data.frame(hour=1:12,
                            number=sample(100, 12))

clock_data <- function(data, base = 'max'){
    colnames(data) <- c('hour', 'percent')
    if (base == 'max'){
        base <- max(data$percent)
    }

    if (nrow(data) == 12){
        data$percent <- data$percent/base
        return(data)
    }else{
        if (nrow(data) == 24){
            data$percent <- data$percent/base
            return(data)
        }
    }
}

clock_histogram <- function(data, clock.dial, type = c('hour', 'minute')[1],
                            percent.100 = 0.8){
    center <- clock.dial$center
    radius <- clock.dial$radius
    hour_system <- clock.dial$hour_system
    p <- clock.dial$plot

    if (nrow(data) == 12){
        for (i in 1:12){
            circle.12 <- circle_data(center, radius*2*percent.100*data[13-i, 2],
                                     npoints = 121, filled = FALSE,
                                     start = 5/12, end = 29/12)
            temdata <- rbind(c(0, 0), circle.12[((i-1)*10+1):(i*10+1),])
            p <- p + geom_polygon(data = temdata, aes(x = x, y = y),
                                  alpha = 0.5,
                                  color = "black", fill = "red")
        }
        return(p)
    }
    p <- clock.dial$plot
    if (nrow(data) == 24){
        for (i in 1:24){
            circle.12 <- circle_data(center, radius*2*percent.100*data[25-i, 2],
                                     npoints = 241, filled = FALSE,
                                     start = 11/24, end = 59/24)
            temdata <- rbind(c(0, 0), circle.12[((i-1)*10+1):(i*10+1),])
            p <- p + geom_polygon(data = temdata, aes(x = x, y = y),
                                  alpha = 0.5,
                                  color = "black", fill = "red")
        }
        p
    }
}

pie_clock <- function(){

}

# code test ---------------------------------------------------------------

data <- clock_data(clock.data.12)
data <- clock.data.24
clock.dial <- clock_ticks(clock_dial(hour_system = 24))
