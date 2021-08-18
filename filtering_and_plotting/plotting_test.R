library(ggplot2)

data <- data.frame(number_of_dogs <- c(10, 20, 25),
                   year <- c(2018, 2019, 2020))

plot<- ggplot(data, aes(x=year, y=number_of_dogs))+
        geom_line()+
        geom_point()

