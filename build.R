library(dplyr)
library(magrittr)
library(ggmap)
library(readr)
library(animation)
library(lubridate)

# First we read the CSV file
parking <- read_csv("parking.csv.gz")

# We generate a few new columns, one for day of the week, and a 
# truncated year-month-day variable.
parking %<>% mutate(wday=wday(Date.Time), 
                    ymd=ymd(paste(year(Date.Time),
                                  month(Date.Time),
                                  day(Date.Time), sep="-")))

# let's get a map centered in santa monica
al1 = get_map(location = c(lon = -118.4969553, lat = 34.0098871), 
              zoom = 15, 
              maptype = 'roadmap')

# we wrap it in a ggplot object
p1 <- ggmap(al1)

# since we plan on visualizing in date/time order, let's go ahead
# and sort the whole data frame that way
parking %<>% arrange(Date.Time)

# let's make a video!
saveVideo({
    # for every unique day
    for (day in unique(parking$ymd)) {
      
      # exctract data for the day we're visualizing, and
      # go ahead and create some summaries of the Avaialble spots
      parking %>% 
        filter(ymd==day) %>% 
        group_by(Latitude, Longitude, Lot) %>% 
        summarise(mean_avail=mean(Available),
                  pct_filled=mean_avail/max(Available),
                  min_avail=min(Available),
                  max_avail=max(Available),
                  variance=var(Available)) -> mydf
      
      # make a plot with a number of geom_points at different
      # levels of transparency, so that it looks cool :)
      p2 <- p1 + geom_point(aes(x=Longitude, y=Latitude, size=mean_avail, color=variance), 
                            data=mydf, alpha=.5) +
        geom_point(aes(x=Longitude, y=Latitude, size=min_avail, color=variance), 
                   data=mydf, alpha=.25) +
        geom_point(aes(x=Longitude, y=Latitude, size=max_avail, color=variance), 
                   data=mydf, alpha=.1) +
        geom_text(aes(x=Longitude, y=Latitude, label=Lot), data=mydf) +
        scale_color_gradient(high="blue", low="green") +
        scale_size(range = c(2, 50)) +
        ggtitle(paste0("Average Parking Available on ", 
          strftime(as.POSIXct(day, origin="1970-01-01"), format= "%A, %B %d"))) +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 1))
      
      # printing the plot here is what allows the animation package
      # to grab the frame and store it for later
      print(p2)
      
    }
  }, "/tmp/animation.mp4", 
  interval = 0.2,
  ani.width = 1920, 
  ani.height = 1080
)

