library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(magrittr)
library(lubridate)
library(scales)


# To do -------------------------------------------------------------------

# make line graph of wind speed over time to complement my bearing graph
# make sure to highlight time of bearing graph on to time chart
# can i control bearing graph by clicking on line graph?
# great resource: https://gallery.shinyapps.io/093-plot-interaction-basic/ 




# Windspeed Graph ---------------------------------------------------------
windspeed_graph <- function(hourly_data){
  g <- ggplot(data = hourly_data, aes(x = time, y = windSpeed)) + 
    geom_line() +
    theme(axis.text.x = element_text(angle = 75, hjust = 1),
          axis.title.x = element_blank(), 
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = 'black', size = .75, linetype = 1),
          panel.grid.major.x = element_blank(),
          legend.title = element_text(hjust = .5),
          legend.title.align = .5,
          plot.title = element_text(hjust = .5, size = 22))
  return(g)         
}

#windspeed_graph(weather$hourly)


# Wind bearing Plot ---------------------------------------------------------
windbearing_graph <- function(hourly_data, select_time) {
  # make the graph for the wind speed at a certain time
  lookup <- data.table(bearing = seq(0, 315, 45),
                       direction = c('North', 'Northeast', 'East', 'Southeast',
                                     'South', 'Southwest', 'West', 'Northwest'))
  
  # get direction of wind
  wind_from_the <- lookup[which(abs(lookup$bearing - hourly_data[time == select_time]$windBearing)  == min(abs(lookup$bearing - hourly_data[time == select_time]$windBearing)))]$direction
  # get descriptor of wind
  # wind_speed_description <- paste0('The windspeed is ',
  #                                  round(hourly_data[time == select_time]$windSpeed),
  #                                  ' mph from the ',
  #                                  wind_from_the)
  wind_speed_description <- paste0('At ', format(select_time, "%l %p"), " on ", wday(select_time, label = TRUE),
                                   ' the windspeed will be ', 
                                   round(hourly_data[time == select_time]$windSpeed),
                                   ' mph  from the ', 
                                   wind_from_the)
  
  #make plot
  g <- ggplot(data = hourly_data[time == select_time], aes(x = windBearing, y = windSpeed)) +
    geom_bar(width = 15, stat = 'identity') +
    scale_x_continuous(limits = c(0, 359), breaks = seq(0, 315, 45), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(hourly_data$windSpeed)), expand = c(0, 0)) +
    ggtitle(wind_speed_description) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(face = 'bold',  margin = margin(t =1), debug = FALSE),
          axis.title.x = element_blank(),
          panel.grid.major = element_line(linetype = 3, color = "grey75"),
          panel.grid.minor = element_line(linetype = 3, colour = "grey25"),
          panel.background = element_blank()) +
    coord_polar(theta = 'x')
  
  return(g)
}


# Week Ahead Plot ---------------------------------------------------------

weekly_graph <-  function(zip_daily_weather){
  if(!is.data.table(zip_daily_weather))
  {zip_daily_weather <- as.data.table(zip_daily_weather)}
  
  max_temp <- max(zip_daily_weather$temperatureMax)
  min_temp <- min(zip_daily_weather$temperatureMin)  
  
  zip_daily_weather[, `:=`(month = month(time), day = day(time))]

  
  zip_daily_weather$day_labels <- zip_daily_weather$time[2:8] %>% 
    weekdays %>% 
    #substr(1,3) %>% 
    c('Today', .)
  zip_daily_weather[, day_labels:=factor(day_labels, levels= day_labels)]
  
  
  ggplot(data = zip_daily_weather) + 
    geom_segment(aes(x = day_labels, 
                     xend = day_labels, 
                     y = temperatureMin, 
                     yend = temperatureMax), 
                 size = 5) +
    geom_text(aes(x = day_labels, 
                  y = temperatureMin, 
                  label = paste0(round(temperatureMin,0), '*')), # these broke suddenly°'
              parse = FALSE, hjust = .5, vjust = 1) +   
    geom_text(aes(x = day_labels, 
                  y = temperatureMax, 
                  label = paste0(round(temperatureMax,0), '*')),  # these broke suddenly°'
              parse = FALSE, hjust = .5, vjust = -.6) + 
    # annotation_custom(img2, xmin=1.5, xmax=2.5, ymin=55, ymax=60) +
    scale_y_continuous(limits = c(min_temp, max_temp)) +
    ggtitle('Highs and Lows of the Week Ahead') +
    ylab('Temperature') +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = .5, size = 22))
}



# Hourly Temperature and Precip Probability -------------------------------

hourly_graph <- function(zip_hourly_weather){
  if(!is.data.table(zip_hourly_weather)){setDT(zip_hourly_weather)}
  temp_max <- max(zip_hourly_weather$temperature)
  temp_min <- min(zip_hourly_weather$temperature)
  
  # need to separate out lengths for dates
  
  first_day_begin <- zip_hourly_weather[1,]$time
  first_day_end <- max(zip_hourly_weather[day(time) == day(zip_hourly_weather[1,]$time)]$time)
  first_day_mid <- first_day_begin + floor((first_day_end-first_day_begin)/2) 
  
  second_day_begin <- min(zip_hourly_weather[day(time) == day(zip_hourly_weather[1,]$time) + 1]$time)
  second_day_end <- max(zip_hourly_weather[day(time) == day(zip_hourly_weather[1,]$time) + 1]$time)
  second_day_mid <- second_day_begin + floor((second_day_end-second_day_begin)/2) 
  
  third_day_end <- max(zip_hourly_weather[day(time) == day(zip_hourly_weather[1,]$time) + 2]$time)
  third_day_begin <- min(zip_hourly_weather[day(time) == day(zip_hourly_weather[1,]$time) + 2]$time)
  third_day_mid <- third_day_begin + floor((third_day_end-third_day_begin)/2) 
  
  plot1 <- ggplot(data = zip_hourly_weather, 
                  aes(x = time, y = temperature,  fill = precipProbability)) +
    geom_bar(stat = 'identity') +
    scale_x_datetime(date_labels = "%l %p", 
                     breaks = zip_hourly_weather$time,
                     expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(.93*temp_min, 1.05*temp_max)) +
    annotate(geom= 'text', 
             label = wday(c(first_day_mid, second_day_mid, third_day_mid), label = TRUE),
             fontface =2, 
             x = c(first_day_mid, second_day_mid, third_day_mid),
             y = .93 * temp_min + .90) +
    annotate(geom='segment',
             x =c(first_day_begin, second_day_begin, third_day_begin),
             xend = c(first_day_end, second_day_end, third_day_end),
             y = .93*temp_min, 
             yend = .93*temp_min) +
    scale_fill_gradientn(colors= c("lightgoldenrod1", "lightblue","darkblue"), 
                         limits = c(0,1),
                         breaks = seq(.25, .75, .25),
                         labels = paste0(seq(.25, .75, .25) * 100, "%"),
                         name = 'Probabiliy\nof\nPrecipitation')  +
    ggtitle('The Next 48 Hours') +
    ylab('Temperature')+
    theme(axis.text.x = element_text(angle = 75, hjust = 1),
          axis.title.x = element_blank(), 
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = 'black', size = .75, linetype = 1),
          panel.grid.major.x = element_blank(),
          legend.title = element_text(hjust = .5),
          legend.title.align = .5,
          plot.title = element_text(hjust = .5, size = 22))
  plot1
  
}
