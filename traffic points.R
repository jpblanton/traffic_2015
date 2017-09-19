map_points <- function(df, start_time, map, date) {

  #mapgilbert <- get_map(location = c(lon = mean(df$longitude, na.rm=TRUE), lat = mean(df$latitude,na.rm=TRUE)), zoom = 7,
  #                    maptype = "roadmap", scale = 2)
  
  st <- str_pad(as.character(start_time), width = 4, side = 'left', pad = '0')
  fin <- str_pad(as.character(start_time+100), width = 4, side = 'left', pad = '0')
  dur <- paste0(st,'-',fin,' ',date)

  col_fill <- paste('traffic_volume_counted_after',st,'to',fin,sep='_')
  #print(col_fill) debugging
# plotting the map with some points on it
  x <- ggmap(mapgilbert) +
    geom_point(data = df, aes_string(x = 'longitude', y = 'latitude', fill = col_fill, alpha = 0.4), size = 3, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
    scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
    ggtitle(dur)
    #geom_text(aes_string(x = -75.2, y = 35.2, label = 'avc'), size = 3, alpha = 1, color = 'black')
}

#mapgilbert <- get_map(location = c(lon = mean(a$longitude, na.rm=TRUE), lat = mean(a$latitude,na.rm=TRUE)), zoom = 7,
#                      maptype = "roadmap", scale = 2)

get_plots <- function(df, map) {
  times <- seq(0000,2300,100)
  dates <- paste('2015-01-',str_pad(seq('01','31'),2,'left','0'),sep='')
  
  #jan <- df %>% filter(month_of_data == '01')
  
  for (i in 1:length(dates)) {
    today <- df %>% filter(date == dates[i])
    assign(paste0('day',i),map(times, ~map_points(today, .x, map, dates[i])))
  }
  print(class(day1))
  return(list(day1,day2,day3,day4,day5,day6,day7,day8,day9,day10,day11,day12,day13,day14,day15,day16,day17,day18,day19,day20,day21,day22,day23,day24,day25,day26,day27,day28,day29,day30,day31))
}