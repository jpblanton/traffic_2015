require(ggmap)
require(stringr)
require(dplyr)
require(DBI)

map_points <- function(df, start_time, map, date) {
  
  #st <- str_pad(start_time, width = 4, side = 'left', pad = '0')
  fin <- str_pad(as.character(as.numeric(start_time)+100), width = 4, side = 'left', pad = '0')
  dur <- paste0(start_time,'-',fin,' ',date)

  #col_fill <- paste('traffic_volume_counted_after',st,'to',fin,sep='_')
  col_fill <- labels[start_time]
  
  df <- df %>% filter(Date == date)
  #print(col_fill) #debugging
# plotting the map with some points on it
  x <- ggmap(mapgilbert) +
    geom_point(data = df, aes_string(x = 'Longitude', y = 'Latitude', fill = col_fill, alpha = 0.4), size = 3, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
    scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
    ggtitle(dur)
    #geom_text(aes_string(x = -75.2, y = 35.2, label = 'avc'), size = 3, alpha = 1, color = 'black')
}

#maybe we need to keep the state with the observations

obs.stat.join <- 'where A.StationID = B.StationID and A.DirectionOfTravel = B.DirectionOfTravel and A.LaneOfTravel = B.LaneOfTravel'

#possible args:
#stationid, date, time, fipsstate, fipscounty, postedsigning, postedsignnumber
map_sql_pts <- function(map, ...) {
  args <- list(...)
  
  sql.str <- paste0('select B.Latitude, B.Longitude, B.StationID, A.', labels[start_time], ', B.Date from traffic.observation A, traffic.station B ', obs.stat.join, ' and A.Date = \'', date, '\';')
}

#mapgilbert <- get_map(location = c(lon = mean(va$Longitude, na.rm=TRUE), lat = mean(va$Latitude,na.rm=TRUE)), zoom = 7,
#                      maptype = "roadmap", scale = 2)

labels <- c('HourOne', 'HourTwo', 'HourThree', 'HourFour', 'HourFive', 'HourSix', 'HourSeven', 'HourEight', 'HourNine', 'HourTen', 'HourEleven', 'HourTwelve', 'HourThirteen', 'HourFourteen', 'HourFifteen', 'HourSixteen', 'HourSeventeen', 'HourEighteen', 'HourNineteen', 'HourTwenty', 'HourTwentyOne', 'HourTwentyTwo', 'HourTwentyThree', 'HourTwentyFour')
names(labels) <- seq(0000,2300,100)
names(labels) <- str_pad(names(labels), width = 4, pad = '0', side = 'left')

get_plots <- function(df, map) {
  times <- seq(0000,2300,100)
  dates <- paste('2015-01-',str_pad(seq('01','31'),2,'left','0'),sep='')
  
  #jan <- df %>% filter(month_of_data == '01')
  
  days <- list()
  for (i in 1:length(dates)) {
    today <- df %>% filter(date == dates[i])
    days[[i]] <- map(times, ~map_points(today, .x, map, dates[i]))
    assign(paste0('day',i),map(times, ~map_points(today, .x, map, dates[i])))
  }
  #print(day1[[1]])
  return(days)
  #return(list(day1,day2,day3,day4,day5,day6,day7,day8,day9,day10,day11,day12,day13,day14,day15,day16,day17,day18,day19,day20,day21,day22,day23,day24,day25,day26,day27,day28,day29,day30,day31))
}

#copied directly from a blank script
#needs to be fixed
print_maps <- functon() {
  #jan <- whole %>% filter(month_of_data == '01')
  
  for (i in 1:length(test)) {
    walk2(.x = test[[i]], .y = seq(test[[i]]), .f = ~ggsave(paste('plot','-',i,'-',str_pad(as.character(.y),3,side='left',pad='0'),'.png',sep=''),.x))
  }
  
  #setup for traffic stuff
  #let stat be station data, obs be the observations, in whichever form
  stat$longitude <- stat$longitude * -1 #right hemisphere
  
}

###################
## Functions for interacting w/MySQL
###################


fields <- dbListFields(mydb, 'observation')

get_statement <- function(vec) {
  values <- paste(vec, collapse = '\', \'')
  insert <- 'INSERT INTO traffic.STATION VALUES ('
  state <- paste(insert, '\'', values, '\');', sep = '')
  return(state)
}

get_obs_statement <- function(vec) {
  #columns <- paste('(', paste(fields, collapse = ', '), ')',sep = '')
  volumes <- vec[5:28] %>% paste(collapse = ', ')
  insert <- 'INSERT INTO traffic.OBSERVATION VALUES ('
  chars <- vec[1:3] %>% paste(collapse = '\',\'')
  state.ch <- paste(insert, '\'', chars, '\', \'', vec$date, '\', ', volumes, ');', sep = '')
  return(state.ch)
}

insert_station <- function() {
  for (i in 1:nrow(stations)) {
    sql <- get_statement(stations[i,])
    #print(sql)
    dbSendQuery(mydb, sql)
  }
}
#insert a pair of sys.time() calls to get how long this takes
insert_obs <- function(src) {
  for (i in 1:nrow(src)) {
    sql <- get_obs_statement(src[i,]) #error handling to log instead of breaking when something happens
    dbSendQuery(mydb, sql)
  }
}

gen_statement <- function(con = 'mydb', schema = 'traffic', table, values) {
  insert <- paste0('INSERT INTO ', schema, '.', table, ' VALUES (\'')
  val.state <- paste(values, collapse = '\', \'')
  return(paste0(insert, val.state, '\');'))
}

point_in_bbox <- function(p, bbox) {
  p.x <- p[1] #x value
  p.y <- p[2] #y value
}