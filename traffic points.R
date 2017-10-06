require(ggmap)
require(stringr)
require(dplyr)
require(DBI)
require(purrr)
require(RMySQL)
require(magick)

#Minimal statement for joining the observation and station tables

obs.stat.join <- 'where A.StationID = B.StationID and A.DirectionOfTravel = B.DirectionOfTravel and A.LaneOfTravel = B.LaneOfTravel'
mydb <- dbConnect(MySQL(), user = 'root', password = pwd)

labels <- c('HourOne', 'HourTwo', 'HourThree', 'HourFour', 'HourFive', 'HourSix', 'HourSeven', 'HourEight', 'HourNine', 'HourTen', 'HourEleven', 'HourTwelve', 'HourThirteen', 'HourFourteen', 'HourFifteen', 'HourSixteen', 'HourSeventeen', 'HourEighteen', 'HourNineteen', 'HourTwenty', 'HourTwentyOne', 'HourTwentyTwo', 'HourTwentyThree', 'HourTwentyFour')
names(labels) <- seq(0000,2300,100)
names(labels) <- str_pad(names(labels), width = 4, pad = '0', side = 'left')

#possible args:
#stationid, date, time, fipsstate, fipscounty, postedsigning, postedsignnumber
#Returns a dataframe of traffic observations, filtered by arguments passed to ...
get_sql_pts <- function(conn = mydb, ...) {
  args <- list(...)
  params <- c('StationID', 'Date', 'time', 'FIPSState', 'FIPSCounty', 'PostedSigning', 'PostedSignNumber')
  stat.sel.str <- paste0('select B.Latitude, B.Longitude, B.FIPSState, B.FIPSCounty, ')# from traffic.observation A, traffic.station B ', obs.stat.join, ' and A.Date = \'', date, '\';')
  from.str <- 'from traffic.observation A, traffic.station B '
  exists <- map_lgl(params, ~!is.null(args[[.]]))
  names(exists) <- params
  obs.str <- ''
  
  if(exists['time']) {
    obs.str <- paste0(' A.StationID, A.', labels[args$time], ', A.Date ')
  } else {
    obs.str <- ' A.* '
  }
  
  short.args <- exists[exists]
  short.args <- short.args[names(short.args) != 'time']
  
  if (exists['FIPSState']) {
    if (is.na(strtoi(args[['FIPSState']]))) {
      args[['FIPSState']] <- unique(fips.tbl$STATE_CODE[fips.tbl$STATE_ABBREV == args[['FIPSState']]]) %>% str_pad(width = 2, pad = '0', side = 'left')
    }
  }
  
  if (exists['FIPSCounty']) {
    if (is.na(strtoi(args[['FIPSCounty']]))) {
      args[['FIPSCounty']] <- unique(fips.tbl$COUNTY_CODE[fips.tbl$COUNTY_NAME == args[['FIPSCounty']] & fips.tbl$STATE_CODE == args[['FIPSState']]]) %>% str_pad(width = 3, pad = '0', side = 'left')
    }
  }
  
  args.str <- ' and '
  for (arg in names(short.args)) {
    args.str <- paste0(args.str, arg, ' = \'', args[[arg]], '\' and ')
  }
  
  args.str <- gsub(' and $', ';', args.str)
  
  query <- paste0(stat.sel.str, obs.str, from.str, obs.stat.join, args.str)
  
  df <- dbGetQuery(conn, query)
  
  return(df)
}

#Plot one snapshot of traffic on a map
#Defaults to HourOne (midnight - 1am)
#Will be expanded
traf_map <- function(map, data, col = 'HourOne') {
  dt <- min(unique(data$Date))
  name <- names(labels)[labels == col]
  base <- ggmap(map, extent = 'normal')
  base + 
    stat_summary_2d(data = data, aes_string(x = 'Longitude', y = 'Latitude', z = col), fun = mean, binwidth = c(.075, .075)) + 
    scale_fill_gradientn(name = 'Traffic', colors = rev(rainbow(7)), trans = 'log10', limits = c(NA, 6500)) + 
    scale_alpha(range = c(0, 0.3), guide = FALSE) + 
    coord_map(projection='mercator',
              xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
              ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat)) + 
    ggtitle(paste(dt,name))
}

#Take in a list of plots to save
write_maps <- function(src) {
  walk2(src, letters[1:length(src)], ~ggsave(filename = paste0('~/blank/map',.y,'.png'), plot = .x))
}

base_map <- function(data) {
  long <- mean(data$Longitude, na.rm = T) #We're assuming data comes straight from SQL, so this is the col name
  lat <- mean(data$Latitude, na.rm = T)
  return(get_map(location = c(lon = long, lat = lat), zoom = 7, scale = 2)) #zoom and scale will change based on loc
}

#Produces series of plots of every hour
make_day <- function(data, map) {
  hrs <- names(data)[startsWith(names(data), 'Hour')]
  day.list <- map(hrs, ~traf_map(map = map, data = data, col = .))
}

make_gif <- function(data, map) {
  img <- image_graph(600, 400, res = 96)
  plts <- make_day(data, map)
  walk(plts, plot) #add all the maps to the image device
  dev.off()
  animation <- image_animate(img, fps = '2')
}

###################   
## Functions for interacting w/MySQL
###################


fields <- dbListFields(mydb, 'traffic.observation')

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