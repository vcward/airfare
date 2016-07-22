# Based on http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
# http://www.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
# https://vanishingcodes.wordpress.com/2015/06/24/use-r-to-plot-flight-routes-on-a-fancy-world-background/

library(maps)
library(geosphere)
library(ggplot2)
library(rgeos)
library(maptools)
library(ggmap)
library(plyr)

# Set lat. - long. bounds for Hawaii
xlim <- c(-160.247086, -154.806625)
ylim <- c(18.91172, 22.23559)
hawaii.map <- map('world', col = '#f2f2f2', fill = TRUE, bg = 'white', lwd = 0.05, xlim = xlim, ylim = ylim)
hawaii <- map_data(hawaii.map)

# Airline Data
fare_summary <- read.csv("~/Documents/airfare/fare_summary.csv")

# Airport locations
airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
hi.airports <- subset(airports, state == 'HI')
hi.airports <- subset(hi.airports, iata == 'HNL' | iata == 'KOA' | iata == 'OGG' | iata == 'LIH' | iata == 'ITO')
hi.airports$iata2 <- factor(hi.airports$iata, levels=c('HNL', 'ITO', 'KOA', 'LIH', 'OGG'))
hi.airports$de.long <- hi.airports$long
hi.airports$de.lat <- hi.airports$lat

or_coord <- c('long', 'lat')
fare_summary[or_coord] <- lapply(or_coord, function(x) hi.airports[[x]][match(fare_summary$origin, hi.airports$iata2)])
dest_coord <- c('de.long', 'de.lat')
fare_summary[dest_coord] <- lapply(dest_coord, function(x) hi.airports[[x]][match(fare_summary$dest, hi.airports$iata2)])

# Function to allow ggplot to draw routes
fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}


# Destination long-lat
dest.ll <- merge(fare_summary, hi.airports, all.x=T, by.x='dest', by.y='iata2')

# Origin long-lat
origin.ll <- merge(fare_summary, hi.airports, all.x=T, by.x='origin', by.y='iata2')

routes <- gcIntermediate(fare_summary[,c('long', 'lat')], fare_summary[,c('de.long', 'de.lat')], 50, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
routes.ff <- fortify.SpatialLinesDataFrame(routes)

routes_count = data.frame('Hawaiian' = fare_summary$pc_hawaiian, 'Aloha' = fare_summary$pc_aloha, 'Go' = fare_summary$pc_go, 'Island' = fare_summary$pc_island, 'Other' = fare_summary$pc_other, 'id' = 1:nrow(fare_summary), 'Origin' = fare_summary$origin)
greatcircles = merge(routes.ff, routes_count, all.x=T, by='id')
hawaiian <- greatcircles[,1:7]
aloha <- greatcircles[,c(1:6, 8)]
go <- greatcircles[,c(1:6, 9)]
island <- greatcircles[,c(1:6, 10)]
other <- greatcircles[,c(1:6, 11)]

ggplot() + 
  geom_polygon(data=hawaii, aes(x=long, y = lat, group = group)) +
  geom_point(data=hi.airports, aes(x=long, y = lat), color = '#ffffff') +
  geom_line(data=hawaiian, aes(long,lat,group=id), color = 'purple') +
  geom_line(data=aloha, aes(long,lat,group=id), color = 'blue') +
  geom_line(data=go, aes(long,lat,group=id), color = 'green') +
  geom_line(data=island, aes(long,lat,group=id), color = 'red')

line.graph <- ggplot(data = fare_summary) +
  geom_line(aes(x = year, y = pc_hawaiian, group = dest, color = dest))
line.graph

line.graph <- ggplot(data = airline_passengers) +
  geom_line(aes(x = dest, y = passengers, group = carriers, color = carriers))
line.graph

arc.graph <- ggplot(data = airline_passengers) +
  geom_curve(aes(x = origin, y = passengers, xend = dest, yend = passengers, color = carriers, size = passengers))
arc.graph
