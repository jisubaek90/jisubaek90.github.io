library(DT)
library(leaflet)
library(RColorBrewer)
library(RPostgres)
library(scales)
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(treemap)
library(shinyAce)

# remote connection to the hotSeoul database----
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'hotSeoul',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25061,
  user = 'proj7',
  password = 'ju4hqet5j79ilho3',
  sslmode = 'require'
)

# hotels table----
hot <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM hotels ORDER BY hotel_id'
)

district <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM districts ORDER BY district_id'
)

res <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM restaurants ORDER BY rest_id'
)

book <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM bookings ORDER BY booking_id'
)

menu <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM menus ORDER BY menu_id'
)

cust <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM customers ORDER BY cust_id'
)

att <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM attractions ORDER BY attr_id'
)

# distance between two coordinates - Haversine formula----
dist <- function(lat1, lng1, lat2, lng2) {
  r <- 6378.1 #radius of earth in km
  f1 <- lat1 * pi / 180
  f2 <- lat2 * pi / 180
  d1 <- (lat2 - lat1) * pi / 180
  d2 <- (lng2 - lng1) * pi / 180
  a <- sin(d1 / 2) ^ 2 + cos(f1) * cos(f2) * sin(d2 / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(round(r * c, 2))
}

# when exiting app, disconnect from the hotSeoul database
onStop(
  function()
  {
    dbDisconnect(con)
  }
)