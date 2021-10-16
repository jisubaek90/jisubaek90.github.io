#ETL for Shiny App
#Team 7 - Seoul

library(tidyverse)
library(RPostgres)

# if remotely
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'hotSeoul',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25061,
  user = 'proj7', 
  password = 'ju4hqet5j79ilho3'
)

#_create district table----
stmt <- 'CREATE TABLE districts (
          district_id varchar(20),
          PRIMARY KEY(district_id)
        );'
dbExecute(con, stmt)

#_create customers table----
stmt <- 'CREATE TABLE customers (
          cust_id char(5),
          first_name varchar(20) NOT NULL,
          last_name varchar(50) NOT NULL,
          email varchar(50) NOT NULL,
          gender varchar(50),
          city varchar(50),
          state varchar(50),
          country varchar(50),
          lat decimal(10,8),
          lng decimal(11,8),
          dob DATE,
          PRIMARY KEY(cust_id)
        );'
dbExecute(con, stmt)

#_create hotels table----
stmt <- 'CREATE TABLE hotels (
          hotel_id char(6),
          district_id varchar(20) NOT NULL,
          hotel_name varchar(50) NOT NULL,
          address varchar(150),
          website varchar(100),
          lng decimal(11,8),
          lat decimal(10,8),
          star_rating int,
          PRIMARY KEY(hotel_id),
          FOREIGN KEY(district_id) REFERENCES districts
        );'
dbExecute(con, stmt)  

#_create bookings table----
stmt <- 'CREATE TABLE bookings (
          booking_id char(11),
          hotel_id char(6) NOT NULL,
          cust_id char(5) NOT NULL,
          booking_dt DATE NOT NULL,
          checkin_dt DATE NOT NULL,
          tot_nights int NOT NULL,
          guests int NOT NULL,
          price_pn NUMERIC(5,2) NOT NULL,
          PRIMARY KEY(booking_id),
          FOREIGN KEY(hotel_id) REFERENCES hotels,
          FOREIGN KEY(cust_id) REFERENCES customers
        );'
dbExecute(con, stmt)  

#_create restaurants table----
stmt <- 'CREATE TABLE restaurants (
          rest_id varchar(5),
          district_id varchar(20) NOT NULL,
          rest_name varchar(50) NOT NULL,
          address varchar(150),
          cuisine varchar(50),
          phone_num varchar(50),
          lat decimal(10,8),
          lng decimal(11,8),
          restaurant_features varchar(150),
          PRIMARY KEY(rest_id),
          FOREIGN KEY(district_id) REFERENCES districts
        );'
dbExecute(con, stmt)

#_create menus table----
stmt <- 'CREATE TABLE menus (
          menu_id varchar(5),
          rest_id varchar(20) NOT NULL,
          menu_name varchar(50) NOT NULL,
          price NUMERIC(10,2) NOT NULL,
          PRIMARY KEY(menu_id),
          FOREIGN KEY(rest_id) REFERENCES restaurants
        );'
dbExecute(con, stmt)

#******************----
#EXTRACT----
#_read source data----
setwd('~/Documents/APAN/SQL and Relational DB/Project') 
df <- read.csv('restaurant_data.csv', stringsAsFactors = FALSE)
df1 <- read.csv('hotel_district.csv', stringsAsFactors = FALSE)
df2 <- read.csv('HotelBookings_Seoul.csv', stringsAsFactors = FALSE)
df3 <- read.csv('HotelCusts_Seoul.csv',stringsAsFactors = FALSE )


#******************----
#TRANSFORM----
#_get seperate values----
df2 <- df2 %>% separate(coord, c('lng', 'lat'), sep = ',')
df2 <- df2 %>% separate(chkin_dt_nt, c('checkin_dt', 'tot_nights'), sep = ';')

#_get unique hotel
hotels<- df2 %>% 
  select(hotel_id, hotel_name, address, website, lat, lng, star_rating) %>% 
  distinct()

#_join hotel and district
hotels <- hotels %>% 
  inner_join(df1) %>% 
  select(hotel_id, district_id, hotel_name, address, website, lat, lng, star_rating, district_id)

#_get unique district
districts <- df1 %>% 
  select(district_id) %>% 
  distinct()

#_prepare bookings data
bookings <- df2 %>% 
  select(booking_id, hotel_id, cust_id, booking_dt, checkin_dt, tot_nights, guests, price_pn) %>% 
  distinct()

bookings$booking_dt = as.Date(bookings$booking_dt)
bookings$checkin_dt = as.Date(bookings$checkin_dt)

#_prepare restaurants data
rest <- df %>% 
  select(rest_id, district_id, rest_name, address, cuisine, phone_num, lat, lng, restaurant_features) %>% 
  distinct()

#_prepare menu data
menus <- df %>% 
  select(menu_id, rest_id, menu_name, price) %>% 
  distinct()

#******************----
#LOAD----
#_load districts table----

dbWriteTable(
  conn = con,
  name = 'districts',
  value = districts,
  row.names = FALSE,
  append = TRUE
)

# load custs table----
df3 <- df3 %>% 
  select(cust_id, first_name, last_name, email, gender, city, state, country, lat, lng, dob) %>% 
  distinct()

dbWriteTable(
  conn = con,
  name = 'customers',
  value = df3,
  row.names = FALSE,
  append = TRUE
)  

#_load hotels table----
dbWriteTable(
  conn = con,
  name = 'hotels',
  value = hotels,
  row.names = FALSE,
  append = TRUE
)

#_load bookings table----
dbWriteTable(
  conn = con,
  name = 'bookings',
  value = bookings,
  row.names = FALSE,
  append = TRUE
)

#_load restaurants table----
dbWriteTable(
  conn = con,
  name = 'restaurants',
  value = rest,
  row.names = FALSE,
  append = TRUE
)

#_load menus table----
dbWriteTable(
  conn = con,
  name = 'menus',
  value = menus,
  row.names = FALSE,
  append = TRUE
)  