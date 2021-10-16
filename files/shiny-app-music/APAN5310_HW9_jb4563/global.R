# the necessary packages----
library(DT)
library(RPostgres)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# connect to the music database----
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'music5310',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25061,
  user = 'apan5310c', 
  password = 'q0nviea5woxynztw'
)

# when exiting app, disconnect from the music database----
onStop(
  function()
  {
    dbDisconnect(con)
  }
)