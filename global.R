# Global for Carbon-travel-App
library(shiny)
library(bslib)
library(shinylogs)
library(tidyverse)
library(sf)
library(sfnetworks)
library(highcharter)
library(reactable)
library(flowmapblue)

#source(here::here("R", "set_paths.R"))
source(here::here("R", "utils_labels.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils_hc.R"))

# Set paths -------------------------------------------------------------

clean_path <- here::here("data", "clean")

# Setup -------------------------------------------------------------------
app_name <- "msf-carbon-app"
app_title <- "MSF Carbon Travel App"
sp_path <- Sys.getenv("SHINYPROXY_PUBLIC_PATH")
is_sp_env <- sp_path != ""
# mapbox api token
mbtkn <- Sys.getenv('MAPBOX_ACCESS_TOKEN')

# Import data -------------------------------------------------------------

# Get all travel data
df_travels <- read_rds(here::here(clean_path, "flights", "full_amex_wagram_cwt.rds"))
# locations for flowmap
locations <- read_rds(here::here(clean_path, "cities", "df_cities.rds")) |> 
  transmute(id = city_code, name = city_name, lat, lon)

# date range
init_year <- sort(unique(df_travels$year))
init_date_range <- format(seq.Date(min(df_travels$invoice_date), max(df_travels$invoice_date), by = "month"), "%Y-%m")
min_date <- min(init_date_range)
max_date <- max(init_date_range)

# local disk cache
# shiny::shinyOptions(cache = cachem::cache_disk(here::here(".cache")))

disconnected <- sever::sever_default(
  title = "Disconnected",
  subtitle = "Sorry your session timed-out or something went wrong",
  button = "Reconnect",
  button_class = "info"
)
