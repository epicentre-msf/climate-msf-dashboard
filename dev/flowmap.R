

# clean_path <- here::here("data", "clean")
# df_travels <- read_rds(here::here(clean_path, "flights", "full_amex_wagram_cwt.rds"))
# df_cities <- read_rds(here::here(clean_path, "cities", "df_cities.rds"))

# locations <- df_cities |> 
#   transmute(
#     id = city_code,
#     name = city_name,
#     lat,
#     lon
#   )

# flows <- df_travels |> 
#   filter(travel_type == "air") |> 
#   count(
#     origin = ori_city_code,
#     dest = dest_city_code,
#     name = "count",
#     sort = TRUE
#   )

# htmlwidgets::saveWidget(
#   flowmap,
#   here::here("data", "flowmap.html")
# )

library(shiny)
library(bslib)
library(flowmapblue)

ui <- page_sidebar(
  theme = bs_theme(version = 5L, base_font = "serif"),
  title = "Flowmap Blue",
  sidebar = sidebar(title = "My Sidebar"),
  card(
    card_header("My Card"),
    card_body(flowmapblueOutput("flowmap"))
  )
)

server <- function(input, output, session) {
  output$flowmap <- renderFlowmapblue({
    flowmapblue(
      locations = ch_locations,
      flows = ch_flows,
      mapboxAccessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'),
      clustering = TRUE,
      darkMode = TRUE,
      animation = FALSE
    )
  })
}

shinyApp(ui, server)

library(shiny)
library(bslib)

ui <- page_sidebar(
  theme = bs_theme(version = 5L, base_font = "serif"),
  title = "Flowmap Blue",
  sidebar = sidebar(title = "My Sidebar"),
  card(
    card_header("My Card"),
    card_body("Placeholder")
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)