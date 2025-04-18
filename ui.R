ui <- tagList(
  tags$head(
    tags$script(src = "js/main.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    shinyjs::useShinyjs(),
    waiter::use_waiter()
  ),
  page_navbar(
    id = "tabs",
    title = app_title,
    collapsible = TRUE,
    fillable_mobile = FALSE,
    inverse = FALSE,
    underline = FALSE,
    position = "fixed-top",
    bg = "#ffffff",
    theme = bs_theme(
      font_scale = 0.8,
      bootswatch = "minty",
      primary = "#4682B4",
      "navbar-bg" = "#f8f9fa"
    ),
    
    mod_travel_analysis_ui("travels"),
    nav_panel(
      "About",
      icon = bsicons::bs_icon("info-circle"),
      div(
        class = "container bigger-text",
        layout_columns(
          card(
            id = "about-card",
            includeMarkdown(here::here("Methodology.md"))
          )
        )
      )
    ),
    
    # nav images and links
    nav_spacer(),
    nav_item(
      tags$a(
        "Developed by Epi-DS",
        href = "https://epicentre-msf.github.io/gallery/",
        target = "_blank",
        class = "text-muted"
      )
    ),
    # nav_item(
    #   tags$a(
    #     shiny::icon("github"),
    #     "Report an issue",
    #     href = "https://github.com/epicentre-msf/carbon-travel-app/issues",
    #     target = "_blank"
    #   )
    # ),
    nav_item(
      tags$a(
        tags$img(
          src = "epicentre_logo.png",
          alt = "Epicentre Logo",
          height = "35px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "Epicentre",
        href = "https://epicentre.msf.org/",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        tags$img(
          src = "climate_msf_logo.png",
          alt = "Climate Smart MSF Logo",
          height = "35px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "Climate Smart MSF",
        href = "https://msfintl.sharepoint.com/sites/ClimateSmartMSF/?OR=Teams-HL&CT=1716448053305",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        tags$img(
          src = "msf_logo.png",
          alt = "MSF Logo",
          height = "35px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "MSF",
        href = "https://msf.org/",
        target = "_blank"
      )
    )
  ),
  waiter::waiter_preloader(
    html = tags$div(
      class = "text-center",
      tags$img(
        src = "epicentre_logo.png",
        width = "100%",
        class = "mx-auto d-block p-2 pb-5",
        style = "max-width: 500px;"
      ),
      tags$img(
        src = "climate_msf_logo.png",
        width = "100%",
        class = "mx-auto d-block p-2 pb-5",
        style = "max-width: 200px;"
      ),
      waiter::spin_3()
    ),
    color = "#ffffff"
  )
)
