library(shiny)
library(magrittr)
devtools::load_all()
years <- paste0(1985:2021)

dataset2021 <- bmat::main_data %>%
  dplyr::left_join(bmat::geographic_info, by = c("iso_alpha_3_code"))

dataset2019 <- readRDS(here::here("test", "datall_M60.rds")) %>%
  dplyr::rename(year_start = start,
         year_end = end,
         year_mid = year,
         iso_alpha_3_code = iso,
         include = modelinclude,
         include_reason = modelinclude_reason,
         rho_bmat = rhovrfinal_bmat,
         usability_percentage = usa) %>%
  dplyr::left_join(bmat::geographic_info, by = c("iso_alpha_3_code"))

#233 and 66 for 2010 onwards
#251 and 78 for 2010 onwards
ui <- fluidPage(
  headerPanel('Data filter application for processed data'),
  sidebarPanel(
    selectInput(inputId = "dataset",
                label = "Dataset selection",
                choices = c("2021 round of estimation - dataset", "2019 round of estimation - dataset")),
    sliderInput(inputId = "yearslide", 
                label = "Year range (where year is mid-year of observation coverage rounded down)", 
                value = c(1985,2021), min = 1985, max = 2021, sep = ""),
    # selectInput(inputId = "yearfirst",
    #           label = "Start year of range (where year is mid-year of observation coverage)",
    #           choices = years),
    # selectInput(inputId = "yearlast",
    #             label = "End year of range (where year is mid-year of observation coverage)",
    #             choices = years,
    #             selected = "2021"),
    selectInput(inputId = "operator",
                label = "Source filter operator",
                choices = c("==", "!=")),
    selectInput(inputId = "type",
                label = "Source",
                choices = c("--", dataset2021$type %>% unique)),
    selectInput(inputId = "whoregion",
                label = "WHO region",
                choices = c("--", dataset2021$region_who %>% unique)),
    selectInput(inputId = "include",
                label = "Included in model",
                choices = c("--",TRUE, FALSE))
    ),
  mainPanel(
    tableOutput("nobs")
  )
)

server <- function(input, output) {
  output$nobs <-renderTable({
    if(input$dataset == "2019 round of estimation - dataset") {
      dataset <- dataset2019
    }
    if(input$dataset == "2021 round of estimation - dataset") {
      dataset <- dataset2021
    }
    x <- dataset %>%
      dplyr::filter(floor(year_mid) >= input$yearslide[1]) %>%
      dplyr::filter(floor(year_mid) <= input$yearslide[2])
    
    #### the filters with variable operators ####
    if(input$type != "--") {
      typ_expression <- paste("type", input$operator, "input$type")
      x <- x %>%
        dplyr::filter(!!rlang::parse_expr(typ_expression)) 
    }
    if(input$whoregion != "--") {
      x <- x %>%
        dplyr::filter(region_who == input$whoregion)
    }
    if(input$include != "--") {
      x <- x %>%
        dplyr::filter(include == input$include)
    }
    x <- x %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::pull(n)
  },
  caption = "Number of observations within specified criteria",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)
  )
}

shinyApp(ui = ui, server = server)
