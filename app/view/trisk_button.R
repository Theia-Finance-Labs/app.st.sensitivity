# Load required packages
box::use(
  semantic.dashboard[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar],
  shiny[div, h1, moduleServer, NS, observe, observeEvent, reactive, reactiveVal, tags],
  shiny.semantic[semanticPage],
  shinyjs[useShinyjs],
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$button(
      id = ns("run_trisk"),
      class = "ui fluid button ", # Added custom class for styling
      "Run Trisk"
    )
  )
}

####### Server

server <- function(
    id,
    assets_data,
    scenarios_data,
    financial_data,
    carbon_data,
    trisk_run_params_r, 
    focus_country_r) {
  moduleServer(id, function(input, output, session) {

    
    params_df_r <- reactiveVal(NULL)
companies_trajectories_r<- reactiveVal(NULL)
    # fetch or compute trisk on button click
    shiny::observeEvent(input$run_trisk, ignoreNULL = T, {
      trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())
      selected_country <- focus_country_r()

      st_results <- trisk.analysis::run_trisk_sa(
            assets_data = assets_data,
            scenarios_data = scenarios_data,
            financial_data = financial_data,
            carbon_data = carbon_data, 
            run_params=trisk_run_params ,
            country_iso2=selected_country)
      
browser()
      
    })

    return(
      list(
            "params_df_r"=params_df_r,
"companies_trajectories_r"=companies_trajectories_r
      )
    )
  })
}
