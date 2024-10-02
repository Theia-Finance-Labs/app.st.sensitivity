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
    tags$div(
      id = ns("mymodal"),
      class = "ui modal",
      tags$div(class = "header", "Processing"),
      tags$div(
        class = "content",
        tags$p("Please wait while the model is being run with the chosen parameters. This may take up to 10 minutes.")
      )
    ),
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
    selected_country_r) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values to store data
    params_df_r <- reactiveVal(NULL)
    companies_trajectories_r <- reactiveVal(NULL)

    # Fetch or compute trisk on button click
    shiny::observeEvent(input$run_trisk, ignoreNULL = TRUE, {
      trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())
      selected_country <- selected_country_r()


      # Wrap the process in a tryCatch block to handle errors
      tryCatch(
        {
          # open modal dialog
          shinyjs::runjs(
            paste0(
              "$('#", session$ns("mymodal"), "').modal({closable: true}).modal('show');"
            )
          )
          st_results <- trisk.analysis::run_trisk_sa(
            assets_data = assets_data,
            scenarios_data = scenarios_data,
            financial_data = financial_data,
            carbon_data = carbon_data,
            run_params = list(trisk_run_params),
            country_iso2 = selected_country
          )

          # Combine/append new results on top of existing params data
          new_params <- st_results$params |>
            dplyr::mutate(country_iso2 = selected_country)
          current_params <- params_df_r()

          if (is.null(current_params)) {
            params_df_r(new_params) # First time, just set it
          } else {
            params_df_r(dplyr::bind_rows(current_params, new_params)) # Append new results
          }

          # Combine/append new results on top of existing trajectories data
          new_trajectories <- st_results$trajectories
          current_trajectories <- companies_trajectories_r()

          if (is.null(current_trajectories)) {
            companies_trajectories_r(new_trajectories) # First time, just set it
          } else {
            companies_trajectories_r(dplyr::bind_rows(current_trajectories, new_trajectories)) # Append new results
          }
        },
        error = function(e) {
          # Handle the error gracefully (log, show message, etc.)
          shiny::showNotification("Trisk run failed. No data added.", type = "error")
          # message("Error in run_trisk_sa: ", e$message)
        }
      )

      # close the modal dialog
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal('hide');"
        )
      )
    })

    # Return the reactive values
    return(
      list(
        "params_df_r" = params_df_r,
        "companies_trajectories_r" = companies_trajectories_r
      )
    )
  })
}
