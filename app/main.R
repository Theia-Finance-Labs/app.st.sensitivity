box::use(
  semantic.dashboard[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar],
  shiny[
    div,
    img,
    moduleServer,
    NS,
    renderUI,
    tags,
    uiOutput
  ],
  shiny.semantic[semanticPage],
  shinyjs[useShinyjs],
)

box::use(
  app/logic/constant[
    AVAILABLE_VARS,
    HIDE_VARS,
    TRISK_INPUT_PATH,
    TRISK_POSTGRES_DB,
    TRISK_POSTGRES_HOST,
    TRISK_POSTGRES_PASSWORD,
    TRISK_POSTGRES_PORT,
    TRISK_POSTGRES_USER
  ],
  app/logic/data_load[download_db_tables_postgres],
  app/view/display_params,
  app/view/sidebar_parameters,
  app/view/trisk_button,
  app/view/plots_trajectories
)



# Define the UI function
#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny.semantic::semanticPage(
    shinyjs::useShinyjs(), # Initialize shinyjs
    # CONTENT PAGE
    tags$div(
      class = "header", 
    ),
    dashboardPage(
      title = "Crispy",
      # dashboardHeader
      dashboardHeader(title = "Sensitivity Analysis"),
      # dashboardSidebar
      dashboardSidebar(
        # Data Section
        tags$div(
          tags$div(
            class = "sidebar-section",
            shiny::tags$div(class = "ui header", "Analysis"),
            shiny::tags$div(class = "ui divider"),
            # Button container with vertical spacing
            tags$div(
              class = "ui stackable aligned grid", # Centered and stackable grid layout for better alignment
              tags$div(
                class = "row",
                trisk_button$ui(ns("trisk_button"))
              )
            )
          ),
          sidebar_parameters$ui(
            ns("sidebar_parameters"),
            available_vars = AVAILABLE_VARS
          ),
          shiny::img(
            src = "static/logo_life_stress.jpg",
            height = "30%", width = "auto",
            style = "
              display: block;
              margin-left: auto;
              margin-right: auto;
              margin-top: 10px;
              margin-bottom: 10px;"
          )
        ),
        size = "very wide",
        visible = TRUE
      ),

      # dashboardBody
      dashboardBody(
      shiny::tags$div(
        class = "ui stackable grid",
        display_params$ui(ns("display_params")),
        plots_trajectories$ui(ns("plots_trajectories"))
        )
      )
    )
  )
}


# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    if (!dir.exists(TRISK_INPUT_PATH)) { 
      dir.create(TRISK_INPUT_PATH) 
    }
    if (length(dir(TRISK_INPUT_PATH)) == 0) { 
      tables <- c(
        "assets_sampled",
        "scenarios",
        "ngfs_carbon_price",
        "financial_features"
      )

      download_db_tables_postgres(
        save_dir = TRISK_INPUT_PATH, 
        tables = tables,
        dbname = TRISK_POSTGRES_DB,
        host = TRISK_POSTGRES_HOST,
        port = TRISK_POSTGRES_PORT,
        user = TRISK_POSTGRES_USER,
        password = TRISK_POSTGRES_PASSWORD
      )
    }

    assets_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "assets_sampled.csv"), show_col_types = FALSE)
    scenarios_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "scenarios.csv"), show_col_types = FALSE)
    financial_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "financial_features.csv"), show_col_types = FALSE)
    carbon_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "ngfs_carbon_price.csv"), show_col_types = FALSE)

    
    perimeter <- sidebar_parameters$server(
      "sidebar_parameters",
      scenarios_data = scenarios_data,
      assets_data = assets_data,
      available_vars = AVAILABLE_VARS, 
      hide_vars = HIDE_VARS 
    )

    trisk_run_params_r <- perimeter$trisk_run_params_r
    selected_country_r <- perimeter$selected_country_r

    st_results_r <- trisk_button$server(
      "trisk_button",
      assets_data = assets_data,
      scenarios_data = scenarios_data,
      financial_data = financial_data,
      carbon_data = carbon_data,
      trisk_run_params_r = trisk_run_params_r,
      selected_country_r=selected_country_r
    )

    params_df_r <- st_results_r$params_df_r
    companies_trajectories_r <- st_results_r$companies_trajectories_r
    
    displayed_params <- display_params$server("display_params", params_df_r, companies_trajectories_r)

    displayed_params_df_r <- displayed_params$displayed_params_df_r
    displayed_trajectories_r <- displayed_params$companies_trajectories_r

    # Generate trajectories plots
    plots_trajectories$server(
      "plots_trajectories",
      trajectories_data_r = displayed_trajectories_r
    )

  })
}
