box::use(
  DT[datatable, dataTableProxy, DTOutput, JS, renderDT],
  semantic.dashboard[box, icon],
  shiny[
    div,
    eventReactive,
    HTML,
    moduleServer,
    NS,
    observe,
    observeEvent,
    reactive,
    reactiveVal,
    reactiveValues,
    selectizeInput,
    tags,
    updateSelectizeInput,
    downloadHandler,
    downloadButton
  ],
  shiny.semantic[button, segment, semanticPage],
  shinyjs[runjs, useShinyjs],
  writexl[write_xlsx]  # Import writexl for Excel files
)

# Define UI
ui <- function(id, portfolio_class = "") {
  ns <- NS(id)
  semantic.dashboard:::box(
    title = "Trisk Runs", width = 16, collapsible = TRUE,
    div(
      DT::dataTableOutput(outputId = ns("portfolio_table")),
      shiny.semantic::button(ns("delete_selected"), "Delete Selected", class = "red"),
      div(style = "margin-top: 10px;",
        shiny::downloadButton(ns("download_btn"), "Download Excel")  # Correct outputId for download button
      )
    )
  )
}
# Define Server Logic
server <- function(id, params_df_r, companies_trajectories_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # To keep track of displayed params
    displayed_params_df_r <- reactiveVal(NULL)

    # Observe when params_df_r changes and update the table
    observeEvent(params_df_r(), ignoreInit = TRUE, {
      displayed_params_df_r(params_df_r()) # Update displayed data
    })

    # Render the datatable with selectable rows
    output$portfolio_table <- DT::renderDT({
      DT::datatable(
        displayed_params_df_r(),
        editable = FALSE,
        selection = "multiple",
        options = list(
          lengthChange = FALSE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          columnDefs = list(
            list(targets = 7, createdCell = JS(
              "function(cell, cellData) {
                $(cell).css('color', cellData < 0 ? 'red' : 'green');
              }"
            ))
          )
        ),
        class = "display compact"
      )
    })

    # Observe delete button click
    observeEvent(input$delete_selected, {
      selected_rows <- input$portfolio_table_rows_selected # Get selected row indices

      # Remove selected rows from params_df_r
      if (length(selected_rows) > 0) {
        new_params_df <- displayed_params_df_r()[-selected_rows, ] # Remove rows by index
        displayed_params_df_r(new_params_df) # Update the displayed params

        # Update the main params_df_r
        params_df_r(new_params_df)

        # Filter companies_trajectories_r based on the remaining run_id in params_df_r
        remaining_run_ids <- new_params_df$run_id
        new_trajectories_df <- companies_trajectories_r() |>
          dplyr::filter(run_id %in% remaining_run_ids)

        # Update companies_trajectories_r with filtered trajectories
        companies_trajectories_r(new_trajectories_df)
      }
    })

    # Reactive value for trajectories_df
    trajectories_df <- reactive({
      # Prepare the initial dataframe using the function
      df <- trisk.analysis:::prepare_for_trisk_line_plot(companies_trajectories_r(), 
                                                        facet_var = "technology", 
                                                        linecolor = "run_id")
      
      # Pivot the table so that run_id becomes columns and production values are spread across those columns
      df_pivoted <- df |>
        dplyr::select(.data$technology, .data$year, .data$run_id, .data$production_pct) |>  # Select relevant columns
        dplyr::mutate(production_pct=.data$production_pct/100) |>
        tidyr::pivot_wider(names_from = .data$run_id, values_from = .data$production_pct)  # Pivot the table

      return(df_pivoted)
    })


    # Download handler for Excel file with a sheet per unique technology
    output$download_btn <- shiny::downloadHandler(
      filename = function() {
        paste("trisk_data_", Sys.Date(), "_", format(Sys.time(), "%H-%M-%S"), ".xlsx", sep = "")
      },
      content = function(file) {
        # Prepare data for each unique technology
        trajectories_sheet <- trajectories_df()
        
        # Split by unique technology
        tech_sheets <- trajectories_sheet |>
          dplyr::group_split(.data$technology)

        # Create a named list for writexl
        tech_sheets_list <- purrr::map(tech_sheets, ~{
          tech_name <- unique(.x$technology)
          .x
        }) |> purrr::set_names(purrr::map_chr(tech_sheets, ~ unique(.x$technology)))
        
        # Add Params sheet
        tech_sheets_list[["Params"]] <- displayed_params_df_r()

        # Use writexl to write multiple sheets into a single Excel file
        writexl::write_xlsx(tech_sheets_list, path = file)
      }
    )

    # Return the updated values
    return(list(
      "displayed_params_df_r" = displayed_params_df_r,
      "companies_trajectories_r" = companies_trajectories_r
    ))
  })
}
