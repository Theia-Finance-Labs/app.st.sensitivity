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
    updateSelectizeInput
  ],
  shiny.semantic[button, segment, semanticPage],
  shinyjs[runjs, useShinyjs],
)

ui <- function(id, portfolio_class = "") {
  ns <- NS(id)
  semantic.dashboard:::box(
    title = "Trisk Runs", width = 16, collapsible = TRUE,
    div(
      DT::dataTableOutput(outputId = ns("portfolio_table")),
      shiny.semantic::button(ns("delete_selected"), "Delete Selected", class = "red")
    )
  )
}
server <- function(
    id,
    params_df_r,
    companies_trajectories_r) {
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

    # Return the updated values
    return(list(
      "displayed_params_df_r" = displayed_params_df_r,
      "companies_trajectories_r" = companies_trajectories_r
    ))
  })
}
