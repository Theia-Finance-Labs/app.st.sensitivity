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
    title = "Portfolio", width = 16, collapsible = TRUE,
    DT::dataTableOutput(outputId = ns("portfolio_table")),
  )
}


server <- function(
    id,
    params_df_r) {
  moduleServer(id, function(input, output, session) {

    observeEvent(params_df_r(), ignoreInit=TRUE, {
        
        table_to_display <- params_df_r() 

        # Hardcode the index for npv_change
        colored_columns <- 7 # npv_change is in the 8th column, so index is 7 (0-based index)


        # Render the datatable
        output$portfolio_table <-     output$portfolio_table <- DT::renderDT(
      {
        DT::datatable(
            table_to_display,
            editable = FALSE, # Disable all cell editing
            selection = "multiple",
            options = list(
            lengthChange = FALSE, # Remove "Show XXX entries" option
            paging = FALSE, # Remove pagination
            searching = FALSE, # Remove search input
            info = FALSE, # Remove "Showing N of X entries"
            columnDefs = list(
                # Apply color change only to the npv_change column
                list(targets = colored_columns, createdCell = JS(
                "function(cell, cellData, rowData) {
                    $(cell).css('color', cellData < 0 ? 'red' : 'green');
                }"
                ))
            )
            ),
            class = "display compact" # Fit the table to the container
        )
      })
  })
  return(params_df_r)

  })}
