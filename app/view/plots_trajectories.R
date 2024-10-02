box::use(
  shiny[moduleServer, NS, observeEvent, plotOutput, renderPlot, tagList, uiOutput],
)



####### UI

ui <- function(id) {
  ns <- NS(id)
  tagList(
    semantic.dashboard::box(
      title = "Production Trajectories",
      width = 16,
      collapsible = FALSE,
      plotOutput(ns("trisk_line_plot_output"), height = "100%")
    )
  )
}


####### Server


server <- function(id, trajectories_data_r) {
  moduleServer(id, function(input, output, session) {
    observeEvent(trajectories_data_r(), ignoreInit = TRUE, {
      # Render plot
      trisk_line_plot <- trisk.analysis::plot_multi_trajectories(
        trajectories_data = trajectories_data_r()
      )

      output$trisk_line_plot_output <- renderPlot(
        {
          trisk_line_plot
        },
        height = function() {
          # Dynamically calculate plot height
          num_facets <- length(unique(trajectories_data_r()[["technology"]]))
          base_height_per_facet <- 200 # TODO GO IN CONF
          total_plot_height <- num_facets * base_height_per_facet
          total_plot_height
        }
      )
    })
  })
}
