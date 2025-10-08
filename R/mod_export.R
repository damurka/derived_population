#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_export_ui <- function(id, plot = TRUE, data = TRUE, class = 'download-row') {
  ns <- NS(id)
  tagList(
    div(
      class = class,
      if (plot) downloadButton(ns('plot'), 'Download Plot'),
      if (data) downloadButton(ns('data'), 'Download Data')
    )
  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id, prefix, plot = NULL, data = NULL){
  moduleServer(
    id = id,
    module = function(input, output, session){
      ns <- session$ns
      stamp <- reactive(format(Sys.time(), '%Y%m%d_%H%M%S'))

      if (!is.null(plot)) {
        output$plot <- downloadHandler(
          filename = function() paste0(prefix(), '_', stamp(), '.png'),
          content = function(file) {
            g <- plot()
            ggsave(file, plot = g, dpi = 300)
          }
        )
      }

      if (!is.null(data)) {
        output$data <- downloadHandler(
          filename = function() paste0(prefix(), '_', stamp(), '.csv'),
          content = function(file) write_csv(data(), file, na = '')
        )
      }
    }
  )
}
