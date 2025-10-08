vaccines <-  list(
  anc   = c('anc1'),
  idelv = c('instdeliveries','instlivebirths'),
  vacc  = c('bcg','ipv1','ipv2','measles1','measles2','opv1','opv2','opv3',
            'penta1','penta2','penta3','pcv1','pcv2','pcv3','rota1','rota2')
)

populations <-  c(
  'Total Births'      = 'totbirths_dhis2',
  'Live Births'       = 'totlivebirths_dhis2',
  'Under 1'         = 'totunder1_dhis2',
  'Total Population' = 'totpop_dhis2'
)


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      skin = 'green',
      title = 'Population Derived',
      header = dashboardHeader(title = 'Population Derived'),
      sidebar = dashboardSidebar(
        fileInput(
          inputId = 'hfd_file',
          label = 'HFD file',
          accept = '.rds'
        ),

        selectizeInput(
          inputId = 'admin_level',
          label = 'Admin Level',
          choices = c('National' = 'national', 'Admin Level 1' = 'adminlevel_1'),
          selected = 'national'
        ),

        uiOutput(outputId = 'region_ui'),

        selectizeInput(
          inputId = 'denominator',
          label = 'Denominator',
          choices = NULL
          # choices = c('DHIS 2' = 'dhis2', 'UN' = 'un'),
          # selected = 'dhis2'
        ),

        selectizeInput(
          inputId = 'population',
          label = 'Population',
          choices = NULL
        ),

        selectizeInput(
          inputId = 'indicator',
          label = 'Indicator',
          choices = list_c(vaccines),
          selected = 'penta1'
        ),

        numericInput(
          inputId = 'survey_year',
          label = 'Survey Year',
          min = 2010,
          max = 2025,
          value = 2020,
          step = 1
        )
      ),
      body = dashboardBody(

        fluidRow(
          tabBox(
            id = 'Populations',
            title = 'Population Trends',

            width = 12,
            tabPanel(
              'Population',
              mod_export_ui('populations'),
              withSpinner(plotOutput('populations'))
            ),
            tabPanel(
              'Births',
              mod_export_ui('other_pops'),
              withSpinner(plotOutput('other_pops'))
            ),
            tabPanel(
              'Under 1',
              mod_export_ui('under_1'),
              withSpinner(plotOutput('under_1'))
            )
          ),

          tabBox(
            id = 'derivations',
            width = 12,

            tabPanel(
              'Plot',
              mod_export_ui('derived_plot', data = FALSE),
              withSpinner(plotOutput('derived_plot'))
            ),
            tabPanel(
              'Table',
              mod_export_ui('derived_table', plot = FALSE),
              withSpinner(reactableOutput('derived_table'))
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "derived.population"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
