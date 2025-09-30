
pacman::p_load(
  shiny,
  shinydashboard,
  purrr,
  cd2030.core,
  ggplot2,
  rlang,
  dplyr,
  tidyr,
  readr,
  scales,
  reactable,
  shinycssloaders
)

source('derivation.R')
source('helpers.R')
source('mod_export.R')

vaccines = list(
  anc   = c('anc1'),
  idelv = c('instdeliveries','instlivebirths'),
  vacc  = c('bcg','ipv1','ipv2','measles1','measles2','opv1','opv2','opv3',
            'penta1','penta2','penta3','pcv1','pcv2','pcv3','rota1','rota2')
)

populations = c(
  'Total Births'      = 'totbirths_dhis2',
  'Live Births'       = 'totlivebirths_dhis2',
  'Under 1'         = 'totunder1_dhis2',
  'Total Population' = 'totpop_dhis2'
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
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
      inputId = 'denominator',
      label = 'Denominator',
      choices = c('DHIS 2' = 'dhis2', 'UN' = 'un'),
      selected = 'dhis2'
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
      inputId = 'base_year',
      label = 'Base Year',
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
          export_ui('populations'),
          withSpinner(plotOutput('populations'))
        ),
        tabPanel(
          'Other Population', 
          export_ui('other_pops'),
          withSpinner(plotOutput('other_pops'))
        )
      ),
      
      tabBox(
        id = 'derivations',
        width = 12,
        
        tabPanel(
          'Plot', 
          export_ui('derived_plot', data = FALSE),
          withSpinner(plotOutput('derived_plot'))
        ),
        tabPanel(
          'Table', 
          export_ui('derived_table', plot = FALSE),
          withSpinner(reactableOutput('derived_table'))
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  cache <- eventReactive(input$hfd_file, {
    req(input$hfd_file)
    
    
    file_path <- input$hfd_file$datapath
    file_name <- input$hfd_file$name
    file_type <- tools::file_ext(file_name)
    
    valid_types <- c('rds')
    if (!file_type %in% valid_types) {
      # messageBox$update_message('error_upload_failed_unsupported_format', 'error')
      return(NULL)
    }
    
    tryCatch({
      cache_instance <- load_cache_data(file_path, 'vaccine')$reactive()
      
      print('Adjusting')
      # cache_instance$adjust_data()
      data <- cache_instance()$data_with_excluded_years %>%
        adjust_service_data(adjustment = 'custom', k_factors = cache_instance()$k_factors)
      cache_instance()$set_adjusted_data(data)
      print('Adjusted')
      
      # cache_instance <- cache_instance
      
      # messageBox$update_message('msg_upload_success', 'success', list(file_name = file_name))
      
      cache_instance()
    },
    error = function(e) {
      # clean_message <- clean_error_message(e)
      # messageBox$update_message('error_upload_failed', 'error', list(clean_message = clean_message))
      NULL
    })
  })
  
  indicator_coverage <- reactive({
    req(cache())
    cache()$calculate_indicator_coverage('national') %>% 
      mutate(
        # totbirths_un = totbirths_un * 1000,
        un_births = un_births * 1000,
        un_population = un_population * 1000,
        totbirths_dhis2 = totbirths_dhis2 * 1000,
        totlivebirths_dhis2 = totlivebirths_dhis2 * 1000,
        totpop_dhis2 = totpop_dhis2 * 1000
      )
  })
  
  derived <- reactive({
    req(indicator_coverage(), input$indicator, input$population, input$base_year)
    derive_coverage(indicator_coverage(), input$indicator, input$population, input$base_year)
  })
  
  observe({
    req(cache())
    if (!is.null(cache()$survey_year) && is.numeric(cache()$survey_year)) {
      updateNumericInput(session, 'base_year', value = cache()$survey_year)
    }
  })
  
  observe({
    req(input$denominator)
    pops <- switch(
      input$denominator,
      dhis2 = populations,
      un = c('Total Births' = 'un_births', 'Total Population' = 'un_population')
    )
    updateSelectizeInput(session, 'population', choices = pops)
  })
  
  derived_plot <- reactive({
    req(derived(), input$indicator)
    
    coverage <- paste0("cov_", input$indicator, "_penta1")
    coverage_derived <- paste0(coverage, 'derived')
    cols <- c(coverage, coverage_derived)
    
    legend_labels <- setNames(
      c(input$indicator, paste0(input$indicator, ' Derived')),
      cols
    )
    
    derived() %>% 
      to_long(value_cols = cols, name = 'indicator', value = 'value') %>% 
      line_plot(
        title_txt = 'Derived Coverage based on penta 1',
        y_lab = 'Coverage',
        y_scale = scale_y_pct,
        legend_breaks = cols,
        legend_labels = legend_labels
      )
  })
  
  output$derived_plot <- renderPlot({
    req(derived_plot())
    derived_plot()
  })
  
  output$derived_table <- renderReactable({
    req(derived())
    reactable_for_derived(derived())
  })
  
  other_pops_plot <- reactive({
    req(indicator_coverage())
    
    value_cols <- c('un_births','totbirths_dhis2','totlivebirths_dhis2','totunder1_dhis2')
    
    indicator_coverage() %>% 
      to_long(value_cols) %>% 
      line_plot(
        title_txt = 'Selected Population Trends by Year and Source',
        y_lab = 'Population Count',
        y_scale = scale_y_pop,
        legend_breaks = value_cols,
        legend_labels = c(
          'un_births'           = 'Total Births UN',
          'totbirths_dhis2'     = 'Total Births DHIS2',
          'totlivebirths_dhis2' = 'Live Births DHIS2',
          'totunder1_dhis2'     = 'Under 1 DHIS2'
        )
      )
  })
  
  output$other_pops <- renderPlot({
    req(indicator_coverage())
    
    value_cols <- c('un_births','totbirths_dhis2','totlivebirths_dhis2','totunder1_dhis2')
    
    indicator_coverage() %>% 
      to_long(value_cols) %>% 
      line_plot(
        title_txt = 'Selected Population Trends by Year and Source',
        y_lab = 'Population Count',
        y_scale = scale_y_pop,
        legend_breaks = value_cols,
        legend_labels = c(
          'un_births'           = 'Total Births UN',
          'totbirths_dhis2'     = 'Total Births DHIS2',
          'totlivebirths_dhis2' = 'Live Births DHIS2',
          'totunder1_dhis2'     = 'Under 1 DHIS2'
        )
      )
  })
  
  populations_plot <- reactive({
    req(indicator_coverage())
    
    value_cols <- c('un_population','totpop_dhis2')
    
    indicator_coverage() %>% 
      to_long(value_cols) %>% 
      line_plot(
        title_txt = 'Total Population Trends by Year and Source',
        y_lab = 'Population Count',
        y_scale = scale_y_pop,
        legend_breaks = value_cols,
        legend_labels = c(
          'un_population' = 'Total Population UN',
          'totpop_dhis2'  = 'Total Population DHIS2'
        )
      )
  })
  
  output$populations <- renderPlot({
    req(populations_plot())
    populations_plot()
  })
  
  export_server(
    id     = 'derived_plot',
    prefix = reactive(sprintf("derived_plot_%s_%s", input$indicator, input$population)),
    plot   = derived_plot
  )
  
  export_server(
    id     = 'derived_table',
    prefix = reactive(sprintf("derived_table_%s_%s", input$indicator, input$population)),
    data   = reactive(derived())
  )
  
  export_server(
    id     = 'populations',
    prefix = reactive('populations_plot'),
    plot   = populations_plot,
    data   = indicator_coverage
  )

  export_server(
    id     = 'other_pops',
    prefix = reactive('other_populations_plot'),
    plot   = other_pops_plot,
    data   = indicator_coverage
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
