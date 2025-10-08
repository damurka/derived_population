#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  cache <- eventReactive(input$hfd_file, {
    req(input$hfd_file)


    file_path <- input$hfd_file$datapath
    file_name <- input$hfd_file$name
    file_type <- tools::file_ext(file_name)

    valid_types <- c('rds')
    if (!file_type %in% valid_types) {
      return(NULL)
    }

    tryCatch({
      cache_instance <- load_cache_data(file_path, 'vaccine')$reactive()

      print('Adjusting')
      data <- cache_instance()$data_with_excluded_years %>%
        adjust_service_data(adjustment = 'custom', k_factors = cache_instance()$k_factors)
      cache_instance()$set_adjusted_data(data)
      print('Adjusted')

      cache_instance()
    },
    error = function(e) {
      NULL
    })
  })

  national_coverage <- reactive({
    req(cache())

    cache()$calculate_indicator_coverage('national') %>%
      mutate(
        totbirths_dhis2 = totbirths_dhis2 * 1000,
        totlivebirths_dhis2 = totlivebirths_dhis2 * 1000,
        totpop_dhis2 = totpop_dhis2 * 1000,
        un_population = un_population * 1000,
        un_births = un_births * 1000
      )
  })

  regional_coverage <- reactive({
    req(cache(), input$region)

    cache()$calculate_indicator_coverage('adminlevel_1') %>%
      mutate(
        totbirths_dhis2 = totbirths_dhis2 * 1000,
        totlivebirths_dhis2 = totlivebirths_dhis2 * 1000,
        totpop_dhis2 = totpop_dhis2 * 1000
      ) %>%
      filter(adminlevel_1 == input$region)
  })

  indicator_coverage <- reactive({
    req(input$admin_level)
    switch (
      input$admin_level,
      national = national_coverage(),
      adminlevel_1 = regional_coverage()
    )
  })

  derived <- reactive({
    req(national_coverage(), input$indicator, input$population, input$survey_year, input$admin_level)
    derive_coverage(national_coverage(), regional_coverage(), input$indicator, input$population, input$survey_year, input$admin_level)
  })

  observe({
    req(cache())
    if (!is.null(cache()$survey_year) && is.numeric(cache()$survey_year)) {
      updateNumericInput(session, 'survey_year', value = cache()$survey_year)
    }
  })

  output$region_ui <- renderUI({
    req(cache(), cache()$adjusted_data, input$admin_level)
    if (input$admin_level == 'adminlevel_1') {

      regions <- cache()$adjusted_data %>%
        distinct(!!sym(input$admin_level)) %>%
        pull(!!sym(input$admin_level))

      selectizeInput(
        inputId = 'region',
        label = 'Region',
        choices = regions
      )
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

  observe({
    req(input$admin_level)

    denominators <- if (input$admin_level == 'national') {
      c('DHIS 2' = 'dhis2', 'UN' = 'un')
    } else {
      c('DHIS 2' = 'dhis2')
    }
    updateSelectizeInput(session, 'denominator', choices =denominators)
  })

  derived_plot <- reactive({
    print(glimpse(derived()))
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
    derived_plot()
  })

  output$derived_table <- renderReactable({
    reactable_for_derived(derived())
  })

  other_pops_plot <- reactive({
    req(indicator_coverage())

    value_cols <- c('un_births','totbirths_dhis2','totlivebirths_dhis2')

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
          'totlivebirths_dhis2' = 'Live Births DHIS2'
        )
      )
  })

  under_1_plot <- reactive({
    req(indicator_coverage())

    value_cols <- c('totunder1_dhis2')

    indicator_coverage() %>%
      to_long(value_cols) %>%
      line_plot(
        title_txt = 'Selected Population Trends by Year and Source',
        y_lab = 'Population Count',
        y_scale = scale_y_pop,
        legend_breaks = value_cols,
        legend_labels = c(
          'totunder1_dhis2'     = 'Under 1 DHIS2'
        )
      )
  })

  output$other_pops <- renderPlot({
    req(indicator_coverage())

    value_cols <- c('un_births','totbirths_dhis2','totlivebirths_dhis2')

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
          'totlivebirths_dhis2' = 'Live Births DHIS2'
        )
      )
  })

  output$under_1 <- renderPlot({
    req(indicator_coverage())

    value_cols <- c('totunder1_dhis2')

    indicator_coverage() %>%
      to_long(value_cols) %>%
      line_plot(
        title_txt = 'Selected Population Trends by Year and Source',
        y_lab = 'Population Count',
        y_scale = scale_y_pop,
        legend_breaks = value_cols,
        legend_labels = c(
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

  mod_export_server(
    id     = 'derived_plot',
    prefix = reactive(sprintf("derived_plot_%s_%s", input$indicator, input$population)),
    plot   = derived_plot
  )

  mod_export_server(
    id     = 'derived_table',
    prefix = reactive(sprintf("derived_table_%s_%s", input$indicator, input$population)),
    data   = reactive(derived())
  )

  mod_export_server(
    id     = 'populations',
    prefix = reactive('populations_plot'),
    plot   = populations_plot,
    data   = indicator_coverage
  )

  mod_export_server(
    id     = 'other_pops',
    prefix = reactive('other_populations_plot'),
    plot   = other_pops_plot,
    data   = indicator_coverage
  )

  mod_export_server(
    id     = 'under_1',
    prefix = reactive('under_1_plot'),
    plot   = under_1_plot,
    data   = indicator_coverage
  )
}
