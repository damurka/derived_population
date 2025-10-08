#' derive_coverage
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
derive_coverage <- function(national_data, regional_data, indicator, population, survey_year, admin_level) {
  check_required(national_data)
  # check_required(regional_data)
  check_required(indicator)
  check_required(population)
  if (!is_integerish(survey_year)) {
    cli::cli_abort("x" = "{.arg survey_year} should be an integer")
  }

  admin_level <- arg_match(admin_level, c('national', 'adminlevel_1'))

  need_cols <- c('year', indicator, population)
  missing_cols <- setdiff(need_cols, names(national_data))
  if (length(missing_cols)) {
    cli::cli_abort(c("x" = "{.arg national_data} missing required columns: {.val {missing_cols}}."))
  }

  survey_year <- survey_year - 1

  indicator <- arg_match(indicator, get_all_indicators())
  indicator_col <- sym(indicator)

  population_col <- sym(population)

  coverage <- paste('cov', indicator, 'penta1', sep = '_')
  coverage_col <- sym(coverage)

  coverage_derived <- paste0(coverage, 'derived')
  coverage_derived_col <- sym(coverage_derived)

  penta1_denom <- get_population_column(indicator, 'penta1')
  penta1_denom_col <- sym(penta1_denom)

  penta1_derived_denom <- paste0(penta1_denom, 'derived')
  penta1_derived_denom_col <- sym(penta1_derived_denom)

  # Keep only necessary columns
  nat_data <- national_data %>%
    select(year, all_of(c(indicator, population, coverage, penta1_denom)))

  # Ensure survey year is not earlier than first year in data
  survey_year <- robust_max(c(survey_year, min(nat_data$year, na.rm = TRUE)), 2024)

  # Extract survey year values for national denominator and population
  survey_row <- nat_data %>% filter(year == survey_year)

  survey_population <- survey_row %>% pull(!!population_col)        # national base population
  survey_penta1_denominator <- survey_row %>% pull(!!penta1_denom_col) # national base DTP1-derived denominator

  # ---- NATIONAL-LEVEL DERIVED DENOMINATOR TRENDS ----
  national <- nat_data %>%
    arrange(year) %>%
    mutate(
      # Step 1: Calculate year on year population change
      population_change = (!!population_col - lag(!!population_col)) / lag(!!population_col) * 100,

      # Step 2: Compute percent change in DHIS2 population over time from survey year
      percent_change = (!!population_col - survey_population) / survey_population * 100,

      # Step 3: Apply percent change to national base denominator
      !!penta1_derived_denom_col := survey_penta1_denominator * (1 + percent_change/100),

      # Step 4: Calculate derived coverage population
      !!coverage_derived_col := !!indicator_col / !!penta1_derived_denom_col * 100,

      year = as.integer(year)
    )

  # ---- SUBNATIONAL: DISTRIBUTE DERIVED NATIONAL DENOMINATOR ----
  data_joined <- if (admin_level == 'adminlevel_1') {

    missing_cols <- setdiff(need_cols, names(regional_data))
    if (length(missing_cols)) {
      cli::cli_abort(c("x" = "{.arg regional_data} missing required columns: {.val {missing_cols}}."))
    }

    # Prepare national population and derived_denom for merge
    national_sel <- national %>%
      select(year, !!penta1_derived_denom_col, !!population_col) %>%
      rename(national_pop = !!population_col)

    reg_data <- regional_data %>%
      select(year, all_of(c(indicator, population, coverage, penta1_denom)))

    reg_data %>%
      left_join(national_sel, join_by(year)) %>%
      arrange(year) %>%
      mutate(
        # Step 1: Compute population change
        population_change = (!!population_col - lag(!!population_col)) / lag(!!population_col) * 100,

        # Step 2: Compute subnational DHIS2 share of national population
        nat_pro = !!population_col / national_pop,

        # Step 3: Apply that share to national derived denominator
        !!penta1_derived_denom_col := !!penta1_derived_denom_col * nat_pro,

        # Step 4: Recalculate coverage
        !!coverage_col := (!!indicator_col / !!penta1_denom_col) * 100,
        !!coverage_derived_col := (!!indicator_col / !!penta1_derived_denom_col) * 100
      ) %>%
      select(-national_pop, -nat_pro)
  } else {
    # If admin level is national, use already-computed national data
    national
  }

  data_joined %>%
    select(
      year,
      !!population_col,
      # percent_change,
      population_change,
      !!indicator_col,
      !!penta1_denom_col,
      !!penta1_derived_denom_col,
      !!coverage_col,
      !!coverage_derived_col
    )
}
