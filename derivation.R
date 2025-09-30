

derive_coverage <- function(.data, indicator, population, base_year) {
  
  base_year <- base_year - 1

  # Match and reference the selected indicator
  indicator <- arg_match(indicator, get_all_indicators())
  indicator_col <- sym(indicator)
  
  coverage <- paste0("cov_", indicator, "_penta1")
  coverage_col <- sym(coverage)
  
  coverage_derived <- paste0(coverage, "derived")
  coverage_derived_col <- sym(coverage_derived)
  
  if (!rlang::has_name(.data, population)) {
    cli::cli_abort(c('x' = '{.val {population}} not available for denominator {.val {denominator}}.'))
  }
  
  pop_col <- sym(population)
  
  penta1_denom <- get_population_column(indicator, "penta1")
  penta1_denom_col <- sym(penta1_denom)
  
  derived_denom <- paste0(penta1_denom, "derived")
  derived_denom_col <- sym(derived_denom)
  
  if (!indicator %in% colnames(.data)) {
    return(NULL)
  }
  
  # If subnational, aggregate to national level first
  nat_data <- .data
  
  # Keep only necessary columns
  nat_data <- nat_data %>%
    select(year, all_of(c(indicator, population, coverage)), !!penta1_denom_col, totunder1_dhis2)
  
  # Ensure base year is not earlier than first year in data
  base_year <- robust_max(c(base_year, min(nat_data$year, na.rm = TRUE)), 2024)
  
  # Extract base year values for national denominator and population
  base_row <- nat_data %>%
    filter(year == base_year)
  
  base_value <- base_row %>% pull(!!pop_col)        # national base population
  # base_value <- base_row %>% pull(totunder1_dhis2) # national base population
  base_denom <- base_row %>% pull(!!penta1_denom_col) # national base DTP1-derived denominator
  
  # ---- NATIONAL-LEVEL DERIVED DENOMINATOR TRENDS ----
  national <- nat_data %>%
    mutate(
      # Step 1: Compute percent change in DHIS2 population over time from base
      percent_change = (!!pop_col - base_value) / base_value * 100,
      # percent_change = (totunder1_dhis2 - base_value) / base_value,
      
      # Step 2: Apply percent change to national base denominator
      !!derived_denom_col := base_denom * (1 + percent_change/100),
      
      # Step 3: Calculate traditional and derived coverage values
      # coverage_old = (!!indicator_col / totinftpenta_penta1) * 100,
      !!coverage_derived_col := !!indicator_col / !!derived_denom_col * 100,
      year = as.integer(year)
    ) %>%
    select(
      year,
      # totunder1_dhis2,
      !!pop_col,
      percent_change,
      !!indicator_col,
      !!penta1_denom_col,
      !!derived_denom_col,
      !!coverage_col,
      !!coverage_derived_col
    )
  
  return(national)
}