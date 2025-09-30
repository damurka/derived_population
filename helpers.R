common_theme <- function() {
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    legend.position = 'bottom'
  )
}

# y scales
scale_y_pop <- function() {
  scale_y_continuous(
    labels = comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  )
}

scale_y_pct <- function() {
  scale_y_continuous(
    labels = function(x) percent(x / 100, accuracy = 1),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  )
}

# long format
to_long <- function(df, value_cols, name = 'group', value = 'value') {
  df %>%
    select(year, all_of(value_cols)) %>%
    pivot_longer(cols = all_of(value_cols), names_to = name, values_to = value) %>%
    rename(group = !!name, value = !!value)
}

# generic line plot
line_plot <- function(long_df, title_txt, y_lab, y_scale, legend_breaks = NULL, legend_labels = NULL) {
  p <- ggplot(long_df, aes(x = year, y = value, colour = group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = title_txt, x = 'Year', y = y_lab, colour = '') +
    y_scale() +
    common_theme()
  if (!is.null(legend_breaks) || !is.null(legend_labels)) {
    p <- p + scale_colour_discrete(breaks = legend_breaks, labels = legend_labels)
  }
  p
}

# reactable formatter factory
reactable_for_derived <- function(df) {
  cols <- list(
    year = colDef(align = 'center', width = 80)
  )
  cov_cols <- grep('^(cov_|per)', names(df), value = TRUE)
  for (nm in cov_cols) {
    cols[[nm]] <- colDef(
      align = 'right',
      format = colFormat(digits = 1, suffix = '%')  # assumes 0–100 values
    )
  }
  num_cols <- setdiff(names(df)[sapply(df, is.numeric)], c('year', cov_cols))
  for (nm in num_cols) {
    cols[[nm]] <- colDef(align = 'right', format = colFormat(separators = TRUE, digits = 0))
  }
  reactable(
    df,
    columns = cols,
    striped = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    defaultPageSize = 20,
    compact = TRUE,
    bordered = TRUE
  )
}