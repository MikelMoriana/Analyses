# Find the subplots with wrong number
find_subplot_species <- function(x, y, z) {
  x |>
    filter(plotID == y) |>
    filter(year == z) |> 
    filter(subPlot %in% c(9, 11, 13, 23, 25, 27)) |> 
    select(subPlot, species, value)
}

# Find in what specific plot and year a species was recorded
find_plot_year <- function(x, y) {
  x |>
    filter(species == y) |>
    select(plotID, year) |>
    unique()
}

# We adjust the make_turf_function from turfmapper to include fertility instead of cover
make_turf_plot_fertile <- function(data, grid_long, year, subturf, species, fertile, title) {
  data <- rename(data,
    subturf = {{ subturf }},
    species = {{ species }},
    year = {{ year }}
  )
  stopifnot(all(data$subturf %in% grid_long$subturf))
  data <- left_join(data, grid_long, by = "subturf")
  ggplot(data, aes(x = .data$.x, y = .data$.y, fill = {{ fertile }})) +
    geom_tile(colour = "grey60") +
    facet_grid(species ~ year) +
    ggtitle(title) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Paired") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.spacing.y = unit(0.04, "lines"),
      panel.grid.minor = element_blank(),
      strip.text.y = element_text(angle = 0)
    )
}

turfplot <- function(x, y) {
  x |>
    filter(plotID == y) |>
    bind(
      x,
      make_turf_plot_fertile(
        data = x,
        grid_long = grid,
        year = year, species = species, subturf = subPlot, fertile = fertile,
        title = glue::glue("Site {x$site}: plot {x$plotID}")
      )
    )
}

turfplot_year <- function(x, y, z) {
  x |>
    filter(plotID == y) |>
    filter(year == z) |> 
    bind(
      x,
      make_turf_plot_fertile(
        data = x,
        grid_long = grid,
        species = species, subturf = subPlot, fertile = fertile,
        title = glue::glue("Site {x$site}: plot {x$plotID}")
      )
    )
}

# We create a function for plotting richness at each subplot from yellow (low richness) to red (high richness)
make_richness_plot <- function(data, grid_long, treatment, year, subturf, richness, title) {
  data <- rename(data,
                 subturf = {{ subturf }},
                 richness = {{ richness }},
                 year = {{ year }},
                 treatment = {{treatment}}
  )
  stopifnot(all(data$subturf %in% grid_long$subturf))
  data <- left_join(data, grid_long, by = "subturf")
  ggplot(data, aes(x = .data$.x, y = .data$.y, fill = {{ richness }})) +
    geom_tile(colour = "grey60") +
    facet_grid(treatment~year) +
    ggtitle(title) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colors = rev(heat.colors(20))) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.spacing.y = unit(0.04, "lines"),
      panel.grid.minor = element_blank(),
      strip.text.y = element_text(angle = 0)
    )
}

richnessplot <- function(x, y) {
  x |>
    filter(plotID == y) |>
    bind(
      x,
      make_richness_plot(
        data = x,
        grid_long = grid,
        year = year, treatment = treatment, subturf = subPlot, richness = richness,
        title = glue::glue("{x$plotID} Warming: {x$warming}")
      )
    )
}
