# Find in what specific plot and year a species was recorded
find_plot_year <- function(x, y) {
  x |> 
    filter(species == y) |> 
    select(plotID, year) |> 
    unique()
}

# We adjust the make_turf_function from turfmapper to include fertility instead of cover
make_turf_plot_fertile <- function (data, grid_long, year, subturf, species, fertile, title) {
  data <- rename(data,
                 subturf = {{subturf}},
                 species = {{species}},
                 year = {{year}})
  stopifnot(all(data$subturf %in% grid_long$subturf))
  data <- left_join(data, grid_long, by = "subturf")
  ggplot(data, aes(x = .data$.x, y = .data$.y, fill = {{fertile}})) + 
    geom_tile(colour = "grey60") + facet_grid(species ~ year) + 
    ggtitle(title) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    scale_fill_brewer(palette = "Paired") +
    theme_bw() + 
    theme(axis.text = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(),
          panel.spacing.y = unit(0.04, "lines"),
          panel.grid.minor = element_blank(), 
          strip.text.y = element_text(angle = 0))
}

turfplot <- function(x, y) {
  x |> 
  filter(plotID == y) |>
  bind(x,
       make_turf_plot_fertile(
         data = x, 
         grid_long = grid, 
         year = year, species = species, subturf = subPlot, fertile = fertile,
         title = glue::glue("Site {x$site}: plot {x$plotID}")
       ))
}
