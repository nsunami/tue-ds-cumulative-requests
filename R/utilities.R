get_long <- function(data){
  data |>
    pivot_longer(
      -Year,
      names_to = "Type",
      values_to = "n"
    ) |>
    group_by(Type) |> 
    mutate(Cumulative_by_type = cumsum(n)) |>
    mutate(prop_per_year = n/ sum(n))
}


plot_cumulative <- function(data){
  
  data <- data |>
    group_by(Type) |>
    mutate(yearly_type_cumulative = cumsum(n))
  
  # Getting the label positions
  requests_4gp_latest <- data |>
    filter(Year == requests_4gp_long$Year |> max()) |>
    arrange(desc(Type)) |>
    ungroup() |>
    mutate(label_position = cumsum(yearly_type_cumulative))
  
  # Plot
  data |>
    group_by(Type) |>
    ggplot(aes(Year, Cumulative_by_type, fill = Type)) +
    geom_area() +
    scale_fill_brewer(palette = "Pastel1") +
    ylab("") +
    xlab("") +
    labs(
      title = "Cumulative Requests Over Time",
    ) +
    geom_text(
      data = requests_4gp_latest,
      aes(
        x = 2023.05,
        y = label_position - 80, 
        label = str_wrap(Type)
      ),
      hjust = "left",
      vjust = "outward",
      size = 7
    ) + 
    # Expanding limits for the labels
    expand_limits(
      y = c(NULL, 3000),
      x = c(2020, 2023.8)
    ) +
    # Annotating the plot 
    annotate(
      "text",
      y = requests_4gp_2023$label_position |> max(),
      x = 2022.5,
      label = str_glue("{requests_4gp_2023$label_position |> max()} requests"),
      size = 10,
      hjust = "right",
      vjust = "center"
    ) + 
    annotate(
      "line",
      y = requests_4gp_2023$label_position |> max(),
      x = c(2022.6, 2023)
    ) +
    # Deleting the legend
    guides(fill = "none") + 
    # Setting the theme
    theme_minimal() +
    theme(
      text = element_text(size = 17),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
    )
}
