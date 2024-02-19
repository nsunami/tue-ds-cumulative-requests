library(tidyverse)
source("R/utilities.R")

# Reading the data
requests <- read_csv("data/requests.csv")

# Cumulative requests over time - with 4 groups
requests_4gp <- requests |>
  transmute(
    Year,
    Privacy = Privacy + Agreements,
    Ethics = ERB,
    RDM = `RDM general requests` + `Open Science` + DMP,
    `Training` = `Meetings & Trainings`
  )

# Transforming to a long format
requests_4gp_long <- requests_4gp |>
  get_long() |>
  group_by(Type) |>
  mutate(yearly_type_cumulative = cumsum(n))

# Creating the cumulative plot
cumulative_plot <- requests_4gp_long |> 
  plot_cumulative() 

# Saving the output
ggsave(
  cumulative_plot,
  filename = "plot/cumulative.jpg",
  width = 3200,
  height = 2500,
  units = "px"
)
