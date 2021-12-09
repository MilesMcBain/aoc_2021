library(tidyverse)
library(readr)
library(stringr)

expand_points <- function(points_vec) {
  coordinates <- 
    as.integer(points_vec)  |>
    setNames(c("x1", "y1", "x2", "y2"))
    
  # If we only see 45 degree lines this will work,
  # since sequences must be same length
  cbind(x = seq(coordinates["x1"], coordinates["x2"]), 
        y = seq(coordinates["y1"], coordinates["y2"]))

} 

read_lines("input5_2.txt") |>
  str_extract_all("[0-9]+") |>
  lapply(expand_points) |>
  do.call(what = rbind) |>
  as.data.frame() |>
  group_by(x, y) |>
  summarise(
    times_covered = n(),
    .groups = "drop"
  ) |> 
  filter(
    times_covered >= 2
  ) |>
  nrow()
