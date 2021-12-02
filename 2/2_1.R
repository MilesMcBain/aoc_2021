library(readr)
library(purrr)

update_position <- function(position, delta) UseMethod("update_position", delta)

update_position.forward <- function(position, delta) {
  position$horizontal <- position$horizontal + delta
  position
}
update_position.down <- function(position, delta) {
  position$depth <- position$depth + delta
  position
}
update_position.up <- function(position, delta) {
  position$depth <- position$depth - delta
  position
}

read_delim(
  "input2_1.txt", 
  delim = " ",
  col_names = c("direction", "distance"),
  col_types = "ci"
  ) |>
transpose() |>
map(~ structure(.x$distance, class = .x$direction)) |> 
reduce(
  .f = \(position, update) {
    update_position(position, update)
  },
  .init = list(horizontal = 0, depth = 0)
) |>
reduce(`*`)


