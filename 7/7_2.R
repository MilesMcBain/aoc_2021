positions <- scan(
  "input7_1.txt",
  what = integer(),
  sep = ","
)

min_pos <- min(positions)
max_pos <- max(positions)

possible_positions <-
  seq(min_pos, max_pos)

move_cost_vec <-
  cumsum(
    seq(1, max_pos - min_pos)
  )

move_cost_function <- function(distance) {
  non_zero_distance <- distance[distance > 0]
  sum(move_cost_vec[non_zero_distance])
}

vapply(possible_positions,
  function(a_position) {
  move_cost_function(abs(a_position - positions))
  },
  integer(1)) |>
  min()

