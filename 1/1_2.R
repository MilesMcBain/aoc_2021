library(readr)
library(slider)
library(dplyr)

read_delim(
  "input1_2.txt",
  delim = "\\n",
  col_names = "depth",
  col_types = "i"
) |>
  mutate(
    threesum = slide_int(
      depth,
      .f = sum,
      .before = 2,
      .complete = TRUE
    ),
    larger = slide_lgl(
      threesum,
      .f = \(vec) vec[[2]] > vec[[1]],
      .before = 1,
      .complete = TRUE
    )
  ) |>
  pull(larger) |>
  sum(na.rm = TRUE)
