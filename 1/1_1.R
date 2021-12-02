library(readr)
library(dplyr)

read_delim(
  "input1_1.txt",
  delim = "\\n",
  col_names = "depth"
) |>
mutate(
  lag_depth = lag(depth),
  increases = depth > lag_depth
) |>
pull(increases) |>
sum(na.rm = TRUE)
