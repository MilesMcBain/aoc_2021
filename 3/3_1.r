library(purrr)

binary_matrix <-
  scan(
    "input3_1.txt",
    character()
  ) |>
  map(~ strsplit(.x, split = character(0)) |>
    unlist() |>
    as.numeric()) |>
  do.call(what = rbind)

binary_matrix_rows<-
  nrow(bin_mat)

one_is_most_common <- colSums(binary_matrix) >= binary_matrix_rows / 2 #

gamma <-
  as.integer(one_is_most_common)

epsilon <-
  as.integer(!one_is_most_common)

as_decimal <- function(bit_vector) sum(bit_vector * 2^(seq(length(bit_vector) - 1, 0)))

as_decimal(gamma) * as_decimal(epsilon)
