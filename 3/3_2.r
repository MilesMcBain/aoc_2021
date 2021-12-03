library(purrr)

binary_matrix <-
  scan(
    "input3_2.txt",
    character()
  ) |>
  map(~ strsplit(.x, split = character(0)) |>
    unlist() |>
    as.numeric()) |>
  do.call(what = rbind)

as_decimal <- function(bit_vector) sum(bit_vector * 2^(seq(length(bit_vector) - 1, 0)))

most_common_bit_oxygen <- function(bit_vec) {
  as.integer(sum(bit_vec) >= (length(bit_vec) / 2))
}

least_common_bit_co2 <- function(bit_vec) {
  as.integer(!(sum(bit_vec) >= (length(bit_vec) / 2)))
}

find_vector <- function(progress, bit_matrix, selector_fn) {
  if (nrow(bit_matrix) == 1) {
    return(c(progress, as.vector(bit_matrix)))
  }
  if (ncol(bit_matrix) == 0) {
    stop("we fucked up!")
  }

  filter_bit <- selector_fn(bit_matrix[ , 1, drop = TRUE])
  new_matrix <- bit_matrix[bit_matrix[ , 1] == filter_bit, , drop = FALSE ]
  new_progress <- c(progress, filter_bit)
  find_vector(new_progress, new_matrix[ , -1 , drop = FALSE], selector_fn)

}

oxygen_binary <- find_vector(
  integer(0),
  binary_matrix,
  most_common_bit_oxygen
)

co2_binary <- find_vector(
  integer(0),
  binary_matrix,
  least_common_bit_co2
)

as_decimal(oxygen_binary) * as_decimal(co2_binary)
