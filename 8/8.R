library(dplyr)
library(purrr)

as_bool_vecs <- function(sample) {
  lapply(
    sample,
    function(pattern) {
      pattern_segs <-
        strsplit(pattern, split = NULL) |>
        unlist()
      bool_vec <-
        logical(7) |>
        setNames(letters[1:7])
      bool_vec[pattern_segs] <- TRUE
      bool_vec
    }
  )
}

deduce_patterns <- function(sample) {
  pattern_seg_count <- 
    lapply(sample, sum) |>
    unlist()
  
  pattern_1 <- sample[[which(pattern_seg_count == 2)]]
  pattern_4 <- sample[[which(pattern_seg_count == 4)]]
  pattern_7 <- sample[[which(pattern_seg_count == 3)]]
  pattern_8 <- sample[[1]] | TRUE

  # find bd
  segs_bd <- pattern_4 & !pattern_1
  # find length 5 patterns
  length_5_patterns <-
    sample[pattern_seg_count == 5]
  # for pattern 5, find length 5 pattern with bd
  pattern_5 <-
    length_5_patterns |>
    Filter(f = \(pattern) all((pattern & segs_bd) == segs_bd)) |>
    unlist()
  # for pattern 3, find length 5 pattern with cf (pattern_1)
  pattern_3 <-
    length_5_patterns |>
    Filter(f = \(pattern) all((pattern & pattern_1) == pattern_1)) |>
    unlist()
  # for pattern 2, find length 5 pattern that is not pattern_5 or pattern_3
  pattern_2 <-
    length_5_patterns |>
    Filter(f = \(pattern) !all(pattern == pattern_3) & !all(pattern == pattern_5)) |>
    unlist()
  
  pattern_9 <-
    pattern_4 | pattern_7 | pattern_5

  pattern_6 <-
    pattern_5 | !pattern_9

  segs_g <-
    xor(pattern_5, pattern_4) & !pattern_7

  segs_d <-
    pattern_3 & !pattern_7 & !segs_g

  pattern_0 <-
    xor(pattern_8, segs_d)

  list(
    pattern_0,
    pattern_1,
    pattern_2,
    pattern_3,
    pattern_4,
    pattern_5,
    pattern_6,
    pattern_7,
    pattern_8,
    pattern_9
  ) |>
  setNames(
    0:9
  )
}

decode_reading <- function(reading, digit_key) {
  reading_bools <-
    as_bool_vecs(reading)

  lapply(
    reading_bools,
    function(digit) {
      lapply(digit_key, `==`, digit) |> 
      Filter(f = all) |> 
      names() 
    }
  )
}

decoded_input <-
  read.delim(
  "input_8_1.txt",
  header = FALSE,
  sep = "|",
  col.names = c("samples", "reading")
) |>
  mutate(
    samples = strsplit(trimws(samples), " "),
    reading = strsplit(trimws(reading), " "),
    samples_binary = lapply(samples, as_bool_vecs),
    digit_key = lapply(samples_binary, deduce_patterns),
    digits = map2(reading, digit_key, decode_reading)
)

# solution 1

unlist(decoded_input$digits) %in% c("1", "4", "7", "8") |> sum()

# solution 2

decoded_input$digits |>
  lapply(paste0, collapse = "") |>
  unlist() |> 
  as.numeric() |>
  sum()