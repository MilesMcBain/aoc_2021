BOARD_SIZE <- 5

number_draws <-
  readLines(input_file, n = 1) |>
  strsplit(",") |>
  unlist() |>
  as.integer()

boards <-
  read.table(
    input_file,
    sep = "",
    skip = 1,
    blank.lines.skip = TRUE
  ) |>
  as.matrix() |>
  unname()


boards_results <-
  array(rep(FALSE, length(boards)), dim = dim(boards))
# Another 3d indicator array

board_num_to_indexes <- function(board_num, .dim = dim(boards)) {
  row <- rep(seq(BOARD_SIZE), times = BOARD_SIZE) +
    (BOARD_SIZE * (board_num - 1))
  col <- rep(seq(BOARD_SIZE), each = BOARD_SIZE)

  cbind(row, col)
}

board_indexes <- lapply(seq(nrow(boards) / BOARD_SIZE), board_num_to_indexes)

pop_draw <- function(number_draws, .dim = dim(boards)) {
  value <- number_draws[[1]]
  number_draws <<- number_draws[-1]
  value
}

apply_draw <- function(boards_results, draw) {
  boards_results[boards == draw] <- TRUE
  boards_results
}

get_winner <- function(boards_results) {
  winners <- lapply(
    board_indexes,
    \(board_ind) {
      board_mat <- matrix(boards_results[board_ind], nrow = BOARD_SIZE)
      any(colSums(board_mat) == BOARD_SIZE) | any(rowSums(board_mat) == BOARD_SIZE)
    }
  ) |>
    unlist()
  head(which(winners), 1)
}

winner <- integer(0)
latest_draw <- integer(0)
while (length(winner) == 0) {
  latest_draw <<- pop_draw(number_draws)
  boards_results <<- apply_draw(boards_results, latest_draw)
  winner <<- get_winner(boards_results)
}

winning_board <- boards[board_num_to_indexes(winner)]
unmarked_on_winning_board <-
  winning_board[!boards_results[board_num_to_indexes(winner)]]

print(latest_draw * sum(unmarked_on_winning_board))
