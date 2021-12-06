input_file <-"./input4_1.txt" 

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

dim(boards) <-
  c(5, 5, nrow(board_data) / 5)
# it's a 3d array now

boards_results <-
  array(rep(FALSE, length(boards)), dim = dim(boards))
# Another 3d indicator array

pop_draw <- function(number_draws) {
        value <- number_draws[[1]]
        number_draws <<- number_draws[-1]
        value
}

apply_draw <- function(boards_results, draw) {
  boards_results[boards == draw] <- TRUE
  boards_results
} 

get_winner <- function(boards_results) {
  winners <- apply(boards_results, 3,
    \(board_mat) {
      any(colSums(board_mat) == 5) | any(rowSums(board_mat) == 5)
    }) |>
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

winning_board <- boards[ , , winner]
unmarked_on_winning_board <- winning_board[!boards_results[ , , winner]]

print(latest_draw * sum(unmarked_on_winning_board))
