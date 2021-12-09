options(scipen = 999)

fish_ages <-
    scan(
    "input6_1.txt",
    what = integer(),
    sep = ","
  ) |> 
  table() |> 
  as.list() |>
  modifyList(
    x = setNames(as.list(rep(0, length(seq(-1, 8)))), seq(-1, 8))
  )

shift_list_left <- function(a_list) {
  list_names <- names(a_list)
  c(a_list[-1], 0) |>
    setNames(list_names)
}

new_population <-
  reduce(
    .x = seq(256),
    .init = fish_ages,
    .f = function(fish_ages, epoch) {
        new_ages <- shift_list_left(fish_ages)
        reproducing <- new_ages[["-1"]] 
        new_ages["-1"] <- 0
        new_ages["8"] <- reproducing
        new_ages["6"] <- new_ages[["6"]] + reproducing
        new_ages
      })

do.call(sum, new_population)
