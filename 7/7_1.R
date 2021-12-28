positions <- scan(
  "input7_1.txt",
  what = integer(),
  sep = ","
)

(positions - median(positions)) |>
  abs() |>
  sum()

