example_input <- data.frame(
  id = 1:3,
  value = c("a", "b", "c")
)

utils::write.csv(
  example_input,
  file = "inst/extdata/example_input.csv",
  row.names = FALSE
)
