library(tidyverse)

readInputFile = function(dayNumber) {
  return(read_csv(paste("inputs/input", dayNumber, ".txt", sep = ""), skip_empty_rows = FALSE, col_names = FALSE))
}

returnAnswerRow = function(dayNumber, part1, part2) {
  return(tibble_row(Day = dayNumber, "Part 1" = part1, "Part 2" = part2))
}

is_subset = function(x1, x2, y1, y2) {
  setX = seq(from = x1, to = x2, by = 1)
  setY = seq(from = y1, to = y2, by = 1)
  return(all(setX %in% setY) | all(setY %in% setX))
}

has_intersect = function(x1, x2, y1, y2) {
  setX = seq(from = x1, to = x2, by = 1)
  setY = seq(from = y1, to = y2, by = 1)
  return(!is_empty(intersect(setX, setY)))
}