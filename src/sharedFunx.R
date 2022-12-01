library(tidyverse)

readInputFile = function(dayNumber) {
  return(read_csv(paste("inputs/input", dayNumber, ".txt", sep = ""), skip_empty_rows = FALSE, col_names = FALSE))
}

returnAnswerRow = function(dayNumber, part1, part2) {
  return(tibble_row(Day = dayNumber, "Part 1" = part1, "Part 2" = part2))
}