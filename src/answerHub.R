library(tidyverse)
source("src/sharedFunx.R")
source("src/day1.R")
source("src/day2.R")
source("src/day3.R")
source("src/day4.R")
source("src/day5.R")
source("src/day6.R")
source("src/day7.R")
source("src/day8.R")
source("src/day9.R")
source("src/day10.R")
source("src/day11.R")
source("src/day12.R")
source("src/day13.R")
source("src/day14.R")
source("src/day15.R")


answerTable = bind_rows(
  day1(),
  day2(),
  day3(),
  day4(),
  day5(),
  day6(),
  day7(),
  day8(),
  day9(),
  day10(), 
  day11(showYourWork = FALSE),
  day12(),
  day13(),
  day14(showYourWork = FALSE),
  day15(showYourWork = FALSE)
)
