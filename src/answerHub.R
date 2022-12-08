library(tidyverse)
source("src/sharedFunx.R")
source("src/day1.R")
source("src/day2.R")
source("src/day3.R")
source("src/day4.R")
source("src/day5.R")
source("src/day6.R")
source("src/day7.R")

answerTable = bind_rows(
  day1(),
  day2(),
  day3(),
  day4(),
  day5(),
  day6(),
  day7()
)
