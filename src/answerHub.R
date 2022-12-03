library(tidyverse)
source("src/sharedFunx.R")
source("src/day1.R")
source("src/day2.R")
source("src/day3.R")

answerTable = bind_rows(
  day1(),
  day2(),
  day3()
)
