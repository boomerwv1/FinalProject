if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


dl <- tempfile()
download.file("https://github.com/boomerwv1/FinalProject/blob/master/nba.games.stats.csv", dl)

dlFILE <- paste(dl,"\\nba.games.stats.csv", sep = "")



States <- read.csv(file = dlFILE, stringsAsFactors = FALSE)
