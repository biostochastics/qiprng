library(testthat)
library(qiprng)

# Set test reporter to show more details
options(testthat.progress.max_fails = 999)
options(testthat.summary.max_fails = 999)

test_check("qiprng")
