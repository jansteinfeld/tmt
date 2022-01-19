context("test-sim.rm")

daten <- tmt:::sim.rm(theta = 100, b = 10, seed = 1111)
set.seed(1111)
theta <- stats::rnorm(100)
set.seed(1111)
beta <- stats::rnorm(10)

set.seed(1111)
daten2 <- tmt:::sim.rm(theta = theta, b = beta, seed = NULL)

set.seed(1111)
daten3a <- tmt:::sim.rm(theta = 100, b = beta, seed = NULL)
set.seed(1111)
daten3b <- tmt:::sim.rm(theta = 100, b = beta, seed = NULL)

set.seed(1111)
daten4a <- tmt:::sim.rm(theta = theta, b = 10, seed = NULL)
set.seed(1111)
daten4b <- tmt:::sim.rm(theta = theta, b = 10, seed = NULL)

# -----------------------------------------------------------------
context("test-sim.rm data structure")
# -----------------------------------------------------------------
test_that("test classic simulation function for the Rasch model", {
  expect_is(daten,"matrix")
  expect_that(nrow(daten), equals(100))
  expect_that(ncol(daten), equals(10))
  expect_that(daten[1,], equals(c(1,0,0,0,1,1,1,1,0,0)))
  expect_that(daten,equals(daten2))
  expect_that(daten3a,equals(daten3b))
  expect_that(daten4a,equals(daten4b))
})
