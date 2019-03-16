context("test-sim.rm")

daten <- tmt:::sim.rm(100,10,1111)

# -----------------------------------------------------------------
context("test-sim.rm data structure")
# -----------------------------------------------------------------
expect_is(daten,"matrix")
expect_that(nrow(daten), equals(100))
expect_that(ncol(daten), equals(10))
expect_that(daten[1,], equals(c(1,0,0,0,0,1,0,1,1,0)))
