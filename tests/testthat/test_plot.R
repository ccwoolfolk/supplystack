context("Plotting of supply stacks")

test_that("Setting \"ann\" throws error", {
  expect_error(plot(supplystack(1:2, 1:2), ann=TRUE))
})

test_that("Setting invalid \"names_cutoff\" throws error", {
  expect_error(plot(supplystack(1:2, 1:2), names_cutoff=1.01))
  expect_error(plot(supplystack(1:2, 1:2), names_cutoff=10))
  expect_error(plot(supplystack(1:2, 1:2), names_cutoff=-1))
  expect_silent(plot(supplystack(1:2, 1:2), names_cutoff=1))
  expect_silent(plot(supplystack(1:2, 1:2), names_cutoff=0))
  expect_silent(plot(supplystack(1:2, 1:2), names_cutoff=0.5))
})