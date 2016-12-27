context("Multiple cost components are handled correctly")

make_ssmulti <- function() {
  costs <- matrix(c(1, 2, 3,
                    4, 5, 6,
                    7, 8, 1),
                  nrow=3, ncol=3,
                  byrow=TRUE)
  
  quantities <- c(10, 10, 10)
  return(supplystack(p=costs, q=quantities,
                     nms=paste("Co", 1:3, sep="")))
}

test_that("Multi cost sorted by total", {
  stack <- make_ssmulti()
  
  expect_equal(stack$p, c(3+6+1,
                          1+4+7,
                          2+5+8))
  
})

test_that("Adding multi cost stacks basic functionality", {
  expect_true(FALSE)
})

test_that("Cost component names can be retrieved", {
  expect_true(FALSE)
})