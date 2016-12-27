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

test_that("Cost components are preserved", {
  stack <- make_ssmulti()
  expect_true(is.matrix(stack$components))
  expect_equal(nrow(stack$components), 3)
  expect_equal(ncol(stack$components), 3)
})

test_that("Multi cost sorted by total", {
  stack <- make_ssmulti()
  
  expect_equal(stack$p, c(3+6+1,
                          1+4+7,
                          2+5+8))
  
  expect_equal(colSums(stack$components), stack$p)
  
})

test_that("Adding multi cost stacks basic functionality", {
  first <- make_ssmulti()
  second <- make_ssmulti()  
  combined <- first + second
  
  expect_equal(length(combined$p), 6)
  expect_equal(length(combined$q), 6)
  expect_equal(sum(combined$q), sum(first$q) + sum(second$q))
  expect_equal(rep(first$p, each=2), combined$p)
  
})

test_that("Adding multi cost stacks preserves cost components", {
  first <- make_ssmulti()
  second <- make_ssmulti()
  combined <- first + second
  
  expect_equal(combined$components[ ,2:3], matrix(c(3, 1,
                                                    6, 4,
                                                    1, 7),
                                                  nrow=3, ncol=2,
                                                  byrow=TRUE))
})

test_that("Adding multi cost stacks with different component names throws error", {
  first <- make_ssmulti()
  second <- first
  
  rownames(first$components) <- c("a", "b", "c")
  rownames(second$components) <- c("d", "b", "c")
  
  expect_error(first + second)

})

test_that("Adding multi cost stacks with different number of cost components throws error", {
  first <- make_ssmulti()
  second <- supplystack(p=matrix(1:8, nrow=4, ncol=2),
                        q=c(10, 10))
  
  expect_error(first + second)

})

test_that("Adding multi cost stacks with different number of suppliers does not throw error", {
  first <- make_ssmulti()
  second <- supplystack(p=matrix(1:6, nrow=3, ncol=2),
                        q=c(10, 10))
  
  combined <- first + second
  
  expect_equal(length(combined$p), 5)
  expect_equal(length(combined$q), 5)
  expect_equal(nrow(combined$components), 3)
  expect_equal(ncol(combined$components), 5)
  
})