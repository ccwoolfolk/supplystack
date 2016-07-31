context("Base functionality of supply stack objects")

test_that("Components are sorted", {
  prices <- c(100, 150, 50)
  quantities <- c(10, 20, 30)
  producer_names <- paste("Producer", c("A","B","C"), sep="")
  
  supplystack_object <- supplystack(p=prices,
                                    q=quantities,
                                    nms=producer_names)
  
  expect_equal(supplystack_object$p, c(50, 100, 150))
  expect_equal(supplystack_object$q, c(30, 10, 20))
  expect_equal(supplystack_object$nms, c("ProducerC", "ProducerA", "ProducerB"))
})

test_that("Returns supply stack object", {
  expect_is(supplystack(p=1:5, q=rep(10, 5)), "supplystack")
})

test_that("Adding stacks together results in a combined stack", {
  prices <- runif(min=100, max=200, n=10)
  quantities <- rep(1:5, 2)
  producer_names <- paste("Producer", 1:10, sep="")
  
  ss_1 <- supplystack(p=prices[1:5],
                      q=quantities[1:5],
                      nms=producer_names[1:5])
  ss_2 <- supplystack(p=prices[6:10],
                      q=quantities[6:10],
                      nms=producer_names[6:10])
  ss_3 <- supplystack(p=prices[6:10],
                      q=quantities[6:10])
  ss_comb <- supplystack(p=prices,
                         q=quantities,
                         nms=producer_names)
  
  expect_equal(ss_1 + ss_2, ss_comb)
  expect_equal(ss_2 + ss_1, ss_comb)
  
})

test_that("Adding stack with names to one without results in no names", {
  prices <- runif(min=100, max=200, n=10)
  quantities <- rep(1:5, 2)
  producer_names <- paste("Producer", 1:10, sep="")
  
  ss_1 <- supplystack(p=prices[1:5],
                      q=quantities[1:5],
                      nms=producer_names[1:5])
  ss_2 <- supplystack(p=prices[6:10],
                      q=quantities[6:10])
  ss_comb <- supplystack(p=prices,
                         q=quantities)
  
  expect_equal(ss_1 + ss_2, ss_comb)
  
})