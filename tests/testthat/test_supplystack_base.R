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