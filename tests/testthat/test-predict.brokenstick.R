library(brokenstick)
context("predict.brokenstick()")

obj <- fit_200
dat <- smocc_200
n <- nrow(dat)
m <- length(unique(dat$id))
k <- length(get_knots(obj))

test_that("returns proper number of rows", {
  expect_equal(nrow(predict(obj, dat)), n)
  expect_equal(nrow(predict(obj, dat, x = NA)), m)
  expect_equal(nrow(predict(obj, x = NA, y = 10)), 1L)
  expect_equal(nrow(predict(obj, x = c(NA, NA), y = c(-1, 10))), 2L)
})

test_that("returns proper number of rows with at = 'knots'", {
  expect_equal(nrow(predict(obj, dat, x = "knots")), m * k)
  expect_equal(nrow(predict(obj, dat, x = NA)), m)
  expect_equal(nrow(predict(obj, x = NA, y = 10)), 1L)
})

test_that("returns proper number of rows with both data & knots", {
  expect_equal(nrow(predict(obj, dat, x = "knots", strip_data = FALSE)), n + k * m)
  expect_equal(nrow(predict(obj, dat, x = NA, y = 10, group = 10001, strip_data = FALSE)), 11)
  expect_equal(nrow(predict(obj, dat, x = c(NA, NA), y = c(-1, 10), group = rep(10001, 2), strip_data = FALSE)), 12)
})

test_that("output = 'vector' and output = 'long' are consistent", {
  expect_equivalent(
    predict(obj, dat)[[".pred"]],
    predict(obj, dat, shape = "vector")
  )
  expect_equal(
    predict(obj, dat, x = 1)[[".pred"]],
    predict(obj, dat, x = 1, shape = "vector")
  )
  expect_equal(
    predict(obj, x = c(NA, 1), y = c(1, NA))[[".pred"]],
    predict(obj, x = c(NA, 1), y = c(1, NA), 10, shape = "vector")
  )
  expect_equal(
    predict(obj, dat, x = "knots")[[".pred"]],
    predict(obj, dat, x = "knots", shape = "vector")
  )
  expect_equal(
    predict(obj, dat, strip_data = FALSE)[[".pred"]],
    predict(obj, dat, strip_data = FALSE, shape = "vector")
  )
  expect_equal(
    predict(obj, x = NA, y = 10, strip_data = FALSE)[[".pred"]],
    predict(obj, x = NA, y = 10, strip_data = FALSE, shape = "vector")
  )
})

exp <- fit_200
dat <- smocc_200
test_that("returns proper number of rows", {
  expect_equal(nrow(predict(exp, dat, x = NA)), 200L)
  expect_equal(nrow(predict(exp, dat, x = c(NA, NA))), 400L)
  expect_equal(nrow(predict(exp, dat, x = NA, y = 1)), 1L)
  expect_equal(nrow(predict(exp, dat, x = c(NA, NA), y = c(-1, 10))), 2L)
  expect_equal(nrow(predict(exp, dat, x = "knots")), 2200L)
  expect_equal(nrow(predict(exp, dat, x = "knots", y = rep(1, 11))), 11L)
})

test_that("accepts intermediate NA in x", {
  expect_equal(
    unlist(predict(exp, x = 1, y = -1)[1, ]),
    unlist(predict(exp, x = c(NA, 1), y = c(1, -1))[2, ])
  )
  expect_equal(
    unlist(predict(exp, x = c(1, NA), y = c(-1, 1))[1, ]),
    unlist(predict(exp, x = c(NA, 1), y = c(1, -1))[2, ])
  )
  expect_equal(
    unlist(predict(exp, x = c(1, 2, NA), y = c(NA, -1, 1))[2, ]),
    unlist(predict(exp, x = c(1, NA, 2), y = c(NA, 1, -1))[3, ])
  )
})

test_that("accepts unordered x", {
  expect_equal(
    round(predict(exp, x = c(1, 2, 3), y = c(-1, 1, 0))[1, 5], 5),
    round(predict(exp, x = c(2, 3, 1), y = c(1, 0, -1))[3, 5], 5)
  )
})

context("predict_brokenstick factor")

fit <- fit_200
dat <- smocc_200
dat$id <- factor(dat$id)

test_that("works if id in new_data is a factor", {
  expect_silent(predict(obj, new_data = dat))
})

dat <- dat[-(1:nrow(dat)), ]
test_that("can handle new_data with zero rows", {
  expect_silent(predict(obj, new_data = dat))
})





# We needed this to solve problem when new_data is a factor
# obj1 <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = 1:2)
# obj2 <- brokenstick(hgt.z ~ age | id, data = dat, knots = 1:2)
# test_that("brokenstick doesn't care about factors", {
#   expect_identical(obj1, obj2)
# })
#
#
# z1 <- predict(obj1, new_data = dat)
# z2 <- predict(obj2, new_data = dat)
# identical(z1, z2)
