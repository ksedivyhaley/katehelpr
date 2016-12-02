context("Converting p-values to star symbols")

test_that("get_pstar produces expected values", {

  pstars <- factor(c("", "*", "**", "***"),
                   levels = c("***", "**", "*", ""))

  expect_equal(get_pstar(0.8), pstars[1])
  expect_equal(get_pstar(0.05), pstars[2])
  expect_equal(get_pstar(0.01), pstars[3])
  expect_equal(get_pstar(0.001), pstars[4])
})

test_that("get_pstar is vectorized", {

  pstars <- factor(c("", "*", "**", "***"),
                   levels = c("***", "**", "*", ""))

  expect_equal(get_pstar(c(0.8, 0.05)), pstars[1:2])

})

test_that("get_pstar produces error for illegal value", {

  expect_error(get_pstar(1.2), "p-value must be between 0 and 1")
  expect_error(get_pstar(c(0.5, 1.2)), "p-value must be between 0 and 1")

  })
