test_that("Returns an error if incorrect table is supplied", {
  expect_error(db_get("pineapple"))
})

i <- db_get("Items")

test_that("Item table has four columns", {
  expect_equal(ncol(i), 4)
})

test_that("Some item will be field test", {
  # field test items should have a missing item_difficultuy value
  expect_equal(any(is.na(i$item_difficulty)), TRUE)
})
