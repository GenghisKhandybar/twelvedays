context("Running pluralize_gift()")


test_that("Goose pluralizes go Geese", {
  expect_equal(pluralize_gift("goose"), "geese")
})

test_that("A normal word like dog becomes dogs", {
  expect_equal(pluralize_gift("dog"), "dogs")
})
