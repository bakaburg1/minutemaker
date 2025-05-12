test_that("apply_single_correction_set works with valid inputs", {
  # Create a test case
  test_text <- "This is a smple text with misspellings and other tpyos."
  corrections <- list(
    "smple" = "simple",
    "misspellings" = "misspellings",
    "tpyos" = "typos"
  )

  result <- apply_single_correction_set(test_text, corrections)

  # Check if corrections were applied correctly
  expect_equal(
    result,
    "This is a simple text with misspellings and other typos."
  )
})

test_that("apply_single_correction_set handles empty inputs", {
  # Test with empty text
  expect_equal(
    minutemaker:::apply_single_correction_set("", list("test" = "value")),
    ""
  )

  # Test with empty corrections map
  expect_equal(
    minutemaker:::apply_single_correction_set("some text", list()),
    "some text"
  )

  # Test with NULL inputs
  expect_equal(
    minutemaker:::apply_single_correction_set(NULL, list("test" = "value")),
    NULL
  )
})

test_that("apply_single_correction_set correctly handles longer terms first", {
  # Create text with overlapping terms
  test_text <- "The system works fine."

  # Create corrections with overlapping keys (shorter included in longer)
  corrections <- list(
    "system" = "framework",
    "system works" = "mechanism operates"
  )

  # Without proper ordering, "system" would be replaced first, breaking "system
  # works"
  result <- minutemaker:::apply_single_correction_set(test_text, corrections)

  # Check that longer term was replaced correctly
  expect_equal(
    result,
    "The mechanism operates fine."
  )
})