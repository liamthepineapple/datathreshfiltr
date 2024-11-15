library(testthat)

test_that("datathreshfiltr works correctly", {
  test_data <- data.frame(
    file.name = c("file1", "file2"),
    peak.number = c(1, 2),
    value1 = c(0.5, 1.5),
    value2 = c(2.5, 0.1),
    stringsAsFactors = FALSE
  )

  write.csv(test_data, "test_file.csv", row.names = FALSE)

  threshold <- 1
  result <- datathreshfiltr("test_file.csv", threshold)

  expected_data <- data.frame(
    file.name = c("file1", "file2"),
    peak.number = as.character(c(1, 2)),
    value1 = c(NA, 1.5),
    value2 = c(2.5, NA),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected_data)
})

