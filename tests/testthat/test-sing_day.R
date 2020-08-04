context("Testing the sing_day function.")

test_df <- data.frame(Full_real_phrases = c("one dollar",
                                            "two dollars",
                                            "three dollars",
                                            "the sense that I live in a very boring family"),
                      Decoy_var = c(1,2,3,4),
                      Day.in.Words = c("Virst","Tsecund", "Thurd", "Fowrth"))

test_that("Sing day works properly for the first day special case",
  expect_equal(sing_day(test_df,1,"Full_real_phrases"), "On the Virst day of Christmas, my true love sent to me,\none dollar \n")
)
