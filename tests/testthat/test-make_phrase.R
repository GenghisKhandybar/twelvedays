context("Testing make_phrase function.")


test_that("Random sentence construction works",
          expect_equal(make_phrase(3, "AGM-158 JASSM Missiles", "flying","silver","into civilians"),
                       "three silver AGM-158 JASSM Missiles flying into civilians"))

