context("Sites")
#
# tbl_new <- new_site_table()
# tbl_get <- get_site_table()
#
# test_that("Dummy table is created.", {
#   expect_true(is.data.frame(tbl_new), TRUE)
#   expect_true(is.data.frame(tbl_get), TRUE)
# })


#Omar tests

test_that("Test for sexagesimal to decimal", {

  expect_error(seg2dec(grade = "", minute = "", second = ""))
  expect_error(seg2dec(grade = "", minute = 10, second = 14))
  expect_error(seg2dec(grade = "", minute = "", second = 14))
  expect_equal(seg2dec(grade = NA , minute = NA, second = NA), as.numeric(NA))
})

#devtools::test()





