source('options/options_definitions.R')




context("dsShareServer::isDataEncodeDS::smk")
test_that("no arguments",
{
    outcome <- isDataEncodedDS()
    expect_true(is.logical(outcome))
    expect_false(outcome)
})

context("dsShareServer::isDataEncodeDS::expt")
test_that("arguments are not correct",
{
  expect_false(isDataEncodedDS(data.server  = "D"))
  expect_false(isDataEncodedDS(data.encoded = "D"))
})

test_that("arguments are correct",
{
  expect_false(isDataEncodedDS(data.server  = "D", data.encoded = "E"))
  expect_false(isDataEncodedDS(data.server  = "does not exist", data.encoded = "not yet created"))
  set.default.options.not.restrictive()
  expect_true(isDataEncodedDS(data.server  = "D", data.encoded = "E"))
})

context("dsShareServer::isDataEncodeDS::expt::.are.params.correct")
test_that("no arguments",
{
  expect_false(.are.params.correct())
})

test_that("one argument",
{
  expect_false(.are.params.correct(data.server  = "D"))
  expect_false(.are.params.correct(data.encoded = "D"))
})

test_that("two arguments object do not exist",
{
  expect_false(.are.params.correct(data.server  = "do not exits", data.encoded = "do not exists"))
})

test_that("two arguments objects exist",
{
  set.default.options.to.null()
  expect_false(.are.params.correct(data.server  = "D", data.encoded = "E"))
  set.default.options.not.restrictive()
  expect_true(.are.params.correct(data.server  = "D", data.encoded = "E"))
})


context("dsShareServer::isDataEncodeDS::expt::.are.similar")
test_that("same values",
{
  expect_true(.are.similar(get("D", pos = 1), get("D", pos = 1)))
  expect_true(.are.similar(get("E", pos = 1), get("E", pos = 1)))
  expect_true(.are.similar(get("F", pos = 1), get("F", pos = 1)))
  set.default.options.restrictive()
  expect_true(.are.similar(1.0, 0.9999))
  set.default.options.not.restrictive()
  expect_false(.are.similar(1.0, 0.9999))
})
