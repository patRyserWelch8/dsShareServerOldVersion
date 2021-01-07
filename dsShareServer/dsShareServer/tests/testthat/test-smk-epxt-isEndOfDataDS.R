source('options/options_definitions.R')
source("data_files/variables.R")

context("dsShareServer::isDataEncodeDS::expt::isEndOfDataDS")
test_that("no option set",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data.encoded = vector_a))
  expect_error(isEndOfDataDS(data.encoded = "vector_a"))
})

set.default.options.to.null()
set.not.allowed()
assignSharingSettingsDS()

test_that("option set, not allowed",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data.encoded = vector_a))
  expect_error(isEndOfDataDS(data.encoded = "vector_a"))
})

options(sharing.near.equal.limit = 1000)
options(param.sharing.allowed = 1)
assignSharingSettingsDS()

test_that("option set, allowed",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data.encoded = vector_a))
  expect_error(isEndOfDataDS(data.encoded = "vector_a"))
  expect_error(isEndOfDataDS(data.encoded = "H"))
})


source("data_files/variables.R")
assignSharingSettingsDS()

data.encoded <- isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B", data.held.in.server = "all.data")


test_that("option set, allowed",
{
  expect_error(isEndOfDataDS())
  expect_error(isEndOfDataDS(data.encoded = vector_a))
  expect_error(isEndOfDataDS(data.encoded = "vector_A"))
  expect_true(isEndOfDataDS(data.encoded = "df_B"))
  expect_error(isEndOfDataDS(data.encoded = "F")) #exist but was not encoded data as above
  expect_error(isEndOfDataDS(data.encoded = "H")) #does not exist
})


#last line
rm(list = ls(pos = 1), pos = 1)
