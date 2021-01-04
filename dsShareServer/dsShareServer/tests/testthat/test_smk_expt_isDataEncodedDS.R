source('options/options_definitions.R')

context("dsShareServer::isDataEncodeDS::expt::are.params.corrrect")
set.default.options.to.null()
test_that("incorrect arguments",
{
  expect_error(.are.params.correct())
  expect_error(.are.params.correct(data.server = "A"))
  expect_error(.are.params.correct(data.server = "A", data.encoded = "B"))
  expect_error(.are.params.correct(data.server = "D", data.encoded = "E"))
})


set.default.options.not.restrictive()
test_that("incorrect arguments",
{
  expect_false(.are.params.correct())
  expect_false(.are.params.correct(data.server = "A"))
  expect_false(.are.params.correct(data.server = "A", data.encoded = "B"))

})

test_that("correct arguments",
{
  expect_true(.are.params.correct(data.server = "D", data.encoded = "E", data.held.in.server = "F"))
})

vector_a <- get("vector_A", pos = 1)
vector_b <- get("vector_B", pos = 1)
vector_c <- get("vector_C", pos = 1)

matrix_a <- get("matrix_A", pos = 1)
matrix_b <- get("matrix_B", pos = 1)
matrix_c <- get("matrix_C", pos = 1)

list_a <- get("list_A", pos = 1)
list_b <- get("list_B", pos = 1)
list_c <- get("list_C", pos = 1)

df_a <- get("df_A", pos = 1)
df_b <- get("df_B", pos = 1)
df_c <- get("df_C", pos = 1)

context("dsShareServer::isDataEncodeDS::expt::.are.significant.same")
test_that("incorrect arguments",
{
  expect_error(.are.significant.same())
  expect_error(.are.significant.same(server = A))
  expect_error(.are.significant.same(server = A, encoded = B))
})

test_that("expected outcome",
{
  expect_true(.are.significant.same(server = vector_a, encoded = vector_a))
  expect_true(.are.significant.same(server = vector_a, encoded = vector_c))
  expect_false(.are.significant.same(server = vector_b, encoded = vector_c))

  expect_true(.are.significant.same(server = vector_a, encoded = matrix_a))
  expect_true(.are.significant.same(server = vector_a, encoded = matrix_c))
  expect_false(.are.significant.same(server = vector_a, encoded = matrix_b))

  expect_true(.are.significant.same(server = matrix_a, encoded = matrix_a))
  expect_true(.are.significant.same(server = matrix_a, encoded = matrix_c))
  expect_false(.are.significant.same(server = matrix_b, encoded = matrix_c))

  # list have to be unlisted before being passed
  expect_error(.are.significant.same(server = list_a, encoded = list_a))
  expect_error(.are.significant.same(server = list_a, encoded = list_c))
  expect_error(.are.significant.same(server = matrix_a, encoded = list_c))

  # data.framee have to be converted  to  matrices before being passed
  expect_error(.are.significant.same(server = df_a, encoded = df_a))
  expect_error(.are.significant.same(server = df_a, encoded = df_c))
  expect_error(.are.significant.same(server = vector_a, encoded = df_c))

  expect_false(.are.significant.same(server = vector_a, encoded = vector_b))
  expect_false(.are.significant.same(server = vector_a, encoded = matrix_b))
})

context("dsShareServer::isDataEncodeDS::expt::.are.values.in.limit")
test_that("incorrect arguments",
{
  expect_error(.are.values.in.limit())
  expect_error(.are.values.in.limit(server = A))
  expect_error(.are.values.in.limit(server = A, encoded = B))
  expect_error(.are.values.in.limit(server = A, encoded = B, limit="9999"))
})

test_that("expected outcome",
{

  expect_true(.are.values.in.limit(server = vector_a, encoded = vector_a, limit = 1000))
  expect_false(.are.values.in.limit(server = vector_a, encoded = vector_b, limit = 1000))
  expect_true(.are.values.in.limit(server = vector_a, encoded = vector_c, limit = 1000))
  expect_false(.are.values.in.limit(server = vector_b, encoded = vector_c, limit = 1000))


  expect_true(.are.values.in.limit(server = vector_a, encoded = vector_a, limit = 0.1))
  expect_false(.are.values.in.limit(server = vector_a, encoded = vector_b, limit = 0.1))
  expect_false(.are.values.in.limit(server = vector_a, encoded = vector_c, limit = 0.1))
  expect_false(.are.values.in.limit(server = vector_b, encoded = vector_c, limit = 0.1))

  #compare by columns. So it is best to comvert to a vector
  expect_error(.are.values.in.limit(server = matrix_a, encoded = matrix_a, limit = 1000))
  expect_error(.are.values.in.limit(server = matrix_a, encoded = matrix_b, limit = 1000))
  expect_error(.are.values.in.limit(server = matrix_a, encoded = matrix_c, limit = 1000))
  expect_error(.are.values.in.limit(server = matrix_b, encoded = matrix_c, limit = 1000))

  #compare by columns. So it is best to comvert to a vector
  expect_false(.are.values.in.limit(server = df_a, encoded = df_a, limit = 1000))
  expect_false(.are.values.in.limit(server = df_a, encoded = df_b, limit = 1000))
  expect_false(.are.values.in.limit(server = df_a, encoded = df_c, limit = 1000))
  expect_false(.are.values.in.limit(server = df_b, encoded = df_c, limit = 1000))

  #compare by elements of list.  So it is best to comvert to a vector
  expect_false(.are.values.in.limit(server = list_a, encoded = list_a, limit = 1000))
  expect_false(.are.values.in.limit(server = list_a, encoded = list_b, limit = 1000))
  expect_false(.are.values.in.limit(server = list_a, encoded = list_c, limit = 1000))
  expect_false(.are.values.in.limit(server = list_b, encoded = list_c, limit = 1000))
})


context("dsShareServer::isDataEncodeDS::expt::.convert.data")
test_that("incorrect argument ",
{
  expect_error(.convert.data())
})

test_that("incorrect argument ",
{
  expect_true(is.null(.convert.data(NULL)))
  expect_true(is.vector(.convert.data("hi")))
  expect_true(is.vector(.convert.data(3)))
  expect_true(is.vector(.convert.data(pi)))
  expect_true(is.vector(.convert.data(TRUE)))
  expect_true(is.vector(vector_a))
  expect_true(is.vector(.convert.data(vector_A)))
  expect_true(is.list(list_a))
  expect_true(is.vector(.convert.data(list_A)))
  expect_true(is.matrix(matrix_a))
  expect_true(is.vector(.convert.data(matrix_A)))
  expect_true(is.data.frame(df_a))
  expect_true(is.vector(.convert.data(df_a)))
})

context("dsShareServer::isDataEncodeDS::expt::.check.encoding")
test_that("incorrect argument ",
{
 expect_error(.check.encoding())
 expect_error(.check.encoding(A))
 expect_error(.check.encoding(A, B, C))
 expect_error(.check.encoding(vector_a))
})

test_that("expected outcome not restrictive",
{
 limit = 0.00001
 expect_false(.check.encoding(vector_a, vector_a, limit))
 expect_true(.check.encoding(vector_a, vector_b, limit))
 expect_false(.check.encoding(vector_a, vector_c, limit))

 a <- .convert.data(matrix_a)
 b <- .convert.data(matrix_b)
 c <- .convert.data(matrix_c)

 expect_false(.check.encoding(a, a, limit))
 expect_true(.check.encoding(a, b, limit))
 expect_false(.check.encoding(a, c, limit))

 a <- .convert.data(list_a)
 b <- .convert.data(list_b)
 c <- .convert.data(list_c)

 expect_false(.check.encoding(a, a, limit))
 expect_true(.check.encoding(a, b, limit))
 expect_false(.check.encoding(a, c, limit))

 a <- .convert.data(df_a)
 b <- .convert.data(df_b)
 c <- .convert.data(df_c)

 expect_false(.check.encoding(a, a, limit))
 expect_true(.check.encoding(a, b, limit))
 expect_false(.check.encoding(a, c, limit))

})

test_that("expected outcome restrictive",
{
  limit = 10000
  expect_false(.check.encoding(vector_a, vector_a, limit))
  expect_true(.check.encoding(vector_a, vector_b, limit))
  expect_false(.check.encoding(vector_a, vector_c, limit))

  a <- .convert.data(matrix_a)
  b <- .convert.data(matrix_b)
  c <- .convert.data(matrix_c)

  expect_false(.check.encoding(a, a, limit))
  expect_true(.check.encoding(a, b, limit))
  expect_false(.check.encoding(a, c, limit))

  a <- .convert.data(list_a)
  b <- .convert.data(list_b)
  c <- .convert.data(list_c)

  expect_false(.check.encoding(a, a, limit))
  expect_true(.check.encoding(a, b, limit))
  expect_false(.check.encoding(a, c, limit))

  a <- .convert.data(df_a)
  b <- .convert.data(df_b)
  c <- .convert.data(df_c)

  expect_false(.check.encoding(a, a, limit))
  expect_true(.check.encoding(a, b, limit))
  expect_false(.check.encoding(a, c,  limit))
})


context("dsShareServer::isDataEncodeDS::smk")
test_that("no arguments",
{
    expect_error(isDataEncodedDS())
})

context("dsShareServer::isDataEncodeDS::expt")
test_that("arguments are not correct",
{
  expect_error(isDataEncodedDS(data.server  = "D"))
  expect_error(isDataEncodedDS(data.encoded = "D"))
})

test_that("arguments are correct",
{
  expect_false(isDataEncodedDS(data.server  = "D", data.encoded = "E", data.held.in.server = "F"))
  expect_error(isDataEncodedDS(data.server  = "does not exist", data.encoded = "not yet created"))
  set.default.options.not.restrictive()
  expect_false(isDataEncodedDS(data.server  = "D", data.encoded = "E",data.held.in.server = "F"))
  expect_true(isDataEncodedDS(data.server  = "vector_B", data.encoded = "df_C",data.held.in.server = "F"))
})

if(FALSE)
{

test_that("expected outcome not restrictive",
{
  set.default.options.not.restrictive()
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_A"))
  expect_false(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_B"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "vector_C"))

  expect_false(isDataEncodedDS(data.server = "list_A", data.encoded = "list_A"))
  expect_false(isDataEncodedDS(data.server = "list_B", data.encoded = "list_B"))
  expect_false(isDataEncodedDS(data.server = "list_C", data.encoded = "list_C"))

  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_A"))
  expect_false(isDataEncodedDS(data.server = "df_B", data.encoded = "df_B"))
  expect_false(isDataEncodedDS(data.server = "df_C", data.encoded = "df_C"))

  expect_false(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_A"))
  expect_false(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_B"))
  expect_false(isDataEncodedDS(data.server = "matrix_C", data.encoded = "matrix_C"))

  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "matrix_A"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_A"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_A"))

  expect_false(isDataEncodedDS(data.server = "vector_B", data.encoded = "matrix_B"))
  expect_false(isDataEncodedDS(data.server = "vector_B", data.encoded = "list_B"))
  expect_true(isDataEncodedDS(data.server = "vector_B", data.encoded = "df_B"))

  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "matrix_C"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "list_C"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "df_C"))

  expect_true(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_B"))
  expect_true(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_C"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_C"))

  expect_true(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_B"))
  expect_true(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_C"))
  expect_false(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_C"))

  expect_true(isDataEncodedDS(data.server = "list_A", data.encoded = "list_B"))
  expect_true(isDataEncodedDS(data.server = "list_B", data.encoded = "list_C"))
  expect_false(isDataEncodedDS(data.server = "list_A", data.encoded = "list_C"))

  expect_true(isDataEncodedDS(data.server = "df_A", data.encoded = "df_B"))
  expect_true(isDataEncodedDS(data.server = "df_B", data.encoded = "df_C"))
  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_C"))

  expect_true(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_B"))
  expect_true(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_B"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_A"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_A"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_C"))
})

test_that("expected outcome not restrictive",
{
  set.default.options.restrictive()
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_A"))
  expect_false(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_B"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "vector_C"))

  expect_false(isDataEncodedDS(data.server = "list_A", data.encoded = "list_A"))
  expect_false(isDataEncodedDS(data.server = "list_B", data.encoded = "list_B"))
  expect_false(isDataEncodedDS(data.server = "list_C", data.encoded = "list_C"))

  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_A"))
  expect_false(isDataEncodedDS(data.server = "df_B", data.encoded = "df_B"))
  expect_false(isDataEncodedDS(data.server = "df_C", data.encoded = "df_C"))

  expect_false(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_A"))
  expect_false(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_B"))
  expect_false(isDataEncodedDS(data.server = "matrix_C", data.encoded = "matrix_C"))

  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "matrix_A"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "list_A"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_A"))

  expect_false(isDataEncodedDS(data.server = "vector_B", data.encoded = "matrix_B"))
  expect_false(isDataEncodedDS(data.server = "vector_B", data.encoded = "list_B"))
  expect_true(isDataEncodedDS(data.server = "vector_B", data.encoded = "df_B"))

  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "matrix_C"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "list_C"))
  expect_false(isDataEncodedDS(data.server = "vector_C", data.encoded = "df_C"))

  expect_true(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_B"))
  expect_true(isDataEncodedDS(data.server = "vector_B", data.encoded = "vector_C"))
  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "vector_C"))

  expect_true(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_B"))
  expect_true(isDataEncodedDS(data.server = "matrix_B", data.encoded = "matrix_C"))
  expect_false(isDataEncodedDS(data.server = "matrix_A", data.encoded = "matrix_C"))

  expect_true(isDataEncodedDS(data.server = "list_A", data.encoded = "list_B"))
  expect_true(isDataEncodedDS(data.server = "list_B", data.encoded = "list_C"))
  expect_false(isDataEncodedDS(data.server = "list_A", data.encoded = "list_C"))

  expect_true(isDataEncodedDS(data.server = "df_A", data.encoded = "df_B"))
  expect_true(isDataEncodedDS(data.server = "df_B", data.encoded = "df_C"))
  expect_false(isDataEncodedDS(data.server = "df_A", data.encoded = "df_C"))

  expect_false(isDataEncodedDS(data.server = "vector_A", data.encoded = "df_C"))
})
}
