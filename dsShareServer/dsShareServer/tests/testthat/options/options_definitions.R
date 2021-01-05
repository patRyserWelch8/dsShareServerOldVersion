# These function are required to test locally the use of options. Those
# are used
# Not recommended
set.default.options.not.restrictive <- function()
{
  options(dsShareServer.near.equal.limit = 0.01)
}

set.default.options.restrictive <- function()
{
  options(dsShareServer.near.equal.limit = 1000000)
}

set.default.options.to.null <- function()
{
  options(dsShareServer.near.equal.limit = NULL)
}
