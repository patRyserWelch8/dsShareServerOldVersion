# Boolean function that checks whether the arguments of the functions are suitable
# for the comparison. Check the type and server variables have been created
.are.params.correct <- function(data.server = NULL, data.encoded = NULL)
{
  outcome <- FALSE
  if(is.character(data.server) & is.character(data.encoded))
  {
    outcome <- exists(data.server, where = 1)  &
               exists(data.encoded, where = 1) &
               !is.null(.Options$dsShareServer.near.equal.limit)
  }
  return(outcome)
}

# check the values are similar; either identitical or nearly equal.
.are.similar <- function(server, encoded)
{

  #check for identical values
  is.identical    <- identical(x = server, y = encoded)
  is.nearly.equal <- FALSE

  if (!is.identical)
  {
    results <- all.equal(target = server, current = encoded)
    if(is.logical(results))
    {
      is.nearly.equal <- TRUE
    }
    else
    {
      results         <- unlist(strsplit(results, " "))
      rel.diff        <- as.numeric(results[length(results)])
      print(rel.diff)
      limit           <- getOption("dsShareServer.near.equal.limit")
      is.nearly.equal <- (rel.diff > limit)
    }
  }

  return(is.identical || is.nearly.equal)
}

# check two data frames are not the same
.are.data.frames.the.same <- function(server, encoded)
{
  if(is.data.frame(server) & is.data.frame(encoded))
  {

  }
}

# check a column

#'@name isDataEncodedDS
#'@param data.server  - character argument representing the name of R object with the original data
#'@param data.encoded - character argument representing the name of R object with the encoded data
#'@param data.encoded
#'@export

isDataEncodedDS <- function(data.server = NULL, data.encoded = NULL)
{
  outcome <- FALSE
  if(.are.params.correct(data.server = data.server, data.encoded = data.encoded))
  {
    # get data from global environment
    server  <- get(data.server,  pos = 1)
    encoded <- get(data.encoded, pos = 1)
    print(.are.similar(server = server, encoded =encoded))
    if (!.are.similar(server = server, encoded =encoded))
    {
      outcome <- TRUE
    }
  }
  return(outcome)
}
