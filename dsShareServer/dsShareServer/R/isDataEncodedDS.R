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

.check.encoding <- function(server, encoded)
{
  step    <- 1
  max     <- 1
  while (step <=  max)
  {

    print(step)
    is.check.failed <- switch(step,
                              identical(server, encoded))
                      #.are.similar(server, encoded))

    if (is.check.failed)
    {
      step <- step + 1000
    }
    else
    {
      step <- step + 1
    }
  }
  return(step == (max+1))
}

# check a column

#'@name isDataEncodedDS
#'@param data.server  - character argument representing the name of R object with the original data
#'@param data.encoded - character argument representing the name of R object with the encoded data
#'@param data.encoded
#'@export

isDataEncodedDS <- function(data.server = NULL, data.encoded = NULL)
{
  outcome  <- FALSE
  step     <- 0
  print(1)
  if(.are.params.correct(data.server = data.server, data.encoded = data.encoded))
  {
    print(21)
    # get data from global environment
    server   <- get(data.server,  pos = 1)
    encoded  <- get(data.encoded, pos = 1)
    outcome  <- .check.encoding(server, encoded)
    print("---------")
    print(outcome)
  }
  return(outcome)
}
