# Boolean function that checks whether the arguments of the functions are suitable
# for the comparison. Check the type and server variables have been created
.are.params.correct <- function(data.server = NULL, data.encoded = NULL)
{
  outcome <- FALSE
  if(is.null(.Options$dsShareServer.near.equal.limit))
  {
    stop("SERVER::ERR:SHARE::003")
  }
  else if(is.character(data.server) & is.character(data.encoded))
  {

    outcome <- exists(data.server, where = 1)  &
               exists(data.encoded, where = 1) &
               !is.null(.Options$dsShareServer.near.equal.limit)
  }
  return(outcome)
}

# This helper function checks the two datasets are significantly the same, which is undesirable.
# It returns TRUE if it is signifincatly the same, and false if it is not.
.are.significant.same <- function(server, encoded)
{
  if (is.numeric(server) & is.numeric(encoded))
  {
    t    <- t.test(server, encoded, conf.level = 0.99)
    mann <- wilcox.test(server, encoded, conf.level = 0.99)
    return(t$p.value >= 0.01 || mann$p.value >= 0.01)
  }
  else
  {
    stop("SERVER::ERR:SHARE::004")
  }

}

# check the limits sets in the  opal server  are preserved through through both datasets
.are.values.in.limit <- function(server, encoded, limit)
{
   if(is.numeric(server) & is.numeric(encoded))
   {
     diff <- abs(summary(server) - summary(encoded))
     return(all(diff <= limit))
   }
   else
   {
     return(FALSE)
   }
}



# This helper function returns false if one of the check has failed. Otherwise, true if the data is sufficiently
# encoded.
# check no 1: identical - same R objects
# check no 2: all.equal - different R objects with same values.
# check no 3: some values from the servers are present in the encoded values
# chekc no 4: some values are not numeric
# check no 5: two datasets are significantly the same (t.test and mann.whitney, confidence level 0.99)
# check no 6: encoded values are still in the limits sets by the data governance
# check no 7: encoded values do not exceed the size of server data
.check.encoding <- function(server, encoded, limit)
{
  step            <- 0
  max             <- 6
  is.check.failed <- TRUE
  while (step <  max)
  {
    is.check.failed <- switch(step + 1,
                              identical(server, encoded), #identical variables
                              is.logical(all.equal(target = server, current = encoded)), #all equql R objects
                              any(server %in% encoded), #some values are present in both datasets
                              !is.numeric(encoded), #has some non-numeric values
                              .are.significant.same(server, encoded),
                              .are.values.in.limit(server, encoded, limit),
                              all(dim(server) >= dim(encoded)))

    step <- step + 1
    if (is.check.failed)
    {
      step <- step + 1000
    }
  }

  return(step == max)
}

# converts data into a vector
.convert.data <- function(data)
{

  if (is.data.frame(data))
  {
    matrix.conversion <- data.matrix(data, rownames.force = NA)
    colnames(matrix.conversion) <- NULL
    outcome <- c( matrix.conversion)
  }
  else if (is.list(data))
  {

    outcome <- unlist(data)
  }
  else if(is.matrix(data))
  {
    outcome <- c(data)
  }
  else
  {
    outcome <- data
  }

  return(outcome)
}

.check.in.limit <- function(server, encoded, limit)
{
  outcome <- TRUE
  #if(is.vector(server))
  #{


  #}


  #outcome  <- all(lapply(server, function(x) {.are.values.in.limit(x,encoded, 1000)}) == FALSE)
  return(outcome)
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
  param.correct <- .are.params.correct(data.server = data.server, data.encoded = data.encoded)

  if(param.correct)
  {

    # get data from global environment
    server   <- .convert.data(get(data.server,  pos = 1))
    encoded  <- .convert.data(get(data.encoded, pos = 1))
    limit    <- getOption("dsShareServer.near.equal.limit")
    #outcome  <- all(lapply(server, function(x) {.are.values.in.limit(x,encoded, 1000)}) == FALSE)
    outcome  <- .check.encoding(server, encoded, limit)

  }
  else
  {
    stop("SERVER::ERR:SHARE::002")
  }
  return(outcome)
}
