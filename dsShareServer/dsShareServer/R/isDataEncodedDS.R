# Boolean function that checks whether the arguments of the functions are suitable
# for the comparison. Check the type and server variables have been created as data frame. Tibbles wrappes data frames.
.are.params.correct <- function(data.server = NULL, data.encoded = NULL, data.held.in.server = NULL)
{
  outcome <- FALSE
  if(is.null(.Options$dsShareServer.near.equal.limit))
  {
    stop("SERVER::ERR:SHARE::003")
  }
  else if(is.character(data.server) & is.character(data.encoded) & is.character(data.held.in.server))
  {
    if(exists(data.server, where = 1)  & exists(data.encoded, where = 1) & exists(data.held.in.server, where = 1))
    {
       server         <- get(data.server, pos = 1)
       encoded        <- get(data.encoded, pos = 1)
       held.in.server <- get(data.held.in.server, pos = 1)
       outcome        <- is.data.frame(server) || is.data.frame(encoded) || is.data.frame(held.in.server)
    }
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
# check no 5: has values are NAs
# check no 6: has some null values
# check no 5: two datasets are significantly the same (t.test and mann.whitney, confidence level 0.99)
# check no 7: encoded values are still in the limits sets by the data governance
# check no 8: encoded values do not exceed the size of server data
# check no 9: some of the data are present in one
.is.encoded <- function(server, encoded, limit)
{
  step            <- 0
  max             <- 9
  is.check.failed <- TRUE

  server.data     <- .convert.data(server)
  encoded.data    <- .convert.data(encoded)
  print(length(server.data))
  print(length(encoded.data))

  while (step <  max)
  {
    is.check.failed <- switch(step + 1,
                              identical(server.data, encoded.data), # 1 identical variables
                              is.logical(all.equal(target = server.data, current = encoded.data)), # 2all equql R objects
                              any(server.data %in% encoded.data), #3 some values are present in both datasets
                              !is.numeric(encoded.data), #4 has some non-numeric values
                              any(is.na(server.data)) || any(is.na(encoded.data)), #5 some values are nas
                              any(is.null(server.data)) || any(is.null(encoded.data)), #6
                              .are.significant.same(server.data, encoded.data), #7 data are significantly the same at the point of centrality
                              .are.values.in.limit(server.data, encoded.data, limit), #8 data are with the limit min, max, mean, median, IQR
                              length(server.data) >= length(encoded.data)) #9 the size data on the server are greater or the same as the encoded data

    step <- step + 1
    if (is.check.failed)
    {
      step <- step + 1000
    }
  }
  return(step == max)
}

.check.encoding.data.server <- function(server, encoded, limit)
{
  is.encoded      <- TRUE
  classes_server  <- lapply(server,class)
  classes_encoded <- lapply(encoded, class)

  for(i in 2:ncol(encoded))
  {
      print(paste0 ("----- i ", i, classes_encoded[[i]], print(" ----")))
      for(j in 2:ncol(server))
      {
         print(paste0 ("j ", j, classes_encoded[[j]]))
         if(grepl(classes_server[[i]], classes_encoded[[j]]))
         {
            is.encoded <- .is.encoded(encoded[i],server[j],limit)
            if(!is.encoded)
            {
              break
            }
         }
      }
  }
  return(is.encoded)
}

# converts data into a vector of numbers.
.convert.data <- function(data)
{

  if (is.data.frame(data))
  {
    #if data frame has some mixed data types the data may not be converted as expected. i.e; character becomes numerical values
    matrix.conversion <- data.matrix(data, rownames.force = NA)
    colnames(matrix.conversion) <- NULL
    outcome <- c(matrix.conversion)
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


# check a column

#'@name isDataEncodedDS
#'@param data.server  - character argument representing the name of a data frame with the original data
#'@param data.encoded - character argument representing the name of a data frame with the encoded data
#'@param data.held.in.server - character argument representing the datasets held in a DataSHIELD server as a data frame
#'@export
isDataEncodedDS <- function(data.server = NULL, data.encoded = NULL, data.held.in.server = NULL)
{
  is.encoded.data      <- FALSE
  is.encoded.variable  <- FALSE
  param.correct        <- .are.params.correct(data.server, data.encoded, data.held.in.server )


  if(param.correct)
  {
    # get data from global environment
    server         <- get(data.server,  pos = 1)
    encoded        <- get(data.encoded, pos = 1)
    held.in.server <- get(data.held.in.server, pos = 1)
    limit          <- getOption("dsShareServer.near.equal.limit")

    is.encoded.variable <- .is.encoded(server, encoded, limit)
    if(is.encoded.variable)
    {
      is.encoded.data <- .check.encoding.data.server(held.in.server,encoded,limit)
    }
  }
  else
  {
    stop("SERVER::ERR:SHARE::002")
  }
  return(is.encoded.data & is.encoded.variable)
}
