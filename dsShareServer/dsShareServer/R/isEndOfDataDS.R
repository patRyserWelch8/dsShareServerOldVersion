
# check the sharing for the following criteria:
# 0. sharing is allowed
# 1. the encoded data is the same as previously stated in the encoding check
# 2. encoded data exists
# 3. encoded data is a data frame
.are.settings.suitable <- function(data.encoded)
{
  outcome <- FALSE
  if(is.sharing.allowed())
  {
    settings  <- get("settings", pos = 1)
    same.name <- identical(settings$encoded.data.name,data.encoded)

    data.exists <- exists(data.encoded, where = 1)
    if(!data.exists)
    {
      stop("SERVER::ERR:SHARE::009")
    }

    correct.format <- is.data.frame(get(data.encoded, pos = 1))
    if(!correct.format)
    {
      stop("SERVER::ERR:SHARE::005")
    }

    same.name <- identical(settings$encoded.data.name,data.encoded)
    if(!same.name)
    {
      stop("SERVER::ERR:SHARE::008")
    }



    outcome <- same.name & data.exists & correct.format
  }
  else
  {
    stop("SERVER::ERR:SHARE::002")
  }
  return(outcome)
}

#'@name isEndOfDataDS
#'@export
#'
isEndOfDataDS <- function(data.encoded = NULL)
{
  outcome <- TRUE
  if(is.character(data.encoded))
  {
    can.continue <- .are.settings.suitable(data.encoded)
  }
  else
  {
    stop("SERVER::ERR:SHARE::010")
  }

  return(outcome)
}
