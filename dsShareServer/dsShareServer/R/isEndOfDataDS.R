

#'@name isEndOfDataDS
#'@export
#'
isEndOfDataDS <- function(data_encoded = NULL)
{
  outcome <- TRUE
  arg.and.settings.suitable <- are.arg.and.settings.suitable(data_encoded)
  if(arg.and.settings.suitable)
  {
    settings <- get("settings", pos = 1)
    transfer <- get.transfer(settings)
    outcome  <- transfer[[settings$current_row]] >= nrow(get(data_encoded,pos = 1))
  }
  return(outcome)
}
