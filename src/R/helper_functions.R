
# Helper functions --------------------------------------------------------

dollarToNumber_vectorised <- function(vector) {
  # Want the vector as character rather than factor while
  # we're doing text processing operations
  vector <- as.character(vector)
  vector <- gsub("(\\$|,)","", vector)
  # Create a numeric vector to store the results in, this will give you
  # warning messages about NA values being introduced because the " K" values
  # can't be converted directly to numeric
  result <- as.numeric(vector)
  # Find all the "$N K" values, and modify the result at those positions
  k_positions <- grep(" K", vector)
  result[k_positions] <- as.numeric(gsub(" K","", vector[k_positions])) * 1000
  # Same for the "$ M" value
  m_positions <- grep(" M", vector)
  result[m_positions] <- as.numeric(gsub(" M","", vector[m_positions])) * 1000000
  return(result)
}
