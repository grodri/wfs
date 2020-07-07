#' Value labels
#'
#' Called internally to retrieve the value labels associated with a variable
#'
#' @param varname A required string with the name of the variable, for example "v010"
#' @param dct Required, a WFS dictionary as returned by the \code{wfs_dct()} function
#' @return A named integer vector with the value labels, or NULL if no labels defined
#'
#' @section Details
#' The code first builds a chain of value-label back-references, and then traverses
#' them in dictionary order, updating labels as found. Non-integer values which result
#' in \code{NA}, including \code{#} and \code{BLK}, are ignored.
#' @noRd
#'
wfs_value_labels <- function(varname, dct) {

  # build chain of back references that actually define labels
  map <- attr(dct, "map")
  loc <- map[varname]
  chain <- NULL
  while(loc > 0) {
      if(loc < length(dct) && substr(dct[loc+1], 1, 1) == " ") {
        chain <- c(chain, loc)
      }
      backref <- stringr::str_to_lower(stringr::str_trim(substr(dct[loc], 67, 72)))
      if(backref != "") {
        loc <- map[backref]
      }
      else loc <- 0
  }
  if(length(chain) < 1) return(NULL)

  # now build hash with value labels replacing existing values
  hash <- NULL
  for(i in length(chain):1){
    loc <- chain[i]
    while(loc + 1 < length(dct) && substr(dct[loc+1], 1, 1) == " ") {
      loc <- loc + 1
      value <- stringr::str_trim(substr(dct[loc], 36, 39))
      # label only integer values
      if(stringr::str_detect(value, "^\\d+$")) {
        label <- stringr::str_trim(substr(dct[loc], 44, 63))
        hash[value] <- label
      }
    }
  }
  if(length(hash) < 1) return(NULL)

  # return as named integer vector
  sort(structure(as.integer(names(hash)), names = hash))

}
