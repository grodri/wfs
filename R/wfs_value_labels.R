# value labels, following chain of back-references
#
wfs_value_labels <- function(varname, dct) {
  # build chain of back references
  map <- attr(dct, "map")
  loc <- map[varname]
  chain <- NULL
  while(loc > 0) {
      if(substr(dct[loc+1], 1, 1) == "*") {
        chain <- c(chain, loc)
      }
      backref <- stringr::str_to_lower(stringr::str_trim(substr(dct[loc], 67, 72)))
      if(backref != "") {
        loc <- map[ref]

      }
      else loc <- 0
  }
  # now build the value labels replacing existing values
  hash <- NULL
  if(length(chain) < 1) return(hash)
  for(i in length(chain):1){
    loc <- chain[i]
    while(loc + 1 < length(dct) && substr(dct[loc+1], 1, 1) == " ") {
      loc <- loc + 1
      value <- as.numeric(substr(dct[loc], ?, ?))
      label <- stringr::str_trim(substr(dct[loc], ?, ?))
      hash[value] <- label
    }
  }
  hash
}
