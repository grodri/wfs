# Expand a comma-separated list of variables given raw dictionary
#
wfs_varlist <- function(varlist, dct) {
  map = attr(dct, "map")
  # all
  if(varlist == "all") {
    return(map)
  }
  # list
  groups <- stringr::str_trim(unlist(stringr::str_split(varlist, ",")))
  vars <- NULL
  for(g in groups) {
    group = g
    # shortcuts
    if(group == "births" || group == "unions") {
      if(group == "births") group = "b011-b245"
      else group <- "m011-m084"
    }
    # range
    range <- unlist(stringr::str_split(group, "-"))
    if(length(range) > 1) {
      vars = c(vars, wfs_range(range, map))
    }
    # wildcards
    else if(stringr::str_detect(group, "[*?]+")) {
      vars = c(vars, wfs_wild(group, map))
    }
    # name
    else {
      if(is.na(map[group])) {
        stop("variable " + group + " not found")
      }
      vars = c(vars, map[group])
    }
  }
  vars
}
