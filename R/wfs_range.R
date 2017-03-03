# expand a range of variables given dictionary map
#
wfs_range <- function(range, map) {
  errmsg <- paste("invalid rage", range)
  if(length(range) != 2 || any(is.na(map[range]))) {
    stop(errmsg)
  }
  bot <- which(map == map[range[1]])
  top <- which(map == map[range[2]])
  if(bot > top ) {
    stop(errmsg)
  }
  map[bot:top]
}
