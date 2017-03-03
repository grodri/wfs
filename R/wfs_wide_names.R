# generate names of variables in union or birth history
#
wfs_wide_names <- function(prefix, maxcount, nvars) {
  top <- min(maxcount, 9)
  events <- paste(prefix, "0", 1:top, sep="")
  if(maxcount > 10) events <- c(events, paste(prefix, 10:maxcount, sep = ""))
  paste(rep(events, rep(nvars, maxcount)), 1:nvars, sep="")
}
