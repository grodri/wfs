# idea is that a variables dataframe will have a column named labelset
# pointing to a varname in the value labels dataframe
# To get the labels for any values that are actually labeled we do a left join

# written as a separate function to help debugging

wfs_value_labels_df <- function(varlist, dct) {
  # construct the value labels dataset
  df <- data.frame(variable = character(0), value = numeric(0), label = character(0))

  varlocs <- wfs_varlist(varlist, dct)
  cat("got varlocs\n")
  varnames <- names(varlocs)
  recs <- dct[varlocs]
  for(i in 1:length(recs)) {
    cat(varnames[i],"\n")
    loc <- varlocs[i]
    labels <- NULL
    # own labels (merged)
    if(loc < length(dct) && substr(dct[loc + 1], 1, 1) == " ") {
      labels <- cbind( variable = varnames[i], wfs_value_labels(varnames[i], dct))
    }
    # back reference
    else if ((ref <-str_trim(substr(dct[loc], 67, 72))) != "") {
      if(length(labels$variable == ref) == 0) {
        labels <- cbind( variable = ref, wfs_label_extract(map[ref], dct))
      }
    }
    if(!is.null(labels)) {
      df <- rbind(df, labels)
    }
  }
  df
}
