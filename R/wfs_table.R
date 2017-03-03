# Thoughts for consideration. A WFS wrapper for table to add percents and,
# more importantly, a variable label. Defines its own print method.

wfs_table <- function(dataframe, varname) {
  j <- which(names(dataframe) == varname)
  tbl <- table(dataframe[,j])
  attr(tbl,"variableLabel") <- attr(dataframe,"variableLabels")[j]
  class(tbl) <- c("wfsTable", class(tbl))
  tbl
}

print.wfsTable <- function(object) {
  label <- attr(object, "variableLabel")
  cat(label,"\n")
  df<-as.data.frame(object)
  df$Pct <- round(100 * df$Freq/sum(df$Freq), 1)
  names(df)[1] <- "Category"
  print.data.frame(df)
}
