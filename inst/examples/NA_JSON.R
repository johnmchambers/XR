xi = 1:3
xi[[2]] = NA
toJSON(xi) # uses "NA"
xiNull <- "[ 1, null, 3]"
## xiNULL and toJSON(xi) produce the same result
identical(xi, fromJSON(xiNull))
# null is already used for string NA's
xc = c("NA", "NB", NA)
toJSON(xc)
