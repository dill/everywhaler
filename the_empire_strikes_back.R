# try to understand measurements from the past...

# possible input types:
# - 5
# - 5'
# - 5'5
# - 5'7 1/2
# - 5'7\"
# - 5'8.5
# - 5'61/2\"
# - 6'0 1/2
# - 5'4 1/2 ?
# - 5'6.25
# - 4'0
# - 5-7.5
# - 5'91/2\" Lt
# - 5'5-7 1/2
# - 5'7 1/2l
# - 5'9'
# - 5'109
# - 6'-
# - 5 '
# - 5'8+
# - 5'+
# - 5' 5\" (2)
# -  5'?
# - 5 ft. 6in.
# - 5ft' 7 in.
# - 5 ft. 8 in
# - 5 ft 4 in.
# - 5 ft. 4 in.
# - 5'4'5
# - 5,' 0
# -  5'8
# - 5'5t 1/4
# - 5''11 1/2
# - 6 ' 11/2
# - 5-8.5
# - 6; 1/2

library(stringr)
library(convertr)

the_empire_strikes_back <- function(x){

  # somewhere to store everything
  x_new <- matrix(0, nrow=length(x), ncol=2)

  # kill the empty quotes
  x[x==""] <- NA
  x[x==" "] <- NA

  # get rid of trailing, preceeding whitespace
  x <- trimws(x)

  # clip other trailing weirdness if we can
  x <- sub("^(.+)\".*$", "\\1", x)
  x <- sub("^(.+'\\s+.+\").+$", "\\1", x)
  x <- sub("(.*)[\\+\\?\\-]$", "\\1", x)
  x <- sub("(.*)[:alpha:]+$", "\\1", x)

  # get rid of trailing, preceeding whitespace, again!
  x <- trimws(x)

  # some people think '' is the same as '
  x <- sub("^(\\d+)\\s?''", "\\1'", x)
  # some people think . is the same as '
  x <- sub("^(\\d+)\\s?\\.", "\\1'", x)
  # some people think - is the same as '
  x <- sub("^(\\d+)\\s?\\-", "\\1'", x)
  # some people think ; is the same as '
  x <- sub("^(\\d+)\\s?;", "\\1'", x)
  # some people think - is the same as '
  x <- sub("(.*)`$", "\\1\"", x)

  # extra spaces mid word
  x <- sub("\\s\\s+", " ", x)

  # single numbers (e.g., "5")
  ind <- grepl("^\\d+$", x)
  x_new[ind, 1] <- as.numeric(x[ind])

  # quote as foot symbol (e.g., "5'")
  ind <- grepl("^\\d+'$", x)
  x_new[ind, 1] <- as.numeric(sub("'", "", x[ind]))

  # quote as foot symbol with space (e.g., "5 '")
  ind <- grepl("^\\d+ '$", x)
  x_new[ind, 1] <- as.numeric(sub(" '", "", x[ind]))

  # feet and inches with ' and no " (e.g., "5'5" or "5'5.25")
  ind <- grepl("^\\d+\\s?'\\s?\\d+(.\\d+)?$", x)
  x_new[ind, ] <- str_split_fixed(x[ind], "'", 2)

  # feet and inches with , (e.g., "5,5")
  ind <- grepl("^\\d+,\\s?\\d+(.\\d+)?$", x)
  x_new[ind, ] <- str_split_fixed(x[ind], ",", 2)

  # just a space and no " or ' etc
  ind <- grepl("^\\d+ \\d+$", x)
  x_new[ind, ] <- str_split_fixed(x[ind], " ", 2)

  # with space and fractional inches
  ind <- grepl("^\\d+\\s?'\\s?\\d+ \\d+/\\d+\"?$", x)
  ft <- sub("^(\\d+)\\s?'\\s?\\d+ \\d+/\\d+\"?$", "\\1", x[ind])
  inch <- sub("^\\d+\\s?'\\s?(\\d+) (\\d+)/(\\d+)\"?$", "\\1+\\2/\\3", x[ind])
  inch <- sapply(inch, function(y) eval(parse(text=y)))
  if(sum(ind)>0){
    x_new[ind, ] <- cbind(ft, inch)
  }

  # with space and fractional inches, no whole inches
  ind <- grepl("^\\d+' \\d+/\\d+$", x)
  ft <- sub("^(\\d+)' \\d+/\\d+$", "\\1", x[ind])
  inch <- sub("^\\d+' (\\d+)/(\\d+)$", "\\1/\\2", x[ind])
  inch <- sapply(inch, function(y) eval(parse(text=y)))
  if(sum(ind)>0){
    x_new[ind, ] <- cbind(ft, inch)
  }

  # feet and inches with ' and escaped " (e.g., "5'5\"" or "5'5.25\"")
  ind <- grepl("^\\d+'\\s?\\d+(.\\d+)?\"$", x)
  spl <- str_split_fixed(x[ind], "'", 2)
  if(nrow(spl)!=0){
    x_new[ind, ] <- sub("\"\\s+", "", spl)
  }

  # feet ' and for some reason inches with ' (e.g., "5'4'")
  ind <- grepl("^\\d+'\\d+(.\\d+)?'$", x)
  spl <- str_split_fixed(x[ind], "'", 2)
  if(nrow(spl)!=0){
    x_new[ind, ] <- sub("'", "", spl)
  }

  # no space and fractional inches (?!) ("5'61/2\"", or "5'61/2")
  ind <- grepl("^\\d+'\\d+\\d+/\\d+(\")?$", x)
  ft <- sub("^(\\d+)'\\d+\\d+/\\d+(\")?$", "\\1", x[ind])
  inch <- sub("^\\d+'(\\d+)(\\d+)/(\\d+)(\")?$", "\\1+\\2/\\3", x[ind])
  inch <- sapply(inch, function(y) eval(parse(text=y)))
  if(sum(ind)>0){
    x_new[ind, ] <- cbind(ft, inch)
  }

  # no space and only fractional inches (?!) ("5'1/2")
  # only one way to have this be non-ambiguous (no whole inches)
  ind <- grepl("^\\d+'?\\s?\\d/\\d+(\")?$", x)
  ft <- sub("^(\\d+)'?\\s?\\d/\\d+(\")?$", "\\1", x[ind])
  inch <- sub("^\\d+'?\\s?(\\d)/(\\d+)(\")?$", "\\1/\\2", x[ind])
  inch <- sapply(inch, function(y) eval(parse(text=y)))
  if(sum(ind)>0){
    x_new[ind, ] <- cbind(ft, inch)
  }

  # feet and inches with ft and in etc
  ind <- grepl("^\\d+.?ft.? \\d+.?in.?$", x)
  ft <- sub("^(\\d+).?ft.? \\d+.?in.?$", "\\1", x[ind])
  inch <- sub("^\\d+.?ft.? (\\d+).?in.?$", "\\1", x[ind])
  if(sum(ind)>0){
    x_new[ind, ] <- cbind(ft, inch)
  }

  ## horrible corner cases

  # something like "5'4 3'4" which we'll interpret as 5' 4 3/4
  ind <- grepl("^\\d+'\\s?\\d+ \\d+'\\d+$", x)
  ft <- sub("^(\\d+)'\\s?\\d+ \\d+'\\d+$", "\\1", x[ind])
  inch <- sub("^\\d+'\\s?(\\d+) (\\d+)'(\\d+)$", "\\1+\\2/\\3", x[ind])
  inch <- sapply(inch, function(y) eval(parse(text=y)))
  if(sum(ind)>0){
    x_new[ind, ] <- cbind(ft, inch)
  }

  # something like "5'5' 1/2" which we'll interpret as 5' 5 1/2
  ind <- grepl("^\\d+'\\d+' \\d+/\\d+$", x)
  ft <- sub("^(\\d+)'\\d+' \\d+/\\d+$", "\\1", x[ind])
  inch <- sub("^\\d+'(\\d+)' (\\d+)/(\\d+)$", "\\1+\\2/\\3", x[ind])
  inch <- sapply(inch, function(y) eval(parse(text=y)))
  if(sum(ind)>0){
    x_new[ind, ] <- cbind(ft, inch)
  }


  mode(x_new) <- "numeric"

  # which didn't parse
  nogo <- apply(x_new, 1, function(x) any(is.na(x))) |
          apply(x_new, 1, function(x) all(x==0))
  if(sum(nogo)>0){
    warning(paste(sum(nogo), "measurements failed to parse."))
    x_new[nogo,] <- NA
  }

  feet <- convert(x_new[, 1], "ft", "m")
  inches <- convert(x_new[, 2], "in", "m")

  return(feet+inches)

}


testers <- c("5", "5'", "5'5", "5'7 1/2", "5'7\"", "5'8.5", "5'61/2\"", "6'0 1/2", "5'4 1/2 ?", "5'6.25", "4'0", "5-7.5", "5'91/2\" Lt", "5'5-7 1/2", "5'7 1/2l", "5'9'", "5'109", "6'-", "5 '", "5'8+", "5'+", "5' 5\" (2)", " 5'?", "5 ft. 6in.", "5ft' 7 in.", "5 ft. 8 in", "5 ft 4 in.", "5 ft. 4 in.", "5'4'5", "5,' 0", " 5'8", "5'5t 1/4", "5''11 1/2", "6 ' 11/2", "5-8.5", "6; 1/2")

tested <- the_empire_strikes_back(testers)
