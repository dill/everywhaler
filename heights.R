# code generously donated by James Curran
# original code at:
#  https://gist.github.com/jmcurran/36734b7d62d4995e0b16ee4b2b7b4075
library(stringr)

im_your_father_luke <- function(h){
  # there is a lot of code dealing with NAs here because the example data set
  # had them it would probably be cleaner if we didn't bother

  ## clean out a bunch of initial guff
  h <- gsub("[\"\\-]", "", h)
  h <- gsub("^ *$", "", h)
  h <- gsub("^(5|6)'([1-9]|10|11)*([13]\\/[248]).*$", "\\1'\\2 \\3", h)
  h <- gsub("^5'5(7|9)(.*$)", "5'\\1\\2", h)

  ## This is the workhorse regexp  
  pattern <- "^ *([1-7]) *(ft[.']?|'{1,2}|,'?|;|\\.)? *([0-9]{1,2})? *(['t]|in\\.*)?([ ]+(1\\/2|([12])\\/3|(1|3)\\/4|([15])\\/6|([1-7])\\/8|1\\/12)|(\\.?[0-9]{1,3}))?[ +\\?`l (2)]*$"

  ## produces a matrix with 11 columns
  ## column 2 should have the feet, column 4 should have the inches,
  ## columm 6 will have the fraction if there is one,
  ## column will have the decimal if there is one
  m <- str_match(h, pattern)

  ## helper function to evaluate the fractions
  convertFracs <- function(x){
    if(is.na(x)){
      return(NA)
    }else{
      return(25.4 * eval(parse(text=x)))
    }
  }


  ## covert the feet, inches and fraction into millimetres
  mm <- cbind(as.numeric(m[,2])*12*25.4, as.numeric(m[,4])*25.4,
              apply(cbind(sapply(m[,6], convertFracs, USE.NAMES = FALSE),
                          sapply(m[,11], convertFracs, USE.NAMES = FALSE)),
                    1, function(row){
               if(all(is.na(row))){
                 return(NA)
               }else if(all(!is.na(row))){
                 return(-1)
               }else if(is.na(row[1]) & !is.na(row[2])){
                 return(row[2])
               }else{
                return(row[1])
               }
             }))

  ## appropriate columns to get a single figure
  toMM <- function(row){
    if(all(is.na(row))){
      return(NA)
    }else if(!is.na(row[1]) & is.na(row[2]) & is.na(row[3])){
      return(row[1])
    }else if(!is.na(row[1]) & !is.na(row[2]) & is.na(row[3])){
      return(row[1] + row[2])
    }else if(!is.na(row[1]) & is.na(row[2]) & !is.na(row[3])){
      return(row[1] + row[3])
    }else{
      return(sum(row))
    }
  }

  return(data.frame(input = h,
                    output = apply(mm, 1, toMM),
                    stringsAsFactors=FALSE))
}

#DLMTestCases = read.csv("DLMTest.csv", stringsAsFactors = FALSE)
hh <- read.csv("crewlist.csv", stringsAsFactors=FALSE)
hh <- hh$Height

# correct the ones that go wrong later
hh[5203] <-   "6' 1 1/2"
hh[38007] <-  "5' 6 3/4"
hh[80343] <-  "5' 10 3/4"
hh[94667] <-  "5' 4 3/4"
hh[111637] <- "6' 1 1/2"



hh <- im_your_father_luke(hh)
#fails = DLMTestCases[abs(DLMTestCases$mm - mine$output) > 0.001,] 

# what goes wrong?
# tt <- mine[is.na(mine[,2]),]
# tt[which(!(tt[,1]=="" & is.na(tt[,2]))),]
#            input output
# 5203     6/1 1/2     NA
# 38007   5'6 33/4     NA
# 80343  5'10 3/34     NA
# 94667    5'4 3'4     NA
# 111637  6 ' 11/2     NA
