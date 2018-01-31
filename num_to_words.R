

numbers <- function(p) {
# Converts a number into a word numeral
# 
# @param p The input can be numeric (1234) or a string, 
# with ("1,234") or without commas ("1234")
  
  # Replaces numeral in the ones place with word version
  # @param x A single digit, can be numeric or a string
  ones <- function(x) {
    ifelse(as.numeric(x) == 0, "",  # if 0 just leave blank
           switch(as.numeric(x),
                  "one", "two", "three",
                  "four", "five", "six",
                  "seven", "eight", "nine"))
  }
  
  # Replaces numeral in tens place with word version
  # @param x A single digit, can be numeric or a string
  tens <- function(x) {
    ifelse(as.numeric(x) == 0, "",  # if 0 just leave blank
           switch(as.numeric(x),
                  "ten", "twenty", "thirty",
                  "forty", "fifty", "sixty",
                  "seventy", "eighty", "ninety"))
  }
  
  # Replace numeral in hundreds place with digit + "hundred"
  # @param x A single digit, can be numeric or a string
  hundreds <- function(x) {
    ifelse(as.numeric(x) == 0, "",  # if 0, leave blank
           paste(ones(x), "hundred"))
  }
  
  # vector of numbers that need converted to their "-teen" version
  from <- c("ten one", "ten two", "ten three", 
            "ten four", "ten five", "ten six",
            "ten seven", "ten eight", "ten nine")
  # list of numerals to match with "from" vector
  to <- c("eleven", "twelve", "thirteen", 
          "fourteen", "fifteen", "sixteen",
          "seventeen", "eighteen", "nineteen")
  
  # remove commas if a string w/ commas is given
  p <- gsub(",", "", p)
  # split number into vector of single digits
  p <- unlist(strsplit(p, ""))
  
  # run "ones" function on 1st digit, every 3rd digit after that
  p[seq(length(p), 1, -3)] <- sapply(seq(length(p), 1, -3), 
                                     function(x) ones(p[x]))
  # run "tens" function on 2nd digit, every third digit after
  p[seq(length(p) - 1, 1, -3)] <- sapply(seq(length(p) - 1, 1, -3), 
                                         function(x) tens(p[x]))
  # run "hundreds" function on 3rd digit, every third digit after
  p[seq(length(p) - 2, 1, -3)] <- sapply(seq(length(p) - 2, 1, -3), 
                                         function(x) hundreds(p[x]))
  
  # add "thousand," after the number in the 1,000s place if it exists
  p <- if (length(p) > 3) {
    append(p, "thousand,", after = (length(p) - 3))
  } else {
    p
  }
  
  # likewise for number in 1,000,000s place
  p <- if (length(p) >= 8) {
    append(p, "million,", after = (length(p) - 7))
  } else {
    p
  }
  
  # remove potential extra spaces between words
  p <- gsub("\\s+", " ", paste(p, collapse = " "))
  
  # replace number words in "from" list with matching word in "to" list
  for(i in 1:length(from)) { 
    p <- gsub(from[i], to[i], p)
  }
  
  # remove "thousand" if the thousands are all zeroes/empty
  p <- gsub(", thousand,", ",", p)
  
  print(p)
}



