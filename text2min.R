### function for converting open ended text into minutes ###
text2min <- function(temp) {
  # import library
  library(stringr)
  library(dplyr)
  library(stringdist)
  library(lubridate)
  
  # convert to character
  temp <- as.character(temp)
  
  # split numbers and characters
  temp_num = vector(, length(temp))
  for (i in 1:length(temp)) {
    temp_num[i] <- str_extract(temp[i], "\\(?[0-9,.,:]+\\)?")[[1]]  # create list with numbers
  }
  temp_chr <- gsub("[[:digit:]]", "", temp)  # create list with characters
  
  # convert to data frames and merge
  temp_num <- as.data.frame(temp_num)
  temp_num$id <- seq.int(nrow(temp_num))
  temp_chr <- as.data.frame(temp_chr)
  temp_chr$id <- seq.int(nrow(temp_chr))
  temp_two <- full_join(temp_num, temp_chr)
  
  # convert to all lowercase
  temp_two$temp_chr <- tolower(temp_two$temp_chr)
  
  for (i in 1:length(temp_two$temp_chr)){
    row <- strsplit(temp_two$temp_chr[i], "[[:space:]]")  # split into words
    
    # determine whether in hours or minutes
    # deal with spelling errors by selecting the word in the string that is most similar to hours or minutes
    temp_two$hours[i] <- amatch("hour", unlist(row), maxDist = 3)
    temp_two$minutes[i] <- amatch("minute", unlist(row), maxDist = 3)
    temp_two$mins[i] <- amatch("min", unlist(row), maxDist = 1)
  }
  
  temp_two <- temp_two %>% mutate(hrvsmn = ifelse(is.na(hours) & is.na(minutes) > 0, NA,
                                                  ifelse(is.na(hours), 1,
                                                         ifelse(is.na(minutes) | is.na(mins), 60, NaN))))
  
  temp_two$hours <- NULL
  temp_two$minutes <- NULL
  temp_two$mins <- NULL
  
  # deal with digits in minutes and hours
  temp_two <- temp_two %>% mutate(temp_num2 = ifelse(grepl(":", as.character(temp_num)), 
                                                     (as.numeric(ms(as.character(temp_num))) / hrvsmn), 
                                                     as.numeric(as.character(temp_num))))
  
  # deal with situations where there are characters preceding the numbers
  nd <- read.csv("num_dict.csv", header = 0)  # read in a dictionary for comparison
  
  for (i in 1:length(temp_two$temp_chr)){
    if (is.na(temp_two$temp_num2[i])){
      row <- strsplit(temp_two$temp_chr[i], "[[:space:]]")  # split into words
      
      # check whether any of the words in nd[1] is near enough words where temp_num is NA
      num_ver <- amatch(unlist(nd[1]), unlist(row), maxDist = 1)
      num_val <- nd[which(!is.na(num_ver)), 2]
      temp_two$temp_num2[i] <- ifelse(length(num_val) == 0, NA, num_val)
      # fill in the value from nd[2]
      # temp_num[i] <- nd[num_loc, 2]
    }

  }
  
  # convert hours to minutes by multiplying by 60
  temp_two <- temp_two %>% mutate(minutes = temp_num2 * hrvsmn)
  
  # remove extra variables = necessary?
  rm(i, temp_num, temp_chr, nd, row)
  
  # return the number of minutes
  return(temp_two$minutes)
}
