# Gary Koplik
# December, 2016
# get_xml.r


# input a 1 column data frame of posixct dates
#   that contains the auction dates of the security of interest
# you will have to specify the security as well
# Goes to treasurydirect.gov, downloads, and parses the xml files
#   into one data frame
# saves two csv files in the same directory path as this document
#   one with the treasury data
#   the other with the dates of auctions that failed to be found

# load libraries -- install them if needed
library(zoo)
library(lubridate)
library(XML)
library(xml2)
library(gtools)

# set threshold for number of auctions to check on a given day
tooManyAuctions <- 4

# create data frame to hold the dates that couldn't be downloaded
failedDates <- data.frame(data = "No failed dates", stringsAsFactors = FALSE)
colnames(failedDates) <- "Failed Dates"

# store a global index value for adding screw up dates to failedDates data frame
failedDates_index <- 1

#### get the URLs to download  ####

# takes a single date in posixct format
# and puts it into url form
# allows number specification (auctionNum) because
#   websites base url on number of treasury auctions that day
makeURL <- function(date, auctionNum){
  date_for_url <- paste(substr(date, 1, 4),
                        substr(date, 6, 7),
                        substr(date, 9, 10), sep = "")
  
  url <- paste("https://www.treasurydirect.gov/xml/R_",
                date_for_url,
                "_",
                auctionNum,
                ".xml", sep = "")
  return(url)
}
# read the xml file's url (specified as string)
# returns the XML object
getData <- function(website){
  # download the XML file from its URL
  #   catch errors if no auctions that day
  result <- tryCatch({
    auctionFile <- read_xml(website)
    # parse the XML file
    data <- xmlParse(auctionFile)
    # put into data frame
    data_asframe <- xmlToDataFrame(data)
    # this results in 2 rows instead of 1:
    
    #   offering data ... NA NA NA NA ...
    #   NA NA NA NA ... results data
    
    # this is a function of the attributes being nested in 2 parts
    #   AuctionAnnouncement and AuctionResults
    # this will be consistent throughout these XML files
    #   => fill in the top values into the bottom row
    #       and only keep that tuple
    data_asframe_onerow <- na.locf(data_asframe)[ 2, ]
    return(data_asframe_onerow)
  }, error = function(err){
    print(err)
    print("You had an error. Chances are, there was no auction this day, and therefore your date must be wrong")
    data_asframe_onerow <- NULL
    return(data_asframe_onerow)
  }
    )
}

# checks to see if the data downloaded is in fact the correct security
#   specified as string actualSecurity
# (since the securities auctioned on the same day are only specified
#   by the order auctioned on the day
#   individual securities' downloading URL numbers are inconsistent)
# also checks if TIPS or FRN (specified with "Y" if true)
checkData <- function(justDownloaded, actualSecurity, TIPS, FRN){
  if (justDownloaded$InflationIndexSecurity == TIPS) {
    # some securities are a few months shorter in lifetime
    #   => need to not exclude these exceptions
    #   10-year notes: 9 or 10 years
    #   30-year bonds: 29 or 30 years
    #   5-year TIPS:   4 or 5 years
    #   10-year TIPS:  9 or 10 years
    #   30-year TIPS:  29 or 30 years
    #   2-year FRN:    1 or 2 years
    print("justDownloaded vs actualSecurity")
    print(justDownloaded[, 1])
    print(actualSecurity)
    # FRN column does not exist before roughly 2014
    if(FRN == "Y" && ("FloatingRate" %in% names(justDownloaded)) == FALSE){
      return(FALSE)
    }
    if(justDownloaded$FloatingRate == FRN ||
       ("FloatingRate" %in% names(justDownloaded)) == FALSE){
      if(justDownloaded[, 1] == actualSecurity){
        return(TRUE)
      }
      else if(actualSecurity == "10-YEAR" &&
              justDownloaded[, 1] == "9-YEAR"){
        return(TRUE) 
      }
      else if(actualSecurity == "30-YEAR" &&
              justDownloaded[, 1] == "29-YEAR"){
        return(TRUE) 
      }
      else if(actualSecurity == "5-YEAR" &&
              TIPS == "Y" &&
              justDownloaded[, 1] == "4-YEAR"){
        return(TRUE) 
      }
      else if(actualSecurity == "10-YEAR" &&
              TIPS == "Y" &&
              justDownloaded[, 1] == "9-YEAR"){
        return(TRUE) 
      }
      else if(actualSecurity == "30-YEAR" &&
              TIPS == "Y" &&
              justDownloaded[, 1] == "29-YEAR"){
        return(TRUE) 
      }
      else if(actualSecurity == "2-YEAR" &&
              FRN == "Y" &&
              justDownloaded[, 1] == "1-YEAR"){
        return(TRUE) 
      }
      
      else {
        print(c("actualSecurity: ", actualSecurity))
        print(c("perceived security: ", justDownloaded[ , 1]))
        return(FALSE)
      }
    }
  }
  
}

# finds the first correct auction
# reads in the column of dates
# returns the index of the first correct auction as well as the auction
#   as a data frame with 2 rows
#   the first row is the row of data
#   the second row only contains the index of the first date as the first value [2, 1]
getFirstAuction <- function(securityDates, firstDate, actualSecurity, TIPS, FRN){
  # create a boolean for while loop
  bool <- TRUE
  # set an auctionNum variable to increment while checking
  #   to see if we get the right auction
  auctionNum <- 1
  # run a while loop to make sure we get the correct auction
  while(bool){
    # get the URL for the first auction
    firstAuctionURL <- makeURL(securityDates[firstDate, ], auctionNum)
    print(firstAuctionURL)
    # if we're past tooManyAuctions (set as global variable above)
    #   time to give up
    if(auctionNum > tooManyAuctions){
      # assign values in global environment
      failedDates[failedDates_index, ] <<- as.character(securityDates[1, ])
      failedDates_index <<- failedDates_index + 1
      print(securityDates[1, ])
      firstAuction <- NULL
      break
    }
    # gather the data and put into data frame format
    firstAuction <- getData(firstAuctionURL)
    # if firstAuction is NULL, then the URL doesn't contain any auctions,
    #   so try the next auctionNum
    if(is.null(firstAuction)){
      auctionNum <- auctionNum + 1
      # check again if we're past tooManyAuctions (set as global variable above)
      #   time to give up
      if(auctionNum > tooManyAuctions){
        # assign values in global environment
        failedDates[failedDates_index, ] <<- as.character(securityDates[1, ])
        failedDates_index <<- failedDates_index + 1
        print(securityDates[1, ])
        firstAuction <- NULL
        break
      }
    }
    else{
      # check if we got the correct auction
      rightAuction <- checkData(firstAuction, actualSecurity, TIPS, FRN)
      print("Was it the right auction: ")
      print(rightAuction)
      # deal with rightAuction = Null case (don't break out of while loop)
      if(is.null(rightAuction)){
        auctionNum <- auctionNum + 1
      }
      # if we have the correct auction, break out of the while loop
      else if(rightAuction){
        break
      }
      # otherwise, we need to re-run everything checking the next auction on that day
      else{
        auctionNum <- auctionNum + 1
      }
    }
  }
  # if we didn't get a first auction, we need a new first auction
  if(is.null(firstAuction)){
    firstDate <- firstDate + 1
    # make sure there are still values to check
    #   (maybe the whole set of dates doesn't work)
    if (firstDate > nrow(securityDates)){
      print("None of your dates work")
      return(NULL)
    }
    # otherwise, run the code using the new firstDate
    else{
      print("first try failed")
      getFirstAuction(securityDates, firstDate, actualSecurity, TIPS, FRN)
    }
  }
  # if we found the auction we want
  #   keep as a data frame we will append to from here on
  else{
    firstAuction[2, 1] <- firstDate
    return(firstAuction)
    
  }
}

# loop over the remaining dates
#   gather the tuples for each auction
#   group into one data frame
#   method parameters the same as gerFirstAuction BUT
#     also includes a starting date parameter (index value) so we start looking
#       after the first auction we found
getOtherAuctions <- function(securityDates, firstDate, auctionFrame,
                             actualSecurity, TIPS, FRN){
  # loop over the date column
  #   (from 1 index past the starting value to the end of the column)
  for(j in (firstDate + 1) : nrow(securityDates) ){
    # create a boolean for while loop
    bool <- TRUE
    # set an auctionNum variable to increment while checking
    #   to see if we get the right auction
    auctionNum <- 1
    # run a while loop to make sure we get the correct auction
    while(bool){
      # get the URL for the first auction
      auctionURL <- makeURL(securityDates[j, ], auctionNum)
      print(auctionURL)
      # if we're past tooManyAuctions (set as global variable above)
      #   time to give up
      if(auctionNum > tooManyAuctions){
        failedDates[failedDates_index, 1] <- as.character(securityDates[j, 1])
        # assign new values to global environment
        failedDates[failedDates_index, 1] <<- failedDates[failedDates_index, 1]
        failedDates_index <<- failedDates_index + 1
        auction <- NULL
        break
      }
      # gather the data and put into data frame format
      auction <- getData(auctionURL)
      
      # if auction is NULL, then the URL doesn't contain any auctions,
      #   so try the next auctionNum
      if(is.null(auction)){
        auctionNum <- auctionNum + 1
        # check again if we're past tooManyAuctions (set as global variable above)
        #   time to give up
        if(auctionNum > tooManyAuctions){
          # assign values in global environment
          failedDates[failedDates_index, ] <<- as.character(securityDates[j, ])
          failedDates_index <<- failedDates_index + 1
          auction <- NULL
          break
        }
      }
      else{
        # check if we got the correct auction
        rightAuction <- checkData(auction, actualSecurity, TIPS, FRN)
        print("Was it the right auction: ")
        print(rightAuction)
        # deal with rightAuction = Null case (don't break out of while loop)
        if(is.null(rightAuction)){
          auctionNum <- auctionNum + 1
        }
        # if we have the correct auction, break out of the while loop
        else if(rightAuction){
          break
        }
        # otherwise, we need to re-run everything checking the next auction on that day
        else{
          auctionNum <- auctionNum + 1
        }
      }
    }
    # in the getFirstAuction method, we needed to make sure we returned an auction
    #   or ran an error
    # but if a date fails for us now, we simply note it and move on
    if(is.null(auction)){
      # pass i.e. don't do the appending in the else statement
    }
    # if we found the auction we want
    #   append to our data frame
    else{
      auctionFrame <- smartbind(auctionFrame, auction)
    }
  }
  return(auctionFrame)
}

# to be used to save the finalTable and failedDates data frames in the same directory
saveFiles <- function(final, fails, securityType){
  # make nice names for the files
  finalTableName <- paste("./auctionsData_", securityType, ".csv", sep = "")
  errorDates <- paste("./failedDates_", securityType, ".csv", sep = "")
  # save them as csv files
  write.csv(final, finalTableName, row.names = FALSE)
  write.csv(fails, errorDates, row.names = FALSE)
  return()
}
  
#### main method ####
# take a 1 column data frame of posixct dates
#   and a type of security (securityType) as a string
# NOTE: the securityType must be specified in a specific way:
#   CMB:             "0-WEEK"
#   4-week T-bills:  "4-WEEK"
#   13-week T-bills: "13-WEEK"
#   26-week T-bills: "26-WEEK"
#   52-week T-bills: "52-WEEK"
#   2-year notes:    "2-YEAR"
#   3-year notes:    "3-YEAR"
#   5-year notes:    "5-YEAR"
#   7-year notes:    "7-YEAR"
#   10-year notes:   "10-YEAR"
#   30-year bonds:   "30-YEAR"
#   5-year TIPS:     "5-YEAR", TIPS = "Y"
#   10-year TIPS:    "10-YEAR", TIPS = "Y"
#   30-year TIPS:    "30-YEAR", TIPS = "Y"
#   2-year FRN:      "2-YEAR", FRN = "Y"
# I have not checked if this code will work for other cases of securities
#   (chances are it would not)
#   most notably, as an example
#     for not case handling when an n year security has (n-1) years and m months til maturity
makeTable <- function(dates, actualSecurity, TIPS = "N", FRN = "N"){
  # do the first auction from the dates to get a correctly sized data frame
  #   to then be able to append to
  
  # set firstDate to 1 to start
  firstDate <- 1
  
  temp <- getFirstAuction(dates, firstDate, actualSecurity, TIPS, FRN)
  
  # if we got null values, there's an issue => return error
  if(is.null(temp) ){
    print("You must have made a mistake")
    return()
  }
  
  # set the first auction value and the index of the first working auction date
  firstAuction <- temp[1, ]
  firstDate <- as.numeric(temp[2, 1])
  
  # now that we have the first auction, we need to loop over the data
  #   to get the rest of the auctions
  finalTable <- getOtherAuctions(dates, firstDate, firstAuction,
                                 actualSecurity, TIPS, FRN)
  
  # save the final data frame as well as the failed dates data frame as csv files
  #   in the same directory
  print("It worked!") 
  return(list(finalTable, failedDates))
}
