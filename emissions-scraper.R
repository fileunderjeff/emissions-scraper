###
# scraper for TCEQ STEERS data (emission events)
# main URL: http://www2.tceq.texas.gov/oce/eer/
###
# INSTRUCTIONS
###
#
# There are many ways to query & organize emissions data. This scraper assumes
# that you have already generated a list of tracking numbers by conducting a 
# search using the URL above.
#
# This scraper takes that list of tracking numbers, generates a unique URL,
# visits that URL, grabs the source code of the page, parses the HTML, arranges
# the data by contaminant, and writes it to a CSV file.
#
# You can run the scraper by changing the file name that reads into
# `emissions.list` on line 30 below. If your column headers are different, 
# you'll need to update them, too.
#
###
# LIBRARIES
###
#
library(rvest)
library(plyr)
library(stringr)
###
# VARIABLES THAT YOU EDIT
###
emissions.list <- read.csv(file="lookup.csv",header=T) # change to your filename of Tracking Numbers
tracking.numbers <- emissions.list$Tracking..Number # change to match your column headers
###
# ASSEMBLE THE UNIQUE URL FOR LOOKUP
###
url.root = "http://www2.tceq.texas.gov/oce/eer/index.cfm?fuseaction=main.getDetails&target="
emissions.list$url <- paste(url.root,tracking.numbers)
emissions.list$url <- gsub(" ","",emissions.list$url)
lookup <- emissions.list$url
###
# FUNCTION TO RUN THROUGH URLS AND GRAB DATA
###
get_violations <- function(url, css_or_xpath="*"){ 
  sess <- html_session(url) 
  raw <- html_node(sess, "#content") %>% html_text() 
  second <- html_nodes(sess, xpath='//*[@id="content"]/table[2]') %>% html_table()
  if(length(second) == 0) {
    Contaminant <- c("No data available")
    Authorization <- c("No data available")
    Limit <- c("No data available")
    Amount.Released <- c("No data available")
    second <- data.frame(Contaminant,Authorization,Limit,Amount.Released)
  } else {
    second <- data.frame(second)
  }
  third <- html_nodes(sess, xpath='//*[@id="content"]/table[3]') %>% html_table()
  if(length(third) == 0) {
    third <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    third <- data.frame(third)
  }
  fourth <- html_nodes(sess, xpath='//*[@id="content"]/table[4]') %>% html_table()
  if(length(fourth) == 0) {
    fourth <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    fourth <- data.frame(fourth)
  }
  fifth <- html_nodes(sess, xpath='//*[@id="content"]/table[5]') %>% html_table()
  if(length(fifth) == 0) {
    fifth <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    fifth <- data.frame(fifth)
  }
  sixth <- html_nodes(sess, xpath='//*[@id="content"]/table[6]') %>% html_table()
  if(length(sixth) == 0) {
    sixth <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    sixth <- data.frame(sixth)
  }
  seven <- html_nodes(sess, xpath='//*[@id="content"]/table[7]') %>% html_table()
  if(length(seven) == 0) {
    seven <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    seven <- data.frame(seven)
  }  
  eight <- html_nodes(sess, xpath='//*[@id="content"]/table[8]') %>% html_table()
  if(length(eight) == 0) {
    eight <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    eight <- data.frame(eight)
  }
  nine <- html_nodes(sess, xpath='//*[@id="content"]/table[9]') %>% html_table()
  if(length(nine) == 0) {
    nine <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    nine <- data.frame(nine)
  }
  ten <- html_nodes(sess, xpath='//*[@id="content"]/table[10]') %>% html_table()
  if(length(ten) == 0) {
    ten <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    ten <- data.frame(ten)
  }  
  eleven <- html_nodes(sess, xpath='//*[@id="content"]/table[11]') %>% html_table()
  if(length(eleven) == 0) {
    eleven <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    eleven <- data.frame(eleven)
  }  
  twelve <- html_nodes(sess, xpath='//*[@id="content"]/table[12]') %>% html_table()
  if(length(twelve) == 0) {
    twelve <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    twelve <- data.frame(twelve)
  }  
  thirteen <- html_nodes(sess, xpath='//*[@id="content"]/table[13]') %>% html_table()
  if(length(thirteen) == 0) {
    thirteen <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    thirteen <- data.frame(thirteen)
  }  
  fourteen <- html_nodes(sess, xpath='//*[@id="content"]/table[14]') %>% html_table()
  if(length(fourteen) == 0) {
    fourteen <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    fourteen <- data.frame(fourteen)
  }  
  fifteen <- html_nodes(sess, xpath='//*[@id="content"]/table[15]') %>% html_table()
  if(length(fifteen) == 0) {
    fifteen <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    fifteen <- data.frame(fifteen)
  }  
  sixteen <- html_nodes(sess, xpath='//*[@id="content"]/table[16]') %>% html_table()
  if(length(sixteen) == 0) {
    sixteen <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    sixteen <- data.frame(sixteen)
  }  
  seventeen <- html_nodes(sess, xpath='//*[@id="content"]/table[17]') %>% html_table()
  if(length(seventeen) == 0) {
    seventeen <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    seventeen <- data.frame(seventeen)
  }  
  eighteen <- html_nodes(sess, xpath='//*[@id="content"]/table[18]') %>% html_table()
  if(length(eighteen) == 0) {
    eighteen <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    eighteen <- data.frame(eighteen)
  }  
  nineteen <- html_nodes(sess, xpath='//*[@id="content"]/table[19]') %>% html_table()
  if(length(nineteen) == 0) {
    nineteen <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    nineteen <- data.frame(nineteen)
  }  
  twenty <- html_nodes(sess, xpath='//*[@id="content"]/table[20]') %>% html_table()
  if(length(twenty) == 0) {
    twenty <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    twenty <- data.frame(twenty)
  }  
  twentyone <- html_nodes(sess, xpath='//*[@id="content"]/table[21]') %>% html_table()
  if(length(twentyone) == 0) {
    twentyone <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    twentyone <- data.frame(twentyone)
  }  
  twentytwo <- html_nodes(sess, xpath='//*[@id="content"]/table[22]') %>% html_table()
  if(length(twentytwo) == 0) {
    twentytwo <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    twentytwo <- data.frame(twentytwo)
  }  
  twentythree <- html_nodes(sess, xpath='//*[@id="content"]/table[23]') %>% html_table()
  if(length(twentythree) == 0) {
    twentythree <- data.frame(Contaminant = character(),Authorization = character(),Limit = character(),Amount.Released = character())
  } else {
    twentythree <- data.frame(twentythree)
  }    
  almost.full <- rbind(second,third,fourth,fifth,sixth,seven,eight,nine,ten,eleven,twelve,thirteen,fourteen,fifteen,sixteen,seventeen,eighteen,nineteen,twenty,twentyone,twentytwo,twentythree)
  full <- cbind(raw,almost.full)
  full$url <- url
  return(full)
}
###
# GET THE URL CONTENT!
#
# The next line of code will run the function to loop through your tracking numbers and grab the
# content for processing. It may take a while, and it doesn't produce any status updates. It can take
# up to 10+ seconds per page/tracking number.
#
#
# WHAT IF I GET A TIMEOUT ERROR?
#
# Sometimes a Tracking Number will show up in the search results, but the actual record will
# throw an Internal Server Error (500). If your script is timing out without completion,
# consider breaking up your tracking numbers into smaller batches. This will help you hone in and identify
# the problem tracking number.
### 
raw_violations <- lapply(lookup, get_violations) # run the function
pv <- ldply (raw_violations, data.frame) # pv stands for Processed Violations
###
# PROCESS THE DATA
# This is a bunch of regex, formatted with a double-escape for R.
###
pv$regulated_entity_name <- str_extract(pv$raw, "name\\r\\n\\t\\t\\r\\n\\t\\t.*\\r")
pv$regulated_entity_name <- gsub("name\\r\\n\\t\\t\\r\\n\\t\\t","",pv$regulated_entity_name)
pv$regulated_entity_name <- gsub("\\r","",pv$regulated_entity_name)
pv$physical_location <- str_extract(pv$raw, "Physical location\\r\\n\\t\\t\\r\\n\\t\\t.*\\r")
pv$physical_location <- gsub("Physical location\\r\\n\\t\\t\\r\\n\\t\\t","",pv$physical_location)
pv$physical_location <- gsub("\\r","",pv$physical_location)
pv$rn_number <- str_extract(pv$raw, "Regulated entity RN number\\r\\n\\t\\t\\r\\n\\t\\t.*\\r")
pv$rn_number <- gsub("Regulated entity RN number\\r\\n\\t\\t\\r\\n\\t\\t","",pv$rn_number)
pv$rn_number <- gsub("\\r","",pv$rn_number)
pv$city <- str_extract(pv$raw, "City, County\\r\\n\\t\\t\\r\\n\\t\\t\\r\\n\\t\\t.*\\r")
pv$county <- pv$city
pv$city <- gsub("City, County\\r\\n\\t\\t\\r\\n\\t\\t\\r\\n\\t\\t", "", pv$city)
pv$county <- gsub("City, County\\r\\n\\t\\t\\r\\n\\t\\t\\r\\n\\t\\t", "", pv$county)
pv$city <- gsub("\\,.*", "", pv$city)
pv$county <- gsub("^\\w+.*\\,", "", pv$county)
pv$event_type <- str_extract(pv$raw, "event\\:\\r\\n\\t\\t\\r\\n\\t\\t.*\\r")
pv$event_type <- gsub("event\\:\\r\\n\\t\\t\\r\\n\\t\\t","",pv$event_type)
pv$event_type <- gsub("\\r","",pv$event_type)
pv$event_began_date <- str_extract(pv$raw, "began\\:\\r\\n\\t\\t.*\\r\\n\\t\\t.*\\r")
pv$event_began_date <- gsub("began\\:\\r\\n\\t\\t","",pv$event_began_date)
pv$event_began_time <- str_extract(pv$event_began_date, ".[0-9].*$")
pv$event_began_date <- gsub("\\s.*$","",pv$event_began_date)
pv$event_began_time <- gsub("\\s","",pv$event_began_time)
pv$based_on <- str_extract(pv$raw, "the:\\r\\n\\t\\t\\r\\n\\t\\t.*\\r")
pv$based_on <- gsub("the:\\r\\n\\t\\t\\r\\n\\t\\t","",pv$based_on)
pv$based_on <- gsub("\\r","",pv$based_on)
pv$event_ended_date <- str_extract(pv$raw, "ended:\\r\\n\\t\\t.*\\r")
pv$event_ended_date <- gsub("ended:\\r\\n\\t\\t","",pv$event_ended_date)
pv$event_ended_date <- gsub("\\r","",pv$event_ended_date)
pv$event_ended_date <- gsub("\\s","",pv$event_ended_date)
pv$event_ended_time <- str_extract(pv$raw, "\\r\\n\\t\\t[0-9].*\\r\\n\\tCa")
pv$event_ended_time <- gsub("\\sCa","",pv$event_ended_time)
pv$cause <- str_extract(pv$raw, "Cause\\r\\n\\t\\t.*\\r")
pv$cause <- gsub("Cause\\r\\n\\t\\t","",pv$cause)
pv$cause <- gsub("\\r","",pv$cause)
pv$action_taken <- str_extract(pv$raw, "taken\\r\\n\\t\\t.*\\r")
pv$action_taken <- gsub("taken\r\n\t\t","",pv$action_taken)
pv$action_taken <- gsub("\\r","",pv$action_taken)
pv$emissions_estimation_method <- str_extract(pv$raw,"method\\r\\n\\t\\t.*\\r")
pv$emissions_estimation_method <- gsub("method\\r\\n\\t\\t","",pv$emissions_estimation_method)
pv$emissions_estimation_method <- gsub("\\r","",pv$emissions_estimation_method)
pv$event_began_date <- as.Date(pv$event_began_date, format="%m/%d/%Y")
pv$raw <- NULL # remove the first data field with unprocessed data
write.csv(pv,file="yourfilename.csv",row.names=F) # write the CSV output