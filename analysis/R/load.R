rm(list = ls())
require(tidyverse)

# setting analysis folder

folder <- "analysis/data/"

#------------------
# Compiling of data
#------------------
# reading in the MasterTable

MasterTable <- read.csv2(
  file = paste0(folder, "meta_data/MasterTableSerialReversal.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# reading in the table of conditions
Conditions <- read.csv2(
  file = paste0(folder, "meta_data/ConditionsSerialReversal.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)

# setting the colnames

mastercolnames <- c(
  "DateTime", "IdRFID", "IdLabel",
  "unitLabel", "eventDuration", "sense1duration",
  "sense1Events", "senseRFIDrecords", "reinforce1value",
  "reinforce1Total", "reinforce1Account", "outFuncLabel",
  "outLabel", "SystemMsg", "MsgValue1", "MsgValue2", "MsgValue3"
)

# writing a function to prepare the raw data from a single day

load_raw_csv <- function(path) {
  nthday <- read.csv2(
    file = path, sep = ";", dec = ".", header = TRUE,
    fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL
  ) %>%
    select(1:17)
  
  # renaming the columns
  if (exists("mastercolnames")) {
    colnames(nthday) <- mastercolnames
  }
  
  # extracting the relevant rows
  
  firstrow <- c()
  lastrow <- c()
  firstrow <- max(which(nthday$MsgValue1 == "start")) + 1
  lastrow <- nrow(nthday)
  
  if (is.na(lastrow) | is.infinite(lastrow)) {
    lastrow <- nrow(nthday)
  }
  
  nthday <- nthday %>%
    slice(firstrow:lastrow)
}

# taking the list of days from the master table
days <- as.list(MasterTable$day)
# taking the list of file paths from the master table
paths <- as.list(paste0(folder, MasterTable$path))

# writing a function to aggregate the data from many days

aggregate_days <- function(paths, days) {
  map(paths, load_raw_csv) %>%
    set_names(days) %>%
    enframe("day", "day_data") %>%
    unnest(day_data) %>%
    mutate(day = as.numeric(day))
}

# using the functions that were written to create one big data frame of raw data

alldays <- aggregate_days(paths, days) %>%
  # removing the columns with irrelevant data 
  select(
    -sense1duration, -sense1Events, -senseRFIDrecords,
    -reinforce1Total, -MsgValue2, -MsgValue3
  )

# joining the Conditions table to the prepared raw data table 

alldays <- left_join(alldays, Conditions, by = c("day"))

#-------------------------------------------------------------------
# Creating a data table with all data including the pump information
#-------------------------------------------------------------------

# reformatting DateTime and display properly, as characters

alldays$DateTime <- sub(",", ".", alldays$DateTime)
alldays$DateTime <- as.POSIXct(as.numeric(alldays$DateTime) * (60 * 60 * 24),
                             origin = "1899-12-30", tz = "GMT"
                             ) 

alldays$DateTime <- format(alldays$DateTime, "%Y-%m-%d %H:%M:%OS3")

# assigning the proper IdLabel to the RFID numbers during the exploration stages
# Group 1 Flight cage 1 Alternate Training day 4 Bat3 changes from 0417690155 to 041769264F

Exploration <- alldays %>%
  filter(Condition == "Exploration") %>%
  select(-IdLabel)

Non_exp <- alldays %>%
  filter(Condition != "Exploration")

Labels <- data.frame(
  IdLabel = c(
    "Bat 1", "Bat 2", "Bat 3", "Bat 4", "Bat 5", "Bat 6",
    "Bat 7", "Bat 8", "Bat 9", "Bat 10", "Bat 11", "Bat 12",
    "Bat 13", "Bat 14", "Bat 15", "Bat 16",
    "Bat 17", "Bat 18", "Bat 19", "Bat 20"
  ),
  IdRFID = c(
    "04176924DC", "04176904DD", "0417690155", "0417691B6F",
    "041768FE9E", "04176912CA", "041768FDDF", "041768F309",
    "0417690887", "0417692FCB", "04176914E8", "0417691A07",
    "0417693122", "041768F88F", "0417690CF1", "0416ECE147",
    "0416D4F295", "041768FF3B", "041768FF4E", "04176917C8"
  )
)

Exploration <- merge(Exploration, Labels, by = "IdRFID", all.x = TRUE)

# putting all the data back together

raw_data <- rbind(Exploration, Non_exp)
raw_data <- raw_data %>%
  arrange(Group, day) %>% 
  # separating the word Bat from the number 
  mutate(
    Bat_word = ifelse((str_detect(IdLabel, "Bat") == TRUE), "Bat", ""),
    Bat_number = ifelse((str_detect(IdLabel, "Bat") == TRUE), as.integer(str_extract(IdLabel, "[0-9]+")), IdLabel),
    IdLabel = ifelse(is.na(IdLabel), IdLabel, paste0(Bat_word, " ", Bat_number))
  ) %>%
  select(-Bat_word, -Bat_number)

# removing the now unnecessary dataframes
rm(
  Conditions, MasterTable, days, paths, Labels, mastercolnames, alldays, Exploration,
  Non_exp
)

write.csv2(raw_data, file = paste0(folder, "/processed_data/raw_data_all.csv"), row.names = FALSE)

#-----------------------------------------------
# Creating a dataset with the data from the bats
#-----------------------------------------------

# filtering out only the visits made by the bats to flowers that were assigned to them, filtering out
# events that were too long and creating one dataset.

raw_data_bats <- raw_data %>% 
  mutate(
    # adding a column that marks the rows to include: the visits to the correct flowers and
    # the rows marking a reversal
    choice_switch = 0, 
    choice_switch = case_when(MsgValue1 == "switch" ~ 1, # noting when a reversal happened
                              str_detect(outLabel, "positive") ~ 1, # noting when a proper choice visit happened, rewarded or not
                              reinforce1Account > 0 ~ 1), # noting when a proper visit was made that was not rewarded when it should have been
    #   ifelse(MsgValue1 == "switch", 1,
    #   ifelse(str_detect(outLabel, "positive"), 1, 0)
    # ),
    # assigning rows marking a reversal with an event Duration of 0
    eventDuration = ifelse(MsgValue1 == "switch", 0, eventDuration)
  ) %>%
  filter(
    # removing visits made by a test transponder
    IdLabel != "Test",
    # filtering the visits to be included
    choice_switch == 1,
    # filtering out unusually long signal interruptions as faulty
    eventDuration < 15000
  ) %>%
  select(-choice_switch) %>%
  mutate(
    # adding columns with the day number and group written out
    day = paste("Day", day, sep = " "),
    Group = paste("Group", Group, sep = " ")
  )

# generating a CSV file of the raw data including all the 
write.csv2(raw_data_bats, file = paste0(folder, "/processed_data/raw_data_bats.csv"), row.names = FALSE)

