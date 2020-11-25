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
    -reinforce1Total, -reinforce1Account, -MsgValue2, -MsgValue3
  )

# joining the Conditions table to the prepared raw data table 

alldays <- left_join(alldays, Conditions, by = c("day"))

#-----------------------------
# Preparing the raw data table
#----------------------------- 

alldays <- alldays  %>%
  # sorting the data chronologically
  arrange(DateTime) %>%
  # changing the format of the DateTime column to show the date and time clearly
  mutate(
    DateTime = as.numeric(str_replace(DateTime, ",", ".")),
    DateTime = as.POSIXct(as.numeric(DateTime) * (60 * 60 * 24),
      origin = "1899-12-30", tz = "UTC"
    ),
    # adding a column that marks the rows to include: the visits to the correct flowers and
    # the rows marking a reversal
    choice_switch = ifelse(MsgValue1 == "switch", 1,
      ifelse(str_detect(outLabel, "positive"), 1, 0)
    ),
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
  # removing now unnecessary columns
  select(-choice_switch, -day) %>%
  mutate(
    # adding a column with the group written out
    Group = paste("Group", Group, sep = " ")
  )

# writing the csv table

write.csv2(alldays, file = paste0(folder, "raw_data.csv"), row.names = FALSE)
