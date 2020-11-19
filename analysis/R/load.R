rm(list = ls())
require(tidyverse)
require(ggplot2)
require(tidyr)
require(lubridate)

# setwd("C:/Users/Sam/Google Drive/Experiments & Data/Reversal Learning - La Selva_2017/Analysis/Analysis/Raw Data")
# getwd()

setwd("/Users/shambhavi/Google Drive/Experiments & Data/Reversal Learning - La Selva_2017/Analysis/Analysis/Raw Data")
getwd()

#------------------
# Compiling of data
#------------------

# loading MasterTable that includes names of csv files, day, experimental condition,
# Group and flight cage
daypathlist <- read.csv2(
  file = paste("MasterTableSerialReversal.csv", sep = ""),
  header = TRUE, sep = ",", na.strings = "NA"
)
# determining the number of days
ndays <- c()
ndays <- length(daypathlist[, 2])
# resetting alldays in case it exists already. This will be the sink for the daily data
alldays <- c()

# combining all files into one table

if (ndays > 0) {
  for (i in 1:ndays) {
    nthday <- read.csv2(
      file = as.character(daypathlist$path[i]), sep = ";", dec = ".", header = TRUE,
      fileEncoding = "UTF-16LE", as.is = T, row.names = NULL
    )
    nthday <- nthday[, -c(18)]
    # making a separate vector with the column names
    mastercolnames <- c(
      "DateTime", "IdRFID", "IdLabel",
      "unitLabel", "eventDuration", "sense1duration",
      "sense1Events", "senseRFIDrecords", "reinforce1value",
      "reinforce1Total", "reinforce1Account", "outFuncLabel",
      "outLabel", "SystemMsg", "MsgValue1", "MsgValue2", "MsgValue3"
    )

    colnames(nthday) <- mastercolnames

    firstrow <- c()
    lastrow <- c()


    firstrow <- max(which(nthday$MsgValue1 == "start")) + 1
    lastrow <- nrow(nthday)

    if (is.na(lastrow) | is.infinite(lastrow)) {
      lastrow <- nrow(nthday)
    }

    nthday <- nthday %>%
      slice(firstrow:lastrow)

    nthday$Day <- daypathlist[i, 1]

    nthday <- nthday %>%
      arrange(DateTime)

    if (i > 1) {
      alldays <- bind_rows(alldays, nthday)
    } else {
      alldays <- nthday
    }

    # making a day column with the day number
    nthday$day <- daypathlist[i, 1]

    nthday$Condition <- daypathlist[i, 3]
    nthday$Group <- daypathlist[i, 4]
    nthday$Cage <- daypathlist[i, 5]

    if (i > 1) {
      alldays <- bind_rows(alldays, nthday)
    } else {
      alldays <- nthday
    }
  }
}

# reformatting DateTime and display properly, as characters
alldays$DateTime <- sub(",", ".", alldays$DateTime)
alldays$DateTime <- as.POSIXct(as.numeric(alldays$DateTime) * (60 * 60 * 24),
  origin = "1899-12-30", tz = "GMT"
)


# removing columns with irrelevant data
alldays <- alldays %>%
  select(
    -sense1duration, -sense1Events, -senseRFIDrecords,
    -reinforce1Total, -reinforce1Account, -MsgValue2, -MsgValue3
  )

# assigning the proper IdLabel to the RFID numbers during the exploration stages
# Group 1 Flight cage 1 Alternate Training day 4 Bat3 changes from 0417690155 to 041769264F

Exploration <- alldays %>%
  filter(Condition == "Exploration") %>%
  select(-IdLabel)

Non_exp <- alldays %>%
  filter(Condition != "Exploration")

Labels <- data.frame(
  IdLabel = c(
    "Bat1", "Bat2", "Bat3", "Bat4", "Bat5", "Bat6",
    "Bat7", "Bat8", "Bat9", "Bat10", "Bat11", "Bat12",
    "Bat13", "Bat14", "Bat15", "Bat16",
    "Bat17", "Bat18", "Bat19", "Bat20"
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
  arrange(Group, day)

# removing the now superfluous dataframes
rm(
  daypathlist, Labels, nthday, i, mastercolnames, ndays, alldays, Exploration,
  Non_exp
)

# filtering out only the visits made by the bats to flowers that were assigned to them,
# and removing the small subset of visits where the bats didn't get rewarded at the assigned flowers

raw_data <- raw_data %>%
  mutate(
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
  select(-choice_switch) %>%
  mutate(
    # adding columns with the day number and group written out
    day = paste("Day", day, sep = " "),
    Group = paste("Group", Group, sep = " ")
  )

# generating a CSV file of the raw data
write.csv2(raw_data, file = "raw_data.csv", row.names = FALSE)
