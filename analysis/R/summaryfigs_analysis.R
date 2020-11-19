rm(list = ls())
require(tidyverse)
require(extrafont)
require(extrafontdb)
require(tidyr)
require(betareg)
require(lme4)
require(lubridate)
require(rstanarm)
require(Hmisc)

#-----------------------------------------
# Loading the data
#-----------------------------------------
setwd("/Users/shambhavi/Google Drive/Experiments & Data/Reversal Learning - La Selva_2017/Analysis/Analysis/Raw Data")
getwd()
raw_data <- read.csv2("Raw_data.csv", sep = ";", header = TRUE)
#--------------------------------------------------------------------------------------------------
# Visualising the data from the three training phases: exploration, flower training and alternation
#--------------------------------------------------------------------------------------------------
# The same kind of visualisation is to be done for all three phases that
# are coded in the raw data table as "Exploration", "FlowerTraining", "Alternation"
# the following function creates this visualisation - how to do this?

training <- raw_data %>%
  filter(
    # the condition in the following line can be adjusted to the training phase to be visualised
    Condition == "Alternation",
    !is.na(reinforce1value)
  ) %>%
  group_by(Group, day, IdLabel) %>%
  # counting the number of visits made to the different flowers by the different bats
  count(unitLabel) %>%
  ungroup() %>%
  group_by(Group, day, IdLabel) %>%
  # counting the total number of visits made by the bats to all the flowers
  mutate(
    total_vis = sum(n),
    unitLabel = as.factor(as.numeric(str_extract(unitLabel, "[0-9]+")))
  )

# visualising the training data

training %>%
  filter(Group != "Group 1") %>% # removing the beta bats
  ggplot(aes(unitLabel, n)) +
  geom_jitter(aes(group = IdLabel, colour = IdLabel)) +
  facet_grid(Group ~ day) +
  xlab("Flower number") +
  ylab("Visits") +
  theme_bw() +
  scale_fill_viridis_d()

#---------------------------------------
# The main experiment - overall choices
#---------------------------------------
# The following terms are used in the analysis of the data:
# 1. visits: each individual flower visit
# 2. trials: a group of 3 consecutive visits
# 3. block: a group of 50 trials between each reversal where one of the flowers is more rewarding
# 1 block is 50 visits

# creating a vector of the bats to be excluded from the main analysis
bats_incomp <- c("Bat7", "Bat19")

# creating a vector of the main experimental days
main_days <- c("Day 1", "Day 2", "Day 3")

# setting binsize and breaks for cutting up the data into block and bins
binsize <- 10
breaks <- seq(0, 3000, binsize)

# preparing the data from the main experiment in a data table
rev_learning_all <- raw_data %>%
  mutate(
    # marking the difference between the normal visits in a block and the switch points
    MsgValue1 = ifelse(MsgValue1 == "switch", MsgValue1, "block")
  ) %>%
  filter(
    # removing the bats that did not complete the experiment
    !IdLabel %in% bats_incomp,
    # filtering out the main experimental data
    Condition == "SerialReversalCounter"
  ) %>%
  rename(Bat = IdLabel) %>%
  arrange(Group, day, Bat, DateTime) %>%
  # grouping the data to count the visits, noting the reversals separately
  group_by(Bat, day) %>%
  mutate(
    # noting whether the bat made a visit to the more or less rewarding flower
    reward_status = ifelse(is.na(reinforce1value), 0, 1),
    # creating a column with the total number of visits made by a bat per day
    count_vis = ifelse(MsgValue1 == "switch", 0, 1), 
    count_vis = cumsum(count_vis)
  ) %>%
  # setting the maximum number of visits a night
  filter(count_vis <= 300) %>%
  ungroup() %>%
  # creating a column with the reversal block number, marking the reversals
  mutate(block = ifelse(MsgValue1 == "switch", 1, 0)) %>%
  group_by(day, Bat) %>%
  mutate(block = cumsum(block)) %>%
  ungroup() %>% 
  group_by(day, Bat, block) %>%
  # creating a column with the number of visits within each block
  mutate(block_vis = ifelse(MsgValue1 == "switch", 0, 1), 
         block_vis = cumsum(block_vis), 
  # creating a new column for visits in each block to be binned
         bins = "") %>%
  ungroup() %>%
  group_by(day, Bat, block) %>%
  # cutting the visits inside each block into bins of the size set earlier
  mutate(bins = as.numeric(cut(block_vis, breaks, include.lowest = TRUE)))

# creating a vector with the beta bats

bats_beta <- c("Bat1", "Bat2", "Bat3", "Bat4")

# creating a data frame with just the beta bats
rev_learning_beta <- rev_learning_all %>%
  filter(Bat %in% bats_beta)

# creating a data frame with the alpha bats and the three main days of the experiment
rev_learning_ind <- rev_learning_all %>%
  filter(!Bat %in% bats_beta)

# averaging the bats' performance over day, block and bin

rev_learning_avg <- rev_learning_ind %>%
  group_by(day, block, bins) %>%
  # calculating the 95% confidence intervals 
  group_modify(~ mean_cl_boot(.x$reward_status, conf.int = 0.95)) %>%
  ungroup() %>%
  group_by(day) %>%
  mutate(
    day_bin = 1:n(),
    day_bin_vis = day_bin * binsize,
    reversal = ifelse(block != lead(block), "switch", "block")
  )

# calculating the bats' 'errors', i.e., the inverse of the previous calculation, and averaging 
# over day, block and bin

rev_learning_err <- rev_learning_ind %>%
  mutate (error = 1-(reward_status)) %>% 
  group_by (day, block, bins) %>%
  # calculating the 95% confidence intervals 
  group_modify(~ mean_cl_boot(.x$error, conf.int = 0.95)) %>%
  ungroup() %>%
  group_by(day) %>%
  mutate(
    day_bin = 1:n(),
    day_bin_vis = day_bin * binsize,
    reversal = ifelse(block != lead(block), "switch", "block")
  )

# Overall summary plots
#######################

# plot 1: plotting the individual bats' choices for the profitable option for every visit

# creating a look-up table so the reversals can be marked in the plots for the main experimental bats
rev_main <- rev_learning_ind %>%
  filter(
    MsgValue1 == "switch",
    day %in% main_days
  ) %>%
  select(day, Bat, count_vis, block, bins, MsgValue1)

rev_learning_ind %>%
  filter(day %in% main_days) %>%
  ggplot(aes(count_vis, reward_status)) +
  geom_line() +
  facet_grid(Bat ~ day) +
  geom_vline(aes(xintercept = count_vis), rev_main, colour = "red") +
  ylab("Choice for the more profitable option") +
  xlab("Visits") +
  theme_classic()

# plot 2: averaging the bats' preferences over day, block and bin

# creating a look-up table for the reversals

rev_main_avg <- rev_learning_avg %>%
  filter(
    reversal == "switch",
    day %in% main_days
  ) %>%
  select(day, block, day_bin, day_bin_vis)

rev_learning_avg %>%
  # filtering only the first three main days of the experiment:
  # one group had the experiment extended a further three days
  filter(day %in% main_days) %>%
  # mutate(visits = bins * 10) %>%
  ggplot(aes(day_bin_vis, y)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.3) +
  facet_grid(. ~ day) +
  scale_x_continuous(breaks = seq(50, 300, by = 50)) +
  ylim(0, 1.02) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75, 1), linetype = "dotted") +
  geom_vline(aes(xintercept = day_bin_vis), rev_main_avg, linetype = "dashed") +
  # geom_vline(xintercept = seq(50, 300, by = 50), linetype = "dashed") +
  theme_classic() +
  labs(x = "Visits", y = "Avg choice for the more profitable option ± 95% CIs")

# plot 3: plotting the individual bats' choices for the less profitable option, the inverse of plot 1

rev_main_err <- rev_learning_err %>%
  filter(
    reversal == "switch",
    day %in% main_days
  ) %>%
  select(day, block, day_bin, day_bin_vis)

rev_learning_err %>%
  # filtering only the first three main days of the experiment:
  # one group had the experiment extended a further three days
  filter(day %in% main_days) %>%
  ggplot(aes(day_bin_vis, y)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.3) +
  facet_grid(. ~ day) +
  scale_x_continuous(breaks = seq(50, 300, by = 50)) +
  ylim(0, 1.02) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75, 1), linetype = "dotted") +
  geom_vline(aes(xintercept = day_bin_vis), rev_main_avg, linetype = "dashed") +
  # geom_vline(xintercept = seq(50, 300, by = 50), linetype = "dashed") +
  theme_classic() +
  labs(x = "Visits", y = "Avg choice for the more profitable option ± 95% CIs")

#-----------------------
# Inter-visit intervals
#-----------------------
rev_learning_all <- rev_learning_all %>%
  group_by(day, Bat) %>%
  mutate(interval = as.integer(difftime(lead(DateTime), DateTime, units = "secs")))

rev_learning_ivi <- rev_learning_all %>%
  ungroup() %>%
  group_by(day) %>%
  filter(!is.na(interval)) %>%
  summarise(ivi = mean(interval))

# Frequency distribution of inter-visit intervals with the data from the alpha bats
filter(
  !Bat %in% bats_beta,
  !Bat %in% bats_incomp,
  day %in% main_days
) %>%
  ggplot(aes(interval, group = Bat)) +
  geom_density(geom = "line", fill = "black", color = "transparent", alpha = 0.08) +
  facet_grid(. ~ day) +
  scale_x_log10() +
  theme_bw() +
  xlab("intervisit intervals [s]") +
  scale_fill_viridis_d()

#--------------------------------------------------------------------
# The main experiment - zooming in on the choices around the reversal
#--------------------------------------------------------------------
# calculating the maximum number of visits in a block (this should be 50)
block_max <- rev_learning_ind %>% 
  ungroup() %>% 
  select(block_vis) %>% 
  summarise(block_vis = max(block_vis)) %>% 
  as.numeric()

# # setting the zoom window, i.e., the number of visits before AND after the reversal to examine
wdw <- 10
# calculating the ratio of the two to cut the visits into groups to take the average
trial_count <- 

rev_learning_zoom <- rev_learning_ind %>% 
  group_by(day, Bat, block) %>% 
  filter(ifelse(count_vis > wdw+1, block_vis > (block_max - wdw) | block_vis < (wdw + 1), 
                block_vis > (block_max - wdw)), 
         count_vis < 300 - wdw) %>% 
  group_by(day, block, count_vis) %>% 
  group_modify(~ mean_cl_boot(.x$reward_status, conf.int = 0.95)) %>% 
  ungroup() %>% 
  filter(count_vis != lead(count_vis)) %>% 
  # figure out how to calculate the reversal and finish it 
  mutate(reversal = count_vis %/% 50)


%>% 
  filter(keep == 0) %>% 
  select(-keep)

reversal <- rep(c(1:5), 3, each = (wdw*2)+1)

rev_learning_zoom %>% 
  filter(day %in% main_days) %>% 
  ggplot(aes(count_vis, y)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.3) + 
  facet_grid(day ~ reversal, scales = "free") + 
  theme_classic()


breaks_zoom <- seq(0, 3000, vis_trial)

Zoom <- rev_learning_all %>%
  group_by(Bat, day) %>%
  # group and take summary values
  # redo the column with the trials as trials = 2 visits
  mutate(
    trials = as.numeric(cut(count_vis, breaks_zoom, include.lowest = TRUE)),
    errors = 1 - reward_status
  )

# calculating errors and correct choices for the individual bats
Zoom_ind <- Zoom %>%
  group_by(day, Bat, block, trials) %>%
  summarise(
    # calculating choices for the more profitable flower for all the individual bats
    avg_prof = mean(reward_status),
    # calculating choices for the less profitable flower for all the individual bats
    avg_errors = mean(errors)
  )

# setting the window for the zoom, which is the window around the reversal that we want to examine

window <- 18
reversals <- rep(c(1:5), each = window)

Zoom <- Zoom %>%
  ungroup() %>%
  group_by(day, block, trials) %>%
  summarise(
    avg_prof_all = mean(reward_status),
    sem_prof_all = sd(reward_status) / sqrt(nbats),
    avg_err_all = mean(errors),
    sem_err_all = sd(errors) / sqrt(nbats)
  ) %>%
  mutate(
    ci_prof = 1.96 * sem_prof_all,
    ci_err = 1.96 * sem_err_all
  ) %>%
  # creating a helper column
  mutate(
    trials_zoom = ifelse(trials <= trial_count, trials, trials %% trial_count),
    trials_zoom = ifelse(trials_zoom == 0, trial_count, trials_zoom)
  ) %>%
  # filtering out the visits that happen just around each reversal
  filter(
    trials_zoom <= (window / 2) | (trials_zoom <= trial_count & trials_zoom > (trial_count - (window / 2))),
    # removing the first few visits of the night
    trials > (window / 2) & trials < (300 / vis_trial - window / 2)
  ) %>%
  ungroup() %>%
  mutate(reversal = rep(reversals, 6))

vline.data <- data.frame(trials = c(25, 50, 75, 100, 125), reversal = c(1, 2, 3, 4, 5))

Zoom %>%
  # group_by(day, block, trials, reversal) %>%
  filter(day %in% main_days) %>%
  ggplot(aes(trials, avg_prof_all)) +
  # geom_errorbar(limits,position = dodge, width = 0.25) +
  geom_line() +
  geom_point() +
  facet_grid(day ~ reversal, scales = "free") +
  geom_vline(aes(xintercept = trials), vline.data, linetype = "dashed") +
  geom_hline(yintercept = c(0.0, 0.25, 0.5, 0.75), linetype = "dotted") +
  scale_x_continuous(breaks = seq(min(Zoom$trials), max(Zoom$trials), by = 2)) +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, by = 0.2)) +
  theme_classic() +
  labs(x = "Trials", y = "Visits to more rewarding flower") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#---------------------------------------------------------------
# The main experiment - anticipation, perseveration and sampling
#---------------------------------------------------------------
# Stable phase: if two successive trials visits are above the mean for that block then the
# animal is in the stable phase

# making a separate data frame for the errors and marking the stable phases and the 3 types of errors:
# 1. anticipation errors, which occur in the unstable phase and in the trials immediately
#    before a reversal
# 2. perseveration errors, which occur in the unstable phase and in the trials immediately
#    after a reversal
# 3. sampling errors, which occur in the stable phase: these can be thought of as
#    'information-gathering' visits

error_types_ind <- rev_learning_all %>%
  # removing the beta bats
  filter(!Bat %in% bats_beta) %>%
  ungroup() %>%
  group_by(day, Bat, block) %>%
  # calculating the average proportion of visits to the more profitable option in each reversal block
  mutate(block_avg = mean(reward_status)) %>%
  group_by(day, Bat, block, block_avg, trials) %>%
  # calculating the average proportion of visits to the more profitable option in each trial
  summarise(trial_avg = mean(reward_status)) %>%
  mutate(
    # noting the 'stable' phases
    stable = ifelse(trial_avg > block_avg & lag(trial_avg) > block_avg, 1, 0),
    stable = ifelse(is.na(stable), 0, stable),
    trial_err = 1 - trial_avg,
    err_type = case_when(
      # noting the perseveration errors
      stable == 0 & trials %% 10 <= 5 ~ "perseveration",
      # noting the anticipation errors:
      # anticipation errors in the first block of the day will be filtered out before plotting
      stable == 0 & trials %% 10 > 5 ~ "anticipation",
      # noting the sampling errors
      stable == 1 ~ "sampling"
    )
  ) %>%
  # removing the anticipation and perseveration errors in the first block of all days
  # as they cannot be definition occur until the bats have experienced at least one reversal
  mutate(keep = case_when(
    block == 1 & err_type == "sampling" ~ 1,
    block != 1 ~ 1,
    block == 1 & err_type != "sampling" ~ 0
  )) %>%
  filter(keep == 1) %>%
  select(-keep)

plot_error <- function(tbl, type) { # create plot of differences in discrimination performances
  tbl %>%
    filter(
      err_type == type,
      !day %in% c("Day 4", "Day 5", "Day 6")
    ) %>%
    ungroup() %>%
    group_by(day, block) %>%
    summarise(
      # calculating the proportion of an error type made in a particular block:
      # in a block, what proportion of choices were of this error type?
      err_avg = mean(trial_err),
      err_ci = 1.96 * (sd(trial_err) / sqrt(nbats))
    ) %>%
    ggplot(aes(block, err_avg)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymax = err_avg + err_ci, ymin = err_avg - err_ci), position = position_dodge(width = 0.1), width = 0.25) +
    facet_grid(. ~ day) +
    scale_x_continuous(breaks = seq(1, 6, by = 1)) +
    ylim(0, 1) +
    geom_hline(yintercept = 0.5, linetype = "dotted") +
    geom_vline(xintercept = seq(1, 6, by = 1), linetype = "dashed") +
    theme_classic() +
    labs(x = "Trials", y = "Average proportion of errors made ± 95% CIs") +
    theme_classic()
}

# plot of perseveration errors
plot_error(error_types_ind, "anticipation")

# plot of anticipation errors
plot_error(error_types_ind, "perseveration")

# plot of sampling errors
plot_error(error_types_ind, "sampling")

#----------------------
# Change point analysis
#----------------------
CPA <- SRL_block %>%
  arrange(DateTime) %>%
  group_by(day, Bat) %>% # Assigning Flower Numbers
  mutate(Flower = ifelse((as.numeric(str_extract(unitLabel, "[0-9]+"))) %% 2 == 1, 0, 1))
Batn <- CPA %>% filter(Bat == "Bat16") # Taking out 1 bat a time - insert bat number here
Batn_cp <- data.frame(Batn$Flower) # Just the trial data
cp.3 <- cp_wrapper(Batn_cp, TRUE, "binomial", 1.8) # CP function from the package
cp.3 <- cp.3 %>%
  filter(Trial != 0) %>%
  mutate(Reversal = seq(50, 850, 50))
Batn_cp <- data.frame(
  Trial = 1:length(Batn_cp[, ]),
  CumRespMeasure = cumsum(Batn_cp)[, ]
)
# plot for cumulative responses
pl3 <- ggplotGrob(ggplot(Batn_cp) +
  geom_line(aes(Trial, CumRespMeasure)) +
  geom_point(data = cp.3, aes(Trial, CumSs), size = 2) +
  geom_vline(xintercept = seq(0, 900, 50)) +
  geom_vline(xintercept = seq(25, 900, 50), linetype = "dashed") +
  geom_vline(xintercept = seq(300, 900, 300), color = "red", size = 1) +
  labs(x = "Trials", y = "Visits to even-numbered flower"))
# plot for average response rate per trial
pl4 <- ggplotGrob(ggplot(cp.3) +
  geom_step(aes(Trial, Slopes)) +
  ylab("Average response rate to even-numbered flower") +
  geom_hline(yintercept = c(0.0, 0.5, 1.0), linetype = "dashed"))
grid::grid.draw(pl4)

#----------------------------------
# The effect of number of reversals
#----------------------------------
# Did accuracy increase with the number of reversals experienced?
rev_learning_stat <- rev_learning_ind %>%
  ungroup() %>%
  filter(day %in% main_days) %>%
  mutate(
    day = ifelse(day == "Day 1", 1, ifelse(day == "Day 2", 2, 3)),
    bins = bins %% 5,
    bins = ifelse(bins == 0, 5, bins)
  ) %>%
  group_by(Bat, day, block, bins) %>% 
  summarise(avg_prof = mean(reward_status))

prof <- stan_glmer(avg_prof ~ (1 | Bat) + day + block + bins + day * block + block * bins,
  data = rev_learning_stat,
  family = binomial
)

summary <- as.data.frame(summary(prof, probs = c(0.025, 0.5, 0.975), digits = 3)) %>% 
  filter(row_number() < 7)

posterior_interval(prof, prob = 0.95)

pers_1 <- rev_learning_stat %>%
  filter(bins %% 10 == 1)
pers_2 <- rev_learning_stat %>%
  filter(bins %% 10 == 2)
pers_3 <- rev_learning_stat %>%
  filter(bins %% 10 == 3)

ant <- rev_learning_stat %>%
  filter(bins %% 10 == 4)

pers_1 %>%
  ungroup() %>%
  group_by(day, block) %>%
  summarise(avg_errors_all = mean(avg_errors)) %>%
  ggplot(aes(block, avg_errors_all)) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ day) +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  ylim(0, 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  xlab("Reversal block") +
  ylab("Perseveration errors made") +
  theme_classic()

pers_2 %>%
  ungroup() %>%
  group_by(day, block) %>%
  summarise(avg_errors_all = mean(avg_errors)) %>%
  ggplot(aes(block, avg_errors_all)) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ day) +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  ylim(0, 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  xlab("Reversal block") +
  ylab("Perseveration errors made") +
  theme_classic()

pers_3 %>%
  ungroup() %>%
  group_by(day, block) %>%
  summarise(avg_errors_all = mean(avg_errors)) %>%
  ggplot(aes(block, avg_errors_all)) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ day) +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  ylim(0, 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  xlab("Reversal block") +
  ylab("Perseveration errors made") +
  theme_classic()

ant %>%
  ungroup() %>%
  group_by(day, block) %>%
  summarise(avg_errors_all = mean(avg_errors)) %>%
  ggplot(aes(block, avg_errors_all)) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ day) +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  ylim(0, 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  xlab("Reversal block") +
  ylab("Anticipation errors made") +
  theme_classic()
