#Random slopes and random intercepts models 
######
#Data prep 
######
setwd("/Users/shambhavi/Google Drive/Experiments & Data/srl_2017_backup_additionalfiles/Analysis/Analysis/Updated clean code")
raw_data <- read.csv2("Raw_data.csv", sep = ";", header = TRUE)

# creating a vector of the bats to be excluded from the main analysis
bats_incomp <- c("Bat7", "Bat19")

# creating a vector of the main experimental days
main_days <- c("Day 1", "Day 2", "Day 3")

# setting binize and breaks for cutting up the data into block and bin
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
         bin = "") %>%
  ungroup() %>%
  group_by(day, Bat, block) %>%
  # cutting the visits inside each block into bin of the size set earlier
  mutate(bin = as.numeric(cut(block_vis, breaks, include.lowest = TRUE)))

# creating a vector with the beta bats
bats_beta <- c("Bat1", "Bat2", "Bat3", "Bat4")

# creating a data frame with just the beta bats
rev_learning_beta <- rev_learning_all %>%
  filter(Bat %in% bats_beta)

# creating a data frame with the alpha bats and the three main days of the experiment
rev_learning_ind <- rev_learning_all %>%
  filter(!Bat %in% bats_beta, 
         day %in% main_days)
#######
# Model 1: The effect of day, block and bin on the proportion of visits to the S+ 
#######
analysis1.2 <- rev_learning_ind %>% 
  ungroup() %>% 
  select(Day, block, bin, Bat, reward_status, block_vis, count_vis) %>% 
  filter(block_vis != 0)
incomplete <- c("Bat12", "Bat14", "Bat20", "Bat15", "Bat13")

######
#Model 1.1: All bats 
######
m1.1 <-
  brm(data = analysis1.2, family = binomial,
      reward_status | trials(1) ~ Day + block + bin +
        Day:block +
        block:bin +
        Day:bin +
        (1 + Day + block + bin | Bat), # random slopes 
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9995, max_treedepth = 15),  
      seed = 12)

# save the model 
save(m1.1, file = "03_stats_rs_m1.1.rda")
#here is the model with the results, ready to be loaded. 
load("03_stats_rs_m1.1.rda")
m1.1 <- m1.2.1
rm(m1.2.1)
#checking the health of the Markov chains - diagnostic trace plots 
color_scheme_set("blue")
plot(m1.1)
# here's the model 
print(m1.1)
# here's a summary of the posterior distribution as a table 
posterior_summary(m1.1)
# pairs plots - this is the posterior distribution 
pairs(m1.1,
      #main = "Pairs plot for model with day, block, bin", 
      off_diag_args = list(size = 0.1, alpha = 0.1))

# look at the neff/N plot too (ratio of effective sample size to total sample size)
neff_ratio(m1.1) %>% 
  mcmc_neff()  
#theme_bw()

# the coefficient plot 
# mcmc_plot(m1.1, 
#           pars = c("^b_", "^sd_"), 
#           size = 1) +
#   theme_bw() +
#   geom_vline(xintercept = 0) + 
#   theme(axis.text.y = element_text(hjust = 0))

mcmc_intervals(m1.1, 
               pars = vars(2:7), 
               point_size = 1.75) + 
  geom_vline(xintercept = 0) + 
  theme_bw()

# let's do the posterior predictions for the same individuals to check the model fit.
nd <- analysis1.2 %>% 
  select(Bat, Day, block, bin, count_vis)

post_fit <-
  predict(m1.1) %>% 
  as_tibble() %>% 
  mutate(Bat = nd$Bat, 
         Day = nd$Day,
         block = nd$block, 
         bin = nd$bin, 
         count_vis = nd$count_vis)

comparison1.1 <- analysis1.2 %>% 
  group_by(Bat, Day, block, bin) %>% 
  group_modify(~ mean_cl_boot(.x$reward_status, conf.int = 0.95))

comparison1.1 <- left_join(comparison1.1, post_fit, by = c("Bat", "Day", "block", "bin"))

comparison1.1 %>% 
  ggplot(aes(count_vis)) + 
  #geom_point(aes(y = y), color = "red", size = 0.2) +
  geom_line(aes(y = y), color = "red") + 
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) + 
  #geom_point(aes(y = Estimate), color = "blue", size = 0.2) + 
  geom_line(aes(y = Estimate), color = "blue") + 
  #geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "blue", alpha = 0.3) + 
  geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "dashed", size = 0.2) + 
  xlab("Visit count") + 
  ylab("Proportion of visits to the S+ option") + 
  facet_grid(Day ~ Bat) + 
  theme_classic()
######
#Model 1.2: Only those bats that completed the experiment 
######
analysis1.2 %>% filter(!Bat %in% incomplete)

m1.2 <-
  brm(data = analysis1.2, family = binomial,
      reward_status | trials(1) ~ Day + block + bin +
        Day:block +
        block:bin +
        Day:bin +
        (1 + Day + block + bin | Bat), # random slopes 
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.9995, max_treedepth = 15),  
      seed = 12)

# save the model 
save(m1.2, file = "03_stats_rs_m1.2.rda")
#here is the model with the results, ready to be loaded. 
load("03_stats_rs_m1.2.rda")
m1.2 <- m1.2.1
rm(m1.2.1)
#checking the health of the Markov chains - diagnostic trace plots 
plot(m1.2)
# here's the model 
print(m1.2)
# here's a summary of the posterior distribution as a table 
posterior_summary(m1.2)
# pairs plots - this is the posterior distribution 
pairs(m1.2,
      #main = "Pairs plot for model with day, block, bin", 
      off_diag_args = list(size = 0.1, alpha = 0.1))

# look at the neff/N plot too (ratio of effective sample size to total sample size)
neff_ratio(m1.2) %>% 
  mcmc_neff()  
#theme_bw()

# the coefficient plot 
# mcmc_plot(m1.2, pars = c("^b_", "^sd_")) +
#   theme_bw() +
#   geom_vline(xintercept = 0) +
#   theme(axis.text.y = element_text(hjust = 0))

mcmc_intervals(m1.2, 
               pars = vars(2:7), 
               point_size = 1.75) + 
  geom_vline(xintercept = 0) + 
  theme_bw()

# let's do the posterior predictions for the same individuals to check the model fit.
nd <- analysis1.2 %>% 
  select(Bat, Day, block, bin, count_vis) %>% 
  filter(!Bat %in% incomplete)

post_fit <-
  predict(m1.2) %>% 
  as_tibble() %>% 
  mutate(Bat = nd$Bat, 
         Day = nd$Day,
         block = nd$block, 
         bin = nd$bin, 
         count_vis = nd$count_vis)

comparison1.2 <- analysis1.2 %>% 
  filter(!Bat %in% incomplete) %>% 
  group_by(Bat, Day, block, bin) %>% 
  group_modify(~ mean_cl_boot(.x$reward_status, conf.int = 0.95))

comparison1.2 <- left_join(comparison1.2, post_fit, by = c("Bat", "Day", "block", "bin"))

comparison1.2 %>% 
  ggplot(aes(count_vis)) + 
  #geom_point(aes(y = y), color = "red", size = 0.2) +
  geom_line(aes(y = y), color = "red") + 
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) + 
  #geom_point(aes(y = Estimate), color = "blue", size = 0.2) + 
  geom_line(aes(y = Estimate), color = "blue") + 
  #geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "blue", alpha = 0.3) + 
  geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "dashed", size = 0.2) + 
  xlab("Visit count") + 
  ylab("Proportion of visits to the S+ option") + 
  facet_grid(Day ~ Bat) + 
  theme_classic()
######
#Is there a difference between the model with the complete bats and the one with them all? 
######
#do this later with grid arrange 
######
# Model 2: The effect of block and bin on the proportion of visits to the S+, excluding day completely. 
######
analysis1.3 <- rev_learning_ind %>% 
  ungroup() %>% 
  select(Day, block, bin, Bat, reward_status, block_vis, count_vis) %>% 
  mutate(block = case_when(Day == 1 ~ block, 
                           Day == 2 ~ block + 6, 
                           Day == 3 ~  block + 12)) %>% 
  select(-Day) %>% 
  group_by(block, bin) %>% 
  filter(block_vis != 0)

m2 <-
  brm(data = analysis1.3, family = binomial,
      reward_status | trials(1) ~ block + bin + block:bin + (1 + block + bin | Bat),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3,
      control = list(adapt_delta = 0.995, max_treedepth = 15),
      seed = 12)

save(m2, file = "03_stats_rs_m2.rda")
load("03_stats_rs_m2.rda")
m2 <- m1.3.1
rm(m1.3.1)

#diagnostic trace plots 
color_scheme_set("green")
plot(m2)
# pairs plots
pairs(m2,
      off_diag_args = list(size = 0.1, alpha = 0.1))

# look at the neff/N plot too 
neff_ratio(m2) %>% 
  mcmc_neff() +
  theme_bw()

# the coefficient plot
mcmc_plot(m2, pars = c("^b_", "^sd_")) +
  theme_bw() +
  geom_vline(xintercept = 0) + 
  theme(axis.text.y = element_text(hjust = 0))

mcmc_intervals(m2, 
               pars = vars(2:4), 
               point_size = 1.75) + 
  geom_vline(xintercept = 0) + 
  theme_bw()
###### 
#Comparison of Model 1.1 and 2
######
m1.1 <- add_criterion(m1.1, "waic")
m2 <- add_criterion(m2, "waic")
w <- loo_compare(m1.1, m2, criterion = "waic") %>% 
  print(simplify = F)

m1.1 <- add_criterion(m1.1, "loo")
m2 <- add_criterion(m2, "loo")
w <- loo_compare(m1.1, m2, criterion = "loo") %>% 
  print(simplify = F)

#these are the Akaike weights of the 2 models 
model_weights(m1.1, m2, weights = "waic") %>% 
  round(digits = 4)
model_weights(m1.1, m2, weights = "loo") %>% 
  round(digits = 4)
loo(m1.1, m2)

# THE MODEL THAT INCLUDES DAY IS WAY MORE LIKELY TO MAKE THE BEST PREDICTIONS ON NEW DATA 

w[, 7:8] %>% 
  data.frame() %>% 
  rownames_to_column(var = "model_name") %>% 
  ggplot(aes(x    = model_name, 
             y    = waic, 
             ymin = waic - se_waic, 
             ymax = waic + se_waic)) +
  geom_pointrange(shape = 21, color = "black", fill = "red") +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "WAIC comparison for the day-block-bin model and block-bin model") +
  theme_classic()
######
# Model 3: The effect of day, block and bin on the proportion of visits to the S+, excluding day 1 
#NOT YET EXECUTED
######
analysis1.4 <- rev_learning_ind %>% 
  ungroup() %>% 
  select(Day, block, bin, Bat, reward_status, block_vis, count_vis) %>% 
  filter(block_vis != 0, 
         Day == 1)

m1.4.1 <-
  brm(data = analysis1.4, family = binomial,
      reward_status | trials(1) ~ Day + block + bin + Day:block + block:bin +
        Day:bin + (1 + Day + block + bin| Bat),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 5, thin = 3, 
      control = list(adapt_delta = 0.995, 
                     max_treedepth = 12),
      seed = 12)

# save the model
save(m1.4.1, file = "03_stats_m1.4.1.rda")
#here is the model with the results, ready to be loaded. 
load("03_stats_m1.4.rda")
summary(m1.4.1)

#diagnostic trace plots 
color_scheme_set("green")
post <- posterior_samples(m1.4.1, add_chain = T)
post %>% 
  select(-lp__, -iter) %>% 
  mcmc_trace(facet_args = list(ncol = 4)) +
  theme(legend.position = c(.75, .06)) 

# pairs plots
pairs(m1.4.1,
      off_diag_args = list(size = 0.1, alpha = 0.1))

# look at the neff/N plot too 
neff_ratio(m1.4.1) %>% 
  mcmc_neff() +
  theme_bw()

# the coefficient plot
mcmc_plot(m1.4.1, pars = c("^r_", "^b_", "^sd_")) +
  theme_bw() +
  theme(axis.text.y = element_text(hjust = 0))

mcmc_intervals(m1.1, 
               pars = vars(2:7), 
               point_size = 1.75) + 
  geom_vline(xintercept = 0) + 
  theme_bw()

#THERE IS STILL AN EFFECT OF BOTH DAY AND BIN EVEN WHEN DAY 1 IS EXCLUDED

# posterior predictions for the same individuals 
nd1.4 <- analysis1.4 %>% 
  select(Bat, Day, block, bin)

post_fit <-
  fitted(m1.4.1,
         newdata = nd1.4) %>% 
  as_tibble() %>% 
  mutate(Bat = analysis1.4$Bat, 
         Day = analysis1.4$Day,
         block = analysis1.4$block, 
         bin = analysis1.4$bin, 
         count_vis = analysis1.4$count_vis)

comparison1.4.1 <- analysis1.4 %>% 
  group_by(Bat, Day, block, bin) %>% 
  group_modify(~ mean_cl_boot(.x$reward_status, conf.int = 0.95))

comparison1.4.1 <- left_join(comparison1.4.1, post_fit, by = c("Bat", "Day", "block", "bin"))

comparison1.4.1 %>% 
  ggplot(aes(count_vis)) + 
  #geom_point(aes(y = y), color = "red", size = 0.2) +
  geom_line(aes(y = y), color = "red") + 
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) + 
  #geom_point(aes(y = Estimate), color = "blue", size = 0.2) + 
  geom_line(aes(y = Estimate), color = "blue") + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "blue", alpha = 0.3) + 
  geom_vline(xintercept = c(50, 100, 150, 200, 250), linetype = "dashed", size = 0.2) + 
  xlab("Visit count") + 
  ylab("Proportion of visits to the S+ option") + 
  facet_grid(Day ~ Bat) + 
  theme_classic()
######
# Model 4: The effect of day and block on the number of perseverative visits following a reversal 
######
analysis3 <- rev_learning_ind %>% 
  ungroup() %>% 
  select(Day, block, Bat, reward_status, block_vis, count_vis) %>%
  filter(block_vis != 0) %>% 
  group_by(Bat, Day, block) %>% 
  mutate(first_rew = cumsum(reward_status)) %>% 
  filter(block > 1, 
         first_rew == 1, 
         reward_status == 1)

m4 <-
  brm(data = analysis3, family = negbinomial,
      block_vis ~ Day + block + Day:block + (1 + Day + block| Bat),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4, thin = 3, 
      control = list(adapt_delta = 0.999, 
                     max_treedepth = 12),
      seed = 12)

save(m4, file = "03_stats_rs_m4_negbinom.rda")
load("03_stats_rs_m4_negbinom.rda")
m4 <- m3.1.1
rm(m3.1.1)
mcmc_plot(m4, pars = c("^b_", "^sd_")) +
  theme_bw() +
  geom_vline(xintercept = 0) + 
  theme(axis.text.y = element_text(hjust = 0))

#A negative-binomial model, more use- fully called a gamma-Poisson model, assumes that each Poisson 
#count observation has its own rate - Stat. Rethinking. This is what we are using here

# checking the trace plots 
color_scheme_set("purple")
plot(m4)
#model summary
print(m4)
#posterior summary
posterior_summary(m4)
# pairs plots - this is the posterior distribution 
pairs(m4, 
      #main = "Pairs plot for model with day, block, bin", 
      off_diag_args = list(size = 0.1, alpha = 0.1))

#the n_eff/N ratio 
neff_ratio(m4) %>% 
  mcmc_neff() +
  theme_bw()

#plotting the coefficients
mcmc_plot(m4, pars = c("^b_", "^sd_")) +
  theme_bw() +
  theme(axis.text.y = element_text(hjust = 0))

mcmc_intervals(m4, 
               pars = vars(2:4), 
               point_size = 1.75) + 
  geom_vline(xintercept = 0) + 
  theme_bw()

#posterior predictions 
nd4 <- analysis3 %>% 
  select(Bat, Day, block)

post_fit <-
  fitted(m4,
         newdata = nd4) %>% 
  as_tibble() %>% 
  mutate(Bat = analysis3$Bat, 
         Day = analysis3$Day,
         block = analysis3$block, 
         count_vis = analysis3$block_vis)

comparison4 <- analysis3 %>% 
  group_by(Bat, Day, block) %>% 
  group_modify(~ mean_cl_boot(.x$block_vis, conf.int = 0.95))

comparison4 <- left_join(analysis3, post_fit, by = c("Bat", "Day", "block"))

comparison4 %>% 
  ggplot(aes(block)) + 
  #geom_point(aes(y = y), color = "red", size = 0.2) +
  geom_line(aes(y = block_vis), color = "red") + 
  #geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) + 
  geom_point(aes(y = Estimate), color = "blue", size = 0.2) + 
  geom_line(aes(y = Estimate), color = "blue") + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "blue", alpha = 0.3) +
  xlab("Block") + 
  ylab("Visits to the previous S+ before the first visit to the new S+ after a reversal") + 
  facet_grid(Day ~ Bat) + 
  theme_bw()


######
#Model 5: The effect of day and bin on the proportion of visits to the S+ in block 1 of every day 
#####
#For the following analysis the response variable is a subset of the whole dataset: day and bin as 
#predictor variables, only analysing block 1
analysis2 <- rev_learning_ind %>% 
  ungroup() %>% 
  select(Day, block, bin, Bat, reward_status, block_vis, count_vis) %>% 
  filter(block == 1) %>% 
  group_by(Day, bin) %>% 
  filter(block_vis != 0) %>% 
  select(-block)

m5 <-
  brm(data = analysis2, family = binomial,
      reward_status | trials(1) ~ Day + bin + Day:bin + (1 + Day + bin| Bat),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4, thin = 3, 
      control = list(adapt_delta = 0.999, 
                     max_treedepth = 15),
      seed = 12)

save(m5, file = "03_stats_m5.rda")

load("03_stats_m5.rda")
#checking the trace plots 

color_scheme_set("orange")
post <- posterior_samples(m5, add_chain = T)

post %>% 
  select(-lp__, -iter) %>% 
  mcmc_trace(facet_args = list(ncol = 4)) +
  theme(legend.position = c(.75, .06)) 
plot(m5)

# coefficients in the table form 
summary(m5)
print(m5)

#coefficients
mcmc_plot(m5, pars = c("^b_", "^sd_")) +
  theme_bw() +
  geom_vline(xintercept = 0) + 
  theme(axis.text.y = element_text(hjust = 0))

mcmc_intervals(m5, 
               pars = vars(2:4), 
               point_size = 1.75) + 
  geom_vline(xintercept = 0) + 
  theme_bw()
#posterior predictions 
nd5 <- analysis2 %>% 
  select(Bat, Day, bin)

post_fit <-
  predict(m5,
         newdata = nd5) %>% 
  as_tibble() %>% 
  mutate(Bat = analysis2$Bat, 
         Day = analysis2$Day,
         bin = analysis2$bin, 
         block_vis = analysis2$block_vis)

comparison5 <- analysis2 %>% 
  group_by(Bat, Day, bin) %>% 
  group_modify(~ mean_cl_boot(.x$reward_status, conf.int = 0.95))

comparison5 <- left_join(comparison5, post_fit, by = c("Bat", "Day", "bin"))

comparison5 %>% 
  ggplot(aes(block_vis)) + 
  #geom_point(aes(y = y), color = "red", size = 0.2) +
  geom_line(aes(y = y), color = "red") + 
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) + 
  geom_point(aes(y = Estimate), color = "blue", size = 0.2) + 
  geom_line(aes(y = Estimate), color = "blue") + 
  #geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "blue", alpha = 0.3) +
  xlab("Visits") + 
  ylab("Proportion of visits to the S+") + 
  facet_grid(Day ~ Bat) + 
  theme_bw()
