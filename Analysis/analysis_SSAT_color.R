library(plyr)
library(dplyr)
library(skimr)
library(ez)

setwd("/Volumes/Mavericks/Users/ray/Workspace/Data_Analysis/SSAT_Color")

file.list <- list.files(
  path = "pilot_data",
  pattern = 'txt',
  full.names = TRUE
)

dat <- ldply(
  file.list, function(file.name) {
    temp = read.csv(file.name, header = TRUE, sep = '\t', skip = 16)
    return(temp)
  })

dat <- dat %>% filter(!block_num %in% c(1:8))

dat$set_size <- as.factor(dat$set_size)

dat_space <- dat %>% filter(search_type == "space")
dat_time <- dat %>% filter(search_type == "time")

space_tally <- dat_space %>%
  select(set_size, present_absent, target_distractor, distractor_distractor) %>%
  group_by(set_size, present_absent, target_distractor, distractor_distractor) %>%
  summarise(trial_count = n())

space_rt <- dat_space %>%
  select(set_size, present_absent, target_distractor, distractor_distractor, spatial_rt) %>%
  group_by(set_size, present_absent, target_distractor, distractor_distractor) %>%
  summarise(meanRT = mean(spatial_rt))

ezPlot(
  data = subset(dat_space, present_absent == 'present')
  , wid = participant
  , dv = spatial_rt
  , within = .(set_size, target_distractor, distractor_distractor)
  , x = set_size
  , row = target_distractor
  , col = distractor_distractor
)

time_tally <- dat_time %>%
  select(present_absent, target_distractor, distractor_distractor) %>%
  group_by(present_absent, target_distractor, distractor_distractor) %>%
  summarise(trial_count = n())

time_rt <- dat_time %>%
  select(present_absent, target_distractor, distractor_distractor, temporal_rt) %>%
  group_by(present_absent, target_distractor, distractor_distractor) %>%
  summarise(meanRT = mean(temporal_rt))

ezPlot(
  data = subset(dat_time, present_absent == 'present')
  , wid = participant
  , dv = temporal_rt
  , within = .(target_distractor, distractor_distractor)
  , within_full = present_absent
  , x = distractor_distractor
  , col = target_distractor
)