library(data.table)
library(ggplot2)
library(tidyverse)

unzip("data_exp_244517-v4.zip")

gorilla_import <- function(folder){
  csvfiles <- list.files(folder, pattern = ".csv", full.names = T, recursive = T)
  data <- list()
  for (datatype in c ("task", "questionnaire")){
    compiled_data <- data.table()
    
    for (f in grep(x = csvfiles, pattern = datatype, value = T)){
      newdata <- read.csv(f)
      newdata$filename <- f
      newdata$pid <- as.factor(newdata$Participant.Private.ID)
    nn <- colnames(newdata)[!colnames(newdata) %in% colnames(compiled_data)]
    if (length(nn) > 0){compiled_data[,nn] <- NA}
      nn <- colnames(compiled_data)[!colnames(compiled_data) %in% colnames(newdata)]
    if (length(nn) > 0){newdata[,nn] <- NA}
      compiled_data <- rbind(compiled_data, newdata)
    }
    
    data[[datatype]] <- compiled_data
  }
  return(data)
}

data <- gorilla_import("gorilla_data")

pres_data <- data$task %>%
  filter(Display == "post_single" & Response.Type == "continue") %>%
  select(pid, 
         datetime = Local.Date.and.Time,
         trial = Trial.Number,
         type = Spreadsheet..type,
         item = Spreadsheet..item,
         exposure_time = Reaction.Time)

## looking time for each social media posts (16 in total)

summary <- pres_data %>%
  summarise(mean = mean(exposure_time),
            sd = sd(exposure_time),
            sd_upper = mean + sd * 3,
            sd_lower = mean - sd * 3)

pres_data <- pres_data %>%
  mutate(use = ifelse(exposure_time < mean(exposure_time) - 3 * sd(exposure_time) | 
                        exposure_time > mean(exposure_time) + 3 * sd(exposure_time), FALSE, TRUE))
## outliers are those that have an exposure time outside 3 sd from the mean

pres_data %>% filter(use == FALSE)
## all values are greater than 87552

react_data <- data$task %>%
  filter(Display == "post_single",
         Response.Type == "action", 
         Tag == "react") %>%
  select(pid, 
         trial = Trial.Number,
         reaction = Response,
         rt = Reaction.Time) %>%
  mutate(reaction = paste0("react_", reaction), react = 1)

react_data <- react_data %>%
  pivot_wider(id_cols = c(pid, trial),
              names_from = reaction,
              values_from = react, 
              values_fn = sum,
              values_fill = 0)

pres_data <- pres_data %>%
  left_join(react_data, by = c("pid", "trial"))

pres_data <- pres_data %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

pres_data <- pres_data %>%
  mutate(react = react_angry + react_heart + react_like + react_laugh + react_surprise + react_sad)

share_data <- data$task %>% 
  filter(Display == "post_single" & Response.Type == "action" & Tag == "share") %>%
  select(pid,
         trial = Trial.Number) %>%
  mutate(share = 1)

pres_data <- left_join(pres_data, share_data, by = c("pid", "trial"))

pres_data <- pres_data %>% mutate(share = ifelse(is.na(share), 0, share))

comment_data <- data$task %>%
  filter(Display == "post_single" & Response.Type == "action" & Tag == "comment") %>%
  select(pid,
         trial = Trial.Number,
         comment = Response)

pres_data <- left_join(pres_data, comment_data, by = c("pid", "trial"))

pres_data <- pres_data %>% mutate(comment = ifelse(!is.na(comment), 1, 0))

write.csv(pres_data, "presentation_data.csv")

data_demo <-data$questionnaire %>%
  filter(Task.Name == "Demographics") %>%
  select(pid,
         item = Object.Name, 
         key = Key, 
         r = Response)

data_demo <- data_demo %>%
   filter(r != "BEGIN" & r != "END" & key != "") %>%
  pivot_wider(id_cols = pid,
              names_from = c(item, key),
              values_from = r)

number_cols <- c("age_value","live_dist_value", "live_time_value",
                 grep(x = colnames(data_demo),pattern="quantised",value = T))

data_demo <- data_demo %>%
  mutate(across(all_of(number_cols), as.numeric)) %>%
  rename_with(~ str_remove(.x, "_value$"), contains("_value"))

data_behav <- data$questionnaire %>% 
  filter(Task.Name == "Behavioural intentions and pilot qs" & Key == "value") %>% 
  select(pid, item = Object.Name, response = Response) %>%
  group_by(pid) %>%
  mutate(mcq_count = cumsum(item == "Multiple Choice"),
         item = ifelse(item == "Multiple Choice" & mcq_count %% 2 != 0, "Multiple_Choice_Group",
                ifelse(item == "Multiple Choice" & mcq_count %% 2 == 0, "Multiple_Choice_Identification", item))) %>%
  ungroup() %>%
  select(-mcq_count)

numeric_items <- c("trees_find_out", "trees_water", "filler_travel", "filler_watch", "filler_pizza", "filler_sci")


data_behav <- data_behav %>%
  mutate(response_numeric = ifelse(item %in% numeric_items,
      suppressWarnings(as.numeric(response)), NA))

write.csv(data_behav, "behav_intentions.csv")

mem_data <- data$task %>%
  filter(Display == "test item" & (Response == "true" | Response == "false")) %>%
  select(pid, trail = Trial.Number, type = Spreadsheet..type,
         memitem = Spreadsheet..memitem, 
         acc = Correct,
         response = Response, 
         rt = Reaction.Time) %>%
  mutate(response = ifelse(response == "true", TRUE, FALSE),
         rt = as.numeric(rt))

mem_data <-  mem_data %>% mutate(use = ifelse(rt<mean(rt) - 3 * sd(rt) | rt>mean(rt) + 3 * sd(rt), FALSE, TRUE))

summary_mem <- mem_data %>%
  summarise(mean = mean(rt),
            sd = sd(rt),
            sd_upper = sd * 3 + mean,
            sd_lower = mean - sd * 3)

memacc_data <- mem_data %>% filter(use == TRUE) %>% pivot_wider(id_cols = pid,
                                                                names_from = type,
                                                                values_from = acc,
                                                                values_fn = mean)

memacc_data <- memacc_data %>% rename("mem_acc_filler" = "filler","mem_acc_trees" = "trees")

memrt_data <-mem_data %>% 
  filter(acc == 1 & use == TRUE) %>% 
  pivot_wider(id_cols = pid, 
              names_from = type, 
              values_from = rt, 
              values_fn = mean)

memrt_data <- memrt_data %>% rename("mem_rt_filler" = "filler","mem_rt_trees" = "trees")

write.csv(mem_data,"memory_data.csv")

pd <- full_join(data_demo, pres_data %>% 
                  filter(type == "trees") %>% 
                  select(pid,
                         exposure_time, 
                         react, 
                         share,
                         comment), 
                by = "pid")

pd <- full_join(pd, data_behav %>%
    pivot_wider(
      id_cols = pid,
      names_from = item,
      values_from = c(response_numeric, response)) %>%
    select(pid, 
           trees_find_out = response_numeric_trees_find_out, 
           trees_water = response_numeric_trees_water,
           Multiple_Choice_Group = response_Multiple_Choice_Group,
           Multiple_Choice_Identification = response_Multiple_Choice_Identification,
           Feedback_Comments = `response_Feedback and comments`),
  by = "pid")

pd <- pd %>%
  left_join(data$task %>%
              group_by(pid) %>%
              summarise(group_allocation = first(`randomiser.i5rb`)),
            by = "pid")

pd <- pd %>%
  left_join(memacc_data, by = "pid")

memacc_data$total_mean <- rowMeans(memacc_data[ , -1])

summary_memory <- memacc_data %>%
  summarise(mean = mean(total_mean),
            sd = sd(total_mean),
            upper = sd * 2 + mean,
            lower = mean - sd *2)


write.csv(pd, "participant_data.csv")

old_group <- pd %>%
  filter(age > 23)

young_group <- pd %>% 
  filter(age <= 23)

young_group$condition <- NA

for (i in 1:nrow(young_group)){
  if (young_group$group_allocation[i] == "Out group"){
    young_group$condition[i] <- "Outgroup"
  }else{
    young_group$condition[i] <- "Ingroup"
  }
}

old_group$condition <- NA

for (i in 1:nrow(old_group)){
  if (old_group$group_allocation[i] == "Out group"){
    old_group$condition[i] <- "Ingroup"
  }else{
    old_group$condition[i] <- "Outgroup"
  }
}

write.csv(young_group, "young_group.csv")
write.csv(old_group, "old_group.csv")

ggplot(young_group_memory, aes(x = condition, y = trees_water, colour = condition)) + 
  geom_violin() + 
  geom_boxplot(width = 0.2, alpha = 0.5) + 
  theme_classic() + 
  labs(y = "Intention to water trees",
       x = "Condition")

ggplot(old_group_memory, aes(x = condition, y = trees_water, colour = condition)) + 
  geom_violin() + 
  geom_boxplot(width = 0.2, alpha = 0.5) + 
  theme_classic() + 
  labs(y = "Intention to water trees",
       x = "Condition")

ggplot(young_group_memory, aes(x = condition, y = trees_find_out, colour = condition)) + 
  geom_violin() + 
  geom_boxplot(width = 0.2, alpha = 0.5) + 
  theme_classic() + 
  labs(y = "Intention to find out more",
       x = "Condition")

ggplot(old_group_memory, aes(x = condition, y = trees_find_out, colour = condition)) + 
  geom_violin() + 
  geom_boxplot(width = 0.2, alpha = 0.5) + 
  theme_classic() + 
  labs(y = "Intention to find out more",
       x = "Condition")

ingroup_young <- young_group %>%
  filter(condition == "Ingroup") %>%
  pull(trees_water)

outgroup_young <- young_group %>%
  filter(condition == "Outgroup") %>%
  pull(trees_water)

t.test(ingroup_young, outgroup_young, paired = FALSE)

ingroup_old <- old_group %>%
  filter(condition == "Ingroup") %>%
  pull(trees_water)

outgroup_old <- old_group %>%
  filter(condition == "Outgroup") %>%
  pull(trees_water)

t.test(ingroup_old, outgroup_old, paired = FALSE)

young_people_looking <- young_group %>%
  filter(exposure_time > mean(exposure_time) - 3 * sd(exposure_time) & exposure_time < mean(exposure_time) + 3 * sd(exposure_time))

old_people_looking <- old_group %>%
  filter(exposure_time > mean(exposure_time) - 3 * sd(exposure_time) & exposure_time < mean(exposure_time) + 3 * sd(exposure_time))

ingroup_look_old <- old_people_looking %>%
  filter(condition == "Ingroup") %>%
  pull(exposure_time)


outgroup_look_old <- old_people_looking %>%
  filter(condition == "Outgroup") %>%
  pull(exposure_time)

t.test(ingroup_look_old, outgroup_look_old)

ingroup_look_young <- young_people_looking %>%
  filter(condition == "Ingroup") %>%
  pull(exposure_time)


outgroup_look_young <- young_people_looking %>%
  filter(condition == "Outgroup") %>%
  pull(exposure_time)


t.test(ingroup_look_young, outgroup_look_young)

ggplot(young_group_memory, aes(x = condition, y = exposure_time, colour = condition)) + 
  geom_violin() + 
  theme_classic()

ggplot(old_group_memory, aes(x = condition, y = exposure_time, colour = condition)) + 
  geom_violin() + 
  theme_classic()

young_group_memory <- young_group %>%
  filter(total_mean > 0.4)

ingroup_memory_y <- young_group_memory %>%
  filter(condition == "Ingroup") %>%
  pull(mem_acc_trees)

outgroup_memory_y <- young_group_memory %>%
  filter(condition == "Outgroup") %>%
  pull(mem_acc_trees)

t.test(ingroup_memory_y, outgroup_memory_y)

old_group_memory <- old_group %>%
  filter(total_mean > 0.4)

ingroup_memory_o <- old_group_memory %>%
  filter(condition == "Ingroup") %>%
  pull(mem_acc_trees)

outgroup_memory_o <- old_group_memory %>%
  filter(condition == "Outgroup") %>%
  pull(mem_acc_trees)

t.test(ingroup_memory_o, outgroup_memory_o)


ingroup_young <- young_group_memory %>%
  filter(condition == "Ingroup") %>%
  pull(trees_water)

outgroup_young <- young_group_memory %>%
  filter(condition == "Outgroup") %>%
  pull(trees_water)

t.test(ingroup_young, outgroup_young, paired = FALSE)

ingroup_old <- old_group_memory %>%
  filter(condition == "Ingroup") %>%
  pull(trees_water)

outgroup_old <- old_group_memory %>%
  filter(condition == "Outgroup") %>%
  pull(trees_water)

t.test(ingroup_old, outgroup_old, paired = FALSE)

ingroup_look_young <- young_group_memory %>%
  filter(condition == "Ingroup") %>%
  pull(exposure_time)


outgroup_look_young <- young_group_memory %>%
  filter(condition == "Outgroup") %>%
  pull(exposure_time)

t.test(outgroup_look_young, ingroup_look_young)

ingroup_look_old <- old_group_memory %>%
  filter(condition == "Ingroup") %>%
  pull(exposure_time)


outgroup_look_old <- old_group_memory %>%
  filter(condition == "Outgroup") %>%
  pull(exposure_time)

t.test(outgroup_look_old, ingroup_look_old)

