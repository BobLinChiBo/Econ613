library(tidyverse)
library(ggplot2)
library(scales)
# Read Data
ind_choice <- read_csv("./Data/datstu_v2.csv")
school_loc <- read_csv("./Data/datsss.csv")
junior_loc <- read_csv("./Data/datjss.csv")
ind_choice <- ind_choice %>% rename(index = V1)
school_loc <- 
  school_loc %>% 
  group_by(schoolcode) %>% 
  filter(!is.na(schoolname)) %>% 
  arrange(nchar(sssdistrict)) %>% 
  slice_tail() %>% 
  select(!V1)
junior_loc <- junior_loc %>% select(!"...1")

### 1
## 1.1
# number of students
nrow(ind_choice)

# number of schools
ind_school <- 
  ind_choice %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank)))
ind_school %>% 
  select(schoolcode) %>%
  n_distinct()

# number of programs
ind_program <- 
  ind_choice %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm")  
ind_program %>%
  select(starts_with("choicepgm")) %>% 
  select(choicepgm) %>%
  n_distinct()


## 1.2
school_program <- 
  ind_choice %>% 
  select(starts_with("schoolcode"), starts_with("choicepgm") ) %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank)),
         program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank))) %>% 
  filter(school_rank == program_rank) %>% 
  distinct(schoolcode, choicepgm) 
school_program %>% nrow()

## 1.3
ind_school_loc <- 
  ind_school %>% 
  left_join(school_loc, by = "schoolcode") %>% 
  mutate(near = jssdistrict == sssdistrict)
ind_school_loc_near <- 
  ind_school_loc %>% 
  filter(near == TRUE) %>% 
  distinct(index, .keep_all = TRUE) 
ind_school_loc_near %>% n_distinct()

## 1.4
ind_school_enter <- 
  ind_school %>% 
  mutate(enter_school = school_rank == rankplace)
school_size <- 
  ind_school_enter %>% 
  group_by(schoolcode) %>% 
  summarise(school_size = sum(enter_school, na.rm = TRUE)) 
school_size

## 1.5
school_cutoff <-
  ind_school_enter %>% 
  filter(enter_school == TRUE) %>% 
  group_by(schoolcode) %>% 
  arrange(score, .by_group = TRUE) %>% 
  select(schoolcode, score) %>% 
  rename(school_cutoff = score) %>% 
  slice_head()
school_cutoff

## 1.6
school_quality <- 
  ind_school_enter %>% 
  filter(enter_school == TRUE) %>% 
  group_by(schoolcode) %>% 
  summarise(school_quality = mean(score))
school_quality

### 2
ind_school_program_enter <- 
  ind_school_enter %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank)),
         enter_program = enter_school & program_rank == school_rank)
program_size <- 
  ind_school_program_enter %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_size = sum(enter_program, na.rm = TRUE)) 
program_cutoff <-
  ind_school_program_enter %>% 
  filter(enter_program == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  arrange(score, .by_group = TRUE) %>% 
  select(schoolcode, choicepgm, score) %>% 
  rename(program_cutoff = score) %>% 
  slice_head()
program_quality <- 
  ind_school_program_enter %>%
  filter(enter_program == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_quality = mean(score))
school_program_level <- 
  school_program %>% 
  left_join(school_size, by = "schoolcode") %>% 
  left_join(school_quality, by = "schoolcode") %>% 
  left_join(school_cutoff, by = "schoolcode") %>% 
  left_join(program_size, by = c("schoolcode", "choicepgm")) %>% 
  left_join(program_quality, by = c("schoolcode", "choicepgm")) %>% 
  left_join(program_cutoff, by = c("schoolcode", "choicepgm")) %>% 
  left_join(school_loc, by = "schoolcode")
school_program_level

### 3
ind_school_jun_loc <- 
  ind_school_loc %>% 
  left_join(junior_loc, by = "jssdistrict") %>% 
  rename(jsslong = point_x, jsslat = point_y) %>% 
  mutate(distance = sqrt( (69.172 * (ssslong-jsslong) * cos(jsslat / 57.3) )^2 + (69.172 * (ssslat-jsslat) )^2  )   )

### 4
ind_recode <- 
  ind_school_enter %>% 
  mutate(scode_rev = substr(schoolcode, 1, 3)) %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank)),
         enter_program = enter_school & program_rank == school_rank,
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts", 
                          ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
                                 ifelse(choicepgm == "General Science", "science", "others"))),
         choice_rev = paste(scode_rev, pgm_rev, sep = "-")) 
recode_cutoff <-
  ind_recode %>% 
  filter(enter_program == TRUE) %>% 
  group_by(choice_rev) %>% 
  arrange(score, .by_group = TRUE) %>% 
  select(choice_rev, score) %>% 
  rename(recode_cutoff = score) %>% 
  slice_head()
recode_cutoff
recode_quality <- 
  ind_recode %>%
  filter(enter_program == TRUE) %>% 
  group_by(choice_rev) %>% 
  summarise(recode_quality = mean(score))
recode_quality

### 5
likelihood_logit_ind <- function(matrix_X, data, choices){
  utility <- matrix_X %*% beta
  
  prob_y <- exp(utility) / (1 + exp(utility))
  prob_y[prob_y > 0.9999] = 0.9999
  prob_y[prob_y < 0.0001] = 0.0001
  return(prob_y)
}
add_dummies <- function(data, factor){
  formula <- as.formula(paste("~ ", factor , ""))
  factor_dummies <- model.matrix(formula, data)
  data_dummies <- cbind(data, factor_dummies[,-1])
  return(data_dummies)
}


## 5.1
#



## 5.2


