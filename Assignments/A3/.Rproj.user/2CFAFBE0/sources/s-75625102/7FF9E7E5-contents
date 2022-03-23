library(tidyverse)
library(ggplot2)
library(scales)
library(mlogit)
library(foreign)
options(digit=20)
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
junior_loc <- junior_loc %>% select(!"...1") %>% na.omit

### 1
## 1.1
# number of students
nrow(ind_choice)

# number of schools
ind_school <- 
  ind_choice %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank))) %>% 
  filter(!is.na(schoolcode))
schools <- 
  ind_school %>% 
  select(schoolcode) %>%
  distinct
nrow(schools)

# number of programs
ind_program <- 
  ind_choice %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  filter(!is.na(choicepgm))
programs <- 
  ind_program %>% 
  select(choicepgm) %>%
  distinct() 
nrow(programs)

## 1.2
ind_school_program <- 
  ind_choice %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank)),
         program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank))) 
school_program <-
  ind_school_program %>% 
  filter(school_rank == program_rank) %>% 
  distinct(schoolcode, choicepgm) %>% 
  na.omit
nrow(school_program)

## 1.3
ind_school_loc <- 
  ind_school %>% 
  left_join(school_loc, by = "schoolcode") %>% 
  left_join(junior_loc, by = "jssdistrict") %>% 
  rename(jsslong = point_x, jsslat = point_y) %>% 
  mutate(near = jsslong == ssslong & jsslat == ssslat)
# Note that some districts have different names but with the same latitude and longitude.
ind_school_loc_near <- 
  ind_school_loc %>% 
  filter(near == TRUE) %>% 
  distinct(index, .keep_all = TRUE) 
nrow(ind_school_loc_near)

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
school_cutoff <- 
  schools %>% 
  left_join(school_cutoff)
school_cutoff

## 1.6
school_quality <- 
  ind_school_enter %>% 
  filter(enter_school == TRUE) %>% 
  group_by(schoolcode) %>% 
  summarise(school_quality = mean(score))
school_quality <- 
  schools %>% 
  left_join(school_quality)
school_quality

### 2
ind_school_program_enter <- 
  ind_school_program %>% 
  mutate(enter =  program_rank == school_rank & program_rank == rankplace)
program_size <- 
  ind_school_program_enter %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_size = sum(enter, na.rm = TRUE)) %>% 
  na.omit
program_cutoff <-
  ind_school_program_enter %>% 
  filter(enter == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  select(schoolcode, choicepgm, score) %>% 
  na.omit %>% 
  arrange(score, .by_group = TRUE) %>% 
  rename(program_cutoff = score) %>% 
  slice_head()
program_quality <- 
  ind_school_program_enter %>%
  filter(enter == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_quality = mean(score)) %>% 
  na.omit()
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
ind_school_loc_distance <- 
  ind_school_loc %>% 
  mutate(distance = sqrt( (69.172 * (ssslong-jsslong) * cos(jsslat / 57.3) )^2 + (69.172 * (ssslat-jsslat) )^2  )   )

### 4
ind_recode <- 
  ind_school_program_enter %>% 
  mutate(scode_rev = substr(schoolcode, 1, 3),
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts",  
                          ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
                                 ifelse(choicepgm == "General Science", "science", "others"))),
         choice_rev = paste(scode_rev, pgm_rev, sep = "_"))
choice_rev <- 
  school_program %>% 
  mutate(scode_rev = substr(schoolcode, 1, 3),
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts", 
                          ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
                                 ifelse(choicepgm == "General Science", "science", "others"))),
         choice_rev = paste(scode_rev, pgm_rev, sep = "_")) %>% 
  select(scode_rev, pgm_rev, choice_rev) %>% 
  distinct()
recode_cutoff <-
  ind_recode %>% 
  filter(enter == TRUE) %>% 
  group_by(choice_rev) %>% 
  arrange(score, .by_group = TRUE) %>%   
  select(choice_rev, score) %>% 
  rename(recode_cutoff = score) %>% 
  slice_head()
recode_cutoff <- 
  choice_rev %>% 
  left_join(recode_cutoff, by = "choice_rev") 
recode_cutoff
recode_quality <- 
  ind_recode %>%
  filter(enter == TRUE) %>% 
  group_by(choice_rev) %>% 
  summarise(recode_quality = mean(score))
recode_quality <- 
  choice_rev %>% 
  left_join(recode_quality, by = "choice_rev")
recode_quality

### 5
add_dummies <- function(data, factor, other_var = ""){
  if(other_var != ""){
    formula <- paste("~ ", factor, " + ", factor, " * " , other_var)
  }
  else{
    formula <- paste("~ ", factor)
  }
  formula <- as.formula(formula)
  factor_dummies <- model.matrix(formula, data)
  new_data <- as_tibble(factor_dummies[, -1])
  return(new_data)
}
choices_utility <- function(coefficients, data, choices, type, h = 0, change_by_choice = 0, change_var = ""){
  if(type == "conditional"){
    new_data <- as.matrix(data[, 1:nrow(choices)])
  }
  else if(type == "multinomial"){
    new_data <- as.matrix(data[, 1:((nrow(choices)-1)*2)])
  }
  result_data <- data
  change_data <- new_data
  change_data[, (nrow(choices)):ncol(change_data)][change_data[, (nrow(choices)):ncol(change_data)] > 0] = h
  change_data[, 1:(nrow(choices)-1)] = 0
  new_data = new_data + change_data
  chosen_utility <- new_data %*% coefficients
  result_data["chosen_utility"] = chosen_utility
  if(type == "conditional"){
    new_data[, 1:nrow(choices)-1] = 0
    if(change_by_choice != 0){
      new_data[, nrow(choices)] = as.matrix(data)[,nrow(choices) + 1]
    }
    if(change_by_choice != 0 & change_var != ""){
      new_h = h * as.numeric((choices[1,1] == change_var))
    }
    else{new_h = h}
    new_data[, nrow(choices)] = new_data[, nrow(choices)] + new_h
  }
  else if(type == "multinomial"){
    new_data[,] = 0
  }
  result_data[paste("utility", choices[1,1], sep = "_")] = new_data %*% coefficients
  for(i in 2:(nrow(choices))){
    new_data[,] = 0
    new_data[, i-1] = 1
    if(type == "multinomial"){
      new_data[, nrow(choices)-2+i] = as.matrix(data)[, ncol(new_data)+1] + h 
    }
    else if(type == "conditional"){
      if(change_by_choice != 0){
        if(change_var != ""){
          new_h = h * as.numeric((choices[i,1] == change_var))
        }
        else{new_h = h}
        new_data[, nrow(choices)] = as.matrix(data)[, nrow(choices) + i] + new_h
      }
      else{
        new_data[, nrow(choices)] = as.matrix(data)[, nrow(choices)] + h
      }
    }
    choice_utility <- new_data %*% coefficients
    result_data[paste("utility", choices[i,1], sep = "_")] = choice_utility
  }
  return(result_data)
}
find_index <- function(name, add_before, sep, data){
  pattern <- paste(add_before, name, sep = sep)
  return(grep(pattern, colnames(data)))
}
choices_prob <- function(result_data, choices){
  index <- sapply(choices %>% pull(1), find_index, add_before = "utility", sep = "_", data = result_data)
  result_data <- result_data %>% mutate(sum_exp_utility = rowSums(across(.cols = all_of(index), .fns = exp)))
  result_data <- result_data %>% mutate(across(all_of(index), function(x) exp(x)/sum_exp_utility))
  chosen_prob <- with(result_data, exp(chosen_utility) / sum_exp_utility)
  chosen_prob[chosen_prob > 0.99999] = 0.99999
  chosen_prob[chosen_prob < 0.00001] = 0.00001
  result_data["chosen_prob"] = chosen_prob
  for (choice in choices %>% pull(1)) {
    colnames(result_data)[which(names(result_data) == choice)] = paste("choice_prob", choice, sep = "_")
    assign("choice_prob",   with(result_data, get(paste("choice_prob", choice, sep = "_"))))
    choice_prob[choice_prob > 0.99999] = 0.99999
    choice_prob[choice_prob < 0.00001] = 0.00001
    result_data[paste("choice_prob", choice, sep = "_")] =  choice_prob
  }
  return(result_data %>% select(starts_with("choice_prob"), "chosen_prob"))
}
likelihood_logit_all <- function(coefficients, data, choices, type, h = 0, change_by_choice = 0, change_var = ""){
  result_data <- choices_utility(coefficients = coefficients, data = data, choices = choices,
                                 type = type, h = h, change_by_choice = change_by_choice, change_var = change_var)
  prob_data <- choices_prob(result_data = result_data, choices = choices)
  return(prob_data)
}
likelihood_logit <- function(coefficients, data, choices, type, h = 0, change_by_choice = 0){
  result_data <- likelihood_logit_all(data = data, coefficients = coefficients, choices = choices, type = type, h = h, change_by_choice = change_by_choice)
  prob_y <- result_data %>% pull(chosen_prob)
  return(prob_y)
}
log_likelihood_logit <- function(coefficients, data, choices, type, change_by_choice = 0){
  prob_y <- likelihood_logit(coefficients = coefficients, data = data, choices = choices, type = type, change_by_choice = change_by_choice)
  log_likelihood <- sum(log(prob_y))
  return(log_likelihood)
}
max_log_likelihood <- function(data, choices, times, beta_start_min, beta_start_max, best_guess = "", type, change_by_choice = 0){
  if(times == 1){
    best_start_point <- runif(n = ncol(data), min = beta_start_min, max = beta_start_max)
  }
  else{
    empty_list <- vector(mode = "list", length = times)
    if(class(best_guess) == "character"){
      start_points <- lapply(empty_list, function(x) runif(n = ncol(data), min = beta_start_min, max = beta_start_max))
      results <- lapply(X = start_points, FUN = optim, 
                      fn = log_likelihood_logit,
                      method = "BFGS",
                      control = list(maxit = 1000, fnscale = -1),
                      data = data, choices = choices, type = type, change_by_choice = change_by_choice)
      logLik_results <- lapply(results, "[[", 2)
      max_logLik <- max(unlist(logLik_results))
      positions <- which(unlist(logLik_results) == max_logLik)
      best_start_point <- start_points[[positions[1]]]
    }
    else{
      best_start_point <- best_guess
    }
  }
  best_result <- optim(best_start_point, 
                       fn = log_likelihood_logit, 
                       method = "BFGS",
                       control = list(trace=6, maxit = 1000, fnscale = -1),
                       data = data, choices = choices, type = type, change_by_choice = change_by_choice,
                       hessian = FALSE)
  return(best_result)
}

# multinomial logistic regression
# utility = alpha_choice + beta_score_choice * score
# choice_rev ~ 0 | score
ind_recode_score_first <- 
  ind_recode %>% 
  filter(school_rank == 1 & program_rank == 1 & !is.na(scode_rev) & !is.na(choice_rev)) %>% 
  select(score, choice_rev, scode_rev, pgm_rev) %>% 
  na.omit
set.seed(123)
ind_recode_score_first.sample <- 
  ind_recode_score_first %>% 
  slice(sample(nrow(ind_recode_score_first), size = nrow(ind_recode_score_first)/250, replace = FALSE))
write.dta(ind_recode_score_first.sample, "q5_sample_stata.dta")
ind_recode_score_first_mlogit <- 
  ind_recode_score_first.sample %>% 
  select(score, choice_rev) %>% 
  mlogit.data(choice = "choice_rev", shape = "wide") 
mlogit_result_q5 <- mlogit(choice_rev ~ 1 | score, data = ind_recode_score_first_mlogit)
best_guess_q5 <- summary(mlogit_result_q5)$coefficients
choices_q5 <-
  ind_recode_score_first.sample %>% 
  select(choice_rev) %>% 
  distinct() %>% 
  arrange(choice_rev) 
ind_recode_score_first_d.sample <- 
  ind_recode_score_first.sample %>% 
  add_dummies(factor = "choice_rev", other_var = "score") %>% 
  select(-score, score)
set.seed(123)
own_result_q5 <- max_log_likelihood(data = ind_recode_score_first_d.sample, 
                                 choices = choices_q5, times = 10, 
                                 beta_start_min = -1, beta_start_max = 1, best_guess = best_guess_q5,
                                 type = "multinomial")
estimates_q5 <- cbind(best_guess_q5, own_result_q5$par)
colnames(estimates_q5) = c("mlogit : est","own : est")
estimates_q5

# marginal effect
marginal_effect_average <- function(change_var = "", coefficients, data, choices, type, h, change_by_choice = 0, choice_wise = 0){
  h_likelihood <- likelihood_logit_all(coefficients = coefficients, data = data, 
                                       choices = choices, type = type, h = h, 
                                       change_by_choice = change_by_choice, change_var = change_var) 
  likelihood_h <- likelihood_logit_all(coefficients = coefficients, data = data, 
                                       choices = choices, type = type, h = -h,
                                       change_by_choice = change_by_choice, change_var = change_var)
  marginal_effect_ind <- (h_likelihood - likelihood_h) / (2*h)
  marginal_effect_average <- 
    marginal_effect_ind %>% summarise_all(mean) %>% 
    select(starts_with("choice_prob")) %>% t()
  colnames(marginal_effect_average) = "average_marginal_effect"
  return(marginal_effect_average)
}
own_marginal_effect_average_q5 <- 
  marginal_effect_average(coefficients = own_result_q5$par, data = ind_recode_score_first_d.sample, choices = choices_q5, type = "multinomial", h = 1/1000) 
own_marginal_effect_average_q5

### 6
# conditional logistic regression
# utility = alpha_choice + beta_quality * quality
# choice_rev ~ quality 
# Define (relative) quality = average program quality in choice rev / individual score
ind_recode_quality_first <- 
  ind_recode %>% 
  filter(school_rank == 1 & program_rank == 1 & !is.na(scode_rev) & !is.na(pgm_rev)) %>% 
  select(index, score, choice_rev, schoolcode, choicepgm) %>% 
  left_join(recode_quality, by = "choice_rev") %>% 
  mutate(relative_quality = recode_quality / score) %>% 
  na.omit
set.seed(123)
ind_recode_quality_first.sample <- 
  ind_recode_quality_first %>% 
  slice(sample(nrow(ind_recode_quality_first), size = nrow(ind_recode_quality_first)/250, replace = FALSE))
choices_q6 <-
  ind_recode_quality_first.sample %>% 
  select(choice_rev) %>% 
  distinct() %>% 
  arrange(choice_rev) 
temp_quality_first_mlogit <- 
  ind_recode_quality_first.sample %>% 
  select(score, choice_rev) %>% 
  mlogit.data(choice = "choice_rev", shape = "wide") 
reshape_temp_quality_first_mlogit <- 
  temp_quality_first_mlogit %>% as_tibble %>% select(1:4)%>% 
  rename(dummy = choice_rev, choice_rev = alt) %>% 
  left_join(recode_quality, by = "choice_rev") %>% 
  mutate(relative_quality = recode_quality / score) %>% 
  rename(id = chid)
#write.dta(reshape_temp_quality_first_mlogit, "q6_sample_stata.dta")
reshape_quality_first_mlogit <- 
  dfidx(reshape_temp_quality_first_mlogit, idx = c("id", "choice_rev"))
mlogit_result_q6 <- mlogit(dummy ~ relative_quality , data = reshape_quality_first_mlogit)
best_guess_q6 <- summary(mlogit_result_q6)$coefficients
ind_recode_quality_first_d.sample <- 
  ind_recode_quality_first.sample %>% 
  add_dummies(factor = "choice_rev") %>% 
  mutate(relative_quality = ind_recode_quality_first.sample$relative_quality) 
for(choice in choices_q6 %>% pull(1)){
  recode_quality_each <- recode_quality[recode_quality["choice_rev"] == choice, "recode_quality"] %>% pull(1)
  ind_recode_quality_first_d.sample[paste("relative_quality_by", choice, sep = "_")] = recode_quality_each / ind_recode_quality_first.sample$score
  }
set.seed(123)
own_result_q6 <- max_log_likelihood(data = ind_recode_quality_first_d.sample, 
                        choices = choices_q6, times = 3, 
                        beta_start_min = -1, beta_start_max = 1, type = "conditional",
                        best_guess = best_guess_q6, change_by_choice = 1)
estimates_q6 <- cbind(best_guess_q6, own_result_q6$par)
colnames(estimates_q6) = c("mlogit : est","own : est")
estimates_q6
own_marginal_effect_average_q6 <- lapply(choices_q6 %>% pull(1), marginal_effect_average,
       coefficients = own_result_q6$par, data = ind_recode_quality_first_d.sample, 
       choices = choices_q6, type = "conditional", h = 1/1000, 
       change_by_choice = 1, choice_wise = 1) %>% as.data.frame()
rownames(own_marginal_effect_average_q6) = paste("relative_quality_change_in", choices_q6 %>% pull(1) ,sep = "_")
colnames(own_marginal_effect_average_q6) = paste("marginal_effect_on_prob", choices_q6 %>% pull(1) ,sep = "_")
own_marginal_effect_average_q6[1:5, 1:5]

### 7
# Using model 2 would be more appropriate to see the effect of excluding "others".
# Model 2 studies the effect school-program quality on students' choices.
# When excluding "others", the relative school-program quality changes.
# Hence, we can see if such change has influence on the probability of students' choices.
# On the other hand, model 1 studies the effect of each student's own score on his choice.
# In particular, the model studies whether student choosing certain choices have higher or lower scores compared with the base choice.
# Then, excluding "others" and comparing the results does not give much economic sense.
utility <- choices_utility(coefficients = own_result_q6$par, type = "conditional", choices = choices_q6, data = ind_recode_quality_first_d.sample, change_by_choice = 1)
prob <- 
  choices_prob(result_data = utility, choices = choices_q6) %>% 
  select(starts_with("choice_prob"))
compare_base_prob <- prob
for (choice in choices_q6 %>% pull(1)) {
  compare_base_prob[paste("relative_prob", choice, sep = "_")] = compare_base_prob[paste("choice_prob", choice, sep = "_")] / compare_base_prob[paste("choice_prob", choices_q6[1,1], sep = "_")]
}

choices_q6_exclude <- 
  choices_q6 %>% 
  left_join(choice_rev, by = "choice_rev") %>% 
  filter(! pgm_rev == "others") %>% 
  select(choice_rev)
prob_exclude <- 
  choices_prob(result_data = utility, choices = choices_q6_exclude) %>% 
  select(starts_with("choice_prob"))
compare_base_prob_ex <- prob_exclude
for (choice in choices_q6_exclude %>% pull(1)) {
  compare_base_prob_ex[paste("relative_prob", choice, sep = "_")] = compare_base_prob_ex[paste("choice_prob", choice, sep = "_")] / compare_base_prob_ex[paste("choice_prob", choices_q6_exclude[1,1], sep = "_")]
}
compare_base_prob_b <- compare_base_prob %>% select(!ends_with("others"))
compare_base_prob_b
compare_base_prob_ex
# How many times of each individual's predicted choices probability after exclusion are compared with before exclusion 
(compare_base_prob_ex / compare_base_prob_b)[1:5,1:20]

