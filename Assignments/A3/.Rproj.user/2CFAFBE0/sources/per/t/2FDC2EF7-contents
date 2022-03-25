library(tidyverse)
library(ggplot2)
library(scales)
library(mlogit)
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
  mutate(scode_rev = substr(schoolcode, 1, 1),
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts", "others"), 
         #                          ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
         #                                 ifelse(choicepgm == "General Science", "science", "others"))),
         choice_rev = paste(scode_rev, pgm_rev, sep = "_"))
choice_rev <- 
  school_program %>% 
  mutate(scode_rev = substr(schoolcode, 1, 1),
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts", "others"),
         #                          ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
         #                                 ifelse(choicepgm == "General Science", "science", "others"))),
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
choices_utility <- function(coefficients, data, choices, type, h = 0){
  if(type == "conditional"){
    new_data <- as.matrix(data)
  }
  else if(type == "multinomial"){
    new_data <- as.matrix(data[, 1:length(data)-1])
  }
  result_data <- data
  change_data <- new_data
  change_data[, (nrow(choices)):ncol(change_data)][change_data[, (nrow(choices)):ncol(change_data)] > 0] = h
  change_data[, 1:(nrow(choices)-1)] = 0
  new_data = new_data + change_data
  chosen_utility <- new_data %*% coefficients
  result_data["chosen_utility"] = chosen_utility
  new_data[, 1:length(data)-1] = 0
  assign(paste("utility", choices[1,1], sep = "_"), new_data %*% coefficients)
  result_data[paste("utility", choices[1,1], sep = "_")] = new_data %*% coefficients
  for(i in 2:(nrow(choices))){
    new_data[, 1:length(data)-1] = 0
    new_data[, i-1] = 1
    if(type == "multinomial"){
      new_data[, nrow(choices)-2+i] = as.numeric(as.matrix(data)[, length(data)] + h)  
    }
    else if(type == "conditional"){
      new_data[, length(data)] = as.numeric(as.matrix(data)[, length(data)] + h)
    }
    choice_utility <- new_data %*% coefficients
    assign(paste("utility", choices[i,1], sep = "_"), choice_utility)
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
likelihood_logit_all <- function(coefficients, data, choices, type, h = 0){
  result_data <- choices_utility(coefficients, data, choices, type, h)
  prob_data <- choices_prob(result_data, choices)
  return(prob_data)
}
likelihood_logit <- function(coefficients, data, choices, type, h = 0){
  result_data <- likelihood_logit_all(data = data, coefficients = coefficients, choices = choices, type = type, h = h)
  prob_y <- result_data %>% pull(chosen_prob)
  return(prob_y)
}
log_likelihood_logit <- function(coefficients, data, choices, type){
  prob_y <- likelihood_logit(coefficients = coefficients, data = data, choices = choices, type = type)
  log_likelihood <- sum(log(prob_y))
  return(log_likelihood)
}
max_log_likelihood <- function(data, choices, times, beta_start_min, beta_start_max, best_guess = "", type){
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
                        data = data, choices = choices, type = type)
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
                       data = data, choices = choices, type = type,
                       hessian = TRUE)
  return(best_result)
}

# choice_rev ~ 0 | score
#rm(list=setdiff(ls(), "ind_recode"))
ind_recode_score_first <- 
  ind_recode %>% 
  filter(school_rank == 1 & program_rank == 1 & !is.na(scode_rev) & !is.na(choice_rev)) %>% 
  select(score, choice_rev, scode_rev, pgm_rev) %>% 
  na.omit
set.seed(123)
ind_recode_score_first.sample <- 
  ind_recode_score_first %>% 
  slice(sample(nrow(ind_recode_score_first), size = nrow(ind_recode_score_first)/100, replace = FALSE))
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
#choices_no_head_q5 <- 
#  choices_q5 %>% slice(-1)
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
choice_mean <- function(chosen, choice_var, mean_var, data){
  result <- data %>% filter(eval(parse(text=choice_var)) == chosen) %>% pull(mean_var) 
  return(mean(result))
}
choices_prob_mean <- function(choices, coefficients, mean_data, type){
  sum_exp_utility_mean <- 0
  choices <- choices %>% pull(1)
  for(chosen in choices) {
    chosen_index <- grep(chosen, choices)
    chosen_index_string <- paste("utility", chosen_index, sep = "")
    if(type == "multinomial"){
      if(chosen_index == 1){
        utility <- 0
      }
      else{
        utility <- coefficients[chosen_index-1] + mean_data[chosen, 1] * coefficients[ length(coefficients)/2 + chosen_index-1 ]
      }
    }
    else if(type == "conditional"){
      if(chosen_index == 1){
        utility <- mean_data[chosen, 1] * coefficients[length(coefficients)]
      }
      else{
        utility <- coefficients[chosen_index-1] + mean_data[chosen, 1] * coefficients[length(coefficients)]
      }
    }
    assign(chosen_index_string, utility)
    sum_exp_utility_mean = sum_exp_utility_mean + exp(utility)
  }
  choices_prob_mean <- mean_data 
  colnames(choices_prob_mean) = "prob_at_mean"
  i <- 1
  for (chosen in choices) {
    choices_prob_mean[i, "prob_at_mean"] = exp(get(paste("utility", i, sep = ""))) / sum_exp_utility_mean
    i = i + 1
  }
  return(choices_prob_mean)
}
marginal_effect_average <- function(coefficients, data, choices, type, h){
  h_likelihood <- likelihood_logit_all(coefficients, data, choices, type, h) 
  likelihood_h <- likelihood_logit_all(coefficients, data, choices, type, -h)
  marginal_effect_ind <- (h_likelihood - likelihood_h) / (2*h)
  #test <- data[1] >= -1
  #result <- vector()
  #for (i in 2:nrow(choices)) {
  #  result_each = marginal_effect_ind[data[i-1] == 1,] %>% summarise_all(mean)
  #  result = rbind(result, result_each)
  #  test = test * (data[i-1] == 0)
  #}
  #result_0 =  marginal_effect_ind[test == 1,] %>% summarise_all(mean)
  #result = rbind(result_0, result)
  marginal_effect_average <- 
    marginal_effect_ind %>% summarise_all(mean) %>% 
    select(starts_with("choice_prob")) %>% t()
  colnames(marginal_effect_average) = "average_marginal_effect"
  return(marginal_effect_average)
}
own_marginal_effect_average <- 
  marginal_effect_average(own_result_q5$par, data = ind_recode_score_first_d.sample, choices = choices_q5, type = "multinomial", h = 1/100000) 
own_marginal_effect_average

# marginal_effect_at_mean <- function(choices, coefficients, mean_data, type, h = 1/1000){
#   h_prob <- choices_prob_mean(coefficients = coefficients, choices = choices, mean_data = mean_data + h, type = type)
#   prob_h <- choices_prob_mean(coefficients = coefficients, choices = choices, mean_data = mean_data - h, type = type)
#   marginal_effect <- (h_prob - prob_h) / (2*h)
#   colnames(marginal_effect) = "marginal_effect"
#   return(marginal_effect)
# }
# choices_mean_q5 <- 
#   sapply(choices_q5 %>% pull(choice_rev), choice_mean, 
#          data = ind_recode_score_first.sample, 
#          choice_var = "choice_rev", mean_var = "score") %>% 
#   as.data.frame()
# colnames(choices_mean_q5) = "score"
#all_mean <- ind_recode_score_first.sample %>% pull(score) %>% mean()
#choices_mean_q5 <- choices_mean_q5 %>% mutate(score = all_mean) 
# own_marginal_effect_q5 <- marginal_effect(coefficients = own_result_q5$par, 
#                                           choices = choices_q5, mean_data = choices_mean_q5,
#                                           type = "multinomial")
# z <- with(ind_recode_score_first_mlogit, data.frame(score = tapply(score, idx(mlogit_result_q5, 2), mean)))
# 
# mlogit_marginal_effect_q5 <- effects(mlogit_result_q5, covariate = "score", type = "ar", data = choices_mean_q5) %>% as.data.frame()
# marginal_effect_q5 <- cbind(own_marginal_effect_q5, mlogit_marginal_effect_q5)
# colnames(marginal_effect_q5) = c("own_marginal_effect", "mlogit_marginal_effect")
# marginal_effect_q5


### 6
# choice_rev ~ school_quality 
ind_recode_quality_first <- 
  ind_recode %>% 
  filter(school_rank == 1 & program_rank == 1 & !is.na(scode_rev) & !is.na(pgm_rev)) %>% 
  select(score, choice_rev, schoolcode, choicepgm, pgm_rev) %>% 
  left_join(recode_quality, by = "choice_rev") %>% 
  na.omit
set.seed(123)
ind_recode_quality_first.sample <- 
  ind_recode_quality_first %>% 
  slice(sample(nrow(ind_recode_quality_first), size = nrow(ind_recode_quality_first)/100, replace = FALSE))
ind_recode_quality_first_mlogit <- 
  ind_recode_quality_first.sample %>% 
  select(recode_quality, choice_rev) %>% 
  mlogit.data(choice = "choice_rev", shape = "wide") 
mlogit_result_q6 <- mlogit(choice_rev ~ recode_quality | 0, data = ind_recode_quality_first_mlogit)
summary(mlogit_result_q6)
best_guess_q6 <- summary(mlogit_result)$coefficients
ind_recode_quality_first_d.sample <- 
  ind_recode_quality_first.sample %>% 
  add_dummies(factor = "choice_rev") %>% 
  mutate(program_quality = ind_recode_quality_first.sample$program_quality) 
choices_q6 <-
  ind_recode_quality_first.sample %>% 
  select(choice_rev) %>% 
  distinct() %>% 
  arrange(choice_rev) 
choices_no_head_q6 <- 
  choices_q6 %>% slice(-1)
set.seed(123)
own_result_q6 <- max_log_likelihood(data = ind_recode_quality_first_d.sample, 
                                    choices = choices_q6, times = 3, 
                                    beta_start_min = -1, beta_start_max = 1, type = "conditional")
own_result_q6_readable <- 
  choices_no_head_q6 %>% 
  rename(term = choice_rev) %>% 
  mutate(coefficients = own_result_q6$par[1:length(own_result_q6$par)-1]) %>% 
  add_case(term = "recode_quality", coefficients = own_result_q6$par[length(own_result_q6$par)])
own_result_q6_readable
choices_mean_q6 <- 
  sapply(choices_q6 %>% pull(choice_rev), choice_mean, 
         data = ind_recode_quality_first, 
         choice_var = "choice_rev", mean_var = "program_quality") %>% 
  as.data.frame()
colnames(choices_mean_q6) = "program_quality"
#all_mean <- ind_recode_quality_first.sample %>% pull(program_quality) %>% mean()
#choices_mean_q6 <- choices_mean_q6 %>% mutate(program_quality = all_mean) 
own_marginal_effect_quality_q6 <- marginal_effect(coefficients = own_result_q6$par, choices = choices_q6, mean_data = choices_mean_q6, type = "conditional")
own_marginal_effect_quality_q6


### 7
# Using model 2 would be more appropriate to see the effect of excluding "others".
# Model 2 studies the effect school-program quality on students' choices.
# When excluding "others", the relative school-program quality changes.
# Hence, we can see if such change has influence on the probability of students' choices.
# On the other hand, model 1 studies the effect of each student's own score on his choice.
# In particular, the model studies whether student choosing certain choices have higher or lower scores compared with the base choice.
# Then, excluding "others" and comparing the results does not give much economic sense.
utility <- choices_utility(coefficients = own_result_q6$par, type = "conditional", choices = choices_q6, data = ind_recode_quality_first_d.sample)
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



data <- ind_recode_quality_first_d.sample
data.list <- split(data, seq(nrow(data)))
prob_y <- sapply(data.list, likelihood_logit_ind, coefficients = own_result_q6, choices = choices_no_head_q6)
ind_recode_prob <- cbind(ind_recode, prob_y)



ind_recode_quality_first_d_ex.sample <- 
  ind_recode_quality_first_ex.sample %>% 
  select(school_quality, choice_rev) %>% 
  add_dummies(factor = "choice_rev") %>% 
  mutate(school_quality = ind_recode_quality_first.sample$school_quality)
choices_q6_ex <-
  ind_recode_quality_first_ex.sample %>% 
  select(choice_rev) %>% 
  distinct() %>% 
  arrange(choice_rev) 
choices_no_head_q6_ex <- 
  choices_q6_ex %>% slice(-1)
own_result_ex <- max_log_likelihood(data = ind_recode_quality_first_d_ex.sample, 
                                    choices = choices_no_head_q6_ex, times = 2, 
                                    beta_start_min = -1, beta_start_max = 1)
data <- ind_recode_quality_first_d_ex.sample
data.list <- split(data, seq(nrow(data)))
prob_y_exclude <- sapply(data.list, likelihood_logit_ind, coefficients = own_result_ex, choices = choices_no_head_q6_ex)
ind_recode_prob_compare <- cbind(ind_recode_prob, prob_y_exclude)




















library(nnet)

ind_recode_quality_first.sample$choice_rev <- factor(ind_recode_quality_first.sample$choice_rev)
a <- multinom(choice_rev ~ school_quality, data = ind_recode_quality_first.sample)
summary(a)
ind_recode_quality_first.sample$choice_rev_N <- as.numeric(ind_recode_quality_first.sample$choice_rev)
mclogit(choice_rev_N ~ school_quality, data = ind_recode_quality_first.sample)

mclogit(
  transport ~ cost,
  data=Transport)






data("Fishing", package = "mlogit")
head(Fish)
library("zoo")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")

m <- mlogit(mode ~ price | income | catch, data = Fish)
# compute a data.frame containing the mean value of the covariates in
# the sample
z <- with(Fish, data.frame(score = tapply(price, idx(m, 2), mean),
                           catch = tapply(catch, idx(m, 2), mean),
                           income = mean(income)))
z
# compute the marginal effects (the second one is an elasticity
## IGNORE_RDIFF_BEGIN
effects(m, covariate = "income", data = z)


colnames(z) = "score"
b <- effects(object = mlogit_result_q5,
             covariate = "score", data = z)
library(nnet)
ind_recode_score_first.sample$pgm_revF = factor(ind_recode_score_first.sample$pgm_rev)
ind_recode_score_first.sample$Out <- relevel(ind_recode_score_first.sample$pgm_revF, ref = toString(choices_q5[1,1]))
model <- multinom(Out ~ score, data = ind_recode_score_first.sample)
summary(model)


marginal_effect_var <- function(choice, data, coefficients, choices){
  new_data <- data %>% summarise_all(mean)
  h <-  data * 0
  h[, var] <- 1 / 100
  likelihood_h <- likelihood_logit(coefficients = coefficients, data = data + h, choices = choices)
  h_likelihood <- likelihood_logit(coefficients = coefficients, data = data - h, choices = choices)
  marginal_effect_var <- mean((likelihood_h - h_likelihood) / (2 * h[, var]))
  return(marginal_effect_var)
}
marginal_effect <- function(data, coefficients, choices){
  vars <- choices %>% pull(1)
  marginal_effect <- sapply(vars, marginal_effect_var, 
                            data = data, coefficients = coefficients, choices = choices)
  marginal_effect <- as.matrix(marginal_effect, ncol = 1)
  colnames(marginal_effect) = "marginal_effect"
  return(marginal_effect)
}

choices_prob_mean <- function(choices, coefficients, mean_data){
  sum_exp_utility_mean <- 0
  choices <- choices %>% pull(1)
  for (chosen in choices) {
    chosen_index <- grep(chosen, choices)
    chosen_index_string <- paste("utility", chosen_index, sep = "")
    if(chosen_index == 1){
      utility <- 0
    }
    else{
      utility <- coefficients[chosen_index-1] + mean_data[chosen, 1] * coefficients[ length(coefficients)/2 + chosen_index-1 ]
    }
    assign(chosen_index_string, utility)
    sum_exp_utility_mean = sum_exp_utility_mean + exp(utility)
  }
  choices_prob_mean <- mean_data 
  colnames(choices_prob_mean) = "prob_at_mean"
  i <- 1
  for (chosen in choices) {
    choices_prob_mean[i, "prob_at_mean"] = exp(get(paste("utility", i, sep = ""))) / sum_exp_utility_mean
    i = i + 1
  }
  return(choices_prob_mean)
}






likelihood_logit_ind <- function(X_n, coefficients, choices, type){
  if(type == "conditional"){
    data <- as.matrix(X_n)
  }
  else if(type == "multinomial"){
    data <- as.matrix(X_n[, 1:length(X_n)-1])
  }
  utility_n <- data %*% coefficients
  data[1, 1:length(X_n)-1] = 0
  sum_exp_utility_n <- exp(data %*% coefficients)
  for (i in 1:nrow(choices)) {
    data[1, 1:length(X_n)-1] = 0
    data[1, i] = 1
    if(type == "multinomial"){
      data[1, nrow(choices)+i] = as.numeric(X_n[1, length(X_n)])
    }
    else if(type == "multinomial"){
      data[1, length(X_n)] = as.numeric(X_n[1, length(X_n)])
    }
    sum_exp_utility_n = sum_exp_utility_n + exp(data %*% coefficients)
  }
  prob_y_n <- exp(utility_n) / sum_exp_utility_n
  prob_y_n[prob_y_n > 0.9999] = 0.9999
  prob_y_n[prob_y_n < 0.0001] = 0.0001
  return(prob_y_n)
}

likelihood_logit <- function(coefficients, data, choices, type){
  data.list <- split(data, seq(nrow(data)))
  prob_y <- sapply(data.list, likelihood_logit_ind, coefficients = coefficients, choices = choices, type = type)
  return(prob_y)
}

a <- likelihood_logit(coefficients = best_guess_q5, data = ind_recode_score_first_d.sample, choices = choices_no_head_q5, type = "multinomial")
b <- likelihood_logit_all(coefficients = best_guess_q5, data = ind_recode_score_first_d.sample, choices = choices_no_head_q5, type = "multinomial")


library(MCMCpack)
data(Nethvote)
ind_recode_quality_first.sample$choice_rev = factor(ind_recode_quality_first.sample$choice_rev)
post2 <- MCMCmnl(choice_rev ~
                   choicevar(program_quality, "program_quality", "1_arts") +
                   choicevar(program_quality, "program_quality", "1_others") +
                   choicevar(program_quality, "program_quality", "2_arts") +
                   choicevar(program_quality, "program_quality", "2_others") +
                   choicevar(program_quality, "program_quality", "3_arts") +
                   choicevar(program_quality, "program_quality", "3_others") +
                   choicevar(program_quality, "program_quality", "4_arts") +
                   choicevar(program_quality, "program_quality", "4_others") +
                   choicevar(program_quality, "program_quality", "5_arts") +
                   choicevar(program_quality, "program_quality", "5_others") +
                   choicevar(program_quality, "program_quality", "6_arts") +
                   choicevar(program_quality, "program_quality", "6_others") +
                   choicevar(program_quality, "program_quality", "7_arts") +
                   choicevar(program_quality, "program_quality", "7_others") +
                   choicevar(program_quality, "program_quality", "8_arts") +
                   choicevar(program_quality, "program_quality", "8_others") +
                   choicevar(program_quality, "program_quality", "9_arts") +
                   choicevar(program_quality, "program_quality", "9_others"),
                 baseline="1_arts",
                 data=ind_recode_quality_first.sample)


## just a choice-specific X var
post1 <- MCMCmnl(vote ~
                   choicevar(distD66, "sqdist", "D66") +
                   choicevar(distPvdA, "sqdist", "PvdA") +
                   choicevar(distVVD, "sqdist", "VVD") +
                   choicevar(distCDA, "sqdist", "CDA"),
                 baseline="D66", mcmc.method="IndMH", B0=0,
                 verbose=500, mcmc=100000, thin=10, tune=1.0,
                 data=Nethvote)

plot(post1)
summary(post1)



## just individual-specific X vars
post2<- MCMCmnl(vote ~
                  relig + class + income + educ + age + urban,
                baseline="D66", mcmc.method="IndMH", B0=0,
                verbose=500, mcmc=100000, thin=10, tune=0.5,
                data=Nethvote)

plot(post2)
summary(post2)



## both choice-specific and individual-specific X vars
post3 <- MCMCmnl(vote ~
                   choicevar(distD66, "sqdist", "D66") +
                   choicevar(distPvdA, "sqdist", "PvdA") +
                   choicevar(distVVD, "sqdist", "VVD") +
                   choicevar(distCDA, "sqdist", "CDA") +
                   relig + class + income + educ + age + urban,
                 baseline="D66", mcmc.method="IndMH", B0=0,
                 verbose=500, mcmc=100000, thin=10, tune=0.5,
                 data=Nethvote)



market_data <- 
  ind_recode_quality_first.sample %>%
  count(choice_rev) %>% 
  mutate(market_share = prop.table(n),
         ln_market_share = log(market_share)) %>% 
  left_join(recode_quality, by = "choice_rev") 
base_share <- market_data %>% pull(ln_market_share)
base_share = base_share[1] 
base_quality <- market_data %>% pull(recode_quality)
base_quality = base_quality[1] 
market_data_reg <- market_data %>%
  mutate(ln_market_share_diff = ln_market_share - base_share,
         quality_diff = recode_quality - base_quality) %>% 
  slice(-1) 
market_data_reg_d <- 
  market_data_reg %>% 
  add_dummies("choice_rev") %>%
  cbind(market_data_reg) %>% 
  as_tibble() %>% 
  dplyr::select(-choice_rev)
market_data_reg$choice_rev = factor(market_data_reg$choice_rev)
lm(ln_market_share_diff ~ quality_diff, data = market_data_reg)



library(mclogit)
data(Transport)
summary(mclogit(
  cbind(resp,suburb)~distance+cost,
  data=Transport
))

data(electors)

summary(mclogit(
  cbind(Freq,interaction(time,class))~econ.left/class+welfare/class+auth/class,
  random=~1|party.time,
  data=within(electors,party.time<-interaction(party,time))))


data("Heating", package = "mlogit")
H <- dfidx(Heating, choice = "depvar", varying = c(3:12))
m <- mlogit(depvar ~ ic + oc , H)
summary(m)

data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
m <- mlogit(mode ~ price | income | catch, data = Fish)
z <- with(Fish, data.frame(price = tapply(price, idx(m, 2), mean)))
effects(m, covariate = "income", data = z)
## IGNORE_RDIFF_END
effects(m, covariate = "price", type = "rr", data = z)
effects(m, covariate = "catch", type = "ar", data = z)

test_choices_mean <- 
  sapply(Fish %>% as_tibble %>% distinct(alt) %>%
           mutate(alt = as.character(alt)) %>% pull(1), choice_mean, 
         data = Fish %>% as_tibble, 
         choice_var = "alt", mean_var = "price") %>% 
  as.data.frame()

library(foreign)
write.dta(ind_recode_score_first.sample, "ind_recode_score_first.sample.dta")


marginal_effect_average <- function(coefficients, data, choices, type, h){
  h_likelihood <- likelihood_logit_all(coefficients, data, choices, type, h) 
  likelihood_h <- likelihood_logit_all(coefficients, data, choices, type, -h)
  marginal_effect_ind <- (h_likelihood - likelihood_h) / (2*h)
  #test <- data[1] >= -1
  #result <- vector()
  #for (i in 2:nrow(choices)) {
  #  result_each = marginal_effect_ind[data[i-1] == 1,] %>% summarise_all(mean)
  #  result = rbind(result, result_each)
  #  test = test * (data[i-1] == 0)
  #}
  #result_0 =  marginal_effect_ind[test == 1,] %>% summarise_all(mean)
  #result = rbind(result_0, result)
  marginal_effect_average <- 
    marginal_effect_ind %>% summarise_all(mean) %>% 
    select(starts_with("choice_prob")) %>% t()
  colnames(marginal_effect_average) = "average_marginal_effect"
  return(marginal_effect_average)
}




c <- ind_recode_quality_first_mlogit %>% as_tibble %>% select(1, 2, 3, 4)
colnames(c)=c("choice_dummy", "ind_index", "choice_rev")
c <- c %>% left_join(recode_quality, by = "choice_rev")
library(foreign)
write.dta(reshape_L_quality_first.sample, "reshape_L_quality_first.sample.dta")


marginal_effect_average <- function(coefficients, data, choices, type, h){
  h_likelihood <- likelihood_logit_all(coefficients, data, choices, type, h) 
  likelihood_h <- likelihood_logit_all(coefficients, data, choices, type, -h)
  marginal_effect_ind <- (h_likelihood - likelihood_h) / (2*h)
  #test <- data[1] >= -1
  #result <- vector()
  #for (i in 2:nrow(choices)) {
  #  result_each = marginal_effect_ind[data[i-1] == 1,] %>% summarise_all(mean)
  #  result = rbind(result, result_each)
  #  test = test * (data[i-1] == 0)
  #}
  #result_0 =  marginal_effect_ind[test == 1,] %>% summarise_all(mean)
  #result = rbind(result_0, result)
  marginal_effect_average <- 
    marginal_effect_ind %>% summarise_all(mean) %>% 
    select(starts_with("choice_prob")) %>% t()
  colnames(marginal_effect_average) = "average_marginal_effect"
  return(marginal_effect_average)
}


reshape_quality_first.sample <- ind_recode_quality_first.sample %>% 
  add_dummies(factor = "choice_rev") %>% cbind(ind_recode_quality_first.sample) %>% select(-choice_rev)
creat_first_dummy <- function(data, add_before, choices){
  test <- data[1] >= -1
  for (i in 2:nrow(choices)) {
    test = test * (data[i-1] == 0)
  }
  colnames(test) = paste(add_before, choices[1,1], sep = "")
  return(cbind(test, data))
}
reshape_quality_first_F.sample <- creat_first_dummy(reshape_quality_first.sample,add_before = "choice_rev", choices_q6)
reshape_L_quality_first.sample <- reshape_quality_first_F.sample %>%   
  pivot_longer(cols = starts_with("choice_rev"), 
               names_to = "choice", values_to = "dummy",
               names_prefix = "choice_rev",) %>% 
  select(-c("recode_quality", "relative_quality")) %>% 
  rename(choice_rev = choice) %>% 
  left_join(recode_quality, by= "choice_rev") %>% 
  mutate(relative_quality = recode_quality/score)