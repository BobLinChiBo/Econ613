library(tidyverse)
library(ggplot2)
library(scales)
library(mlogit)
options(digits=7)
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
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts", "others"),
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
log_likelihood_logit <- function(coefficients, data, choices, type){
  prob_y <- likelihood_logit(coefficients = coefficients, data = data, choices = choices, type = type)
  log_likelihood <- sum(log(prob_y))
  return(log_likelihood)
}
max_log_likelihood <- function(data, choices, times, beta_start_min, beta_start_max, best_guess = "", type){
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
mlogit_result_q5 <- mlogit(choice_rev ~ 0 | score, data = ind_recode_score_first_mlogit)
best_guess_q5 <- summary(mlogit_result_q5)$coefficients
choices_q5 <-
  ind_recode_score_first.sample %>% 
  select(choice_rev) %>% 
  distinct() %>% 
  arrange(choice_rev) 
choices_no_head_q5 <- 
  choices_q5 %>% slice(-1)
ind_recode_score_first_d.sample <- 
  ind_recode_score_first.sample %>% 
  add_dummies(factor = "choice_rev", other_var = "score") %>% 
  select(-score, score)
own_result_q5 <- max_log_likelihood(data = ind_recode_score_first_d.sample, 
                                    choices = choices_no_head_q5, times = 10, 
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
marginal_effect <- function(choices, coefficients, mean_data){
  h <- 1/1000
  h_prob <- choices_prob_mean(coefficients = coefficients, choices = choices, mean_data = mean_data + h)
  prob_h <- choices_prob_mean(coefficients = coefficients, choices = choices, mean_data = mean_data - h)
  marginal_effect <- (h_prob - prob_h) / (2*h)
  return(marginal_effect)
}
choices_mean_q5 <- 
  sapply(choices_q5 %>% pull(choice_rev), choice_mean, 
         data = ind_recode_score_first.sample, 
         choice_var = "choice_rev", mean_var = "score") %>% 
  as.data.frame
colnames(choices_mean_q5) = "score"
own_marginal_effect_q5 <- marginal_effect(coefficients = own_result_q5$par, choices = choices_q5, mean_data = choices_mean_q5)
mlogit_marginal_effect_q5 <- effects(mlogit_result_q5, covariate = "score", data = choices_mean_q5) %>% as.data.frame()
marginal_effect_q5 <- cbind(own_marginal_effect_q5, mlogit_marginal_effect_q5)
colnames(marginal_effect_q5) = c("own_marginal_effect", "mlogit_marginal_effect")
marginal_effect_q5


### 6
# choice_rev ~ school_quality 
ind_recode_quality_first <- 
  ind_recode %>% 
  filter(school_rank == 1 & program_rank == 1 & !is.na(scode_rev) & !is.na(pgm_rev)) %>% 
  left_join(recode_quality, by = "choice_rev") %>% 
  select(recode_quality, choice_rev) %>% 
  na.omit
set.seed(123)
ind_recode_quality_first.sample <- 
  ind_recode_quality_first %>% 
  slice(sample(nrow(ind_recode_quality_first), size = nrow(ind_recode_quality_first)/100, replace = FALSE))
#ind_recode_quality_first_mlogit <- 
#  ind_recode_quality_first %>% 
#  select(recode_quality, choice_rev) %>% 
#  mlogit.data(choice = "choice_rev", shape = "wide") 
#mlogit_result <- mlogit(choice_rev ~ recode_quality | 1 | 0, data = ind_recode_quality_first_mlogit)
#best_guess_q6 <- summary(mlogit_result)$coefficients
ind_recode_quality_first_d.sample <- 
  ind_recode_quality_first.sample %>% 
  add_dummies(factor = "choice_rev") %>% 
  mutate(recode_quality = ind_recode_quality_first.sample$recode_quality) 
choices_q6 <-
  ind_recode_quality_first.sample %>% 
  select(choice_rev) %>% 
  distinct() %>% 
  arrange(choice_rev) 
choices_no_head_q6 <- 
  choices_q6 %>% slice(-1)
own_result_q6 <- max_log_likelihood(data = ind_recode_quality_first_d.sample, 
                                    choices = choices_no_head_q6, times = 1, 
                                    beta_start_min = -1, beta_start_max = 1, type = "conditional")
choices_mean <- 
  sapply(choices_q5 %>% pull(choice_rev), choice_mean, 
         data = ind_recode_score_first.sample, 
         choice_var = "choice_rev", mean_var = "score") %>% 
  as.data.frame()
colnames(choices_mean) = "score"
own_marginal_effect_q5 <- marginal_effect(coefficients = own_result_q5$par, choices = choices_q5, mean_data = choices_mean)
mlogit_marginal_effect_q5 <- effects(mlogit_result_q5, covariate = "score", data = choices_mean) %>% as.data.frame()
marginal_effect_q5 <- cbind(own_marginal_effect_q5, mlogit_marginal_effect_q5)
colnames(marginal_effect_q5) = c("own_marginal_effect", "mlogit_marginal_effect")
marginal_effect_q5


### 7
# Using model 2 would be more appropriate to see the effect of excluding "others".
# Model 2 studies the effect school-program quality on students' choices.
# When excluding "others", the relative school-program quality changes.
# Hence, we can see if such change have influence on the effect school-program quality on students' choices.
# On the other hand, model 1 studies the effect of each student's own score on his choice.
# In particular, the model studies whether student choosing certain choices have higher or lower scores.
# Then, excluding "others" and comparing the results does not give much economic sense.

data <- ind_recode_quality_first_d.sample
data.list <- split(data, seq(nrow(data)))
prob_y <- sapply(data.list, likelihood_logit_ind, coefficients = own_result_q6, choices = choices_no_head_q6)
ind_recode_prob <- cbind(ind_recode, prob_y)

ind_recode_quality_first_ex.sample <- 
  ind_recode_quality_first.sample %>% 
  filter(! pgm_rev == "other") 

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
