library(tidyverse)
library(ggplot2)
library(scales)
library(mlogit)
library(dfidx)
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
#                                   ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
#                                          ifelse(choicepgm == "General Science", "science", "others"))),
         choice_rev = paste(scode_rev, pgm_rev, sep = "_"))
choice_rev <- 
  school_program %>% 
  mutate(scode_rev = substr(schoolcode, 1, 1),
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts", "others"),
#                                   ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
#                                          ifelse(choicepgm == "General Science", "science", "others"))),
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
  left_join(recode_cutoff) 
recode_cutoff
recode_quality <- 
  ind_recode %>%
  filter(enter == TRUE) %>% 
  group_by(choice_rev) %>% 
  summarise(recode_quality = mean(score))
recode_quality <- 
  choice_rev %>% 
  left_join(recode_quality)
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
likelihood_logit_ind <- function(X_n, coefficients, choices){
  utility_n <- as.matrix(X_n) %*% as.matrix(coefficients)
  sum_exp_utility_n <- 0
  for (i in 1:length(choices)) {
    X_n[1:length(choices)] = 0
    X_n[i] = 1
    sum_exp_utility_n = sum_exp_utility_n + exp(as.matrix(X_n) %*% as.matrix(coefficients))
  }
  prob_y_n <- exp(utility_n) / sum_exp_utility_n
  prob_y_n[prob_y_n > 0.9999] = 0.9999
  prob_y_n[prob_y_n < 0.0001] = 0.0001
  return(prob_y_n)
}
log_likelihood_logit <- function(coefficients, data, choices){
  data.list <- split(data, seq(nrow(data)))
  prob_y <- sapply(data.list, likelihood_logit_ind, coefficients = coefficients, choices = choices)
  log_likelihood <- sum(log(prob_y))
  return(log_likelihood)
}
max_log_likelihood <- function(data, choices, times, beta_start_min, beta_start_max, best_guess = ""){
  empty_list <- vector(mode = "list", length = times)
  if(class(best_guess) == "character"){
    start_points <- lapply(empty_list, function(x) runif(n = ncol(data), min = beta_start_min, max = beta_start_max))
    results <- lapply(X = start_points, FUN = optim, 
                      fn = log_likelihood_logit,
                      method = "BFGS",
                      control = list(maxit = 1000, fnscale = -1),
                      data = data, choices = choices)
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
                       data = data, choices = choices,
                       hessian = TRUE)
  return(best_result)
}

# choice_rev ~ 0 | score
#rm(list=setdiff(ls(), "ind_recode"))
ind_recode_score_first <- 
  ind_recode %>% 
  filter(school_rank == 1 & program_rank == 1 & !is.na(scode_rev) & !is.na(pgm_rev)) %>% 
  select(score, pgm_rev) %>% 
  na.omit
ind_recode_score_first.sample <- 
  ind_recode_score_first %>% 
  slice(sample(nrow(ind_recode_score_first), size = nrow(ind_recode_score_first)/50, replace = FALSE))
ind_recode_score_first_mlogit <- 
  mlogit.data(ind_recode_score_first.sample, choice = "pgm_rev", shape = "wide") 
mlogit_result <- mlogit(pgm_rev ~ 0 | score, data = ind_recode_score_first_mlogit)
best_guess <- summary(mlogit_result)$coefficients
choices <-
  ind_recode_score_first.sample %>% 
  select(pgm_rev) %>% 
  distinct() %>% 
  arrange(pgm_rev) 
choices_no_head <- 
  choices %>% slice(-1)
ind_recode_score_first_d.sample <- 
  ind_recode_score_first.sample %>% 
  add_dummies(factor = "pgm_rev", other_var = "score") %>% 
  select(-score)
c <- max_log_likelihood(data = ind_recode_score_first_d.sample, 
                        choices = choices_no_head, times = 10, 
                        beta_start_min = -1, beta_start_max = 1, best_guess = best_guess)


marginal_effect_var <- function(var, raw_data, choice_var, other_var, coefficients, choices){
    h <-  raw_data * 0
    h[, var] <- 1 / 1000000
    new_data_h <- raw_data  + h
    h_new_data <- raw_data  - h
    data_h <- add_dummies(new_data_h, choice_var, other_var)
    h_data <- add_dummies(h_new_data, choice_var, other_var)
    likelihood_h <- exp(get(log_likelihood_logit)(coefficients = coefficients, data = new_data  + h))
    h_likelihood <- exp(get(log_likelihood_logit)(coefficients = coefficients, data = new_data  - h))
#    likelihood <- exp(get(log_likelihood_logit)(coefficients = coefficients, data = new_data))
    marginal_effect_var <- mean((likelihood_h - h_likelihood) / (2*h[, var]))
  return(marginal_effect_var)
}
marginal_effect <- function(data, type, outcome, regressors, coefficients, method){
  y_X <- construct_y_X(data, outcome, regressors)
  marginal_effect <- sapply(regressors, marginal_effect_var, 
                            type = type, y = y_X[[1]], matrix_X = y_X[[2]], coefficients, method = method)
  marginal_effect <- as.matrix(marginal_effect, ncol = 1)
  colnames(marginal_effect) = "marginal_effect"
  return(marginal_effect)
}



### 6
# choice_rev ~ school_quality 
ind_recode_quality_first <- 
  ind_recode %>% 
  filter(school_rank == 1 & program_rank == 1 & !is.na(scode_rev) & !is.na(pgm_rev)) %>% 
  left_join(school_quality, by = "schoolcode") %>% 
  select(school_quality, choice_rev) %>% 
  na.omit
ind_recode_quality_first.sample <- 
  ind_recode_quality_first %>% 
  slice(sample(nrow(ind_recode_quality_first), size = nrow(ind_recode_quality_first)/10, replace = FALSE))
ind_recode_quality_first_mlogit <- 
  mlogit.data(ind_recode_quality_first, choice = "choice_rev", shape = "wide") 
mlogit_result <- mlogit(choice_rev ~ school_quality, data = ind_recode_quality_first_mlogit)
best_guess <- summary(mlogit_result)$coefficients
choices <-
  ind_recode_quality_first.sample %>% 
  select(choice_rev) %>% 
  distinct() %>% 
  arrange(choice_rev) 
choices_no_head <- 
  choices %>% slice(-1)
ind_recode_quality_first_d.sample <- 
  ind_recode_quality_first.sample %>% 
  add_dummies(factor = "choice_rev") %>% 
  mutate(school_quality = ind_recode_quality_first.sample$school_quality)
d <- max_log_likelihood(data = ind_recode_quality_first_d.sample, 
                        choices = choices_no_head, times = 2, 
                        beta_start_min = -1, beta_start_max = 1)






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
library("zoo")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
m <- mlogit(mode ~ price | income | catch, data = Fish)
# compute a data.frame containing the mean value of the covariates in
# the sample
z <- with(Fish, data.frame(price = tapply(price, idx(m, 2), mean),
                           catch = tapply(catch, idx(m, 2), mean),
                           income = mean(income)))
# compute the marginal effects (the second one is an elasticity
## IGNORE_RDIFF_BEGIN
effects(m, covariate = "income", data = z)