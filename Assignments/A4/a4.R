library(tidyverse)
library(ggplot2)
library(scales)
library(sampleSelection)
library(AER)
library(plm)
library(numDeriv)
options(scipen=10, digit=20)
##### Exercise 1
### Q1 & Q2 Read and Organize Data
raw_cross_sectional <- 
  read_csv("./Data/dat_A4.csv", 
           col_types = cols(.default = "d")) %>% 
  mutate(KEY_SEX_1997 = factor(KEY_SEX_1997, labels = c("male", "female")),
         KEY_RACE_ETHNICITY_1997 = factor(KEY_RACE_ETHNICITY_1997, labels = c("black", "hispanic", "mixed", "non")),
         CV_MARSTAT_COLLAPSED_2019 = factor(CV_MARSTAT_COLLAPSED_2019, labels = c("never", "married", "seperated", "divorced", "widowed"))) %>%
  rename(birth_year = KEY_BDATE_Y_1997,
    gender = KEY_SEX_1997,
    marital_status = CV_MARSTAT_COLLAPSED_2019,
    number_child_under_18_in_house = CV_BIO_CHILD_HH_U18_2019,
    race = KEY_RACE_ETHNICITY_1997,
    income = YINC_1700_2019,
    self_edu_degree = YSCH.3113_2019) %>% 
  mutate(zero_income = income == 0 & !is.na(income),
         missing_income = is.na(income),
         missing_or_zero_income = zero_income | missing_income,
         positive_income = !missing_or_zero_income) %>% 
  select(!starts_with("..."))
cross_sectional <- raw_cross_sectional %>% 
  mutate(across(starts_with("CV_HGC_BIO"), function(x) ifelse(x == 95, NA, x)),
         married = marital_status == "married",
         age = 2019 - birth_year,
         work_exp = rowSums(across(starts_with("CV_WKSWK_JOB"), function(x) x/52), na.rm = TRUE),
         bio_parent_edu_year = rowSums(across(starts_with("CV_HGC_BIO"))),
         res_parent_edu_year = rowSums(across(starts_with("CV_HGC_RES"))),
         all_parent_edu_year = rowSums(across(starts_with("CV_HGC"))),
         self_edu_degree_year = case_when(self_edu_degree == 1 ~ 0,
                                          self_edu_degree == 2 ~ 12,
                                          self_edu_degree == 3 ~ 12,
                                          self_edu_degree == 4 ~ 14,
                                          self_edu_degree == 5 ~ 16,
                                          self_edu_degree == 6 ~ 18,
                                          self_edu_degree == 7 ~ 23,
                                          self_edu_degree == 8 ~ 19,
                                          self_edu_degree <= 0 ~ NA_real_),
         all_edu_year = all_parent_edu_year + self_edu_degree_year)
### Q3 Visualize Data
## Income across age, gender, number of children
cross_sectional_positive_income <- 
  cross_sectional %>%
  filter(positive_income == 1)
cross_sectional_positive_income %>%
  ggplot(aes(x = age, y = income, group = age)) +
  geom_boxplot(position="dodge") +
  scale_y_continuous(labels = label_comma())
cross_sectional_positive_income %>%
  ggplot(aes(x = gender, y = income, group = gender)) +
  geom_boxplot(position="dodge") +
  scale_y_continuous(labels = label_comma()) + 
  scale_x_discrete(labels = c("1" = "MALE", "2" = "FEMALE"))
cross_sectional_positive_income %>%
  filter(! is.na(number_child_under_18_in_house)) %>% 
  mutate(number_child_under_18_in_house = as.factor(number_child_under_18_in_house)) %>% 
  ggplot(aes(x = number_child_under_18_in_house, y = income, group = number_child_under_18_in_house)) +
  geom_boxplot(position="dodge") +
  scale_y_continuous(labels = label_comma())+
  scale_x_discrete()

## Percentage of zero_income / missing_income / missing_or_zero_income
cross_sectional %>%
  group_by(age) %>% 
  summarise(across(c(zero_income, missing_income, missing_or_zero_income),.fns =  mean, na.rm = TRUE))
cross_sectional %>%
  group_by(gender) %>% 
  summarise(across(c(zero_income, missing_income, missing_or_zero_income),.fns =  mean, na.rm = TRUE))
cross_sectional %>%
  group_by(marital_status) %>% 
  summarise(across(c(zero_income, missing_income, missing_or_zero_income),.fns =  mean, na.rm = TRUE))
cross_sectional %>%
  group_by(number_child_under_18_in_house) %>% 
  summarise(across(c(zero_income, missing_income, missing_or_zero_income),.fns =  mean, na.rm = TRUE))
cross_sectional %>%
  group_by(marital_status, number_child_under_18_in_house) %>% 
  summarise(across(c(zero_income, missing_income, missing_or_zero_income),.fns =  mean, na.rm = TRUE))

## Interpretation
# older people tend to have slightly higher income; males tend to have higher income than female
# people with separated marital status tend to have more percentage of zero-income
# no clear relation between number of children and income

##### Exercise 2
### Q1
X <- cross_sectional %>% 
  mutate(income = ifelse(is.na(income), 0, income)) %>% 
  select(income, positive_income, age, gender, married, number_child_under_18_in_house,
         self_edu_degree_year, all_parent_edu_year, work_exp) %>% 
  drop_na()
naive_lm <- lm(income ~ gender + married + self_edu_degree_year + all_parent_edu_year + work_exp, X, subset = positive_income == 1)
summary(naive_lm)
# On average, after controlling other variables, females tend to have lower income than males.
# On average, after controlling other variables, people with higher education attainment tend to have higher income.
# On average, after controlling other variables, people whose parents have higher education attainment tend to have higher income.
# On average, after controlling other variables, people with more working experience tend to have higher income.
# On average, after controlling other variables, married people have higher income than people with other marital status

# However, we can only observe the income of individuals who accept a job opportunity; otherwise the observation is zero or missing.
# Moreover, whether an individual is willing to accept the job is correlated with the potential income of the job.
# Hence, the selection problem may exist and the estimation may be biased.

### Q2
# Heckman model explicitly models the individual's probability of being selected (the so-called selection equation) and the conditional expectation of the outcome variable (the so-called outcome equation).
# By assuming that the error terms of the above two equation are jointly normal, the bias of directly evaluating outcome equation can be viewed as a form of omitted-variables bias (the missing part is exactly the inverse mills ratio).
# Then, by adding the inverse mills ratio which is calculated from the predicted values of the selection equation as a control variable in outcome equation, such bias is corrected.

### Q3
# first stage
selection_glm <- glm(positive_income ~ age + gender + married + number_child_under_18_in_house + self_edu_degree_year + all_parent_edu_year, family = binomial(link = "probit"), X)
summary(selection_glm)

# second stage
X_probit_predicted <- 
  X %>% mutate(probit_predicted = predict.glm(selection_glm, newdata = X),
         inverse_mills_ratio = dnorm(probit_predicted) / pnorm(probit_predicted))
X_probit_predicted_missing <- X_probit_predicted %>% mutate(income = ifelse(income == 0, NA_real_, income))
outcome_lm <- lm(income ~ gender + married + self_edu_degree_year + all_parent_edu_year + work_exp + inverse_mills_ratio, X_probit_predicted_missing)
summary(outcome_lm)

# adjust standard error (reference: https://cran.r-project.org/web/packages/sampleSelection/vignettes/selection.pdf)
data <- data.matrix(X_probit_predicted %>% mutate("(Intercept)" = 1) %>% relocate("(Intercept)"))
positive <- data[,"income"] > 0
data[, "gender"] = data[, "gender"] - 1
errors <- outcome_lm$residuals
probit_predicted <- data[, "probit_predicted"]
inverse_mills_ratio <- data[, "inverse_mills_ratio"]
inverse_mills_ratio_coef <- outcome_lm$coefficients["inverse_mills_ratio"]
delta_i <- inverse_mills_ratio * (inverse_mills_ratio + probit_predicted)
delta_i = delta_i[positive]
variance_hat <- ((errors %*% errors) / length(errors) +  (inverse_mills_ratio_coef^2) * sum(delta_i, na.rm = TRUE) / length(errors))[1,1]
X_outcome <- data[positive, c("(Intercept)", "gender", "married", "self_edu_degree_year", "all_parent_edu_year", "work_exp", "inverse_mills_ratio")]
X_selection <- data[positive, c("(Intercept)", "age", "gender", "married", "number_child_under_18_in_house", "self_edu_degree_year", "all_parent_edu_year")]
q <- (inverse_mills_ratio_coef / sqrt(variance_hat))
delta_matrix <- delta_i * diag(length(delta_i))
V <- vcov(selection_glm)
V <- V
Q_matrix <- q^2 * (t(X_outcome) %*% delta_matrix %*% X_selection) %*% V %*% (t(X_selection) %*% delta_matrix %*% X_outcome)
adjusted_var <-  variance_hat * solve(t(X_outcome) %*% X_outcome) %*% 
  (t(X_outcome) %*% (diag(length(delta_i)) - q^2 * delta_matrix) %*% X_outcome + Q_matrix) %*% 
  solve(t(X_outcome) %*% X_outcome)
adjusted_var <- sqrt(diag(adjusted_var))

# check answer
heckman_heckit = heckit(positive_income ~ age + gender + married + number_child_under_18_in_house + self_edu_degree_year + all_parent_edu_year,
                        income ~ gender + married + self_edu_degree_year + all_parent_edu_year + work_exp,
                        data = X_probit_predicted_missing)

# combining results
first_stage_estimates <- cbind(summary(heckman_heckit)$estimate[1:length(selection_glm$coefficients), 1:2], summary(selection_glm)$coefficients[, 1:2])
colnames(first_stage_estimates) = c("heckit : est", "heckit :se", "own : est", "own :se")
second_stage_estimates <- cbind(summary(heckman_heckit)$estimate[length(selection_glm$coefficients)+1:length(outcome_lm$coefficients), 1:2], summary(outcome_lm)$coefficients[, 1], adjusted_var)
colnames(second_stage_estimates) = c("heckit : est", "heckit :se", "own : est", "own :se")
first_stage_estimates
second_stage_estimates

# compare with naive
# Basically, the interpretation is the same as Q1 since the signs of coefficients (except the intercept) have no change.
# However, the magnitude of some coefficients change.
# Main differences come from gender, married, and individual's education attainment.
# Such differences may result from these factors are correlated with the factors that determine both individual's income and individual's choice of whether accepting a job. 
compare_estimates <- cbind(summary(naive_lm)$coefficients[,1:2], summary(outcome_lm)$coefficients[-length(selection_glm$coefficients), 1], adjusted_var[-length(outcome_lm$coefficients)])
colnames(compare_estimates) = c("naive : est", "naive :se", "own : est", "own :se")
compare_estimates

##### Exercise 3
### Q1
# censored value may be at 100,000
cross_sectional %>% 
  ggplot(aes(x=income)) + 
  geom_histogram(binwidth = 5000) +
  scale_x_continuous(labels = label_comma())
max(cross_sectional$income, na.rm = TRUE)

### Q2
X_positive <- X %>% filter(positive_income == 1)
tobit_tobit <- tobit(income ~ gender + married + self_edu_degree_year + all_parent_edu_year + work_exp,
                     right = 100000, data = X_positive, robust=TRUE)
summary(tobit_tobit)

### Q3
likelihood_probit_ind <- function(coefficients, y, matrix_X, right){
  sigma <- exp(coefficients[length(coefficients)])
  beta <- coefficients[-length(coefficients)]
  y_latent <- matrix_X %*% beta
  prob_1 <- (y >= right) * (1- pnorm((right-y_latent)/sigma))
  prob_2 <- (y < right) * dnorm((y-y_latent)/sigma) / sigma
  prob_y <- prob_1 + prob_2
  prob_y[prob_y > 0.999999999999999999999999999999] = 0.999999999999999999999999999999
  prob_y[prob_y < 0.000000000000000000000000000001] = 0.000000000000000000000000000001
  return(prob_y)
}
log_likelihood <- function(coefficients, y, matrix_X, right){
  prob_y <- likelihood_probit_ind(coefficients, y, matrix_X, right)
  log_likelihood <- sum(log(prob_y))
  return(log_likelihood)
}
max_log_likelihood <- function(y, matrix_X, times = 1000, beta_start_min = -20000, beta_start_max = 20000, best_guess = "", right){
  empty_list <- vector(mode = "list", length = times)
  if(class(best_guess)=="character"){
    start_points <- lapply(empty_list, function(x) runif(n = ncol(matrix_X)+1, min = beta_start_min, max = beta_start_max))
    results <- lapply(X = start_points, FUN = optim, 
                      fn = log_likelihood,
                      method = "BFGS",
                      control = list(maxit = 1000, fnscale = -1, trace = 4),
                      y = y, matrix_X = matrix_X, right = right)
    logLik_results <- lapply(results, "[[", 2)
    max_logLik <- max(unlist(logLik_results))
    positions <- which(unlist(logLik_results) == max_logLik)
    best_start_point <- start_points[[positions[1]]]
  }
  else{
    best_start_point <- best_guess
  }
  best_result <- optim(best_start_point, 
                       fn = log_likelihood, 
                       method = "BFGS",
                       control = list(maxit = 1000, fnscale = -1, trace = 4),
                       y = y, matrix_X = matrix_X, right = right,
                       hessian = TRUE)
  return(best_result)
}
matrix_X <- X_positive %>% 
  select(income, gender, married, self_edu_degree_year, all_parent_edu_year, work_exp) 
matrix_X <- model.matrix(~ gender + married + self_edu_degree_year + all_parent_edu_year + work_exp, matrix_X)
best_guess <- summary(tobit_tobit)$coefficients[,1]
tobit_own <- max_log_likelihood(best_guess=best_guess, y = X_positive$income, matrix_X = matrix_X, right = 100000)
hessian_own <- hessian(log_likelihood, tobit_own$par, y = X_positive$income, matrix_X = matrix_X, right = 100000)
fisher_info_matrix <-  solve(-hessian_own)
std_error <- sqrt(diag((fisher_info_matrix)))
tobit_estimates <- cbind(summary(tobit_tobit)$coefficients[,1:2], tobit_own$par, std_error)
colnames(tobit_estimates) = c("tobit : est", "tobit :se", "own : est", "own :se")
tobit_estimates

### Q4
# Basically, the interpretation is the same as Exercise 3 Q1 since the signs of coefficients have no change.
# However, the magnitude of some coefficients change.
# Main differences come from gender and individual's and their parents' education attainment.
compare_estimates <- cbind(summary(naive_lm)$coefficients[,1:2], tobit_own$par[-length(tobit_own$par)], std_error[-length(tobit_own$par)])
colnames(compare_estimates) = c("naive : est", "naive :se", "own : est", "own :se")
compare_estimates

##### Exercise 4
raw_panel <- 
  read_csv("./Data/dat_A4_panel.csv", 
           col_types = cols(.default = "d")) %>% 
  select(!starts_with("..."))
panel <- raw_panel %>% 
  rename(birth_year = KEY_BDATE_Y_1997,
         gender = KEY_SEX_1997,
         race = KEY_RACE_ETHNICITY_1997) %>%  
  rename_with(~ str_replace(., "YINC-1700", "my_income_y")) %>% 
  rename_with(~ str_replace(., "CV_MARSTAT_COLLAPSED", "marital_status_y")) %>%
  select(!contains("EVER_EDT")) %>% 
  rename_with(~ str_replace(., "CV_HIGHEST_DEGREE_.+?_", "self_edu_degree_y_")) %>% 
  mutate(age = 2019 - birth_year,
         gender = factor(gender, labels = c("male", "female")),
         race = factor(race, labels = c("black", "hispanic", "mixed", "non")),
         across(starts_with("marital_status"), function(x) x == 1, .names = "my_married_{.col}"),
         across(starts_with("self_edu_degree"), function(x) case_when(x == 1 ~ 0,
                                                                      x == 2 ~ 12,
                                                                      x == 3 ~ 12,
                                                                      x == 4 ~ 14,
                                                                      x == 5 ~ 16,
                                                                      x == 6 ~ 18,
                                                                      x == 7 ~ 23,
                                                                      x == 8 ~ 19,
                                                                      x <= 0 ~ NA_real_), .names = "my_edu_{.col}"))
for (year in c(1997:2011, 2013, 2015, 2017, 2019)) {
  panel <- panel %>% 
    mutate("my_work_exp_y_{year}" := rowSums(across(starts_with("CV_WKSWK_JOB") & ends_with(toString(year)), function(x) x/52), na.rm = TRUE))
}
panel <- panel %>% mutate(across(starts_with("my_work_exp_y_"), function(x) case_when(x == 0 ~ NA_real_,
                                                                             x != 0 ~ x)))

### Q1
# Each individual has different ability. Such ability cannot be observable but correlated to their wages.
# Then, when we regress their wages, their ability will be included in the error term.
# However, individuals' ability in this period must correlate with their ability in earlier periods and future periods.
# That is, when we use panel data to do the regression, our error terms are not independent from each other, causing bias in our estimation.

### Q2
panel_long <- panel %>% 
  pivot_longer(starts_with("my_"), names_to = c(".value", "year"), names_sep = "_y_") %>% 
  rename(id = PUBID_1997,
         education  = my_edu_self_edu_degree,
         income = my_income,
         married = my_married_marital_status,
         work_exp = my_work_exp) %>% 
  select(id, year, income, education, married, work_exp) %>%
  drop_na()
panel_plm <- pdata.frame(panel_long, index = c("id", "year"))

## between
between_data <- panel_long %>% 
  group_by(id) %>% summarise(mean_married = mean(married),
                             mean_income = mean(income),
                             mean_education = mean(education),
                             mean_work_exp = mean(work_exp))
between_lm <- lm(mean_income ~ mean_education + mean_married + mean_work_exp, data = between_data)
between_plm <- plm(income ~ education + factor(married) + work_exp, data = panel_plm, model = "between")
compare_estimates <- cbind(summary(between_plm)$coefficients[,1:2], summary(between_lm)$coefficients[,1:2])
colnames(compare_estimates) = c("plm : est", "plm :se", "own : est", "own :se")
rownames(compare_estimates) = c("(Intercept)", "education", "married", "work_exp")
compare_estimates

## within
within_data <- panel_long %>% 
  group_by(id) %>% 
  mutate(count = n()) %>% 
  filter(count > 1) %>% 
  mutate(mean_married = mean(married),
         mean_income = mean(income),
         mean_education = mean(education),
         mean_work_exp = mean(work_exp)) %>% 
  mutate(dif_income = income - mean_income,
         dif_education = education - mean_education,
         dif_married = married - mean_married,
         dif_work_exp = work_exp - mean_work_exp) %>% 
  distinct()
within_lm <- lm(dif_income ~ -1 + dif_education + dif_married + dif_work_exp, data = within_data)
summary(within_lm)
# However, the standard errors need to be adjust since we estimate the means of each individual. Reference:https://www.stata.com/manuals/xtxtreg.pdf (Methods and formulas)
n_individual <- within_data %>% select(id) %>%  n_distinct()
adjust_std <- sqrt(summary(within_lm)$coefficients[,2]^2 * (summary(within_lm)$fstatistic[3])/ (summary(within_lm)$fstatistic[3] - n_individual))
within_plm <- plm(income ~  education + factor(married) + work_exp, data = panel_plm, model = "within", effect = "individual")
compare_estimates <- cbind(summary(within_plm)$coefficients[, 1:2], summary(within_lm)$coefficients[, 1], adjust_std)
colnames(compare_estimates) = c("plm : est", "plm :se", "own : est", "own :se")
rownames(compare_estimates) = c("education", "married", "work_exp")
compare_estimates

## first difference
fd_data <- panel_long %>% 
  group_by(id) %>% 
  mutate(lead_income = dplyr::lead(income, order_by = year),
         lead_education = dplyr::lead(education, order_by = year),
         lead_married = dplyr::lead(married, order_by = year),
         lead_work_exp = dplyr::lead(work_exp, order_by = year)) %>% 
  mutate(dif_income = lead_income - income,
         dif_education = lead_education - education,
         dif_married = lead_married - married,
         dif_work_exp = lead_work_exp - work_exp)
fd_lm <- lm(dif_income ~ -1 + dif_education + dif_married + dif_work_exp, data = fd_data)
fd_plm <- plm(income ~ -1 + education + relevel(factor(married), ref = "TRUE") + work_exp, data = panel_plm, model = "fd", effect = "individual")
compare_estimates <- cbind(summary(fd_plm)$coefficients[, 1:2], summary(fd_lm)$coefficients[, 1:2])
colnames(compare_estimates) = c("plm : est", "plm :se", "own : est", "own :se")
rownames(compare_estimates) = c("education", "married", "work_exp")
compare_estimates

### Q3
# The signs of coefficients of all three estimators are the same.
# They shows that, on average, while controlling other factors, 
# people with higher education tend to have higher income;
# married people tend to have higher income;
# people with more working experience tend to have higher income.
# However, the magnitudes of coefficients of these estimators are quite different.
# We first have to note that theoretically, between estimator is quite different from other estimators.
# the reason is that between estimator kind of estimates the time-average effect of factors on each person and discards other time variation information
# while other two estimators assume each person has certain unobserved time invariant factors affecting his income.
# By subtracting mean or value from last period, such factors can be eliminated. 
# However, if those factors do change over time, then the two estimators cannot well deal with them and would result in missing variable bias.
# For the same reason, subtracting mean or subtracting last period's value could have very different results (the two different estimators imply different amount of effect of unobserved time variant factors on each person's income).
# Furthermore, since our panel data is unbalanced, such a problem is exacerbated.
compare_estimates <- cbind(summary(between_lm)$coefficients[, 1], c(NA, summary(within_lm)$coefficients[, 1]), c(NA, summary(fd_lm)$coefficients[, 1]))
colnames(compare_estimates) = c("between : own", "within :own", "fd : own")
rownames(compare_estimates) = c("(Intercept)", "education", "married", "work_exp")
compare_estimates

