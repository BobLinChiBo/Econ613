rm(list = ls())
library(tidyverse)
library(ggplot2)
library(scales)
setwd("C:/Users/boblin/Documents/GitHub/Econ613/Assignments/A2")

read_datind <- function(filename){
  path <- paste("./Data/", filename, sep = "")
  return(
    suppressMessages( # to suppress the message that arises from reading a column without name
      read_csv(path, 
               col_types = list(idind = "c", idmen = "c", profession = "c") # read "idind","idmen", and "profession" as character 
      )) %>% 
      mutate(across(c(idind, idmen, empstat, profession, gender), as_factor)) %>% # to reduce the size of data set  
      select(!starts_with("...")) # remove first column
  )
}

### Exercise 1
datind2009 <- read_datind("datind2009.csv")
datind2009_clear <- datind2009 %>% filter(!is.na(wage) & wage != 0)

calculation <- function(y, X){
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  y_hat <- X %*% beta_hat
  error <- y - y_hat
  S_square <- (t(error) %*% error / (length(y) - ncol(X)))[1,1]
  var_beta <- S_square * solve(t(X) %*% X)
  Std_Error <- sqrt(diag(var_beta))
  regression_result <- matrix(c(beta_hat, t(Std_Error)), nrow = ncol(X), ncol = 2)
  colnames(regression_result) = c("Coefficients", "Std_Error")
  rownames(regression_result) = colnames(X)
  return(regression_result)
}
construct_y_X <- function(data, outcome, regressors){
  y <- data %>% select(outcome) %>% as.matrix()
  X <- data %>% mutate(constant = 1) %>% select(constant, regressors) %>% as.matrix()
  colnames(X) = c("constant", regressors)
  y_X <- list(y, X)
  return(y_X)
}
regression_own <- function(data, outcome, regressors){
  y_X <- construct_y_X(data, outcome, regressors)
  calculation(y_X[[1]], y_X[[2]])
}

## 1 
y_X <- construct_y_X(datind2009_clear, "wage", "age")
correlation_result <- cor(y_X[[2]], y_X[[1]], use = "complete.obs") # warning since no variation on constant term
colnames(correlation_result) = "wage_correlation"
rownames(correlation_result) = c("intercept", "age")
correlation_result

## 2
fun_reg <- regression_own(datind2009_clear, "wage", "age")
fun_reg
lm_reg <- lm(wage ~ age, datind2009_clear) # Check
summary(lm_reg)$coefficients[, c("Estimate","Std. Error"), drop = FALSE] # Correct

## 3
bootstrap <- function(data, outcome, regressors, times){
  set.seed(123)
  samples <- lapply(rep(list(data), times), FUN = slice_sample, n = length(data$wage), replace = TRUE)
  regression_results <- lapply(samples, regression_own, outcome = outcome, regressors = regressors)
  number_regressors <- nrow(regression_results[[1]])
  coefs_results <- lapply(regression_results, "[", 1:number_regressors, "Coefficients")
  coefs <- transpose(coefs_results)
  coefs <- lapply(coefs, unlist)
  SDs <- lapply(coefs, sd)
  bootstrap_result <- matrix(SDs, ncol = 1)
  colnames(bootstrap_result) ="Std_Error"
  rownames(bootstrap_result) = c("intercept", regressors)
  return(bootstrap_result)
}
# The estimated standard error of using the two strategies are different though the difference is not large.
# The standard formulas of the OLS assume the variance of error term is homoskedastic and not autocorrelated.
# However, bootstrap directly uses the information that the data brings. 
fun_reg[, "Std_Error", drop = FALSE]
bootstrap(datind2009_clear, "wage", "age", 49)
bootstrap(datind2009_clear, "wage", "age", 499)

### Exercise 2
years <- 2005:2018
filenames = paste(rep("datind", length(years)), years, ".csv", sep = "")
data_ind <- map_dfr(filenames, read_datind)
data_ind_clear <- 
  data_ind %>% 
  filter(age >= 18) %>% 
  filter(!is.na(wage) & wage != 0) %>% 
  mutate(year = as.factor(year))
## 1
data_ind_age_group <- 
  data_ind_clear %>% 
  mutate(age_group = cut(age, c(18, 25, 30, 35, 40, 45, 50, 55, 60, max(age)), include.lowest = TRUE)) 

## 2
# Plotting only those wage lower than 100,000 to make the graph readable 
# As age rises (until 60), the wage seems also rises.
data_ind_age_group %>% 
ggplot(aes(x = age_group, y = wage)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_comma(), limits = c(NA, 100000))

## 3
# After adding the year fixed effect, the coefficient of age increases and the coefficient of intercept decreases.
add_dummies <- function(var, data){
  formula <- as.formula(paste("~ ", var , ""))
  var_dummies <- model.matrix(formula, data)
  data_dummies <- cbind(data, var_dummies[,-1])
  return(data_dummies)
}
data_ind_year <- add_dummies("year", data_ind_clear)
year_regressors <- paste("year", years[-1], sep = "")
regression_own(data_ind_year, "wage", c("age", year_regressors))
lm_year_reg <- lm(wage ~ age + year, data_ind_clear) # Check
summary(lm_year_reg)$coefficients[c("(Intercept)", "age"), c("Estimate", "Std. Error")] # Correct
fun_reg


### Exercise 3
datind2007 <- read_datind("datind2007.csv")

## 1
datind2007_clear <- 
  datind2007 %>% 
  filter(empstat != "Inactive" & empstat != "Retired")

## 2

logLik_probit <- function(beta, y, matrix_X){
  y_latent <- matrix_X %*% beta
  prob_y_1 <- pnorm(y_latent)
  prob_y_1[prob_y_1 > 0.9999] = 0.9999
  prob_y_1[prob_y_1 < 0.0001] = 0.0001
  log_likelihood <- sum(y * log(prob_y_1) + (1-y) * log(1-prob_y_1))
  return(log_likelihood)
}

## 3
max_log_likelihood <- function(data, logLik_fun, y, matrix_X, times, beta_start_min, beta_start_max){
  set.seed(123)
  empty_list <- vector(mode = "list", length = times)
  start_points <- lapply(empty_list, function(x) runif(n = ncol(matrix_X), min = beta_start_min, max = beta_start_max))
  results <- lapply(X = start_points, FUN = optim, 
                    fn = get(logLik_fun),
                    method = "BFGS",
                    control = list(trace = 6, maxit = 1000, fnscale = -1),
                    y = y, matrix_X = matrix_X)
  logLik_results <- lapply(results, "[[", 2)
  max_logLik <- max(unlist(logLik_results))
  positions <- which(unlist(logLik_results) == max_logLik)
  best_start_point <- start_points[[positions[1]]]
  best_result <- optim(best_start_point, 
                       get(logLik_fun), 
                       method = "BFGS",
                       control = list(trace = 6, maxit = 1000, fnscale = -1),
                       y = y, matrix_X = matrix_X,
                       hessian = TRUE)
  return(best_result)
}
calculate_std <- function(best_result){
  fisher_info_matrix = solve(-best_result$hessian)       
  std_error  = sqrt(diag(fisher_info_matrix))
  return(std_error)
}
compare_results <- function(data, type, y, matrix_X, best_result){
  regressors <- paste(colnames(matrix_X)[-1], collapse = ' + ')
  formula <- paste(colnames(y), "~", regressors, sep = " ")
  formula <- as.formula(formula)
  reg_glm = glm(formula, family = binomial(link = type), data)
  summary(reg_glm)
  test_beta = reg_glm$coefficients
  logLik(reg_glm)
  if(type == "probit"){  
    logLik_probit(test_beta, y, matrix_X)
  }
  else if (type == "logit"){  
    logLik_logit(test_beta, y, matrix_X)
  }
  else{return("type can only be either probit or logit")}
  std_error <- calculate_std(best_result)
  estimates <- cbind(summary(reg_glm)$coefficients[, 1], summary(reg_glm)$coefficients[, 2], best_result$par, std_error)
  colnames(estimates) = c("GLM : est","GLM :se","own : est","own :se")
  return(estimates)
}

reg_probit_logit <- function(data, type, outcome, regressors, times, beta_start_min, beta_start_max){
  y_X <- construct_y_X(data, outcome, regressors)
  if(times < 50){
    return("times shoud be larger than 49")
  }
  if(type == "probit"){  
    logLik_fun = "logLik_probit"
  }
  else if (type == "logit"){  
    logLik_fun = "logLik_logit"
  }
  else{return("type can only be either probit or logit")}
  best_result <- max_log_likelihood(data, logLik_fun = logLik_fun, y = y_X[[1]], matrix_X = y_X[[2]], 
                     times = times, beta_start_min = beta_start_min, beta_start_max = beta_start_max)
  compare_results(data, type, y = y_X[[1]], matrix_X = y_X[[2]], best_result)
  
}
datind2007_employ <-  datind2007_clear %>% mutate(employed =  empstat == "Employed")
reg_probit_logit(datind2007_employ, "probit", outcome = "employed", regressors = "age",
                 times = 50, beta_start_min = -3, beta_start_max = 3)


## 4
# Conceptually, you cannot including wages as a determinant of labor market participation.
# The reason is that if, and only if, people have positive wage then they are employed.
# However, the data does show that some people are unemployed but with positive wage or are employed but with zero wage.
datind2007_employ_wage <- datind2007_employ %>% mutate(positive_wage = wage > 0) %>% filter(!is.na(wage))
datind2007_employ_wage %>%
  count(employed, positive_wage) %>% 
  pivot_wider(names_from = employed, values_from = n)

reg_probit_logit(datind2007_employ_wage, "probit", outcome = "employed", regressors = c("age", "wage"),
                 times = 100, beta_start_min = -0.4, beta_start_max = 0.4)

### Exercise 4
years <- 2005:2015
filenames = paste(rep("datind", length(years)), years, ".csv", sep = "")
data_ind <- map_dfr(filenames, read_datind)
## 1
data_ind_clear <- 
  data_ind %>% 
  filter(empstat != "Inactive" & empstat != "Retired") %>% 
  mutate(year = as.factor(year))

## 2
logLik_logit <- function(beta, y, matrix_X){
  y_latent <- matrix_X %*% beta
  prob_y_1 <- exp(y_latent) / (1 + exp(y_latent))
  prob_y_1[prob_y_1 > 0.9999] = 0.9999
  prob_y_1[prob_y_1 < 0.0001] = 0.0001
  log_likelihood <- sum(y * log(prob_y_1) + (1-y) * log(1-prob_y_1))
  return(log_likelihood)
}
data_ind_year <- add_dummies("year", data_ind_clear) %>% mutate(employed =  empstat == "Employed")
year_regressors <- paste("year", years[-1], sep = "")
# logit
logit_results <- reg_probit_logit(data_ind_year, "logit", outcome = "employed", regressors = c("age", year_regressors),
                 times = 100, beta_start_min = -1, beta_start_max = 1)
# probit
probit_results <- reg_probit_logit(data_ind_year, "probit", outcome = "employed", regressors = c("age", year_regressors),
                 times = 100, beta_start_min = -1, beta_start_max = 1)
# linear
regression_own(data_ind_year, "employed", c("age", year_regressors))
summary(lm(employed ~ age + year, data_ind_year))$coefficients # check

## 3



### Exercise 5

marginal_effect_var <- function(var, type, y, matrix_X, coefficients){
  if(type == "probit"){  
    logLik_fun = "logLik_probit"
  }
  else if (type == "logit"){  
    logLik_fun = "logLik_logit"
  }
  h <-  coefficients * 0
  h[var] <- coefficients[var] / 100
  print(get(logLik_fun)(beta = coefficients + h , y = y, matrix_X = matrix_X))
  likelihood_h <- exp(get(logLik_fun)(beta = coefficients + h , y = y, matrix_X = matrix_X))
  h_likelihood <- exp(get(logLik_fun)(beta = coefficients - h , y = y, matrix_X = matrix_X))
  print(exp(get(logLik_fun)(beta = coefficients + h , y = y, matrix_X = matrix_X)))
  return((likelihood_h - h_likelihood) / 2*h[var])
}

marginal_effect <- function(data, type, outcome, regressors, coefficients){
  y_X <- construct_y_X(data, outcome, regressors = names(coefficients)[-1])
  return(sapply(regressors, marginal_effect_var, type = "probit", y = y_X[[1]], matrix_X = y_X[[2]], coefficients))
}

marginal_effect(data_ind_year, "probit", "employed", c("age", year_regressors), probit_results[,"own : est"])

