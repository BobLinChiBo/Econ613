rm(list = ls())
library(tidyverse)
library(ggplot2)
library(scales)
library(ineq) # For double checking
setwd("C:/Users/boblin/Documents/GitHub/Econ613/Assignments/A1")

# Note that since R stores numbers in 32 bit integers or 64 bit double precision,
# the maximum integer R can store as number is: 
options(digits = 22) # set how much digit R should display when printing
2 ^ .Machine$double.digits # 9007199254740992 in my computer
# Hence, the number larger than 9007199254740992 cannot be stored as number without losing presion.
# We shall find that "idind" (19 digit) and "idmen" (16 digit) can be larger than 9007199254740992 (16 digit).
# Also, "profession" may take value "00".
# As a result, we shall store "idind", "idmen", and "profession" as character.
options(digits = 7)

### Exercise 1
read_dathh <- function(filename){
  path <- paste("./Data/", filename, sep = "")
  return(
    suppressMessages( # to suppress the message that arises from reading a column without name
      read_csv(path, 
               col_types = list(idmen = "c") # read "idmen" as character 
               )) %>% 
      mutate(across(c(idmen, mstatus, move, location), as_factor)) %>% # to reduce the size of data set  
      select(!starts_with("...")) # remove first column
  )
}
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

# We also need to check if idmen in dathh and idind in datind are unique
check_unique_idmen_row <- function(data){
  a <- data %>% select(idmen) %>% n_distinct(na.rm = TRUE) 
  b <- data %>% nrow()
  return(a==b)
}
check_unique_idind_row <- function(data){
  a <- data %>% select(idind) %>% n_distinct(na.rm = TRUE) 
  b <- data %>% nrow() 
  return(a==b)
}
years <- 2004:2019
filenames <- paste(rep("dathh",length(years)), years, ".csv", sep = "")
temp_file <- map(filenames, read_dathh)
temp_logic <- map_lgl(temp_file, check_unique_idmen_row)
years[!temp_logic] # idmen are unique for each data set 
filenames <- paste(rep("datind",length(years)), years, ".csv", sep = "")
temp_file <- map(filenames, read_datind)
temp_logic <- map_lgl(temp_file, check_unique_idind_row)
years[!temp_logic] # Duplicate idind are found in year 2013

# Find errors source
# Errors are from wrong idmen in datind2013.
datind2013 <- read_datind("datind2013.csv")
datind2013 %>% select(-idmen) %>% distinct() %>% check_unique_idind_row

# Duplicate idind have different idmen.
# In fact, idind already implictly contain its idmen (in the middle part of idind)
# So we should find that the wrong idmen = true idmen + 1 
duplicate_ind_logi <- duplicated(datind2013$idind)
duplicate_ind <- datind2013 %>% filter(duplicate_ind_logi) %>% select(idind, idmen)
duplicate_ind

## 1.1
temp_data <- read_dathh("dathh2007.csv")
temp_data %>% nrow()

## 1.2
temp_data <- read_dathh("dathh2005.csv")
temp_data %>%  filter(mstatus == "Couple, with Kids") %>% nrow()

## 1.3
temp_data <- read_datind("datind2008.csv")
temp_data %>% nrow()

## 1.4
temp_data <- read_datind("datind2016.csv")
temp_data %>% filter(age >= 25 & age <= 35) %>% nrow()

## 1.5
temp_data <- read_datind("datind2009.csv")
temp_data %>%
  count(profession, gender) %>% 
  pivot_wider(names_from = gender,values_from = n)

## 1.6
# library(ggplot2)
# library(scales)
datind2005 <- read_datind("datind2005.csv")
datind2019 <- read_datind("datind2019.csv")
# Rough graphs 
datind2005 %>%
  filter(! is.na(wage)) %>% 
  ggplot(aes(x = wage)) + 
  geom_density() 
datind2019 %>%
  filter(! is.na(wage)) %>% 
  ggplot(aes(x = wage)) + 
  geom_density() 

# To make graphs more readable (ignoring some extreme cases (wage > 100000))
datind2005 %>%
  filter(! is.na(wage)) %>% 
  ggplot(aes(x = wage)) + 
  geom_density(bw = 5000) +
  scale_x_continuous(labels = label_comma(), limits = c(NA, 100000)) +
  scale_y_continuous(labels = label_comma())
datind2019 %>%
  filter(! is.na(wage)) %>% 
  ggplot(aes(x = wage)) + 
  geom_density(bw = 5000) +
  scale_x_continuous(labels = label_comma(), limits = c(NA, 100000)) +
  scale_y_continuous(labels = label_comma())

# Calculate Gini coefficient according to https://www.statsdirect.com/help/default.htm#nonparametric_methods/gini.htm
gini_wage <- function(dataset){
  wage <- dataset$wage[!is.na(dataset$wage)]
  wage <- sort(wage)
  n <- length(wage)
  numerator <- 2*sum((wage-sum(wage)/n) * 1:n)
  denominator <- n^2*mean(wage) 
  return(numerator/denominator)
}

datind2005 %>%
  summarise(MEAN = mean(wage, na.rm = TRUE), 
            SD = sd(wage, na.rm = TRUE), 
            IDR = quantile(wage, 9/10, na.rm = TRUE) - quantile(wage, 1/10, na.rm = TRUE)) %>%
  mutate(GINI = gini_wage(datind2005))

# Double check Gini coefficient with package ineq
# library(ineq)
datind2005 %>%
  filter(!is.na(wage)) %>%
  pull(wage) %>%
  Gini() %>%
  all.equal(gini_wage(datind2005))

## 1.7
datind2010 <- read_datind("datind2010.csv")
datind2010 %>% 
  filter(! is.na(age)) %>% 
  ggplot(aes(x = age)) + 
  geom_density() 

# Compare male and female wage with histogram
datind2010 %>%
  filter(! is.na(age)) %>% 
  ggplot(aes(x=age, fill=gender)) +
  geom_histogram(position = "dodge") 
# Compare male and female age with density
# The distribution of female age is more concentrated at higher age.
datind2010 %>%
  filter(! is.na(age)) %>% 
  ggplot(aes(x=age, fill=gender, group=gender)) +
  geom_density(alpha =.4) 
  
  
## 1.8
# Extract household in Paris
dathh2011 <- read_dathh("dathh2011.csv")
household_Paris <-
  dathh2011 %>%
  filter(location == "Paris") %>%
  distinct(idmen) %>%
  pull(idmen)

# Count individuals in those households 
datind2011 <- read_datind("datind2011.csv")
datind2011 %>%
  filter(idmen %in% household_Paris) %>%
  nrow()


### Exercise 2
## 2.1.1
years = 2004:2019
filenames = paste(rep("datind", length(years)), years, ".csv", sep = "")
data_ind <- map_dfr(filenames, read_datind) 
data_ind_clear <- # delete duplicate data
  data_ind %>%  
  arrange(idmen) %>% # distinct() select the first one of duplicates
  distinct(idind, year, .keep_all = TRUE)

## 2.1.2
filenames = paste(rep("dathh", length(years)), years, ".csv", sep = "")
data_hh <- map_dfr(filenames, read_dathh)

## 2.1.3
tbl_vars(data_ind)[tbl_vars(data_ind_clear) %in% tbl_vars(data_hh)]

## 2.1.4
data_merged <- left_join(data_ind_clear, data_hh, by = c("idmen", "year"))

## 2.2.1 
large_household <- 
  data_merged %>%
  distinct(idind, idmen, year) %>%
  count(idmen, year, sort = TRUE) %>%
  filter(n >= 4) 
# Total frequency
large_household %>% nrow()
# Total distinct
large_household %>% select(idmen) %>% n_distinct()
# Count per year
large_household %>% count(year) 

## 2.2.2
unemployed_household <- 
  data_merged %>%
  filter(empstat == "Unemployed") %>%
  distinct(idind, idmen, year) %>%
  count(idmen, year, sort = TRUE) %>%
  filter(n >= 1) 
# Total frequency
unemployed_household %>% nrow()
# Total distinct
unemployed_household %>% select(idmen) %>% n_distinct()
# Count per year
unemployed_household %>% count(year) 

## 2.2.3
profession_household <-
  data_merged %>%
  filter(!is.na(profession)) %>%
  distinct(idind, idmen, year, profession) %>%
  count(idmen, year, profession, sort = TRUE) %>%
  filter(n >= 2) %>% 
  distinct(idmen, year)
# Total frequency
profession_household %>% nrow()
# Total distinct
profession_household %>% select(idmen) %>% n_distinct()
# Count per year
profession_household %>% count(year) 

## 2.2.4
kids_ind <- 
  data_merged %>%
  filter(mstatus == "Couple, with Kids") %>%
  distinct(idind, year) 
# Total frequency
kids_ind %>% nrow()
# Total distinct
kids_ind %>% select(idind) %>% n_distinct()
# Count per year
kids_ind  %>% count(year) 

## 2.2.5
Paris_ind <- 
  data_merged %>%
  filter(location == "Paris") %>%
  distinct(idind, year)
# Total frequency
Paris_ind %>% nrow()
# Total distinct
Paris_ind %>% select(idind) %>% n_distinct()
# Count per year
Paris_ind  %>% count(year) 

## 2.2.6
data_merged %>%
  distinct(idind, idmen, year) %>%
  count(idmen, year, sort = TRUE) %>%
  slice_max(n)

## 2.2.7
household_2010 <- data_merged %>% filter(year == 2010) %>% pull(idmen) 
household_2011 <- data_merged %>% filter(year == 2011) %>% pull(idmen) 
household_2010_11 <- household_2010[household_2010 %in% household_2011]
length(household_2010_11)

### Exercise 3
## 3.1
# Note that there are 2 households that are in the "datind" data set but not in the "dathh" data set,
data_ind %>% 
  filter(! (idmen %in% data_hh$idmen)) %>% 
  distinct(idmen)
# while all households in the "dathh" data set are in the "datind" data set.
data_hh %>% 
  filter(! (idmen %in% data_ind$idmen)) %>% 
  distinct(idmen)
# Since the question focus on the households, we use the "dathh" data set , rather than the merged one.
household_group <-
  data_hh %>%
  group_by(idmen) %>%
  arrange(year) 
  
year_enter_hh <-
  household_group %>%
  filter(row_number()==1) %>%
  mutate(year_enter = year) %>%
  select(idmen, year_enter) 
year_exit_hh <-
  household_group %>%
  filter(row_number()==n()) %>%
  mutate(year_exit = year + 1) %>%
  select(idmen, year_exit)
years_in_survey <- 
  left_join(year_enter_hh, year_exit_hh)  %>%
  mutate(years_in_survey = year_exit - year_enter) 
# Check if gap year
years_in_survey <- 
  household_group %>% 
  summarise(length = n()) %>% 
  mutate(year_enter = year_enter_hh$year_enter,
         year_exit = year_exit_hh$year_exit,
         years_in_survey = years_in_survey$years_in_survey) %>% 
  mutate(gap = length < years_in_survey) 
years_in_survey %>% 
  filter(gap == TRUE) %>% 
  nrow()

# Plot the distributions of the time spent in the panel but consider different cases:
# consecutive years: only count consecutive years and ignore any household with gap year
# total years: count the total years a household spent in the panel regardless there is a gap year or not
# They are roughly the same
years_in_survey %>%
  filter(gap == FALSE) %>% 
  ggplot() + 
  geom_density(aes(x = years_in_survey, color="consecutive years"), bw = 0.5) +
  geom_density(data = years_in_survey, aes(x = length, color="total years"), bw = 0.5) +
  scale_color_manual("", breaks=c("consecutive years", "total years"), values = c("blue","red")) +
  guides(color = guide_legend(override.aes = list(fill = c("blue", "red"))))

## 3.2
data_datent <-
  data_merged %>%
  mutate(year_is_datent = (datent == year)) 

# Report first ten observations
data_datent %>%
  select(idind, idmen, year, datent, year_is_datent) %>% 
  slice(1:10)

# Report share per year
datent_percentage <- 
  data_datent %>%
  group_by(year) %>%
  summarise(datent_rate = mean(year_is_datent, na.rm=TRUE)) 
datent_percentage %>%
  ggplot(aes(x = year, y = datent_rate)) +
  geom_line() 

## 3.3
data_migration <-
  data_merged %>%
  mutate(migration = ((myear == year) & !is.na(myear)) | ((move == 2)& !is.na(move))) %>%
  mutate(migration = ifelse( is.na(myear) & is.na(move) , NA, migration)) 

# Report first ten observations
data_migration %>%
  select(idind, idmen, year, myear, move, migration) %>%  
  slice(1:10)

# Report share per year
migration_percentage <-
  data_migration %>%
  group_by(year) %>%
  summarise(migration_rate = mean(migration, na.rm = TRUE)) 
migration_percentage %>%
  ggplot(aes(x = year, y = migration_rate)) +
  geom_line() 


## 3.4
# Prefer the method of using "datent" 
# since there is a gap between 2014 and 2015 if we use the other method,
# which may result from the fact that the method uses "myear" before 2014 and "move" after 2014
# and respondents may have different interpretation on the corresponding questions of the two variables.
# Also, there are more missing values for "move" than for "datent", which may decrease the precision of the measurement of migration.  
# In addition, the definition of "move" is in fact different from the "datent" == year.
# when considering an individual can exit and reenter the panel, the survey that "move" compare previous address from may be not the survey of last year. 
left_join(datent_percentage, migration_percentage) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = datent_rate), color = "red") +
  geom_line(aes(y = migration_rate), color = "blue") 

## 3.5
data_last_year_work <- 
  data_datent %>%
  group_by(idind) %>%
  mutate(year = year + 1, 
         profession_last_year = profession,
         empstat_last_year = empstat) %>% 
  select(year, idind, profession_last_year, empstat_last_year) %>% 
  right_join(data_datent, by = c("idind", "year"))
data_change_work_by_year <- 
  data_last_year_work %>%  
  mutate(cahnge_profession_this_year = profession != profession_last_year) %>% 
  mutate(change_empstat_this_year = empstat != empstat_last_year) %>% 
  mutate(change_work_this_year = cahnge_profession_this_year | change_empstat_this_year) 
change_work_migration_by_year <- 
  data_change_work_by_year %>% 
  ungroup() %>% 
  filter(change_work_this_year == TRUE) %>% 
  filter(year_is_datent == TRUE)

change_work_migration_by_year %>% 
  distinct(idmen,year) %>% 
  count(year) 


### Exercise 4
# We crate a dummies table indicating whether an individual is in the survey of certain year 
years = 2004:2019
distinct_ind_data <- distinct(data_ind, idind, year)
dummies <- outer(distinct_ind_data$year, years, "==") 
colnames(dummies) <- years
data_ind_dummies <- 
  as_tibble(dummies) %>% 
  mutate(idind = distinct_ind_data$idind) %>% 
  group_by(idind) %>% 
  summarise(across(.fns = sum)) %>% 
  ungroup()

# We first compute year by year attrition rate: 
# numerator = number of people that are both in the survey of this year and last year
# denominator = number of people that are in the survey of last year
year_by_year_compare <- 
  data_ind_dummies %>% 
  mutate(across(-c("2004", idind), 
                ~ . & select(data_ind_dummies, 
                             grep(cur_column(), colnames(data_ind_dummies)) - 1),
                .names = "consecutive_{.col}"))  
survival_number <- 
  year_by_year_compare %>% 
  summarise(across(-idind, sum)) 
survival_rate_year_by_year <- 
  survival_number %>% 
  summarise(across(c("2004":"2018"), 
                   ~ select(survival_number, 
                            grep(cur_column(), colnames(data_ind_dummies)) + length(2004:2018)) / . 
                   )) 
attriion_rate_year_by_year <- 1 - survival_rate_year_by_year 
colnames(attriion_rate_year_by_year) <- years[-1]
attriion_rate_year_by_year <- 
  attriion_rate_year_by_year %>% 
  pivot_longer(everything(), names_to = "year", values_to = "attrition_rate_year_by_year") 
attriion_rate_year_by_year

# We then compute attrition rate based on the certain base year:
# numerator = number of people that are in the panel from base year till this year
# denominator = number of people that are in the survey of base year
compute_attrition_rate <- 
  function(base_year, data_id_dummies){
    if (base_year >= 2019 | base_year < 2004) {
      return("Cannot compute. Base year shoud be 2004-2018")
    }
    data_id_dummies <- 
      data_id_dummies %>% 
      pivot_longer(cols = -idind,
                   names_to = "year", 
                   values_to = "participate") %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(participate = ifelse(year < base_year, 0, participate))
    
    survival_participate_number <- 
      data_id_dummies %>% 
      group_by(idind) %>% 
      arrange(year) %>% 
      mutate(consecutive_years = cumsum(participate),
             survive = consecutive_years == (year - base_year + 1) & consecutive_years != 0) %>% 
      group_by(year) %>% 
      mutate(survival_number = sum(survive == TRUE),
             participate_number = sum(participate == TRUE)) %>% 
      distinct(year, survival_number, participate_number)
    
    base_year_number <- 
      survival_participate_number %>% 
      filter(year == base_year) %>% 
      pull(participate_number)
    
    attrition_rate <- 
      survival_participate_number %>% 
      mutate(survival_rate = survival_number/base_year_number,
             attrition_rate = ifelse(year < base_year, NA, 1 - survival_rate)) %>% 
      select(year, attrition_rate) 
  return(attrition_rate)
  }
# Take 2012 as an example
compute_attrition_rate(2012,data_ind_dummies)

# The whole table
compute_attrition_rate_for_table <- function(base_years, data_id_dummies){
  t_compute_attrition_rate <- function(base_year, data_id_dummies){
    return(
        compute_attrition_rate(base_year, data_id_dummies) %>% 
        pivot_wider(names_from = year, values_from = attrition_rate))
  }
  
  table <- map_dfr(base_years, t_compute_attrition_rate, data_ind_dummies) %>% t()
  colnames(table) <- paste("base_years", base_years, sep = "_")
  table <- as_tibble(table) %>% mutate(year = years) %>% relocate(year)
  return(table)
}
base_years = 2004:2018
attrition_rate_table <- compute_attrition_rate_for_table(base_years, data_ind_dummies)
attrition_rate_table
# Double check
diag(as.matrix(attrition_rate_table)[2:length(years),1:length(base_years)+1]) == attriion_rate_year_by_year$attrition_rate_year_by_year
