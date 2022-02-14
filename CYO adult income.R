#### Setup & Introduction
# install relevant packages (if necessary) and call libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readxl)

# download data from my github profile
dat <- read_csv("https://raw.githubusercontent.com/thisisfelix/edX-CYO-project/main/adult.csv")

# show summary table
knitr::kable(summary(dat)[, 1:5], caption = "Raw data summary")
knitr::kable(summary(dat)[, 6:10])
knitr::kable(summary(dat)[, 11:15])

# add table of column names & descriptions:
desc_table <- tibble(Column = colnames(dat),
                     Description = c("Age of a respondent", "Employment type (e.g. private sector, government employee, self-employed, ...)", "'Final Weight' - a summary statistic for people with similar socio-economic status", "Highest achieved education level", "Numerical representation of the 'Education' variable", "Marital status", "Description of the individual's occupation", "Description of the individual's relationship", "Race", "Sex", "Individual's capital gains", "Individual's capital losses", "Work hours per week", "Country of origin", "Description if income is above or below $50k"))
knitr::kable(desc_table, caption = "Columns in the data set")

# add first rows of data in a table
knitr::kable(head(dat)[, 1:8], caption = "First rows of the data", align = 'c')
knitr::kable(head(dat)[, 9:15], align = 'c')

# clean the data to get rid of ? lines
clean_dat <- dat %>% filter(age != "?" & workclass != "?" & fnlwgt != "?" & education != "?" & education.num != "?" & marital.status != "?" & occupation != "?" & relationship != "?" & race != "?" & sex != "?" & capital.gain != "?" & capital.loss != "?" & hours.per.week != "?" & native.country != "?" & income != "?")

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = clean_dat$age, times = 1, p = 0.15, list = FALSE)
adult_inc <- clean_dat[-test_index,]
temp <- clean_dat[test_index,]

validation <- temp %>%
  semi_join(adult_inc, by = "workclass") %>%
  semi_join(adult_inc, by = "education") %>%
  semi_join(adult_inc, by = "education.num") %>%
  semi_join(adult_inc, by = "marital.status") %>%
  semi_join(adult_inc, by = "occupation") %>%
  semi_join(adult_inc, by = "relationship") %>%
  semi_join(adult_inc, by = "race") %>%
  semi_join(adult_inc, by = "sex") %>%
  semi_join(adult_inc, by = "native.country")

removed <- anti_join(temp, validation)

adult_inc <- rbind(adult_inc, removed)

#### Data Exploration & Visualization
# Income distribution by age group
adult_inc %>% ggplot(aes(age, fill = income)) + geom_histogram(binwidth = 5) + ggtitle("Fig.1: Income distribution by age group")

# Income distribution by workclass
adult_inc %>% ggplot(aes(workclass, fill = income)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Fig.2: Income distribution by work class")

# create list of education.num and education
education_table <- adult_inc %>% distinct(education, education.num) %>% slice_min(order_by = education.num, n = nrow(.))
knitr::kable(education_table, caption = "Education levels in the data set")

# plot education against income groups
adult_inc %>% mutate(education = reorder(education, education.num, FUN = mean)) %>% ggplot(aes(education, fill = income)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Fig. 3: Income levels by education")

# Investigate the connection between the two variables
adult_inc %>% ggplot(aes(relationship)) + geom_histogram(stat = "count") + facet_wrap(~marital.status) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Fig. 4: Relationship and marital status types")

# plot marital status against income groups
adult_inc %>% ggplot(aes(marital.status, fill = income)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Fig. 5: Income levels by marital status")

# plot occupation types against income levels
adult_inc %>% ggplot(aes(occupation, fill = income)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Fig. 6: Income levels by occupation")

# plot race against income levels
adult_inc %>% ggplot(aes(race, fill = income)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Fig. 7: Income levels by race")

# plot sex against income levels
adult_inc %>% ggplot(aes(sex, fill = income)) + geom_histogram(stat = "count") + ggtitle("Fig. 8: Income levels by sex")

# create new column
adult_inc <- adult_inc %>% mutate(capital.change = capital.gain - capital.loss)

# plot capital changes against income levels
adult_inc %>% ggplot(aes(capital.change, fill = income)) + geom_histogram(binwidth = 5000) + ggtitle("Fig. 9: Income levels by capital change")

# plot hours per week against income levels
adult_inc %>% ggplot(aes(hours.per.week, fill = income)) + geom_histogram(binwidth = 5) + ggtitle("Fig. 10: Income levels by weekly work hours")

# create table of native countries
country_table <- adult_inc %>% group_by(native.country) %>% summarize(count = n()) %>% arrange(., desc(count))
knitr::kable(country_table, caption = "List of native countries in the sample")

# set South Korea / South Africa split
adult_inc <- adult_inc %>% mutate(native.country = ifelse(native.country == "South" & race == "Asian-Pac-Islander", "South Korea", native.country)) %>% mutate(native.country = ifelse(native.country == "South", "South Africa", native.country))
# plot income levels by native country
adult_inc %>% ggplot(aes(native.country, fill = income)) + geom_histogram(stat = "count") + coord_flip() + ggtitle("Fig. 11: Income levels by native country")

# plot income levels by native country (ex USA)
adult_inc %>% filter(native.country != "United-States") %>% ggplot(aes(native.country, fill = income)) + geom_histogram(stat = "count") + coord_flip() + ggtitle("Fig. 12: Income levels by native country (ex USA)")

#### Model Design
# create test and train sets
set.seed(1910, sample.kind = "Rounding")
test_set_index <- createDataPartition(y = adult_inc$age, times = 1, p = 0.1, list = FALSE)
train_set <- adult_inc[-test_set_index,]
temp <- adult_inc[test_set_index,]

test_set <- temp %>%
  semi_join(adult_inc, by = "workclass") %>%
  semi_join(adult_inc, by = "education") %>%
  semi_join(adult_inc, by = "education.num") %>%
  semi_join(adult_inc, by = "marital.status") %>%
  semi_join(adult_inc, by = "occupation") %>%
  semi_join(adult_inc, by = "relationship") %>%
  semi_join(adult_inc, by = "race") %>%
  semi_join(adult_inc, by = "sex") %>%
  semi_join(adult_inc, by = "native.country")

removed <- anti_join(temp, test_set)

test_set <- rbind(test_set, removed)

# set up summary table
results_table <- tibble(Model = "Target", 
                        Accuracy = 0.85, 
                        Sensitivity = 0.75, 
                        Specificity = 0.75)

## GLM Model
# train the model
train_glm <- train(income ~ age + workclass + education.num + marital.status + 
                     occupation + race + sex + capital.change + hours.per.week +
                     native.country, method = "glm", data = train_set)
# create predictions for the test set
y_hat_glm <- predict(train_glm, test_set, type = "raw")
# create confusion matrix
cm_glm <- confusionMatrix(y_hat_glm, factor(test_set$income))
# add results to summary table
results_table <- bind_rows(results_table, tibble(Model = "GLM", 
                                                 Accuracy = cm_glm$overall[[1]], 
                                                 Sensitivity = cm_glm$byClass[[1]], 
                                                 Specificity = cm_glm$byClass[[2]]))

## LDA Model
# train the model
train_lda <- train(income ~ age + workclass + education + marital.status +
                     occupation + race + sex + capital.change + hours.per.week +
                     native.country, method = "lda", data = train_set)
# create predictions for the test set
y_hat_lda <- predict(train_lda, test_set)
# create confusion matrix
cm_lda <- confusionMatrix(y_hat_lda, factor(test_set$income))
# add results to the summary table
results_table <- bind_rows(results_table, tibble(Model = "LDA", 
                                                 Accuracy = cm_lda$overall[[1]], 
                                                 Sensitivity = cm_lda$byClass[[1]], 
                                                 Specificity = cm_lda$byClass[[2]]))

## QDA Model
# train the model
train_qda <- train(income ~ age + education.num + sex + marital.status, 
                   method = "qda", data = train_set)
# create predictions for the test set
y_hat_qda <- predict(train_qda, test_set)
# create confusion matrix
cm_qda <- confusionMatrix(y_hat_qda, factor(test_set$income))
# add results to the summary table
results_table <- bind_rows(results_table, tibble(Model = "QDA", 
                                                 Accuracy = cm_qda$overall[[1]], 
                                                 Sensitivity = cm_qda$byClass[[1]], 
                                                 Specificity = cm_qda$byClass[[2]]))

## Decision Tree model
# train the model
train_rpart <- train(income ~ age + workclass + education + marital.status + occupation + 
                       race + sex + capital.change + hours.per.week + native.country,
                     method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)), 
                     data = train_set)
# create predictions for the test set
y_hat_rpart <- predict(train_rpart, test_set)
# create confusion matrix
cm_rpart <- confusionMatrix(y_hat_rpart, factor(test_set$income))
# add results to results table
results_table <- bind_rows(results_table, tibble(Model = "Classification Tree (rpart)", 
                                                 Accuracy = cm_rpart$overall[[1]], 
                                                 Sensitivity = cm_rpart$byClass[[1]], 
                                                 Specificity = cm_rpart$byClass[[2]]))
# best tune parameter
best_cp_rpart <- train_rpart$bestTune[[1]]

# plot the decision tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.5)

#### Final Hold Out Test
## Set up validation data set like adult_inc
# add capital.change
validation <- validation %>% mutate(capital.change = capital.gain - capital.loss)
# sort any native.country = "South" into South Korea and South Africa 
validation <- validation %>% 
  mutate(native.country = ifelse(native.country == "South" & race == "Asian-Pac-Islander",
                                 "South Korea", native.country)) %>%
  mutate(native.country = ifelse(native.country == "South", "South Africa", native.country))

## Decision Tree Model
# train model
final_train_rpart <- train(income ~ age + workclass + education + marital.status +
                             occupation + race + sex + capital.change + hours.per.week +
                             native.country, method = "rpart", cp = best_cp_rpart,
                           data = adult_inc)
# create predictions on validation set
final_y_hat_rpart <- predict(final_train_rpart, validation)
# create confusion matrix
cm_final_rpart <- confusionMatrix(final_y_hat_rpart, factor(validation$income))
# final results table
final_table <- tibble(Model = c("Target", "Final Classification Tree (rpart)"),
                      Accuracy = c(0.85, cm_final_rpart$overall[[1]]),
                      Sensitivity = c(0.75, cm_final_rpart$byClass[[1]]),
                      Specificity = c(0.75, cm_final_rpart$byClass[[2]]))
knitr::kable(final_table, caption = "Final Model Results")