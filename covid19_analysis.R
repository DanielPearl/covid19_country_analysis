# Load packages
library(arules)
library(caret)
library(e1071)
library(FSelector)
library(klaR)
library(rattle)
library(rpart)
library(rpart.plot)
library(stringr)
library(xlsx)

# Read file
file_path = "~/Syracuse/03_Sprint2020/IST707_Data_Analysis/project/covid19_indicators.xlsx"
indicators = read.xlsx(file_path, 1)

## -------------------
## Data Pre-processing
## -------------------

# The data has been prepared for two different types of models. The first type contains dependent variables that are categorical with independent variables that are numeric. The training and testing data labeled as c_indicators and c_indicators is constructed for the models that require numerical data-types for precision. These models are SVM, KNN, Random Forest, and clustering. For the remaining model, the training and testing data labeled as d_indicators and d_test requires all columns to be categorical. This is used to construct a decision tree.

# The first step in data pre-processing involved removing all the columns that were not being used. The following columns were removed from the dataframe: date, new_cases, new_deaths, new_cases_per_million, new_deaths_per_million, new_deaths_per_million, new_tests, new_tests_smoothed, new_tests_per_thousand, new_tests_smoothed_per_thousand, tests_units, stringency_index. The next step involved removing the row corresponding to the entire world. Leaving in a column that contained sum data would distort any calculated results from the numeric columns. 

# The next step involved discretizing just the dependent variable columns to more easily interpret the results. These columns are: total_cases_per_million and total_deaths_per_million. For most of the discretization, the discretize function from the arules package was used. This function takes in a method, the number of breaks/bins, and the labels. For the dependent variables, 5 bins were created to represent the severity of both the infection rate and the death rate. The method used to divide data into these bins is frequency. By setting the method to frequency, we are ensuring that the values from the columns will be divided up evenly between the bins. This ensures we can maintain a substantial representation within all the bins. The labels that we ended up using were: very low, low, moderate, high, very high.

# For the d_indicators dataframe, most of the columns are discretized using the same discretize function. Those columns with fewer pieces of data will contain 3 bins while those with more will contain 5. The age column is discretized using the cut function to divide the data into 4 different bins: 10-19, 20-29, 30-39, 40-49. The cut function contains the break parameter which is given a sequence to divide the ages up evenly.

# The last part of preprocessing the data involves replacing the null values. For the c_indicators dataframe, all the null values within the numeric columns are replaced by the average mean of those columns. For the d_indicators dataframe, a new bin labeled as ‘unknown’ is created to handle all the null values. In order to do this, the columns are converted to strings where all the null values are filled in with the string ‘unknown’ before they are converted back to factors. The following code is used to do this:
  
# Remove unnecessary columns
indicators$date = NULL
indicators$new_cases = NULL
indicators$new_deaths = NULL
indicators$new_cases_per_million = NULL
indicators$new_deaths_per_million = NULL
indicators$new_tests = NULL
indicators$new_tests_smoothed = NULL
indicators$new_tests_per_thousand = NULL
indicators$new_tests_smoothed_per_thousand = NULL
indicators$tests_units = NULL
indicators$stringency_index = NULL
indicators$total_cases = NULL
indicators$total_deaths = NULL
indicators$total_tests = NULL

# Remove World from locations
indicators = indicators[indicators$iso_code != "OWID_WRL",]

# Create labels list for discretization
x_labels = c("low", "moderate", "high")
y_labels = c("very low", "low", "moderate", "high", "very high")

# Discretize total cases per million 
indicators$total_cases_per_million = discretize(indicators$total_cases_per_million, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(indicators$total_cases_per_million)

# Discretize total deaths per million 
indicators$total_deaths_per_million = discretize(indicators$total_deaths_per_million, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(indicators$total_deaths_per_million)

# Divide indicators into discrete and continuous
c_indicators = indicators
d_indicators = indicators

# Fill null values with the column average
for(i in 3:ncol(c_indicators)){
  c_indicators[is.na(c_indicators[,i]), i] = mean(c_indicators[,i], na.rm = TRUE)}

# Set seeds for replication
#set.seed(550)

# Equal frequency binning used for all numeric values 

# Discretize total test
d_indicators$total_tests = discretize(d_indicators$total_tests, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$total_tests)

# Discretize total test per thousand
d_indicators$total_tests_per_thousand = discretize(d_indicators$total_tests_per_thousand, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$total_tests_per_thousand)

# Discretize total test per thousand
d_indicators$population = discretize(d_indicators$population, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(d_indicators$population)

# Discretize total test per thousand
d_indicators$population_density = discretize(d_indicators$population_density, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(d_indicators$population_density)

# Discretize median age
d_indicators$median_age = cut(d_indicators$median_age, breaks = seq(10, 50, by = 10), label=c("10-19", "20-29", "30-39", "40-49"))
table(d_indicators$median_age )

# Discretize 65+
d_indicators$aged_65_older = discretize(d_indicators$aged_65_older, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$aged_65_older)

# Discretize 65+
d_indicators$aged_70_older = discretize(d_indicators$aged_70_older, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$aged_70_older)

# Discretize GDP per capita
d_indicators$gdp_per_capita = discretize(d_indicators$gdp_per_capita, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(d_indicators$gdp_per_capita)

# Discretize extreme poverty
d_indicators$extreme_poverty = discretize(d_indicators$extreme_poverty, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$extreme_poverty)

# Discretize cvd death rate
d_indicators$cvd_death_rate = discretize(d_indicators$cvd_death_rate, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(d_indicators$cvd_death_rate)

# Discretize diabetes prevalence
d_indicators$diabetes_prevalence = discretize(d_indicators$diabetes_prevalence, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(d_indicators$diabetes_prevalence)

# Discretize female smokers
d_indicators$female_smokers = discretize(d_indicators$female_smokers, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$female_smokers)

# Discretize male smokers
d_indicators$male_smokers = discretize(d_indicators$male_smokers, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$male_smokers)

# Discretize handwashing facilities
d_indicators$handwashing_facilities = discretize(d_indicators$handwashing_facilities, method = "frequency", breaks = 3, dig.lab = 3, labels=x_labels)
table(d_indicators$handwashing_facilities)

# Discretize hospital beds per 100k
d_indicators$hospital_beds_per_100k = discretize(d_indicators$hospital_beds_per_100k, method = "frequency", breaks = 5, dig.lab = 5, labels=y_labels)
table(d_indicators$hospital_beds_per_100k)

# Fill null values with unknown
i = sapply(d_indicators, is.factor) 
d_indicators[i] = lapply(d_indicators[i], as.character) 
d_indicators[is.na(d_indicators)] = "unknown"
d_indicators[i] = lapply(d_indicators[i], as.factor) 

# Partition into training and testing dataframes
n = nrow(d_indicators)
index = sample(n, n*.7, replace=FALSE)
# Create training and testing data for discrete datafram
d_train = d_indicators[index,]
d_test = d_indicators[-index,]
# Create training and testing data for continuous datafram
c_train = c_indicators[index,]
c_test = c_indicators[-index,]

# Define the independent variables
x_vars = c("total_tests_per_thousand", "population", "population_density", "median_age", "aged_65_older", "aged_70_older", "gdp_per_capita", "extreme_poverty", "cvd_death_rate", "diabetes_prevalence", "female_smokers", "male_smokers", "handwashing_facilities", "hospital_beds_per_100k")

# This function takes in x and y variables and returns a formula
create_formula = function(x,y){
  f <- as.formula(
  paste(y, 
  paste(x, collapse = " + "), 
  sep = " ~ "))}

# -------------------
# Decision Trees (DT)
# -------------------

## Decision Tree parameters

# The rpart library is used to create a decision tree model from the R caret package. The model is tuned using the repeated cross validation method. This method is used to tune the model by dividing the data into subsets and repeating a number of times. For this parameter, the data is divided into 10 subsets and repeated 3 times. This is used to prevent the model from overfitting. The metric for optimization is set to accuracy which means that the model will tune iteself until it reaches the most accurate results. The tune length is set to 20 which is the number of tries to reach the optimal value.

# Create formula from the create_formula function
formula_cases = create_formula(x_vars, "total_cases_per_million")
formula_deaths = create_formula(x_vars, "total_deaths_per_million")

# Set cross validation hyper-parameter for models
tr_control = trainControl(method = 'repeatedcv', number=10, repeats = 3)

# Build decision tree model for cases per million
dt_model_cases = train(formula_cases,
                 data=d_train,
                 method="rpart",
                 metric="Accuracy",
                 trControl=tr_control,
                 tuneLength = 20
)

# Visualize Decision Tree
rpart.plot(dt_model_cases$finalModel, extra = 100)

# Prediction for total cases per million
dt_pred_cases = predict(dt_model_cases, newdata=d_test)

# Create confusion matrix for total cases per million
t <- table(d_test$total_cases_per_million, dt_pred_cases)
result = confusionMatrix(t)
result # Show confusion matrix results
result[-1] # Show results divided into classes

# Build decision tree model for deaths per million
dt_model_deaths = train(formula_deaths,
                       data=d_train,
                       method="rpart",
                       metric="Accuracy",
                       trControl=tr_control,
                       tuneLength = 20
                       )

# Visualize Decision Tree
rpart.plot(dt_model_deaths$finalModel, extra = 100)

# Prediction for total cases per million
dt_pred_deaths = predict(dt_model_deaths, newdata=d_test)

# Create confusion matrix for total cases per million
t <- table(d_test$total_cases_per_million, dt_pred_deaths)
result = confusionMatrix(t)
result # Show confusion matrix results
result[-1] # Show results divided into classes

# Combine	the	predictions	with countries.
country = data.frame(d_test$location)
cases = data.frame(dt_pred_cases)
deaths = data.frame(dt_pred_deaths)
covid19_dt_pred = cbind(country, cases, deaths)

# Change column names
labels = c("country", "cases", "deaths")
colnames(covid19_dt_pred) = labels

# Write prediction dataframe to csv file
write.csv(covid19_dt_pred, 'decision_tree_pred.csv')

## Decision Tree Analysis

# The top indicators for the infection rate are handwashing facilities, gdp_per_capita, cvd death rate, median age between 30-49, age percentage over 65, and hospital beds. Some of the node connections seem intuitive. For example countries with a lower gdp per capita and a higher cardio vascular death rate have a high infection rate. Though we're assuming most of these indicators lead to a change in infection rate, it's possible that the infection rate might lead to a number of changes in indicators. One example of this is the number of hospital beds. Rather than assume that the number of hospital beds are affecting the infection rate, it might also be reasonable to suggest that a higher infection rate might lead to a higher number of hospital beds. 

# The top indicators for the death rate are percentage of population older than 70, population density, hospital beds, and gdp per capita. Age seems to be correlated with the number of deaths. Countries with a low percentage of elderly people over the age of 70 have a lower death rate. Countries with a higher population density also have a higher death rate. Fewer hospital beds seem to indicate a higher death rate as well. It's possible that the relationship between the y-variable rates and the x-variable (hospital beds) are inverted between the two models. 

# Both the infection and death rate decision tree models performed poorly with an accuracy of around 30-45%. In addition to this, both the precision and recall of these models performed far under 50%. One explanation for this might be the small dataset size. A dataset that showed the demographic details for every individual infected would be far more valuable and accurate. Since this data only shows the summary in each country, various unknown cultural phenomenon might also be influencing both dependent variables. 

# -----------------------------
# Support Vector Machines (SVM)
# -----------------------------

# The SVM model uses an algorithm that can solve both linearly separable and inseparable problems. The goal is to find a linear hyperplane that can separate the data and maximize the margin between the data points.

# The method used to build this model is svmRadial. This is the shape for the kernel. The SVM model will use the trControl parameter to tune itself using the same repeated cross validation defined when creating the decision tree. The cost hyperparameter represented by C in tune_grid corresponds to the number of training errors allowed. Sigma is the width of the Gaussian distribution. The model will iterate through values of C and sigma between 0 and 1. The hyper-parameter preProcess is set to center and scale. This is used to estimate the location and scale of the predictors. 

# Set hyper-parameters for svm model
tune_grid = expand.grid(sigma = seq(0, 1, 0.1), C = c(0.1, 0.2, 0.3, 0.4, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# Build svm model for cases per million
svm_model_cases = train(formula_cases, 
                        data=c_train, 
                        method = 'svmRadial', 
                        metric="Accuracy",
                        preProcess = c('center', 'scale'), 
                        trControl = tr_control, 
                        tuneLength = 10,
                        tuneGrid=tune_grid)
    
# Prediction for total cases per million
svm_pred_cases = predict(svm_model_cases, newdata=c_test)

# Create confusion matrix for total cases per million
t <- table(c_test$total_cases_per_million, svm_pred_cases)
result = confusionMatrix(t)
result # Show confusion matrix results
result[-1] # Show results divided into classes

# Build svm model for deaths per million
svm_model_deaths = train(formula_deaths, 
                        data=c_train, 
                        method = 'svmRadial', 
                        metric="Accuracy",
                        preProcess = c('center', 'scale'), 
                        trControl = tr_control, 
                        tuneLength = 10,
                        tuneGrid=tune_grid)

# Prediction for total cases per million
svm_pred_deaths = predict(svm_model_deaths, newdata=c_test)

# Create confusion matrix for total deaths per million
t <- table(c_test$total_deaths_per_million, svm_pred_deaths)
result = confusionMatrix(t)
result # Show confusion matrix results
result[-1] # Show results divided into classes

# Combine	the	predictions	with countries.
country = data.frame(c_test$location)
cases = data.frame(svm_pred_cases)
deaths = data.frame(svm_pred_deaths)
covid19_svm_pred = cbind(country, cases, deaths)

# Change column names
labels = c("country", "cases", "deaths")
colnames(covid19_svm_pred) = labels

# Write prediction dataframe to csv file
write.csv(covid19_svm_pred, 'svm_pred.csv')

## SVM Analysis

# The confusion matrix shows an Accuracy of 40-50% for both the infection and death rate which indicates that this model is better at predicting than the decision tree. The precision, recall, and F-measure are fluctuate quite a bit. From looking at the separate classes, it looks like the infection rate extremities are the easiest to predict with a balanced accuracy of 74% for very low rates and 66.6% for very high rates. For the death rate, the two highest measures are the easiest to predict with a balanced accuracy of 69.6% for high rates and 82.6% for very high rates.

# ------------------
# Random Forest (RF)
# ------------------

# The random forest algorithm creates random vectors to build multiple decision trees which are ultimately combined together. It is a type of ensemble learning algorithm.

# The tune grid hyper-parameter uses bagging to create a training model that replaces all the values in the full training sample set. It will randomly pick a small set of attributes to build each base model. It will then choose the square root of the original feature size and assign random values from the original dataset. Again the center and scale parameter is set to center and scale and the repeated cross validation control is used to tune the model. The rf method is used with the caret package.

# Set hyper-parameters for rf models
tune_grid = expand.grid(.mtry = c(sqrt(ncol(d_train))))

# Build rf model for cases per million
rf_model_cases = train(formula_deaths, 
                  data=c_train, 
                  method = 'rf', 
                  metric="Accuracy",
                  preProcess = c('center', 'scale'), 
                  trControl = tr_control, 
                  tuneLength = 10, 
                  tuneGrid = tune_grid)

# Prediction for for cases per million
rf_pred_cases = predict(rf_model_cases, c_test)

# Create confusion matrix for total cases
t = table(c_test$total_cases_per_million, rf_pred_cases)
rf_result = confusionMatrix(t)
rf_result
rf_result[-1] # Show results divided into classes

# Build rf model for deaths per million
rf_model_deaths = train(formula_deaths, 
                       data=c_train, 
                       method = 'rf', 
                       metric="Accuracy",
                       preProcess = c('center', 'scale'), 
                       trControl = tr_control, 
                       tuneLength = 10, 
                       tuneGrid = tune_grid)

# Prediction for cases per million
rf_pred_deaths = predict(rf_model_deaths, c_test)

# Create confusion matrix for total cases
t = table(c_test$total_cases_per_million, rf_pred_deaths)
rf_result = confusionMatrix(t)
rf_result
rf_result[-1] # Show results divided into classes

# Combine	the	predictions	with countries.
country = data.frame(c_test$location)
cases = data.frame(rf_pred_cases)
deaths = data.frame(rf_pred_deaths)
covid19_rf_pred = cbind(country, cases, deaths)

# Change column names
labels = c("country", "cases", "deaths")
colnames(covid19_rf_pred) = labels

# Write prediction dataframe to csv file
write.csv(covid19_rf_pred, 'random_forest_pred.csv')

## Random Forest Analysis

# The confusion matrix shows an Accuracy of around 30% which indicates that this is a poor model for predicting the labels. The precision and recall also have a value around 30%. From looking at the separate classes, it also appears that the extremities (very low and very high) are the easiest to predict for the infection rate and the two highest (high and very high) are the easiest to predict for the death rate.

# --------------------------
# K-Nearest Neighbors (K-NN)
# --------------------------

# K-NN analysis is instance based learning which stores the training examples without calculating anything during the training process. Classification and prediction are delayed until new examples are given.

# The tune grid hyperparameter is set to iterate between 1 and 25. Setting tune grid defines the data points (k). A higher value (more data points) for k is more likely to increase the likelihood for different classes to be included that shouldn't be included. The downside is an increase of bias and a decrease of variance. If the model is underfit the stability will be low and the bias will be high. A K-NN Overfitted model has a low bias and higher variance. 

# Set hyper-parameters for knn model
tune_grid = expand.grid(k=seq(1, 25, by=1))

# Create knn model for cases per million
knn_model_cases = train(formula_cases, 
                  data = c_train, 
                  method = 'knn', 
                  preProcess = c('center', 'scale'), 
                  trControl = tr_control, 
                  metric="Accuracy", 
                  tuneLength = 10, 
                  tuneGrid = tune_grid)
plot(knn_model_cases)

# Prediction for cases per million
knn_pred_cases = predict(knn_model_cases, c_test)
t <- table(c_test$total_cases_per_million, knn_pred_cases)
knn_result = confusionMatrix(t)
knn_result
knn_result[-1] # Show results divided into classes

# Create knn model for deaths per million
knn_model_deaths = train(formula_deaths, 
                        data = c_train, 
                        method = 'knn', 
                        preProcess = c('center', 'scale'), 
                        trControl = tr_control, 
                        metric="Accuracy", 
                        tuneLength = 10, 
                        tuneGrid = tune_grid)
plot(knn_model_deaths)

# Prediction for deaths per million
knn_pred_deaths = predict(knn_model_deaths, c_test)
t <- table(c_test$total_deaths_per_million, knn_pred_deaths)
knn_result = confusionMatrix(t)
knn_result
knn_result[-1] # Show results divided into classes

# Combine	the	predictions	with countries.
country = data.frame(c_test$location)
cases = data.frame(knn_pred_cases)
deaths = data.frame(knn_pred_deaths)
covid19_knn_pred = cbind(country, cases, deaths)

# Change column names
labels = c("country", "cases", "deaths")
colnames(covid19_knn_pred) = labels

# Write prediction dataframe to csv file
write.csv(covid19_knn_pred, 'knn_pred.csv')

## K-NN Analysis 

# For the infection rate, it looks like the accuracy of the model peaks around 19 neighboring points. For the death rate, the accuracy of the model peaks around 8 neightboring points. These values are used to optimize the model. The confusion matrix shows an Accuracy of between 40-50% which indicates that this model outperforms the decision tree and random forest models. The precision and recall have a range between 40-50% for both infection and death rate. From looking at the separate classes, the prediction fits the pattern shown from the previous models (more easily predictable extremities for infection rate and more easily predictable higher classifications for the death rate).

# Remove unused columns
c_train$iso_code = NULL
c_train$location = NULL

# Get informatino Gain for infection
infection_weight = information.gain(total_cases_per_million ~ . -total_deaths_per_million, c_train)
infection_weight

# Get informatino Gain for deaths
death_weight = information.gain(total_deaths_per_million ~ . -total_cases_per_million, c_train)
death_weight
