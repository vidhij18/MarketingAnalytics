# == Clean ====================================================================
rm(list=ls())

# == Libraries ================================================================
# Load packages
library(RODBC)
library(ggplot2)
library(dplyr)
library(glmnet)
library(readr)


# == Data retrieving ==========================================================
# Connect to sql server
db = odbcConnect("mysql_server_64", uid="root", pwd="")

# Create table with campaign names and campaign codes
df_campaigns = data.frame(
  campaign_name = c(rep('March', 3), rep('April', 2), rep('May', 6), rep('June', 4), rep('August', 2),
                    rep('October', 2), rep('November', 8), rep('December_A', 2), rep('December_B', 2)),
  campaign_id = c('C164', 'C158', 'C177', 'C153', 'C173', 'C136', 'C137', 'C138', 'C159', 'C160', 'C161',
                  'C139', 'C141', 'C162', 'C163', 'C155', 'C174', 'C142', 'C165', 'C144', 'C145', 'C146',
                  'C175', 'C166', 'C167', 'C168', 'C179', 'C148', 'C172', 'C170', 'C181')
)

# Comments : We will train our models on a donor-level dataset with all donors targeted by one of the
# campaigns in our scope 
# We will then make a prediction for only the donors who made a donation between start of 2016 and end 2018
# considering all others are lost, with their metrics computed as of end of 2017

# Avg amount, max amount, loyalty, frequency (total up to LY)
# runtime : 156s
# methodology : scope table gives the contact list of targeted donor and for each we compute
# the avg donation amount up to last year
query = "
SELECT scope.campaign_id, scope.contact_id, scope.action_date,
AVG(IF(a.amount IS NULL, 0, a.amount)) as avg_don_amount_all_ly,
MAX(IF(a.amount IS NULL, 0, a.amount)) as max_don_amount_all_ly,
DATEDIFF(scope.action_date, MIN(a.act_date))/365 as loyalty,
DATEDIFF(scope.action_date, MIN(a.act_date))/COUNT(a.id) as frequency
FROM (
SELECT campaign_id, contact_id, action_date
FROM ma_charity_full.actions
WHERE campaign_id IN ('C164', 'C158', 'C177', 'C153', 'C173', 'C136', 'C137', 'C138', 'C159', 'C160', 'C161',
                  'C139', 'C141', 'C162', 'C163', 'C155', 'C174', 'C142', 'C165', 'C144', 'C145', 'C146',
                  'C175', 'C166', 'C167', 'C168', 'C179', 'C148', 'C172', 'C170', 'C181')
) as scope
LEFT JOIN
ma_charity_full.acts as a
ON 
	(scope.contact_id = a.contact_id) AND
    (YEAR(scope.action_date) > YEAR(a.act_date))
WHERE
	(a.act_type_id = 'DO' OR a.act_type_id IS NULL)
GROUP BY 1, 2, 3
ORDER BY 1, 2;
"

df_rfm_all_ly = sqlQuery(db, query)

# Same for prediction set
query ="
SELECT scope.contact_id,
AVG(IF(a.amount IS NULL, 0, a.amount)) as avg_don_amount_all_ly,
MAX(IF(a.amount IS NULL, 0, a.amount)) as max_don_amount_all_ly,
DATEDIFF(MAX(a.act_date), MIN(a.act_date))/365 as loyalty,
DATEDIFF(MAX(a.act_date), MIN(a.act_date))/COUNT(a.id) as frequency
FROM (
SELECT DISTINCT(contact_id)
FROM ma_charity_full.acts
WHERE 
	(act_type_id = 'DO') AND
    (YEAR(act_date) <= 2018) AND
    (YEAR(act_date) >= 2016)
) as scope
LEFT JOIN
ma_charity_full.acts as a
ON 
	(scope.contact_id = a.contact_id) 
WHERE
	(a.act_type_id = 'DO')
GROUP BY 1
ORDER BY 1;
"

testset_rfm_all_ly = sqlQuery(db, query)

# Recency
# runtime : 158s
query = "
SELECT scope.campaign_id, scope.contact_id, scope.action_date,
MIN(DATEDIFF(scope.action_date, a.act_date)) as recency
FROM (
  SELECT campaign_id, contact_id, action_date
  FROM ma_charity_full.actions
  WHERE campaign_id IN ('C164', 'C158', 'C177', 'C153', 'C173', 'C136', 'C137', 'C138', 'C159', 'C160', 'C161',
                        'C139', 'C141', 'C162', 'C163', 'C155', 'C174', 'C142', 'C165', 'C144', 'C145', 'C146',
                        'C175', 'C166', 'C167', 'C168', 'C179', 'C148', 'C172', 'C170', 'C181')
) as scope
LEFT JOIN
ma_charity_full.acts as a
ON 
(scope.contact_id = a.contact_id) AND
(scope.action_date > a.act_date)
WHERE
(a.act_type_id = 'DO')
GROUP BY 1, 2, 3
ORDER BY 1, 2;
"

df_recency = sqlQuery(db, query)

# Target variable (did the donor respond to campaign ?)
# runtime : 120s
# methodology : scope table gives the contact list of targeted donor and for each we look
# into the act table whether the donor made a donation
query = "
SELECT scope.campaign_id, scope.contact_id, scope.action_date,
IF(a.id IS NULL, 0, 1) as target, a.amount as amount
FROM (
SELECT campaign_id, contact_id, action_date
FROM ma_charity_full.actions
WHERE campaign_id IN ('C164', 'C158', 'C177', 'C153', 'C173', 'C136', 'C137', 'C138', 'C159', 'C160', 'C161',
                  'C139', 'C141', 'C162', 'C163', 'C155', 'C174', 'C142', 'C165', 'C144', 'C145', 'C146',
                  'C175', 'C166', 'C167', 'C168', 'C179', 'C148', 'C172', 'C170', 'C181')
) as scope
LEFT JOIN
ma_charity_full.acts as a
ON 
	(scope.contact_id = a.contact_id) AND
    (scope.campaign_id = a.campaign_id)
WHERE
	(a.act_type_id = 'DO' OR a.act_type_id IS NULL)
ORDER BY 1, 2;
"

df_target = sqlQuery(db, query)

# Response rate (LY)
# runtime : 237s
# methodology : sent : table with # message sent to each donor 
# response : table with # message responded to each donor
query = "
SELECT sent.campaign_id, sent.contact_id, sent.action_date,
response.nb_messages_responded_ly/sent.nb_messages_sent_ly as response_rate
FROM(
  SELECT scope.campaign_id, scope.contact_id, scope.action_date,
  COUNT(a.id) as nb_messages_sent_ly
  FROM (
    SELECT campaign_id, contact_id, action_date
    FROM ma_charity_full.actions
    WHERE campaign_id IN ('C164', 'C158', 'C177', 'C153', 'C173', 'C136', 'C137', 'C138', 'C159', 'C160', 'C161',
                          'C139', 'C141', 'C162', 'C163', 'C155', 'C174', 'C142', 'C165', 'C144', 'C145', 'C146',
                          'C175', 'C166', 'C167', 'C168', 'C179', 'C148', 'C172', 'C170', 'C181')
  ) as scope
  LEFT JOIN
  ma_charity_full.actions as a
  ON 
  (scope.contact_id = a.contact_id) AND
  (YEAR(scope.action_date) = YEAR(a.action_date) + 1)
  GROUP BY 1, 2, 3
) as sent
LEFT JOIN (
  SELECT scope.campaign_id, scope.contact_id, scope.action_date,
  COUNT(a.campaign_id) as nb_messages_responded_ly
  FROM (
    SELECT campaign_id, contact_id, action_date
    FROM ma_charity_full.actions
    WHERE campaign_id IN ('C164', 'C158', 'C177', 'C153', 'C173', 'C136', 'C137', 'C138', 'C159', 'C160', 'C161',
                          'C139', 'C141', 'C162', 'C163', 'C155', 'C174', 'C142', 'C165', 'C144', 'C145', 'C146',
                          'C175', 'C166', 'C167', 'C168', 'C179', 'C148', 'C172', 'C170', 'C181')
  ) as scope
  LEFT JOIN
  ma_charity_full.acts as a
  ON 
  (scope.contact_id = a.contact_id) AND
  (YEAR(scope.action_date) = YEAR(a.act_date) + 1)
  WHERE
  (a.act_type_id = 'DO' OR a.act_type_id IS NULL)
  GROUP BY 1, 2, 3
) as response
ON
(sent.campaign_id = response.campaign_id) AND
(sent.contact_id = response.contact_id)
ORDER BY 1, 2;
"

df_response_rate_ly = sqlQuery(db, query)

# Same for prediction set (response rate compute for 2016 and 2017 joinly)
query = "
SELECT sent.contact_id,
response.nb_messages_responded_ly/sent.nb_messages_sent_ly as response_rate
FROM(
SELECT scope.contact_id,
COUNT(a.id) as nb_messages_sent_ly
FROM (
SELECT DISTINCT(contact_id)
FROM ma_charity_full.acts
WHERE 
	(act_type_id = 'DO') AND
    (YEAR(act_date) <= 2018) AND
    (YEAR(act_date) >= 2016)
) as scope
LEFT JOIN
ma_charity_full.actions as a
ON 
	(scope.contact_id = a.contact_id)
WHERE
	(YEAR(a.action_date) = 2016 OR YEAR(a.action_date) = 2017)
GROUP BY 1
) as sent
LEFT JOIN (
SELECT scope.contact_id, COUNT(IF(a.campaign_id IS NULL, 0, 1)) as nb_messages_responded_ly
FROM (
SELECT DISTINCT(contact_id)
FROM ma_charity_full.acts
WHERE 
	(act_type_id = 'DO') AND
    (YEAR(act_date) <= 2018) AND
    (YEAR(act_date) >= 2016)
) as scope
LEFT JOIN
ma_charity_full.acts as a
ON 
	(scope.contact_id = a.contact_id)
WHERE
	(a.act_type_id = 'DO' OR a.act_type_id IS NULL) AND
    (YEAR(a.act_date) = 2016 OR YEAR(a.act_date) = 2017)
GROUP BY 1
) as response
ON
    (sent.contact_id = response.contact_id)
ORDER BY 1;
"
testset_response_rate_ly = sqlQuery(db, query)

# == Data processing (training set) ==========================================================
# Check NA values
any(is.na.data.frame(df_rfm_all_ly))
summary(df_rfm_all_ly) # NA values for 'loyalty' and 'frequency' columns -> due to new donors
df_rfm_all_ly_woNA = df_rfm_all_ly[!is.na(df_rfm_all_ly$loyalty), ] # drop NA rows
any(is.na.data.frame(df_rfm_all_ly_woNA))

any(is.na.data.frame(df_recency))

any(is.na.data.frame(df_target)) # NA values in 'amount' column for donors who didn't respond to campaign

any(is.na.data.frame(df_response_rate_ly))
summary(df_response_rate_ly) # NA values for 'response_rate' column -> due to new donors
df_response_rate_ly_woNA = df_response_rate_ly[!is.na(df_response_rate_ly$response_rate), ] # drop NA rows
any(is.na.data.frame(df_response_rate_ly_woNA))

# Join all datasets (perform inner joins to avoid NA values)
df_marketing = df_campaigns %>% 
  inner_join(df_rfm_all_ly_woNA, by = c('campaign_id'='campaign_id')) %>% 
  inner_join(df_recency, by = c('campaign_id'='campaign_id', 'contact_id'='contact_id')) %>% 
  inner_join(df_target, by = c('campaign_id'='campaign_id', 'contact_id'='contact_id')) %>% 
  inner_join(df_response_rate_ly_woNA, by = c('campaign_id'='campaign_id', 'contact_id'='contact_id'))

# Overview of our dataset
head(df_marketing)
str(df_marketing)
summary(df_marketing) # around 10% of rows with target variable to 1 

# Drop 'action_date.x', 'action_date.y' columns
df_marketing$action_date.x = NULL
df_marketing$action_date.y = NULL
df_marketing$action_date.x.x = NULL
df_marketing$action_date.y.y = NULL

# Check NA values (disregarding the 'amount' column)
any(is.na.data.frame(df_marketing[, !(names(df_marketing) %in% c('amount'))]))


# == Data processing (test set) ==========================================================
# Check NA values
any(is.na.data.frame(testset_rfm_all_ly))

any(is.na.data.frame(testset_response_rate_ly)) 
# NA values for donors who made a donation without responding to any solicitaation
testset_response_rate_ly_woNA = testset_response_rate_ly[!is.na(testset_response_rate_ly$response_rate), ]

# Join all datasets (perform inner joins to avoid NA values)
testset = testset_rfm_all_ly %>% 
  inner_join(testset_response_rate_ly_woNA)

# Overview of our dataset
head(testset)
str(testset)
summary(testset) # around 10% of rows with target variable to 1 

# ================================================================================= #
#                                                                                   #
#             MODEL 1 : Is the donor going to respond to the campaign ?             #
#                                                                                   #
# ================================================================================= #

# == Start with March model =======================================================
# Feature engineering
df_marketing = df_marketing %>% 
  mutate(log_avg_don_amount_all_ly = log(avg_don_amount_all_ly),
         log_max_don_amount_all_ly = log(max_don_amount_all_ly),
         log_frequency = log(frequency),
         log_loyalty = log(loyalty),
         log_recency = log(recency),
         freq_recency = frequency*recency)

# Same on testset
testset = testset %>% 
  mutate(log_avg_don_amount_all_ly = log(avg_don_amount_all_ly),
         log_max_don_amount_all_ly = log(max_don_amount_all_ly),
         log_frequency = log(frequency),
         log_loyalty = log(loyalty),
         recency = 0,
         log_recency = 0,
         freq_recency = 0)

# Select March dataset training
df_marketing_Mar = df_marketing[df_marketing$campaign_name == 'March', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Mar[, predictors])
y = df_marketing_Mar$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 5 predictors (model we retain)
# also quite convinient the lambda.1se model didn't pick the 'recency' variable since we don't know yet
# thevalues for this variable for future campaigns we will lauch in 2019

# Predictions
prediction_lasso_Mar = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                           type = "response",
                           s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Mar) = "March"
rownames(prediction_lasso_Mar) = testset$contact_id


# == April model =======================================================
# Select March dataset training
df_marketing_Apr = df_marketing[df_marketing$campaign_name == 'April', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Apr[, predictors])
y = df_marketing_Apr$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 4 predictors (model we retain)
# we will disregard log_recency coef by setting all variable values to 0

# Predictions
prediction_lasso_Apr = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                           type = "response",
                           s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Apr) = "April"
rownames(prediction_lasso_Apr) = testset$contact_id

# == May model =======================================================
# Select March dataset training
df_marketing_May = df_marketing[df_marketing$campaign_name == 'May', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_May[, predictors])
y = df_marketing_May$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 4 predictors (model we retain)
# we will disregard log_recency coef by setting all variable values to 0

# Predictions
prediction_lasso_May = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                               type = "response",
                               s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_May) = "May"
rownames(prediction_lasso_May) = testset$contact_id

# == June model =======================================================
# Select March dataset training
df_marketing_Jun = df_marketing[df_marketing$campaign_name == 'June', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Jun[, predictors])
y = df_marketing_Jun$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 5 predictors (model we retain)
# log_recency is an important coef, we will use log_frequency as a proxy (dodgy asumption but not many choices)

testset_bis = testset %>% 
  mutate(log_recency = log_frequency)

# Predictions
prediction_lasso_Jun = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset_bis[, predictors]), 
                               type = "response",
                               s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Jun) = "June"
rownames(prediction_lasso_Jun) = testset$contact_id

# == August model =======================================================
# Select March dataset training
df_marketing_Aug = df_marketing[df_marketing$campaign_name == 'August', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Aug[, predictors])
y = df_marketing_Aug$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 4 predictors (model we retain)
# we will disregard log_recency coef by setting all variable values to 0

# Predictions
prediction_lasso_Aug = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                               type = "response",
                               s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Aug) = "August"
rownames(prediction_lasso_Aug) = testset$contact_id

# == October model =======================================================
# Select March dataset training
df_marketing_Oct = df_marketing[df_marketing$campaign_name == 'October', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Oct[, predictors])
y = df_marketing_Oct$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 4 predictors (model we retain)
# log_recency is an important coef, we will use log_frequency as a proxy (dodgy asumption but not many choices)

testset_bis = testset %>% 
  mutate(log_recency = log_frequency)

# Predictions
prediction_lasso_Oct = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset_bis[, predictors]), 
                               type = "response",
                               s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Oct) = "October"
rownames(prediction_lasso_Oct) = testset$contact_id


# == November model =======================================================
# Select March dataset training
df_marketing_Nov = df_marketing[df_marketing$campaign_name == 'November', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Nov[, predictors])
y = df_marketing_Nov$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 4 predictors (model we retain)
# log_recency is an important coef, we will use log_frequency as a proxy (dodgy asumption but not many choices)
# freq_recency not an important coef, we'll set it at 0

testset_bis = testset %>% 
  mutate(log_recency = log_frequency)

# Predictions
prediction_lasso_Nov = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset_bis[, predictors]), 
                               type = "response",
                               s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Nov) = "November"
rownames(prediction_lasso_Nov) = testset$contact_id

# == December_A model =======================================================
# Select March dataset training
df_marketing_Dec_A = df_marketing[df_marketing$campaign_name == 'December_A', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Dec_A[, predictors])
y = df_marketing_Dec_A$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 4 predictors (model we retain)
# log_recency is an important coef, we will use log_frequency as a proxy (dodgy asumption but not many choices)
# freq_recency not an important coef, we'll set it at 0

testset_bis = testset %>% 
  mutate(log_recency = log_frequency)

# Predictions
prediction_lasso_Dec_A = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset_bis[, predictors]), 
                               type = "response",
                               s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Dec_A) = "December_A"
rownames(prediction_lasso_Dec_A) = testset$contact_id


# == December_B model =======================================================
# Select March dataset training
df_marketing_Dec_B = df_marketing[df_marketing$campaign_name == 'December_B', ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_Dec_B[, predictors])
y = df_marketing_Dec_B$target

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors 
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # select 4 predictors (model we retain)
# log_recency is an important coef, we will use log_frequency as a proxy (dodgy asumption but not many choices)
# freq_recency not an important coef, we'll set it at 0

testset_bis = testset %>% 
  mutate(log_recency = log_frequency)

# Predictions
prediction_lasso_Dec_B = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset_bis[, predictors]), 
                                 type = "response",
                                 s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Dec_B) = "December_B"
rownames(prediction_lasso_Dec_B) = testset$contact_id


# == Merge all probabilities in a central matrix ================================================
testset_results = cbind(prediction_lasso_Mar, prediction_lasso_Apr, prediction_lasso_May, 
                        prediction_lasso_Jun, prediction_lasso_Aug, prediction_lasso_Oct, 
                        prediction_lasso_Nov, prediction_lasso_Dec_A, prediction_lasso_Dec_B)

# Plot results as a heatmap
heatmap(testset_results, Colv = NA, Rowv = NA, scale="column")
heatmap(testset_results, Colv = NA, Rowv = NA)
 
# Sample testresults matrix for cleaner heatmap
sub_testset_results = testset_results[sample(rownames(testset_results), 100), ]
heatmap(sub_testset_results, Colv = NA, Rowv = NA, scale="column")
heatmap(sub_testset_results, Colv = NA, Rowv = NA)

write.table(testset_results, file = "response_probabilities_predictions.csv")












# ================================================================================= #
#                                                                                   #
#                 MODEL 2 : How much is the donor going to donate                   #
#                                                                                   #
# ================================================================================= #

# == Start with March model =======================================================
# Select March dataset training
df_marketing_donAmount_Mar = df_marketing[(df_marketing$campaign_name == 'March') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Mar[, predictors])
y = df_marketing_donAmount_Mar$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep 2 predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain no predictors

# Predictions
prediction_lasso_Mar_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                               type = "response",
                               s = cv.fit_lasso$lambda.min)

colnames(prediction_lasso_Mar_amount) = "March"
rownames(prediction_lasso_Mar_amount) = testset$contact_id


# == April model ==============================================================
# Select April dataset training
df_marketing_donAmount_Apr = df_marketing[(df_marketing$campaign_name == 'April') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Apr[, predictors])
y = df_marketing_donAmount_Apr$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep 2 predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain no predictors

# Predictions
prediction_lasso_Apr_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                      type = "response",
                                      s = cv.fit_lasso$lambda.min)

colnames(prediction_lasso_Apr_amount) = "April"
rownames(prediction_lasso_Apr_amount) = testset$contact_id


# == May model ==============================================================
# Select May dataset training
df_marketing_donAmount_May = df_marketing[(df_marketing$campaign_name == 'May') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_May[, predictors])
y = df_marketing_donAmount_May$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep 3 predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain no predictors

# Predictions
prediction_lasso_May_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                      type = "response",
                                      s = cv.fit_lasso$lambda.min)

colnames(prediction_lasso_May_amount) = "May"
rownames(prediction_lasso_May_amount) = testset$contact_id


# == June model ==============================================================
# Select JUne dataset training
df_marketing_donAmount_Jun = df_marketing[(df_marketing$campaign_name == 'June') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Jun[, predictors])
y = df_marketing_donAmount_Jun$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain 1 predictors

# Predictions
prediction_lasso_Jun_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                      type = "response",
                                      s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Jun_amount) = "June"
rownames(prediction_lasso_Jun_amount) = testset$contact_id


# == August model ==============================================================
# Select August dataset training
df_marketing_donAmount_Aug = df_marketing[(df_marketing$campaign_name == 'August') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Aug[, predictors])
y = df_marketing_donAmount_Aug$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain 1 predictors

# Predictions
prediction_lasso_Aug_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                      type = "response",
                                      s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Aug_amount) = "August"
rownames(prediction_lasso_Aug_amount) = testset$contact_id


# == October model ==============================================================
# Select October dataset training
df_marketing_donAmount_Oct = df_marketing[(df_marketing$campaign_name == 'October') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Oct[, predictors])
y = df_marketing_donAmount_Oct$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep 3 predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain 1 predictors

# Predictions
prediction_lasso_Oct_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                      type = "response",
                                      s = cv.fit_lasso$lambda.min)

colnames(prediction_lasso_Oct_amount) = "October"
rownames(prediction_lasso_Oct_amount) = testset$contact_id


# == November model ==============================================================
# Select November dataset training
df_marketing_donAmount_Nov = df_marketing[(df_marketing$campaign_name == 'November') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Nov[, predictors])
y = df_marketing_donAmount_Nov$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep 4 predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain 2 predictors

# Predictions
prediction_lasso_Nov_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                      type = "response",
                                      s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Nov_amount) = "November"
rownames(prediction_lasso_Nov_amount) = testset$contact_id


# == December_A model ==============================================================
# Select December_A dataset training
df_marketing_donAmount_Dec_A = df_marketing[(df_marketing$campaign_name == 'December_A') & 
                                            (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Dec_A[, predictors])
y = df_marketing_donAmount_Dec_A$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep 2 predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain no predictors

# Predictions
prediction_lasso_Dec_A_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                      type = "response",
                                      s = cv.fit_lasso$lambda.min)

colnames(prediction_lasso_Dec_A_amount) = "December_A"
rownames(prediction_lasso_Dec_A_amount) = testset$contact_id


# == December_B model ==============================================================
# Select December_B dataset training
df_marketing_donAmount_Dec_B = df_marketing[(df_marketing$campaign_name == 'December_B') & 
                                              (df_marketing$target == 1), ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
predictors = c("avg_don_amount_all_ly", "max_don_amount_all_ly", "loyalty", "frequency", "response_rate",
               "log_avg_don_amount_all_ly", "log_max_don_amount_all_ly", "log_frequency", "log_loyalty",
               "log_recency", "freq_recency")
x = model.matrix(~ ., df_marketing_donAmount_Dec_B[, predictors])
y = df_marketing_donAmount_Dec_B$amount

# Lasso Logistic Regression
set.seed(42)
cv.fit_lasso = cv.glmnet(x, y, family = 'gaussian', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) # keep all predictors
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.1se) # retain 2 predictors

# Predictions
prediction_lasso_Dec_B_amount = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testset[, predictors]), 
                                        type = "response",
                                        s = cv.fit_lasso$lambda.1se)

colnames(prediction_lasso_Dec_B_amount) = "December_B"
rownames(prediction_lasso_Dec_B_amount) = testset$contact_id


# == Merge all predicted amount in a central matrix =============================================
testset_amount_results = cbind(prediction_lasso_Mar_amount, prediction_lasso_Apr_amount, 
                               prediction_lasso_May_amount, prediction_lasso_Jun_amount, 
                               prediction_lasso_Aug_amount, prediction_lasso_Oct_amount, 
                               prediction_lasso_Nov_amount, prediction_lasso_Dec_A_amount, 
                               prediction_lasso_Dec_B_amount)

# Expected donation amount 
testset_expected_results = cbind(testset_results, testset_amount_results)
testset_expected_results_df = as.data.frame(testset_expected_results)

# rename columns to distinguish probabilities and amount
names(testset_expected_results_df) = c("March", "April", "May", "June", "August", "October", "November",
                                       "December_A", "December_B", "March.1", "April.1", "May.1", "June.1",
                                       "August.1", "October.1", "November.1", "December_A.1", "December_B.1")

# Compute expected donation amount
testset_expected_results_df = testset_expected_results_df %>% 
  mutate(March = March*March.1,
         April = April*April.1,
         May = May*May.1,
         June = June*June.1,
         August = August*August.1,
         October = October*October.1,
         November = November*November.1,
         December_A = December_A*December_A.1,
         December_B = December_B*December_B.1,)

# Compute number of donors we should target
testset_expected_results_df = testset_expected_results_df %>% 
  mutate(March_count = if_else(March>0.85, 1, 0),
         April_count = if_else(April>0.85, 1, 0),
         May_count = if_else(May>0.85, 1, 0),
         June_count = if_else(June>0.85, 1, 0),
         August_count = if_else(August>0.85, 1, 0),
         October_count = if_else(October>0.85, 1, 0),
         November_count = if_else(November>0.85, 1, 0),
         December_A_count = if_else(December_A>0.85, 1, 0),
         December_B_count = if_else(December_B>0.85, 1, 0))

testset_expected_results_df = testset_expected_results_df %>% 
  mutate(March_amount_targeted = March_count*March,
         April_amount_targeted = April_count*April,
         May_amount_targeted = May_count*May,
         June_amount_targeted = June_count*June,
         August_amount_targeted = August_count*August,
         October_amount_targeted = October_count*October,
         November_amount_targeted = November_count*November,
         December_A_amount_targeted = December_A_count*December_A,
         December_B_amount_targeted = December_B_count*December_B)

 final_result = testset_expected_results_df %>% 
  select(March_count, March_amount_targeted, April_count, April_amount_targeted, 
         May_count, May_amount_targeted, June_count, June_amount_targeted, August_count,
         August_amount_targeted, October_count, October_amount_targeted, November_count, 
         November_amount_targeted, December_A_count, December_A_amount_targeted, December_B_count,
         December_B_amount_targeted)%>% 
  summarise_all(~ sum(.x, na.rm = TRUE))
 
# Show as matrix
final_result_matrix = t(data.matrix(final_result))




# Plot results as a heatmap
heatmap(testset_amount_results, Colv = NA, Rowv = NA, scale="column")
heatmap(testset_amount_results, Colv = NA, Rowv = NA)

# Sample testresults matrix for cleaner heatmap
sub_testset_results = testset_amount_results[sample(rownames(testset_amount_results), 100), ]
heatmap(sub_testset_results, Colv = NA, Rowv = NA, scale="column")
heatmap(sub_testset_results, Colv = NA, Rowv = NA)

write.table(testset_amount_results, file = "response_probabilities_predictions.csv")














