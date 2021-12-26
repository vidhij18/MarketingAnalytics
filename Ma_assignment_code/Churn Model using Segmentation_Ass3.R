library(RODBC)
library(dplyr)
library(ggplot2)
install.packages("devtools")
library(devtools)
install.packages("markovchain")


db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db,"USE ma_charity_full")

#we take last 7 years of data
query = "SELECT contact_id, act_date, amount, month(act_date) as donation_month, year(act_date) as donation_year, 
ifnull(prefix_id, 'MR') as prefix_id
FROM acts a, contacts c
WHERE a.contact_id = c.id
and (act_type_id LIKE 'DO') 
and year(act_date) >= '2013'
ORDER BY 1, 2;"

data = sqlQuery(db, query)
#print(head(data))

monthly_timing = c("2018-07-01","2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01",
                   "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01")
#final_churn_rate = data.frame(campaign_month = Date(), churners = integer(), actives = integer(), churn_rate = double())
churner_contacts_vector = vector()
active_contacts_vector = vector()
churn_rate = vector()

# Add headers and interpret the last column as a date, extract year of purchase
#colnames(data) = c('contact_id', 'amount', 'act_date')
data$act_date = as.Date(data$act_date, "%Y-%m-%d")
#data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))


print(monthly_timing)

for (month_date in monthly_timing){
  
  data$days_since = as.numeric(difftime(time1 = month_date,
                                        time2 = data$act_date,
                                        units = "days"))
  
  #print(head(data))
  # Invoke library to compute key marketing indicators using SQL language
  library(sqldf)
  # Segment customers in 2015
  customer_latest = sqldf("SELECT contact_id,
  MIN(days_since) AS 'recency',
  MAX(days_since) AS 'first_purchase',
  COUNT(*) AS 'frequency',
  AVG(amount) AS 'amount'
  FROM data GROUP BY 1")
  
  customer_latest$segment = "NA"
  #users before the last three years
  customer_latest$segment[which(customer_latest$recency > 365*3)] = "inactive"
  
  # between 2-3 years
  customer_latest$segment[which(customer_latest$recency <= 365*3 & customer_latest$recency > 365*2)] = "cold"
  #between 2 to 1 years
  customer_latest$segment[which(customer_latest$recency <= 365*2 & customer_latest$recency > 365*1)] = "warm"
  #active in the last one year
  customer_latest$segment[which(customer_latest$recency <= 365)] = "active"
  
  #first warm user
  customer_latest$segment[which(customer_latest$segment == "warm" & customer_latest$first_purchase <= 365*2)] = "new warm"
  #warm but low customer value
  customer_latest$segment[which(customer_latest$segment == "warm" & customer_latest$amount < 100)] = "warm low value"
  #warm but high customer value
  customer_latest$segment[which(customer_latest$segment == "warm" & customer_latest$amount >= 100)] = "warm high value"
  
  customer_latest$segment[which(customer_latest$segment == "active" & customer_latest$first_purchase <= 365)] = "new active"
  #newly active low customer value
  customer_latest$segment[which(customer_latest$segment == "active" & customer_latest$amount < 100)] = "active low value"
  #newly acitve high customer value
  customer_latest$segment[which(customer_latest$segment == "active" & customer_latest$amount >= 100)] = "active high value"
  customer_latest$segment = factor(x = customer_latest$segment, levels = c("inactive", "cold",
                                                                           "warm high value", "warm low value", "new warm",
                                                                           "active high value", "active low value", "new active"))
  
  # Segment customers in 2014
  customer_old = sqldf("SELECT contact_id,
  MIN(days_since) - 365 AS 'recency',
  MAX(days_since) - 365 AS 'first_purchase',
  COUNT(*) AS 'frequency',
  AVG(amount) AS 'amount'
  FROM data
  WHERE days_since > 365
  GROUP BY 1")
  customer_old$segment = "NA"
  customer_old$segment[which(customer_old$recency > 365*3)] = "inactive"
  customer_old$segment[which(customer_old$recency <= 365*3 & customer_old$recency > 365*2)] = "cold"
  customer_old$segment[which(customer_old$recency <= 365*2 & customer_old$recency > 365*1)] = "warm"
  customer_old$segment[which(customer_old$recency <= 365)] = "active"
  customer_old$segment[which(customer_old$segment == "warm" & customer_old$first_purchase <= 365*2)] = "new warm"
  customer_old$segment[which(customer_old$segment == "warm" & customer_old$amount < 100)] = "warm low value"
  customer_old$segment[which(customer_old$segment == "warm" & customer_old$amount >= 100)] = "warm high value"
  customer_old$segment[which(customer_old$segment == "active" & customer_old$first_purchase <= 365)] = "new active"
  customer_old$segment[which(customer_old$segment == "active" & customer_old$amount < 100)] = "active low value"
  customer_old$segment[which(customer_old$segment == "active" & customer_old$amount >= 100)] = "active high value"
  customer_old$segment = factor(x = customer_old$segment, levels = c("inactive", "cold",
                                                                     "warm high value", "warm low value", "new warm",
                                                                     "active high value", "active low value", "new active"))
  #print(head(customer_latest))
  #print(head(customer_old))
  
  # Compute transition matrix
  new_data = merge(x = customer_old, y = customer_latest, by = "contact_id", all.x = TRUE)
  #head(new_data)
  transition = table(new_data$segment.x, new_data$segment.y)
  print(month_date)
  print(transition)
  
  # Divide each row by its sum
  transition = transition / rowSums(transition)
  print(transition)
  
  
  #creating churners
  churner_contacts = filter(new_data,
                            (new_data$segment.x == "cold" & new_data$segment.y == "inactive" ) |
                            (new_data$segment.x == "active high value" & new_data$segment.y == "warm high value") |
                            (new_data$segment.x == "active low value" & new_data$segment.y == "warn low value") |
                            (new_data$segment.x == "new active" & new_data$segment.y == "new warm") |
                            (new_data$segment.x == "active high value" & new_data$segment.y == "active low value")
                            )
  #print(head(churner_contacts))
  #print(nrow(churner_contacts))
  
  #creating list of total active users
  active_contacts = filter(new_data, (new_data$segment.x != "inactive" & new_data$segment.y != "inactive" ))
  #print(head(active_contacts))
  #print(nrow(active_contacts))
  
  churner_contacts_vector <- c(churner_contacts_vector, nrow(churner_contacts))
  active_contacts_vector <- c(active_contacts_vector, nrow(active_contacts))
  churn_rate <- c(churn_rate, nrow(churner_contacts)/nrow(active_contacts))
  
  
  #creating possible churn rate -> WITHOUT ADDITION OF NEW USER FLUCTUATION
  #THE ASSUMPTION OF NEW USERS EVERY MONTH REMAINS THE SAME TO CALCULATE CHURN FROM PAST YEAR = 12 MONTHS PERFORMANCE
}




final_churn_rate = cbind("monthly_timing" = (monthly_timing), "churners" = as.numeric(churner_contacts_vector), 
                         "actives" = as.numeric(active_contacts_vector), "churn_rate" = as.double(churn_rate))
final_churn_rate = as.data.frame(final_churn_rate)
final_churn_rate[['churn_rate']] = as.numeric(final_churn_rate[['churners']])/ as.numeric(final_churn_rate[['actives']])
final_churn_rate2 = cbind("monthly_timing" = (monthly_timing), "churners" =as.numeric(active_contacts_vector)- as.numeric(churner_contacts_vector), 
                                  "actives" = as.numeric(active_contacts_vector), "churn_rate" = as.double(churn_rate),"churn?"="No")


final_df1 = cbind("monthly_timing" = (monthly_timing), "churners" = as.numeric(churner_contacts_vector), 
                         "actives" = as.numeric(active_contacts_vector),"churn?"="Yes")

final_df2 = cbind("monthly_timing" = (monthly_timing), "churners" =as.numeric(active_contacts_vector)- as.numeric(churner_contacts_vector), 
                          "actives" = as.numeric(active_contacts_vector),"churn?"="No")

final = rbind(final_df1,final_df2)
final=as.data.frame(final)

plot5= ggplot(final, aes(x=monthly_timing,y=actives,fill=churners))+geom_histogram(binwidth = 1) +labs(x = "Months",title = "Churn rate by tenure")


plt<-ggplot(final_churn_rate, aes(x=monthly_timing, y=churners)) + geom_bar(stat="identity",fill="cornflowerblue") + 
  ggtitle("Churn Rate by Month") + 
  labs(y = "$", x = "Year")
plt


#transition 1
install.packages("Gmisc")
library(Gmisc)
library(markovchain)

tm <- rbind(c(0.947464875,0,0,0,0,0.002443494,0.050091631,0),c(0.872132567,0,0,0,0,0.006739628,0.121127806,0),c(0,0.619217082,0,0,0,0.348754448,0.03202847,0),c(0,0.648071256,0,0,0,0.0024779,0.349450844,0),c(0,0.801993849,0,0,0,0.011347969,0.186658182,0),c(0,0,0.215195433,0,0,0.760649978,0.024154589,0),c(0,0,0,0.293530021,0,0.005348516,0.701121463,0),c(0,0,0,0,0.565683837,0.037726465,0.396589698,0))

states <- c("inactive", "cold", "warm high value","warm low value","new warm","active high value","active low value","new active")
mc <- new("markovchain", states=states, transitionMatrix=tm, name="X")
dimnames(tm) <- list(states, states)
mc@transitionMatrix <- tm
plot(mc)

#second graph

tm2 <- rbind(c(0.970660147,0,0,0,0,0.002013519,0.027326334,0),c(0.88546851,0,0,0,0,0.006205837,0.108325653,0),c(0,0.636363636,0,0,0,0.335664336,0.027972028,0),c(0,0.681713408,0,0,0,0.002241004,0.316045588,0),c(0,0.80763489,0,0,0,0.01172958,0.18063553,0),c(0,0,0.254576782,0,0,0.722808529,0.022614689,0),c(0,0,0,0.34153509,0,0.004598961,0.653865949,0),c(0,0,0,0,0.598344693,0.033288705,0.368366602,0))
dimnames(tm2) <- list(states, states)
mc@transitionMatrix <- tm2
plot(mc)

#third graph

tm2 <- rbind(c(0.973331445,0,0,0,0,0.002029642,0.024638912,0),c(0.887933153,0,0,0,0,0.006021135,0.106045711,0),c(0,0.642694064,0,0,0,0.332191781,0.025114155,0),c(0,0.690427438,0,0,0,0.002303558,0.307269004,0),c(0,0.808474214,0,0,0,0.012304729,0.179221057,0),c(0,0,0.264907765,0,0,0.712569713,0.022522523,0),c(0,0,0,0.35231,0,0.004417267,0.643272733,0),c(0,0,0,0,0.606966075,0.031570687,0.361463238,0))
dimnames(tm2) <- list(states, states)
mc@transitionMatrix <- tm2
plot(mc)

#fourth Graph
tm2 <- rbind(c(0.975720999,0,0,0,0,0.001796738,0.022482263,0),c(0.88715574,0,0,0,0,0.005666585,0.107177675,0),c(0,0.654066438,0,0,0,0.32187858,0.024054983,0),c(0,0.702711365,0,0,0,0.002307544,0.294981091,0),c(0,0.816143981,0,0,0,0.012393577,0.171462442,0),c(0,0,0.294054748,0,0,0.685414885,0.020530368,0),c(0,0,0,0.378754144,0,0.004123167,0.617122689,0),c(0,0,0,0,0.620117774,0.03034491,0.349537315,0))
dimnames(tm2) <- list(states, states)
mc@transitionMatrix <- tm2
plot(mc)

#fifth Graph
tm2 <- rbind(c(0.9982242,0,0,0,0,0.000032885,0.001742905,0),c(0.9923467,0,0,0,0,0.0001055632,0.007547767,0),c(0,0.9632035,0,0,0,0.03679654,0,0),c(0,0.968107,0,0,0,0.00006051803,0.03183249,0),c(0,0.9809436,0,0,0,0.00118281,0.01787357,0),c(0,0,0.9585206,0,0,0.04025337,0.001225991,0),c(0,0,0,0.9581118,0,0.0002514729,0.04163673,0),c(0,0,0,0,0.9633708,0.002214508,0.03441465,0))
dimnames(tm2) <- list(states, states)
mc@transitionMatrix <- tm2
plot(mc)




