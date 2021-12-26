install.packages("CLVTools")
install.packages("gapminder")

# Load the package
library(RODBC)
library(CLVTools)

library(ggplot2)
library(gapminder)


db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db,"USE ma_charity_full")

# Close the connection
odbcClose(db)

query = "SELECT contact_id, act_date, amount
         FROM acts
         WHERE (act_type_id LIKE 'DO')
         AND (act_date > 19900101)
         ORDER BY 1, 2;"
data = sqlQuery(db, query)
print(head(data))


new.data = clvdata(data.transactions = data,
                   date.format = "ymd",
                   time.unit   = "year",
                   name.id     = "contact_id",
                   name.date   = "act_date",
                   name.price  = "amount")

# Estimate model
est.pnbd = pnbd(clv.data = new.data)

# Report model parameters, confidence intervals
print(summary(est.pnbd))
print(coef(est.pnbd))
print(confint(est.pnbd))
print(plot(est.pnbd))


# Predict CLV over the next 10 year
results = predict(object = est.pnbd, prediction.end = 10)
print(results)

head(results$predicted.mean.spending)
max(results$predicted.mean.spending)
min(results$predicted.mean.spending)
mean(results$predicted.mean.spending)
set.seed(123)

#Normalise the data

boxplot(results$predicted.CLV)
Q <- quantile(results$predicted.mean.spending, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(results$predicted.mean.spending)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
eliminated<- subset(results, results$predicted.mean.spending > (Q[1] - 1.5*iqr) & results$predicted.mean.spending < (Q[2]+1.5*iqr))

#Plot of estimated mean donations
plot1= ggplot(eliminated, aes(x=eliminated$predicted.mean.spending))+ geom_histogram(color="darkblue",fill="darkblue")

plot1+labs(x = "Predicted Mean Donations")+ggtitle("Expected mean donations")+scale_y_continuous(name="Number of Donors", labels = scales::comma)

#Plot of estimated CLV
plot3= ggplot(eliminated, aes(x=eliminated$predicted.CLV))+geom_histogram(color="darkblue", fill="darkblue",binwidth = 100)

plot3+labs(x = "CLV")+ggtitle("Customer Lifetime Value")+scale_y_continuous(name="Number of Donars", labels = scales::comma)
plot3

final2 = data.frame(data$contact_id,results)

final = data.frame(results)
write.table(final, file = 'Final_CLT_10year.txt', sep="\t")
