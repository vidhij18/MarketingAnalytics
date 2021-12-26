library(dplyr)
#import outcome of the two models
response <- read.csv("C:/Users/Chiara/OneDrive/Desktop/MSc DSBA/2 year T1/Strategic Marketing/Assignment 3/response_probabilities_predictions.csv", row.names=1, sep="")
colSums(is.na(response))

clt <- read.delim("C:/Users/Chiara/Downloads/Final_CLT_10year.txt")

rownames(clt) = clt$Id
#merge the two tables by row name
data = response %>% merge(clt, by=0)

rownames(data) = data$Row.names

data = data %>% select(-c("Row.names", "Id", "period.first",
                          "period.last", "period.length",
                          "DERT", "PAlive"))

data = na.omit(data)
colSums(is.na(data))

#_______________________________________________________________________
#K-MEANs with long term variables and campaigns as description

data1 = data %>% select(c("CET", "predicted.mean.spending", "predicted.CLV"))

k = kmeans(x = scale(data1), centers = 4, nstart = 50)

# Print cluster size, standardized centers, and
# un-standardized centers
print(k$size)
print(k$centers)

data = data %>% mutate(cluster = k$cluster)

description = data %>%
  group_by(cluster) %>%
  summarise(march = mean(March), 
            april = mean(April),
            may = mean(May),
            june = mean(June),
            august = mean(August),
            october = mean(October),
            november = mean(November),
            december_A = mean(December_A),
            december_B = mean(December_B),
            min_CET = min(CET),
            mean_CET = mean(CET),
            max_CET = max(CET),
            min_spending = min(predicted.mean.spending),
            mean_spending = mean(predicted.mean.spending),
            max_spending = max(predicted.mean.spending),
            min_CLV = min(predicted.CLV),
            mean_CLV = mean(predicted.CLV),
            max_CLV = max(predicted.CLV)
            )

#______________________________________________________
#PLOT SOLICITATIONS
solicitations <- description %>% select(-c("min_CET","mean_CET", "max_CET",
                                   "min_spending", "mean_spending","max_spending",
                                   "min_CLV", "mean_CLV", "max_CLV"))

solicitations = t(solicitations)

colnames(solicitations) = c("Segment 1", "Segment 2", "Segment 3", "Segment 4")

solicitations = as.data.frame(solicitations)

solicitations = solicitations[-1,]
solicitations$month = rownames(solicitations)

solicitations$month= factor(solicitations$month, 
                            levels=c("march", "april", "may",
                                     "june", "august", "october",
                                     "november", "december_A", "december_B"))

ggplot(solicitations, aes(x = month, y=`Segment 1`))+
  geom_line(aes(color = "Persuadables"),group = 1,size =1.5)+
  geom_line(aes(y=`Segment 2`, color = "Low Maintenance"),group = 1,size =1.5)+
  geom_line(aes(y=`Segment 3`,color = "Super Donors"),group = 1,size =1.5)+
  geom_line(aes(y=`Segment 4`,color = "Lost Causes"), group = 1,size =1.5)+
  theme_classic()+
  scale_y_continuous(labels = scales::percent)+
  scale_color_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle=90))+
  labs(x = "Campaigns", y="Probability of donation", color ="Segments")
  
