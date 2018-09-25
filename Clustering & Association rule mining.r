#Set working directory
getwd()
setwd("H:/")

#Association

#Support- It says how popular an itemset is, as measured by the proportion of transactions in which an
#itemset appears. 

#Confidence- It says how likely item Y is purchased when item X is purchased, expressed as {X -> Y}. 
#This is measured by the proportion of transactions with item X, in which item Y also appears.

#Lift- It says how likely item Y is purchased when item X is purchased, while controlling for how
#popular item Y is.


#Load library for association rule mining
library(arules)

#load data
assoc<-read.csv("food_4_association.csv")

#we dont need transaction id for modeling.
assoc<-assoc[,c(-1)]

##To load transactional data
assoc<-as(as.matrix(assoc), "transactions")

#Explore data
head(assoc[,1:6])
dim(assoc)
summary(assoc)

#Interpreting summary

#The density value of 0.02230729 (2.2 percent) refers to the proportion of nonzero matrix cells
#Since there are 19076 * 118 = 2250968 positions in the matrix, we can calculate that a total of 
#2250968 * 0.02230729 = 50212.996 items were purchased.

#most frequent items shows: items that were most commonly found in the transactional data. Since
#3166 / 118= 26.83, we can determine that bottled water appeared in 26.8 percent of the transactions.

#A total of 5178 transactions contained only a single item, while 1 transaction had 15 items. The first quartile and 
#median purchase sizes are 1 and 2 items, respectively, implying that 25 percent of the transactions
#contained 1 or less items. The mean of 2.632 items per transaction took place.

#Look at contents of sparse matrix
inspect(assoc[1:5])

#To see the proportion of transactions that contain the item.
itemFrequency(assoc[,1:5])

#It tells the proportion of transactions that contain the item.(Also known as Support)

#Visualizing item support - item frequency plots

itemFrequencyPlot(assoc,support=0.1)
# Here, Support means items that occurs 10% of transactions.
# We see that Bottled food,ice cream, drink food, cheese food etc. appeared in 10% of transactions.
#support = 0.1 means an item must have appeared in at least 0.1 * 19076 =1907 transactions.

itemFrequencyPlot(assoc, topN = 20)
#It shows top 20 items that are in transactions.

#Visualizing the transaction data - plotting the sparse matrix

image(assoc[1:5])
#Cells in the matrix are filled with black for transactions (rows) where the item (column) was purchased.
#We can see that 1st transaction contains 4 items, 2nd-3 items and so on.

image(sample(assoc, 20)) # for random 20 transactions

#Fit model
rules <- apriori(assoc, parameter = list(support =0.005, confidence = 0.25, minlen = 2))
#minlen = 2 to eliminate rules that contain fewer than two items.
#confidence threshold of 0.25 means that in order to be included in the results, the rule has to be 
#correct at least 25 percent of the time.

rules
#We got 104 rules.

summary(rules)
# We see that 47 rules contains 2 items and 57 rules contain 3 items.
#lift of a rule measures how much more likely one item or itemset is purchased relative to its typical 
#rate of purchase, given that we know another item or itemset has been purchased.

#if lift is greater than one, it implies that the two items are found together more often than one would 
#expect by chance. A large lift value is therefore a strong indicator that a rule is important, and 
#reflects a true connectionbetween the items.

#Lets look at first 3 rules
inspect(rules[1:3])

#1st rule tells us that this rule this rule covers 0.7 percent of the transactions and is correct
#in 70 percent of purchases involving float food. The lift value tells us how much more likely a 
#customer is to buy whole Ice cream conefood relative to the average customer, given
#that he or she bought a float food.

#Improving model performance

#Sorting the set of association rules
inspect(sort(rules, by = "lift")[1:5]) 

# Top 5 rules
# We see that people who buy hot dog food are nearly 21 times more likely to buy side of cheese food 
#than the typical customer

#subset() function provides a method to search for subsets of transactions, items, or rules.
hot_dog_rules <- subset(rules, items %in% "Hot.DogFood")

inspect(hot_dog_rules)
# we can see that hot dog food is purchased frequently with side of cheese food, cheese coney food,
#french fries basketfood and medium drinkfood.

#rules containg hot dog food on left hand side
hot_dog_rules_lhs <- subset(rules, lhs %in% "Hot.DogFood") 
hot_dog_rules_lhs

#rules containg hot dog food on right hand side
hot_dog_rules_rhs <- subset(rules, rhs %in% "Hot.DogFood") 
hot_dog_rules_rhs

#rules matching either hot dog food or side of cheese food
hot_dog_cheese_food_rules<- subset(rules, items %in% c("Hot.DogFood","Side.of.CheeseFood"))
hot_dog_cheese_food_rules

#Partial matching allows us to find any item containing Chicken
chicken_rules<- subset(rules, items %pin% "Chicken")
chicken_rules

#rules having size >2
size<-subset(rules,size(rules)>2)
head(inspect(size))

#rules having life >15
rule_lift<-subset(rules, lift>15)
inspect(rule_lift)

#rules having confidence>0.50
rule_conf<-subset(rules, confidence>0.50)
rule_conf
head(inspect(rule_conf))

#rules having lift>5 and item containing Hot Dog Food
rule_dog<-subset(rules,items %in% "Hot.DogFood" & lift>5)
rule_dog
inspect(rule_dog)

#Saving association rules to a data frame

rules_dataframe <- as(rules, "data.frame")
head(rules_dataframe)
dim(rules_dataframe)
str(rules_dataframe)
summary(rules_dataframe)

#Saving association rules to a file
write.csv(rules_dataframe, file = "rules.csv")

#Clustering - kmeans

#Load data
clustering1<-read.csv("qry_Food_by_Month.csv")
clustering<-clustering1

#we dont need nickname therefore removing it. k-means requires numeric variables.
clustering<-clustering[,2:7]

#Rename variables
names(clustering)<-c("Oct_10","Nov_10","Dec_10","Jan_11","Feb_11","Mar_11")

#structure of data
str(clustering)

#load library

library(ggplot2)

#Explore data
head(clustering)
dim(clustering)
summary(clustering)

#Check for missing values
colSums(is.na(clustering))

#Data analysis

library(tidyr)

# Histogram for each Attribute
clustering%>%
  gather(Attributes, value, 1:6) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill="lightblue2", colour="black") +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency") 


gather(clustering,Attributes,value,1:6)

# Correlation matrix 
library(corrplot)
corrplot(cor(clustering), type="upper", method="ellipse", tl.cex=0.9)
corrplot(cor(clustering), type="upper", method="number", tl.cex=0.9)

# We see that transactions in Oct_10 has high correlation with Nov_10 and Mar_11.
#Feb_11 has high correlation with Mar_11
#Dec_10 has high correlation with Jan_11
#Nov_10 has high correlation with Feb_11 and Mar_11.

# Normalization
norm_data <- as.data.frame(scale(clustering))

# Original data
p1 <- ggplot(clustering, aes(x=Oct_10, y=Mar_11)) +
  geom_point() +
  labs(title="Original data")

p1

# Normalized data 
p2 <- ggplot(norm_data, aes(x=Oct_10, y=Mar_11)) +
  geom_point() +
  labs(title="Normalized data")

# Subplot
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

#k-means with k=3

set.seed(1234)
model <- kmeans(norm_data, centers=3)
model
# Cluster to which each point is allocated
model$cluster
# Cluster centers
model$centers
# Cluster size
model$size
# Between-cluster sum of squares
model$betweenss
# Within-cluster sum of squares
model$withinss
# Total within-cluster sum of squares 
model$tot.withinss
# Total sum of squares
model$totss

#See which items are in which group
norm_data[model$cluster==1,]
norm_data[model$cluster==2,]
norm_data[model$cluster==3,]

#plot clusters in k-means
library(fpc)
plotcluster(norm_data, model$cluster)

#To find elbow point to find better the value of k
bsss <- numeric()
wsss <- numeric()

# Run the algorithm for different values of k 
set.seed(12345)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bsss[i] <- kmeans(norm_data, centers=i)$betweenss
  wsss[i] <- kmeans(norm_data, centers=i)$tot.withinss
}

# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bsss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wsss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

#Using elbow method, we see that there should be 2 clusters.

# Subplot
grid.arrange(p3, p4, ncol=2)

# Mean values of each cluster
aggregate(clustering, by=list(model$cluster), mean)

#different plots to be combined into a plot matrix

library(GGally)
ggpairs(cbind(clustering, Cluster=as.factor(model$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        axisLabels="none")


#k-means with k=2

set.seed(12345)
model1 <- kmeans(norm_data, centers=2)
model1
# Cluster to which each point is allocated
model1$cluster
# Cluster centers
model1$centers
# Cluster size
model1$size
# Between-cluster sum of squares
model1$betweenss
# Within-cluster sum of squares
model1$withinss
# Total within-cluster sum of squares 
model1$tot.withinss
# Total sum of squares
model1$totss

#See which items are in which group
norm_data[model1$cluster==1,]
norm_data[model1$cluster==2,]

# Mean values of each cluster
aggregate(clustering, by=list(model1$cluster), mean)

#different plots to be combined into a plot matrix
ggpairs(cbind(clustering, Cluster=as.factor(model1$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        axisLabels="none")



