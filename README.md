# Clustering-and-Market-Basket-Analysis on Cincinnati Zoo data

Data description:

The Cincinnati Zoo was founded in 1873 and officially opened in 1875. It is the second oldest in the nation after Pennsylvania Zoo and 
serves over a million visitors each day. The goal of this analysis is to identify useful and hidden information in the data collected 
by the zoo and to study the buying and visiting behavior of zoo members. 

The data comes from 2 files - one with aggregated sales data of food items across the months Oct’10, Nov’10, Dec’10, Jan’11, Feb’12, 
and Mar’12 and another one with individual transaction data of customer purchase behaviour across the same months and food items.

Source: 

For clustering: http://homepages.uc.edu/~maifg/DataMining/data/qry_Food_by_Month.csv
For Association Rule Mining: http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv

Technical details:

Here,I have used 2 unsupervised learning techniques.

1. K-means clustering algorithm to find clusters of similar food items.
2. Apriori algorithm to find out which items might be purchased together.

Language used: R
