# Customer-Satisfaction-Analysis

**Introduction:** To help our client Southeast Airlines to improve its quality, we designed this project to research factors that will highly influence their client's satisfaction. We analyzed the Airline Customer data to find ways that can lead to an increase in customer satisfaction. This was a part of our IST 687: Data Science coursework. Our entire analysis was done using R programming.

**Programming Language:** R (dplyr, ggplot2, arules, arulesViz, e1071)
**Tools:** Trello, Microsoft PowerPoint  

**Dataset:** Southeast Airlines collected data via the internet and surveys. This dataset contains a total of 28 variables. Our target variable is the Satisfaction variable. It contained approximately 130k observations. The dataset was for the first three months of 2014 i.e. from January 2014 to March 2014. 

**Data Cleaning and Transforming:** We started by cleaning the data extensively. We removed columns that had a high percentage of NA values. We replace some of them with 0 as we couldn’t afford to lose data by removing them. Then we cleaned data which was a bit weird. For example, there were many decimals by mistake. So we extracted such data and cleaned them. 

**Exploratory Data Analysis:** We started analyzing the data by using visualization graphs and plots. We used R packages for visualizing our data. We build various bar graphs, heat maps, box plots, line graphs, and pie charts to answer important business questions. Later, we also used statistical techniques to understand the data and know the relationship between each variable. Our main aim was to find the relationship between different variables with our satisfaction variable. 

**Machine Learning Models:** For each of our models, we divided the data into Training and Validation datasets. Then, we fit the model on our training data and tested it on our validation dataset. We also perform hyperparameter tuning using different parameters based on our model and we evaluated our model based on different evaluation metrics.

1. Association Rule Mining: We first divided the satisfaction variable into High and Low. We found out important rules which lead to either high satisfaction or low satisfaction. We also leveraged this model to find the significant variables. 

2. Linear Regression: We implement a multiple linear regression model and then selected significant variables for our final model based on their p-values.

3. Support Vector Machine: We also implemented a Support Vector Machine model on our data. We achieved an accuracy of 80% for this model. 

**Conclusion:** We recommended various actionable insights to our clients using our analysis and machine learning models. One of them being Blue status travel passengers has the least satisfaction so we think that the airlines should change their policies towards that type. Older people were less satisfied so there should be more services provided to them.
