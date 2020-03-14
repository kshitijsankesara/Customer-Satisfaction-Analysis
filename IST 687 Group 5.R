library(gdata)
library(moments)
library(car)
library(MASS)
library(xtable)
library(dplyr)
library(nnet)
library(ggplot2)
library(highcharter)
library(viridis)
library(ade4)
library(readxl)
library(ROCR)
library(pROC)
library(pscl)

####################################################################################################################
# Read Data
ProjectData <-read.csv( "Satisfaction Survey.CSV")
################################################### Data Cleansing #################################################

# Show NAs in each colum
colSums(is.na(ProjectData))

# Treat NA values
CleanData <- ProjectData
CleanData$Departure.Delay.in.Minutes[is.na(ProjectData$Departure.Delay.in.Minutes)] <-0
CleanData$Arrival.Delay.in.Minutes[is.na(ProjectData$Arrival.Delay.in.Minutes)] <-0
CleanData$Flight.time.in.minutes[is.na(ProjectData$Flight.time.in.minutes)] <-0
CleanData <- na.omit(CleanData)
colSums(is.na(CleanData))
table(CleanData$Satisfaction)

# Formating the data
ind1 <- which(CleanData$Satisfaction == '4.00.2.00')
CleanData$Satisfaction[ind1[1]] <- 4
CleanData$Satisfaction[ind1[2]] <- 4
ind2 <- which(CleanData$Satisfaction == '4.00.5')
CleanData$Satisfaction[ind2[1]] <- 5
ind3 <- which(CleanData$Satisfaction == '2.5')
CleanData$Satisfaction[ind3] <- 3
ind4 <- which(CleanData$Satisfaction == '3.5')
CleanData$Satisfaction[ind4] <- 4
ind5 <- which(CleanData$Satisfaction == '4.5')
CleanData$Satisfaction[ind5] <- 5
CleanData$Satisfaction <- factor(CleanData$Satisfaction)
table(factor(CleanData$Satisfaction))
CleanData$Satisfaction <- as.numeric(CleanData$Satisfaction)
CleanData$Year.of.First.Flight <-factor(CleanData$Year.of.First.Flight)
CleanData$Price.Sensitivity <- factor(CleanData$Price.Sensitivity)
CleanData$Flight.Distance <- factor(CleanData$Flight.Distance)
CleanData$X..of.Flight.with.other.Airlines <-factor(CleanData$X..of.Flight.with.other.Airlines)
CleanData$Flight.cancelled <- factor(CleanData$Flight.cancelled)


#################################################### Client Data ##############################################
# select client data
ClientData <- CleanData[CleanData$Airline.Name == 'Southeast Airlines Co. ', ]
ClientData <-subset(ClientData, select = -c(Airline.Name,Airline.Code))

###############################################################################################################
#############################################  Sample the data Function   #####################################
SamF <- function(data, a) {
  smp_size <- floor(a * nrow(data))
  train_ind <- sample(nrow(data), size = smp_size)
  train <- data[train_ind,]
  test <- data[-train_ind,]
  return(list(train, test))
}
########################################################  MCA/PCA  Method      #################################
### Remove SATISFACTION
a <- data.frame(lapply(ClientData, as.factor))
a <- a[, -1]

## Fit MCA
library(FactoMineR)
library(factoextra)
res.mca <- MCA(a)
res = dimdesc(res.mca, axes = 1:2, proba = 0.05)
res$`Dim 1`$quali

###
ind <- get_mca_ind(res.mca)
var <- get_mca_var(res.mca)
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

summary.MCA(res.mca)
## eigvalue
eig.val <- res.mca$eig
barplot(
  eig.val[, 2],
  names.arg = 1:nrow(eig.val),
  main = "Variances Explained by Dimensions (%)",
  xlab = "Principal Dimensions",
  ylab = "Percentage of variances",
  col = "steelblue"
)
# Add connected line segments to the plot
lines(
  x = 1:nrow(eig.val),
  eig.val[, 2],
  type = "b",
  pch = 19,
  col = "red"
)

fviz_eig(res.mca)
fviz_mca_biplot(res.mca)
fviz_mca_ind(res.mca)
fviz_mca_var(res.mca)
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())


# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

#######################################################      Association rule       ########################################
library(arules)
library(arulesViz)

ARData <- ClientData
vBuckets <- replicate(length(ARData$Satisfaction), "N")
vBuckets[ARData$Satisfaction > 3] <- "Y"
ARData$Satisfaction <- vBuckets

for (i in 1:ncol(ARData)) {
  ARData[, i] <- factor(ARData[, i])
}

train <- SamF(ARData, 0.7)[[1]]
test <- SamF(ARData, 0.7)[[2]]


ARDataX <- as(train, "transactions")

#itemFreq <-itemFrequency(ARDataX)

#itemFrequencyPlot(ARDataX)
inspect(ARDataX[1:5])

summary(ARDataX)

itemFrequencyPlot(ARDataX, topN = 10, horiz = T)

image(sample(ARDataX, 100))

RuleDF <-apriori(ARDataX ,parameter = list(support = 0.1, confidence = 0.8),appearance = list(rhs = c('Satisfaction=N', 'Satisfaction=Y'),default = 'lhs'))
RuleDF <-apriori(train ,parameter = list(support = 0.2, confidence = 0.8),appearance = list(rhs = c('Satisfaction=N', 'Satisfaction=Y'),default = 'lhs'))

inspect(RuleDF [1:20])

ordered_rules <- sort(RuleDF, by = "lift")
inspect(ordered_rules[1:20])

############################################################       Linear     Model    ##################################################################
train <- SamF(ClientData, 0.7)[[1]]
test <- SamF(ClientData, 0.7)[[2]]
ModelReduced <- lm(as.numeric(Satisfaction) ~ Airline.Status + Age + Gender  + No.of.Flights.p.a.+Type.of.Travel+Class +Flight.cancelled+Arrival.Delay.greater.5.Mins, data = train)
ModelFull <- lm(as.numeric(Satisfaction) ~ ., data = train)
summary(ModelFull)
summary(ModelReduced)
anova(ModelFull,ModelReduced)
########################################################
# Add predictions 
pred.int <- predict(ModelReduced,test, interval = "prediction")
Predict<-predict(ModelReduced, newdata = test, type = "response")
test$SatisfactionPred <-predict(ModelReduced, newdata = test, type = "response")

#pr <- prediction(Predict, test$Satisfaction)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")

# accuracy rate
id <- which(test$Satisfaction!=round(test$SatisfactionPred), arr.ind = TRUE)
Rate <- 1-length(id)/nrow(test)
print(Rate)

# residual plot
qqnorm(ModelReduced$residuals)
qqline(ModelReduced$residuals)
#shapiro.test(ModelReduced$residuals)

#########################################################      Ordinal Logistic Model   ####################################
OLM_ModelFull <-polr(factor(Satisfaction) ~.,data = train,Hess=TRUE)
OLM_ModelReduced <- polr(factor(Satisfaction) ~ Airline.Status + Age + Gender  + No.of.Flights.p.a.+Type.of.Travel + Class + Scheduled.Departure.Hour+Flight.cancelled+Arrival.Delay.greater.5.Mins, data = train,Hess=TRUE)

summary(OLM_ModelFull)
summary(OLM_ModelReduced)

Anova(OLM_ModelFull,OLM_ModelReduced)

# P value t value table
ctable <- coef(summary(OLM_ModelReduced))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable <- cbind(ctable, "p value" = p)
ctable

# confident interval
ci <- confint(OLM_ModelReduced)
confint.default(OLM_ModelReduced)
exp(coef(OLM_ModelReduced))
exp(cbind(OR = coef(OLM_ModelReduced), ci))

###
Predict<-predict(OLM_ModelReduced, newdata = test, type = "class")
test$SatisfactionPred <-predict(OLM_ModelReduced, newdata = test, type = "class")

#pr <- prediction(Predict, test$Satisfaction)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")

## accuracy rate
id <- which(test$Satisfaction!=test$SatisfactionPred, arr.ind = TRUE)
Rate <- 1-length(id)/nrow(test)
print(Rate)


#######################################################  SVM  #############################################################
library(kernlab)
library(e1071)

### set up data
vBuckets <- replicate(length(ClientData$Satisfaction), "N")
vBuckets[ClientData$Satisfaction > 3] <- "Y"
ClientDataSVM <- ClientData
ClientDataSVM$Satisfaction <- vBuckets
dim(ClientDataSVM)
trainSVM <- SamF(ClientDataSVM,0.7)[[1]]
testSVM <- SamF(ClientDataSVM, 0.7)[[2]]

#### Full Model & Reduced Model
svm_Full <-
  ksvm(Satisfaction ~ .,data = trainSVM,kernel = "rbfdot",kpar = "automatic", C = 5,cross = 3,prob.model = TRUE)

svm_Reduced <-
  ksvm(Satisfaction ~ Airline.Status + Age + Gender  + No.of.Flights.p.a.+Type.of.Travel+Class +Flight.cancelled+Arrival.Delay.greater.5.Mins ,data = trainSVM,kernel = "rbfdot",kpar = "automatic", C = 5,cross = 3,prob.model = TRUE)


svm_Reduced
hist(alpha(svm_Reduced)[[1]], main = 'Support Vector Histogramwith C=5', xlab =
       'Support Vector Values')

### Predict
svmPred <- predict(svm_Reduced,testSVM, type = "votes")
str(svmPred)
head(svmPred)
svmPred
compTable <- data.frame(testSVM$Satisfaction,svmPred[2,])
table(compTable)


################################################################### Graph  Ana #############################################################################

#### Heat Map Satisfaction VS all others
i = 0
for (i in 1:ncol(ClientData)) {
  HM <- data.frame(ClientData$Satisfaction, ClientData[, i])
  HM <- data.frame(table(HM))
  colnames(HM) <- c('x', 'y', 'f')
  #pdf('Plot.pdf')
  print(hchart(HM, "heatmap", hcaes(
    x = x, y = y, value = f
  ))%>% hc_colorAxis(type = "logarithmic"))
  
}

### Bar Chart Satisfaction Rate from Various Airline Status

ARData %>% 
  select(Airline.Status, Satisfaction) %>% 
  group_by(Airline.Status) %>% 
  summarise(Rate = sum(Satisfaction == 'Y')/length(Airline.Status))-> AR_byS

hchart(AR_byS, "column", hcaes(Airline.Status, y = Rate)) %>%
  hc_colorAxis(stops = color_stops(n = 3, colors = c( "#21908C", "#04eadf","#440154"))) %>%
  hc_add_theme(hc_theme_db()) %>%
  hc_title(text = "Satisfaction Rate from Various Airline Status : ") %>%
  hc_legend(enabled = FALSE)



############################ Satisfication Level Bar Chart ############################
propSat<- prop.table(table(ClientData$Satisfaction)) 
smallData = as.data.frame(propSat)
colnames(smallData)=c('SatisficationLevel', 'Frequency')
str(smallData)

gbar=ggplot(smallData, aes(x=SatisficationLevel , y=Frequency, fill=SatisficationLevel ))

gbar + geom_bar(stat = 'identity') + scale_fill_brewer() + ggtitle('Percentage of each Satisfication Level') +
  guides(fill = FALSE) + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1))

####################### scatter plot 
scatterplot(ClientData$Age,ClientData$Satisfaction)
scatterplot(ClientData$Gender,ClientData$Satisfaction)



########## other 
aa <- lmer(Satisfaction ~ Airline.Status  * Gender  + (1|Flight.cancelled) + Arrival.Delay.greater.5.Mins,data = ClientData)
summary(aa)
lsmip(aa,Airline.Status  ~ Gender)


