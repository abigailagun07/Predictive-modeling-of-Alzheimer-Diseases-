#Libraries
library(tidyverse)
library(moments)
library(corrplot)
library(gridExtra)
library(factoextra)
library(cluster) 
library(fpc)
library(caret)

#set working directory
setwd("C:/Users/adedo/OneDrive/Documents/masters/university work/MA335/coursework ma335")

#read data
data<- read.csv("project data.csv")
#============================================================================
# 2.0 pre-analysis

names(data)[2] <- c("Gender") # Rename variable M/F to Gender

data$Gender<- ifelse(data$Gender == "M", 0, 1) #convert M/F to numeric values, #0 = male, 1 = female

data<- data[data$Group != "Converted", ] #remove rows with Group = Converted

colSums(is.na(data))#identify rows with missing values. # 19 in SES and 2 in MMSE 

data <- na.omit(data) #remove rows with missing values
#=============================================================================
## 3.1 - Descriptive statistics

table(data$Group)#identify distribution of demented and non-demented in the data

#filter data to only demented and non-demented.
demented_group <- filter(data, Group == "Demented")

nondemented_group<- filter(data, Group == "Nondemented")

summary(demented_group)#get summary statistics of the demented group

summary(nondemented_group) #get summary of the demented group

#get the mode of each attribute, filter by demented and non-demented
#create mode function
attnames <- names(demented_group)# get variable names
mode_funct<- function(c) {
  distinct_values<- unique(c)
  tab_var<- tabulate(match(c, distinct_values))
  mode_index<- which.max(tab_var)
  mode_value<- distinct_values[mode_index]
  return(mode_value)}

#get mode of all attributes for the demented group
for (i in 2:10) {column_mode <- mode_funct(demented_group[[i]]) 
  column_name <- attnames[i]
  print(paste(column_name, "- Mode:", column_mode))}

#get mode of all attributes for the non-demented group
for (i in 2:10) {column_mode <- mode_funct(nondemented_group[[i]]) 
  column_name <- attnames[i]
  print(paste(column_name, "- Mode:", column_mode))}

### Graphical representation 
#Density plot
features<- c("Gender","Age","EDUC","SES","MMSE","CDR","eTIV","nWBV","ASF")

#create a function to generate individual density plots
plot_function <- function(features) {ggplot(data, aes_string(x = features, fill = "Group")) +
    geom_density(alpha = 0.7)+ labs(x = features)+ theme_minimal()+theme() + scale_fill_manual(values = c("red", "steelblue"))}

# for each attributes, a density plots is generated
plot_1 <- lapply(features, plot_function) 

# visualise plots in a grid
do.call(grid.arrange, c(plot_1, ncol = 3))


##correlation plot
data$Group<- ifelse(data$Group=="Demented", 0, 1) #change "Group" to numeric. 0 = demented, 1= non-demented

data.corr<- cor(data) #apply cor function

corrplot(data.corr, method = "number", type= "lower", bg= "grey")# visualise using plot
#===========================================================================
## 3.2 Clustering

data_no_y<- data[,-1] #remove response variable  

scaled_data<- scale(data_no_y) #scale the data

#visualise to get the optimal value for K
fviz_nbclust(scaled_data, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2, col="red")

kmeans3 <- kmeans(scaled_data, centers = 3, nstart = 20)

kmeans3

k3<- fviz_cluster(kmeans3, data = scaled_data, main="Cluster plot k = 3")

kmeans2 <- kmeans(scaled_data, centers = 2, nstart = 20)

kmeans2

k2<- fviz_cluster(kmeans2, data = scaled_data, main="cluster plot k = 2")

grid.arrange(k3, k2, nrow = 1) #combine plot

#silhouette coefficient
cluster_labels_k2 <- kmeans2$cluster

dissimilarity_matrix <- dist(scaled_data)

silhouette_score_k2 <- cluster.stats(dissimilarity_matrix, cluster_labels_k2)$avg.silwidth

silhouette_score_k2
#==========================================================================
##3.3 Feature selection
data$Group<- as.factor(data$Group) #convert "Group" to factor

#backward wrapper method 
back_fit <- glm(Group ~ ., data = data, family = binomial)

back_model<- step(back_fit, direction = "backward")

summary(back_model)

#forward wrapper method
forw_fit<-glm(Group~1,family = binomial, data=data)

forw_model<-step(forw_fit,scope=~Gender+Age+EDUC+SES+MMSE+CDR+eTIV+nWBV+ASF,
                      method='forward')

summary(forw_model)

#compare backward with forward wrapper
lr_test <- anova(back_model, forw_model, test = "Chisq")

lr_test

#=========================================================================
## 3.4 Logistic regression

#change "Group" back to its original label for easier understanding/read
data$Group<- ifelse(data$Group==0, "Demented", "Nondemented") 

data$Group <- factor(data$Group) #convert "Group" to factor

#perform cross validation
trControl <- trainControl(method = "cv", number = 10)

glm_fit_forw <- train(Group ~ CDR + ASF + EDUC + MMSE,method = "glm",family = "binomial",trControl = trControl,metric = "Accuracy", data = data)

glm_predict <- predict(glm_fit_forw, data)

tab_forw<-table(glm_predict, data$Group)

#check confusion matrix
confusionMatrix(tab_forw)

#identify important variables
important_variables <- varImp(glm_fit_forw)

important_variables

