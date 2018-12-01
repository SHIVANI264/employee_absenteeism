rm(list=ls())
install.packages("DMwR")
install.packages("caret")
install.packages("randomForest")
install.packages("mlr")
install.packages("dummies")
library(caret)
install.packages("rpart",dependencies=TRUE)
# Installing and loading few libraries
library(corrgram)
library(dummies)
library(rpart)
library(ggplot2)
library(randomForest)
library(grid)
library(gtable)
install.packages("xlsx", dependencies=TRUE)
install.packages("gridExtra")
library(gridExtra)
install.packages("corrgram")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for f64-bit version
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version
library(rJava)
library(DMwR)
library(xlsx)


# Set working directory
setwd("C:/Users/jatin/Videos/Music/Documents/project_edwisor2")

getwd()

df = read.xlsx('Absenteeism_at_work_Project.xls', sheetIndex = 1)


str(df)

# From the above EDA and problem statement categorising data in 2 category "continuous" and "catagorical"

continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
                    'Work.load.Average.day.', 'Transportation.expense',
                     'Hit.target', 'Weight', 'Height', 
                      'Body.mass.index', 'Absenteeism.time.in.hours')



categorical_vars = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
                    'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
                     'Social.smoker', 'Son', 'Pet')



#Creating dataframe with missing values present in each variable

missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"

#Calculating percentage missing value

missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100 



# Sorting missing_val in Descending order

missing_val = missing_val[order(-missing_val$Missing_percentage),]

row.names(missing_val) = NULL



# Reordering columns

missing_val = missing_val[,c(2,1)]



# Saving output result into csv file

write.csv(missing_val, "Missing_perc_R.csv", row.names = F)

#df[8,20] = NA

df
#Mean Method

#df$Body.mass.index[is.na(df$Body.mass.index)] = mean(df$Body.mass.index, na.rm = T)


df = df[which(!is.na(df$Absenteeism.time.in.hours)),]

df_new =df
#Median Method

# df$Body.mass.index[is.na(df$Body.mass.index)] = median(df$Body.mass.index, na.rm = T)




 # kNN Imputation

df = knnImputation(df, k = 3)

sum(is.na(df))


# Boxplot for continuous variables

for (i in 1:length(continuous_vars))
  
  
{
  
  assign(paste0("gn",i), ggplot(aes_string(y = (continuous_vars[i]), x = "Absenteeism.time.in.hours"), data = subset(df))+ 
          stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18, 
                        outlier.size=1, notch=FALSE) +
                      theme(legend.position="bottom")+
                       labs(y=continuous_vars[i],x="Absenteeism.time.in.hours")+
               ggtitle(paste("Box plot of absenteeism for",continuous_vars[i])))
  
}



# ## Plotting plots together

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)


#Replace all outliers with NA and impute

for(i in continuous_vars)
  
{
  
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df[,i][df[,i] %in% val] = NA
  
}



# Imputing missing values

df = knnImputation(df,k=3)




# Correlation Plot 

corrgram(df[,continuous_vars], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")






summary(aov(formula = Absenteeism.time.in.hours~ID,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Education,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Son,data = df))
summary(aov(formula = Absenteeism.time.in.hours~Pet,data = df))



df = subset(df, select = -c(Weight))
df_new = df
# Normalization



# Updating the continuous and catagorical variable

continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
                    'Work.load.Average.day.', 'Transportation.expense',
                      'Hit.target', 'Height', 
                      'Body.mass.index')



categorical_vars = c('ID','Reason.for.absence','Disciplinary.failure', 
                     'Social.drinker', 'Son', 'Pet', 'Month.of.absence', 'Day.of.the.week', 'Seasons',
                     'Education', 'Social.smoker')



for(i in continuous_vars)
  
{
  
  print(i)
  
  df[,i] = (df[,i] - min(df[,i]))/(max(df[,i])-min(df[,i]))
  
}



set.seed(123)


library(mlr)


df = dummy.data.frame(df, categorical_vars)

train.index = sample(1:nrow(df), 0.8 * nrow(df))

train = df[ train.index,]

test  = df[-train.index,]

df1= df

df1 = subset(df1,select = -c(Absenteeism.time.in.hours))

set.seed(123)
train1.index=sample(1:nrow(df1),0.8* nrow(df1))

train1 =df1[train1.index,]

test1 = df1[-train1.index,]



#principal component analysis

prin_comp = prcomp(train1)

prin_comp$x


#compute standard deviation of each principal component

std_dev = prin_comp$sdev



#compute variance

pr_var = std_dev^2



#proportion of variance explained

prop_varex = pr_var/sum(pr_var)



#cumulative scree plot

plot(cumsum(prop_varex), xlab = "Principal Component",
     
     ylab = "Cumulative Proportion of Variance Explained",
     
     type = "b")



#add a training set with principal components

train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)



# From the above plot selecting 50 components since it explains almost 95+ % data variance

train.data =train.data[,1:50]



#transform test into PCA

test.data = predict(prin_comp, newdata = test1)

test.data = as.data.frame(test.data)



#select the first 50 components

test.data=test.data[,1:50]
#Develop Model on training data

fit_LR = lm(Absenteeism.time.in.hours ~ ., data = train.data)



#Lets predict for training data

pred_LR_train = predict(fit_LR, train.data)



#Lets predict for testing data

pred_LR_test = predict(fit_LR,test.data)



# For training data 

print(postResample(pred = pred_LR_train, obs = train$Absenteeism.time.in.hours))



# For testing data 

print(postResample(pred = pred_LR_test, obs =test$Absenteeism.time.in.hours))

#Develop Model on training data

fit_RF = randomForest(Absenteeism.time.in.hours ~., data = train.data)

#Lets predict for training data

pred_RF_train = predict(fit_RF, train.data)



#Lets predict for testing data

pred_RF_test = predict(fit_RF,test.data)



# For training data 

print(postResample(pred = pred_RF_train, obs = train$Absenteeism.time.in.hours))



# For testing data 

print(postResample(pred = pred_RF_test, obs = test$Absenteeism.time.in.hours))



#### Decision trees

#Develop Model on training data

fit_DT = rpart(Absenteeism.time.in.hours ~., data = train.data, method = "anova")





#Lets predict for training data

pred_DT_train = predict(fit_DT, train.data)



#Lets predict for training data

pred_DT_test = predict(fit_DT,test.data)





# For training data 

print(postResample(pred = pred_DT_train, obs = train$Absenteeism.time.in.hours))



# For testing data 

print(postResample(pred = pred_DT_test, obs = test$Absenteeism.time.in.hours))






