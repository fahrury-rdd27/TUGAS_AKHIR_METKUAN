wbcd = read.csv("wisc_bc_data.csv", stringsAsFactors = F)

#---- Exploring and Preparing Data ----

#data structure
str(wbcd)

#exclude ID var
wbcd = wbcd[-1]

attach(wbcd)

table(diagnosis)

#changing the type of diagnosis var into factor and rename the labels
wbcd$diagnosis = factor(wbcd$diagnosis, levels = c("B", "M"),
                        labels = c("Benign", "Malignant"))

#seeing the result
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

#focus on three features and see the summary
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#---- Transformation: Normalizing numeric data ----#

#initiate function of normalization
normalize = function(x){
  return((x - min(x))/(max(x) - min(x)))
}

#example use of normalization function
normalize(c(1,2,3,4,5)) 
normalize(c(1000, 2000, 3000, 4000, 5000))
#the result should show the same

#applying the func to all numeric var
wbcd_n = as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])

#---- Data Preparation: creating Training and Test datasets ----

#splitting data into training and test datasets

wbcd_training = wbcd_n[1:469, ] #first 469 to be training dataset
wbcd_test = wbcd_n[470:569, ] #last 100 to be test dataset

#creating class labels from diagnosis var
wbcd_train_labels = wbcd[1:469, 1]
wbcd_test_labels = wbcd[470:569, 1]

#---- Training a model on the data ----

library("class") #knn implementation packages

wbcd_test_pred = knn(train = wbcd_training,
                     test = wbcd_test,
                     cl = wbcd_train_labels,
                     k = 21)
#---- Evaluating model performances ----
install.packages("gmodels")
library("gmodels")

CrossTable(x = wbcd_test_labels,
           y=wbcd_test_pred,
           prop.chisq = F)
