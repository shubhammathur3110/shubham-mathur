i <- read.csv("wisc.csv", stringsAsFactors = FALSE)
#structure of the object name 
str(i)
i <- i[-1]
View(i)
table(i$diagnosis)
i$diagnosis = factor(i$diagnosis, levels = c("B", "M"),
                     labels = c("Benigs", "Malignant"))
View(i)
round(prop.table(table(i$diagnosis)) * 100, digits = 1)
summary(i[c("radius_mean", "area_mean", "smoothness_mean")])
summary(i)
#create normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#test normalization function

i_n = as.data.frame(lapply(i[2:31], normalize))
View(i_n)
summary(i_n$area_mean)
i_train = i_n[1:469,]
i_test = i_n[470:569,]
i_train_labels = i[1:469, 1]
i_test_labels = i[470:569, 1]
library(class)
i_test_pred = knn(train = i_train, test = i_test,
                  cl = i_train_labels, k = 21)
install.packages(gmodels)
library(gmodels)
CrossTable(x = i_test_labels, y = i_test_pred,
           prop.chisq = FALSE)
