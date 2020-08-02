#reading the datasets#
act_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")
x_test <- read.table("X_test.txt")
x_train <- read.table("X_train.txt")
y_test <- read.table("y_test.txt")
y_train <- read.table("y_train.txt")
subject_test <- read.table("subject_test.txt")
subject_train <- read.table("subject_train.txt")

#renaming the datasets#
names(act_labels) <- c("number","activity")
names(features) <- c("number","feature")

#combining x_test and x_train into x_comb#
x_comb <- rbind(x_test,x_train)
names(x_comb) <- features$feature

#combining y_test and y_train into y_comb#
y_comb <- rbind(y_test,y_train)

#extract mean and SD from x_comb#
ext_features <- grep("std()|mean()[^meanFreq]",features$feature)
x_ext <- x_comb[(ext_features)]

#labelling the activity#
activitylabel <- function(row){
  x <- row[[1]]
  row[[2]] = act_labels$activity[x]
}

activity <- apply(y_comb, 1, activitylabel)

#combining x_comb and y_comb into xy_comb#
xy_comb <- cbind(x_ext,activity)

#combining subject_test and subject_train#
subjectcomb <- rbind(subject_test,subject_train)
names(subjectcomb) <- c("subjectno")

#combining subjectcomb with xy_comb#
finalds <- cbind(xy_comb, subjectcomb)

#grouping based on activity and subject, then using summarise to find the means#
grouped <- group_by(finalds,activity,subjectno)
meangrouped <- summarise_all(grouped, mean)
write.table(newgrouped, "meangrouped.txt", row.name = FALSE)
