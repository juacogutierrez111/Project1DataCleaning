

## Merges the training and the test sets to create one data set.

## get the data.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f))

## Unzip file...

## Load the data
library(data.table)
dtSubTrain <- fread(file.path(getwd(), "train", "subject_train.txt"))
dtSubTest <- fread(file.path(getwd(), "test", "subject_test.txt"))

dtActivityTrain <- fread(file.path(getwd(), "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(getwd(), "test", "Y_test.txt"))

fileToDataTable <- function(f) {
    df <- read.table(f)
    dt <- data.table(df)
}

dtTrain <- fileToDataTable(file.path(getwd(), "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(getwd(), "test", "X_test.txt"))

## Merge the training and the test sets
##Concatenate the data tables.
dtSubject <- rbind(dtSubTrain, dtSubTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

## Merge columns.
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

## Set key.
setkey(dt, subject, activityNum)


## Extract the mean and standard deviation
dtFeatures <- fread(file.path(getwd() ,"features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

##Convert the column numbers to a vector of variable names matching columns in dt.

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

## Descriptive activity names
dtActivityNames <- fread(file.path(getwd(), "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

## Label with descriptive activity names
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dt, subject, activityNum, activityName)

## Melt/Merge...
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

## Create new equivalent variables for activityName / featureName 
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

## Seperate features from featureName.
grepthis <- function (regex) {
  grepl(regex, dt$feature)
}

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

##  Create a tidy data set ith the average of each variable for each activity and each subject.

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

##  Save the tidy data set to a file
f <- file.path(getwd(), "DatasetHumanActivityRecognitionUsingSmartphones.txt")
write.table(dtTidy, f, quote = FALSE, sep = "\t", row.names = FALSE)








