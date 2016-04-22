setwd("C:/Users/akhilkateja/Desktop")
mydata <-
  read.csv(
    "train.csv",
    header = TRUE,
    na.strings = (""),
    stringsAsFactors = FALSE
  )


#Process Age
for (i in 1:length(mydata$AgeuponOutcome)) {
  if (!is.na(mydata$AgeuponOutcome[i])) {
    arr <- strsplit(toString(mydata$AgeuponOutcome[i]), " ")
    
    if (grepl("week", arr[[1]][2])) {
      mydata$AgeuponOutcome[i] = (strtoi(arr[[1]][1])) * 7
    } else if (grepl("month", arr[[1]][2])) {
      mydata$AgeuponOutcome[i] = (strtoi(arr[[1]][1])) * 30
    } else if (grepl("year", arr[[1]][2])) {
      mydata$AgeuponOutcome[i] = (strtoi(arr[[1]][1])) * 365
    } else if (grepl("day", arr[[1]][2])) {
      mydata$AgeuponOutcome[i] = (strtoi(arr[[1]][1]))
    }
  }
}

#Process Color
for (i in 1:length(mydata$Color)) {
  if (!is.na(mydata$Color[i])) {
    if (grepl("/", mydata$Color[i])) {
      mydata$Color[i] <- "TwoColor"
    } else if (grepl("Tricolor", mydata$Color[i])) {
      mydata$Color[i] <- mydata$Color[i]
    } else{
      mydata$Color[i] <- "SingleColor"
    }
  }
}

#Process Breed
mydata$Breed <- gsub(" Mix", "", mydata$Breed)
for (i in 1:length(mydata$Breed)) {
  if (!is.na(mydata$Breed[i])) {
    if (grepl("/", mydata$Breed[i])) {
      arr <- strsplit(toString(mydata$Breed[i]), "/")
      mydata$Breed[i] <- arr[[1]][1]
    }
  }
}

#Process Date
hour <- c()
day <- c()
month <- c()
year <- c()
for (i in 1:length(mydata$DateTime)) {
  if (!is.na(mydata$DateTime[i])) {
    arr <- strsplit(toString(mydata$DateTime[i]), " ")
    date <- strsplit(toString(arr[[1]][1]), "/")
    time <- strsplit(toString(arr[[1]][2]), ":")
    hour[i] <- c(time[[1]][1])
    day[i] <- c(date[[1]][2])
    month[i] <- c(date[[1]][1])
    year[i] <- c(date[[1]][3])
    
  }
}

#Process Name
for (i in 1:length(mydata$Name)) {
  if (!is.na(mydata$Name[i])) {
    mydata$Name[i] <- "Name"
  } else{
    mydata$Name[i] <- "Nameless"
  }
}
library(randomForest)

#Factor Variables
AUO <- as.numeric(mydata$AgeuponOutcome)
AUO <- na.roughfix(AUO)
AUO <- factor(AUO)

AT <- factor(mydata$AnimalType)
B <- factor(mydata$Breed)
C <- factor(mydata$Color)
OT <- factor(mydata$OutcomeType)
SUO <- factor(mydata$SexuponOutcome)
N <- factor(mydata$Name)

hour <- factor(hour)
month <- factor(hour)
day <- factor(day)
year <- factor(year)

training_data <-
  data.frame(
    OutcomeType = OT,
    AgeuponOutcome = AUO,
    AnimalType = AT,
    Color = C,
    SexuponOutcome = SUO,
    Name = N,
    hour = hour,
    month = month,
    day = day,
    year = year
  )


train <- training_data[1:21000, ]
test  <- training_data[21001:26729, ]

#train <- x[1:21000, ]
#test_data <- x[21001:26729, ]

#Create Random Forest

rf <-
  randomForest(
    train$OutcomeType ~ train$AgeuponOutcome + train$AnimalType + train$Color +
      train$SexuponOutcome + train$Name + train$hour + train$month + train$day + train$year,
    ntree = 600,
    importance = TRUE
  )
prediction <- predict(rf, newdata = test)
solution <- data.frame(prediction)
write.csv(solution, 'rf_solution.csv', row.names = F)
