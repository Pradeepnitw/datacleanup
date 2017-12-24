read.csv("dirty_data.csv")
dirty_data <- read.csv("dirty_data.csv")
head(dirty_data)


# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

processed_data <- read.csv("dirty_data.csv")

processed_data$Street <- trim(processed_data$Street)
processed_data$Street.2 <- trim(processed_data$Street.2)


paste(dirty_data$Street)
paste(processed_data$Street)

paste(dirty_data$Street.2)
paste(processed_data$Street.2)

##Replacing special characters and accent for Street1

replchar <- function (x) gsub("Ì", " ", x)
processed_data$Street <- replchar(processed_data$Street)
head(processed_data)

processed_data$Street  <- gsub("[^0-9A-Za-z///' ]","'" , processed_data$Street ,ignore.case = TRUE)
processed_data$Street <- gsub("'","" , processed_data$Street ,ignore.case = TRUE)
head(processed_data)

##Replacing special characters and accent for Street2

replchar <- function (x) gsub("Ì", " ", x)
processed_data$Street.2 <- replchar(processed_data$Street.2)
head(processed_data)

processed_data$Street.2  <- gsub("[^0-9A-Za-z///' ]","'" , processed_data$Street.2 ,ignore.case = TRUE)
processed_data$Street.2 <- gsub("'","" , processed_data$Street.2 ,ignore.case = TRUE)
head(processed_data)

##Captalize first letter of Street name

library(Hmisc)
processed_data$Street <- capitalize(processed_data$Street)
processed_data$Street.2 <- capitalize(processed_data$Street.2)



##Replace Road with Rd., Street with St. and Avenue with Ave. for all Street names in Street1

processed_data$Street  <- gsub("Road|road","Rd.", processed_data$Street)
processed_data$Street  <- gsub("Street","St.", processed_data$Street)
processed_data$Street  <- gsub("Avenue","Ave.", processed_data$Street)
head(processed_data)

##Replace Road with Rd., Street with St. and Avenue with Ave. for all Steet name in Street2

processed_data$Street.2  <- gsub("Road|road","Rd.", processed_data$Street.2)
processed_data$Street.2  <- gsub("Street","St.", processed_data$Street.2)
processed_data$Street.2  <- gsub("Avenue","Ave.", processed_data$Street.2)
head(processed_data)

