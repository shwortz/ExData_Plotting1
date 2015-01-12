d <- read.table("household_power_consumption.txt", sep = ";", nrows = 5, header = T)
name <- names(d)
name <- gsub("_", " ", name)
simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1, 1)), substring(s, 2),
            sep = "", collapse = " ")
}
for(i in seq_along(name)) {
      name[i] <- simpleCap(name[i])
}


d <- read.table("household_power_consumption.txt", sep = ";", skip = 65000, nrows = 5000)
summary(which(d$V1 == "1/2/2007"))
summary(which(d$V1 == "2/2/2007"))
d <-d[1638:4517,]
names(d) <- name

##Checked for missing values ?
d.test  <-  d
for(i in seq_along(d.test)) {
      d.test[, i] <- as.character(d.test[,i])
}
l <- lapply(d.test, function(x) grepl("[?]", x))
lapply(l, table)

##
d$Date <- as.Date(d$Date, format= "%d/%m/%Y")
d$Time <- as.character(d$Time)
d$Time <- paste(d$Date, d$Time)
d$Time <- strptime(d$Time, format = "%Y-%m-%d %H:%M:%S")

##Plot 1
png(filename = "plot1.png", width = 480, height = 480)
hist(d[,3], main = "Global Active Power", xlab= "Global Active Power (kilowatts)", col = "red")
dev.off()