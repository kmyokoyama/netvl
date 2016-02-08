netvl.read.csv <- function(file.name) {
    dat <- read.csv(file.name, header = T, na.strings = "NA")
    dat$date <- as.Date(dat$date, "%Y-%m-%d")
    
    return(dat)
}

netvl.write.csv <- function(data, file.name) {
    write.table(x = data, file  = file.name, sep = ", ", row.names = F)
}

netvl.to.ts <- function(data, col.name) {
    if (!(col.name %in% colnames(data))) {
        stop("No such type in data frame.")
    }
    
    col <- data[, col.name]
    
    if (!("date" %in% colnames(data))) {
        stop("Data invalid. First column must be 'date'")
    }
    
    start.date <- as.character(data[1, "date"])
    date.split <- unlist(strsplit(start.date, split = "-"))
    year <- as.numeric(date.split[1])
    month <- as.numeric(date.split[2])
    quarter <- floor((month-1)/3)+1
    ts <- ts(col, start=c(year, quarter), frequency=4)
    
    return(ts)
}
