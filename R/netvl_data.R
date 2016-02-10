netvl.read.csv <- function(file.name) {
    dat <- read.csv(file.name, header = T, na.strings = "NA")
    dat$date <- as.Date(dat$date, "%Y-%m-%d")
    
    return(dat)
}

netvl.write.csv <- function(data, file.name) {
    write.table(x = data, file  = file.name,
                sep = ", ", row.names = F, col.names = T, quote = F)
}

netvl.to.ts <- function(data, col.name, interval = "quarter") {
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
    day <- as.numeric(date.split[3])

    if (interval == "quarter") {
        start <- c(year, floor((month-1)/3)+1)
        freq = 4
    }
    else if (interval == "year") {
        start <- c(year)
        freq = 1
    }
    else if (interval == "month") {
        start <- c(year, month)
        freq = 12
    }
    else if (interval == "day") {
        start <- c(year, month, day)
        freq = 365
    }
    else {
        stop("Indefined interval.")
    }
    
    ts <- ts(col, start = start, frequency = freq)
    
    return(ts)
}
