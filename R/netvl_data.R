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

.netvl.subset.stock <- function(report.df, stock.df,
                                by.mean = FALSE, include.first = TRUE) {
    if (by.mean == FALSE & include.first == FALSE) {
        stop("Specify method.")
    }
    else if (by.mean == FALSE & include.first == TRUE) {
        subset.stock <- NULL
        
        for (i in 1:nrow(report.df)) {
            date.i <- report.df[i, "date"]
            s <- NULL
            date.range <- stock.df$date >= date.i
            s <- stock.df[date.range, ][1, ]
            if (!any(date.range))  { break }
            if (!is.null(s)) {
                subset.stock <- rbind(subset.stock, s)
            }
        }
        
        rownames(subset.stock) <- 1:nrow(subset.stock)
    }
    else if(by.mean == TRUE & include.first == FALSE) {
        subset.stock <- NULL
        dates <- numeric(0)
        class(dates) <- "Date"
        i <- 1
        
        for (i in 1:nrow(report.df)) {
            date.prev <- report.df[ifelse(i == 1, 1, (i-1)), "date"]
            date.i <- report.df[i, "date"]
            s <- NULL
            
            if (i == 1) {
                date.range <- (stock.df$date <= date.i)
                if (!any(date.range)) { break }
                s <- stock.df[date.range, ]
            }
            else {
                date.range <- (stock.df$date >= date.prev & stock.df$date <= date.i)
                if (!any(date.range)) { break }
                s <- stock.df[date.range, ]
            }
            
            date.last <- s[nrow(s), "date"]
            dates <- c(dates, date.last)
            s <- s[, 2:ncol(s)]
            
            if (!is.null(s)) {
                s <- as.vector(apply(as.matrix(s), 2, mean, na.rm = T))
                subset.stock <- rbind(subset.stock, s)
            }
        }
        
        subset.stock <- data.frame(matrix(subset.stock, nrow = nrow(subset.stock)))
        subset.stock <- cbind(dates, subset.stock)
        
        names(subset.stock) <- names(stock.df)
    }
    else if(by.mean == TRUE & include.first == TRUE) {
        subset.stock <- NULL
        dates <- numeric(0)
        class(dates) <- "Date"
        i <- 1
        
        for (i in 1:nrow(report.df)) {
            date.prev <- report.df[ifelse(i == 1, 1, (i-1)), "date"]
            date.i <- report.df[i, "date"]
            s <- NULL
            
            if (i == 1) {
                date.range <- (stock.df$date <= date.i)
                if (!any(date.range)) { break }
                s <- stock.df[date.range, ]
                date.next <- (stock.df$date >= date.i)
                if (any(date.next)) {
                    s <- rbind(s, stock.df[date.next, ][1, ])
                }
            }
            else {
                date.range <- (stock.df$date >= date.prev & stock.df$date <= date.i)
                if (!any(date.range)) { break }
                date.range[min(which(date.range == TRUE))] <- FALSE
                s <- stock.df[date.range, ]
                date.next <- (stock.df$date >= date.i)
                if (any(date.next)) {
                    s <- rbind(s, stock.df[date.next, ][1, ])
                }
            }
            
            date.last <- s[nrow(s), "date"]
            dates <- c(dates, date.last)
            s <- s[, 2:ncol(s)]
            
            if (!is.null(s)) {
                s <- as.vector(apply(as.matrix(s), 2, mean, na.rm = T))
                subset.stock <- rbind(subset.stock, s)
            }
        }
        
        subset.stock <- data.frame(matrix(subset.stock, nrow = nrow(subset.stock)))
        subset.stock <- cbind(dates, subset.stock)
        
        names(subset.stock) <- names(stock.df)
    }
    
    return(subset.stock)
}
