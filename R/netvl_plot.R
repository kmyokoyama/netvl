netvl.plot.xy <- function(data, y.type, regression = TRUE) {
    if (!(y.type %in% colnames(data))) {
        stop("No such type in data frame.")
    }
    
    if (requireNamespace("ggplot2", quietly = TRUE)
       & requireNamespace("scales", quietly = TRUE)) {
        if (y.type == "revenue") {
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "maus", y = "revenue"))
            y.label <- "Revenue (in million USD)"
        }
        else if (y.type == "costs") {
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "maus", y = "costs"))
            y.label <- "Total costs (in million USD)"
        }
        else {
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "maus", y = y.type))
            y.label <- y.type
        }
        
        p <- p + ggplot2::theme_gray()
        p <- p + ggplot2::geom_point(size = 3, color = "black")
        p <- p + ggplot2::geom_point(size = 2, color = "darkblue")
        p <- p + ggplot2::geom_line(color = "darkblue")
        p <- p + ggplot2::xlab("Monthly Active Users (in million)")
        p <- p + ggplot2::ylab(y.label)
        
        if (regression)
            p <- p + ggplot2::geom_smooth(method = "lm", color = "purple")
        
        print(p)
    }
}

netvl.plot.ts <- function(data, ts.type = NULL, ylab = NULL, title = NULL, print = TRUE) {
    if (!is.null(ts.type)) {
        if (!(ts.type %in% colnames(data))) {
            stop("No such type in data frame.")
        }
    }
    
    if (requireNamespace("ggplot2", quietly = TRUE)
        &requireNamespace("scales", quietly = TRUE)) {
        if (is.null(ts.type)) {
            dat <- data.frame(date = as.vector(time(data)),
                              ts = as.vector(data))
            p <- ggplot2::ggplot(dat, ggplot2::aes_string(x = "date", y = "ts"))
            y.label = deparse(substitute(data))
        }
        else if (ts.type == "maus") {
            if (!("maus" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "date", y = "maus"))
            y.label <- "Monthly Active Users (in million)"
            p <- p + ggplot2::scale_x_date(breaks = "1 year", labels = scales::date_format("%b-%Y"))
        }
        else if (ts.type == "revenue") {
            if (!("revenue" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "date", y = "revenue"))
            y.label <- "Revenue (in million USD)"
            p <- p + ggplot2::scale_x_date(breaks = "1 year", labels = scales::date_format("%b-%Y"))
        }
        else if (ts.type == "costs") {
            if (!("costs" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "date", y = "costs"))
            y.label <- "Total costs (in million USD)"
            p <- p + ggplot2::scale_x_date(breaks = "1 year", labels = scales::date_format("%b-%Y"))
        }
        else {
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "date", y = ts.type))
            y.label <- ts.type
            p <- p + ggplot2::scale_x_date(breaks = "1 year", labels = scales::date_format("%b-%Y"))
        }

        if(!is.null(ylab)) {
            y.label = ylab
        }
        if(!is.null(title)) {
            title.label = title
        }
        else {
            title.label = "Time series"
        }
        
        p <- p + ggplot2::theme_light()
        p <- p + ggplot2::geom_point(size = 3, color = "black")
        p <- p + ggplot2::geom_point(size = 2, color = "darkblue")
        p <- p + ggplot2::geom_line(color = "darkblue")
        p <- p + ggplot2::xlab("Date") + ggplot2::ylab(y.label)
        p <- p + ggplot2::labs(title = title.label)
        
        if (print) {
            print(p)
        }
        else {
            return(p)
        }
    }
}
