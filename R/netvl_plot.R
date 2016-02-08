netvl.plot.xy <- function(data, y.type, regression = TRUE) {
    if(requireNamespace("ggplot2", quietly = TRUE)
       & requireNamespace("scales", quietly = TRUE)) {
        if (y.type == "revenue") {
            if (!("revenue" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "maus", y = "revenue"))
            y.label <- "Revenue (in million USD)"
        }
        else if (y.type == "costs") {
            if (!("costs" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "maus", y = "costs"))
            y.label <- "Total costs (in million USD)"
        }
        
        p <- p + ggplot2::theme_gray()
        p <- p + ggplot2::geom_point(size = 3, color = "black")
        p <- p + ggplot2::geom_point(size = 2, color = "darkblue")
        p <- p + ggplot2::geom_line(color = "darkblue")
        p <- p + ggplot2::xlab("Monthly Active Users (in million)")
        p <- p + ggplot2::ylab(y.label)
        
        if(regression)
            p <- p + ggplot2::geom_smooth(method = "lm", color = "purple")
        
        print(p)
    }
}

netvl.plot.ts <- function(data, ts.type) {
    if(requireNamespace("ggplot2", quietly = TRUE)
       &requireNamespace("scales", quietly = TRUE)) {
        if (ts.type == "maus") {
            if (!("maus" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "date", y = "maus"))
            y.label <- "Monthly Active Users (in million)"
        }
        else if (ts.type == "revenue") {
            if (!("revenue" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "date", y = "revenue"))
            y.label <- "Revenue (in million USD)"
        }
        else if (ts.type == "costs") {
            if (!("costs" %in% colnames(data))) {
                stop("No such type in data frame.")
            }
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "date", y = "costs"))
            y.label <- "Total costs (in million USD)"
        }
        
        p <- p + ggplot2::theme_light()
        p <- p + ggplot2::geom_point(size = 3, color = "black")
        p <- p + ggplot2::geom_point(size = 2, color = "darkblue")
        p <- p + ggplot2::geom_line(color = "darkblue")
        p <- p + ggplot2::scale_x_date(breaks = "1 year", labels = scales::date_format("%b-%Y"))
        p <- p + ggplot2::xlab("Date") + ggplot2::ylab(y.label)
        p <- p + ggplot2::labs(title = "Time series")
        
        print(p)
    }
}
