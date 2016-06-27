function (pos = 1, unit = "KB", digits = 0, dim = FALSE, sort = FALSE, 
    class = NULL, invert = FALSE, ...) 
{

      sort.f <- function(x, col = 1, reverse = F, renumber = T)
      {
             if(reverse)
                      out <- x[rev(do.call(order, x[, col, drop=F])),  ]
              else out <- x[do.call(order, data.frame(x[, col, drop=F])),  ]
              if(renumber) {
                      if(is.data.frame(x))
                              dimnames(out)[[1]] <- 1:nrow(out)
             }
             out
      }

      renum <- function(x, no.num = F)
      {
              if(no.num)
                      dimnames(x)[[1]] <- rep("", nrow(x))
              else dimnames(x)[[1]] <- 1:nrow(x)
              x
      }

# ------------------------------------------------------------------------------------------

    get.object.class <- function(object.name, pos) {
        object <- get(object.name, pos = pos)
        class <- class(object)[1]
        return(class)
    }
    get.object.dim <- function(object.name, pos) {
        object <- get(object.name, pos = pos)
        if (class(object)[1] == "function") 
            dim <- ""
        else if (!is.null(dim(object))) 
            dim <- paste(dim(object), collapse = " x ")
        else dim <- length(object)
        return(dim)
    }
    get.object.size <- function(object.name, pos) {
        object <- get(object.name, pos = pos)
        size <- try(unclass(object.size(object)), silent = TRUE)
        if (class(size) == "try-error") 
            size <- 0
        return(size)
    }
    unit <- match.arg(unit, c("bytes", "KB", "MB"))
    denominator <- switch(unit, KB = 1024, MB = 1024^2, 1)
    original.rank <- NULL
    if (is.character(pos)) 
        pos <- match(pos, search())
    if (is.list(pos)) {
        if (is.null(names(pos))) 
            stop("All elements of a list must be named")
        original.rank <- rank(names(pos))
        pos <- as.environment(pos)
    }
    if (length(ls(pos, ...)) == 0) {
        object.frame <- data.frame()
    }
    else if (environmentName(as.environment(pos)) == "Autoloads") {
        object.frame <- data.frame(rep("function", length(ls(pos, 
            ...))), rep(0, length(ls(pos, ...))), row.names = ls(pos, 
            ...))
        if (dim) {
            object.frame <- cbind(object.frame, rep("", nrow(object.frame)))
            names(object.frame) <- c("Class", unit, "Dim")
        }
        else names(object.frame) <- c("Class", unit)
    }
    else {
        class.vector <- sapply(ls(pos, ...), get.object.class, 
            pos = pos)
        size.vector <- sapply(ls(pos, ...), get.object.size, 
            pos = pos)
        size.vector <- round(size.vector/denominator, digits)
        object.frame <- data.frame(class.vector = class.vector, 
            size.vector = size.vector, row.names = names(size.vector))
        names(object.frame) <- c("Class", unit)
        if (dim) 
            object.frame <- cbind(object.frame, Dim = sapply(ls(pos, 
                ...), get.object.dim, pos = pos))
    }
    if (!sort && !is.null(original.rank)) 
        object.frame <- object.frame[original.rank, ]
    if (!is.null(class)) {
        include <- object.frame$Class %in% class
        if (invert) 
            include <- !include
        object.frame <- object.frame[include, ]
    }

    object.frame <- sort.f(renum(cbind(row.names(object.frame), object.frame[,1:2])),3)
    return(object.frame)
}
<environment: namespace:gdata>
