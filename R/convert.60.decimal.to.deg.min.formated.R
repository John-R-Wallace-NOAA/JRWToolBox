convert.60.decimal.to.deg.min.formated <- function(x, minute.round = 3, sep=":") {
        
        format.f <- function(x, digits = 2, Max.digits = F) {
	   if(Max.digits) {
		save <- options(digits = 17)$digits
		on.exit(options(digits = save))
	   }
	   assign("digits", digits, pos = 1)
	   FUNC <- function(x)
	   {
		format(round(c(x, as.numeric(paste(rep(1, digits + 1), collapse = "", sep = ""))/10^digits), digits))[1]
	   }
	   apply(matrix(x, ncol = 1), 1, FUNC)
         }

         grep.tf <- function(pattern, text) {
	    (1:length(text)) %in% grep(pattern, text)
         }

	scalar.convert <- function(x, minute.round = NULL) {
		x <- abs(x)
		deg <- trunc(x)
		min <- (x - deg) * 60
		if(!is.null(minute.round)) {
			if(round(min, minute.round) == 60 & !is.na(min)) {
				min <- 0
				deg <- deg + 1
			}
			min.char <- format.f(min, minute.round)
		}
		else min.char <- as.character(min)
		tf <- min < 10
		tf[is.na(tf)] <- F
		min.char[tf] <- paste("0", min.char[tf], sep = "")
		out <- paste(deg, sep, min.char, sep = "")
		out[grep.tf("*NA*", out)] <- ""
		out
	}

	apply(matrix(x, ncol = 1), 1, scalar.convert, minute.round = minute.round)
}

