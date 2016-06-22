ls  <- 
function(pattern = "[!.]*", pos = 1, all = F, long = F, wild = T, mode = "any", open = F, fix = F, 
	...)
{
#
#   DATE WRITTEN:  February 1995      LAST REVISED:   03 November 2005
#   AUTHOR:  John R. Wallace (jrw@fish.washington.edu)
#
	if(all) pos <- 1:length(search())

	if(!missing(pattern)) {
		if(!(is.character(substitute(pattern)))) {
			pattern <- deparse(substitute(pattern))
		}
		
                if(wild) 
		       pattern<-paste(pattern, "+", sep="")
	        if(F) {
		    cat("\n"); print(pattern); cat("\n")
		}
		""
		if(long) {
			ll(pattern = pattern)
			return(invisible())
		}
		else out <- lapply(as.list(pos), objects, pat = pattern, ...)
	}
	else {
		if(long)
			out <- lapply(as.list(pos), function(a.pos, mode)
			{
				obj.sum <- objects.summary(where = a.pos, mode = mode)
				obj.sum[order(obj.sum[, 1]),  ]
			}
			, mode = mode)
		else out <- lapply(as.list(pos), objects, pat = pattern)
	}
	if(length(out) == 1 & long == F) {
		out <- unlist(out)
		print(out, q = F)
	}
	else {
		xlen <- length(out)
		this <- paste("[[", 1:xlen, "]]", sep = "")
		for(i in 1:xlen) {
			if(length(out[[i]]) > 0) {
				if(is.data.frame(out[[i]]) == T) {
				  if(dim(out[[i]])[1] > 0) {
				    cat(this[i], ":\n", sep = "")
				    print(out[[i]], prefix = this[i], q = F)
				    cat("\n")
				  }
				}
				else {
				  cat(this[i], ":\n", sep = "")
				  print(out[[i]], prefix = this[i], q = F)
				  cat("\n")
				}
			}
		}
	}
	if(open) {
		for(i in 1:length(out)) {
			cat("\n\n***", out[i], "***\n")
			print(eval(parse(text = out[i])))
		}
	}
	if(length(out) == 1 & fix == T)
		assign(substitute(out), edit(eval(parse(text = out))), w = 1)
	invisible(out)
}

