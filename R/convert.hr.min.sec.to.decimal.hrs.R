
convert.hr.min.sec.to.decimal.hrs <- function(hr.min.sec, sep = ':') {

    convert <- function(hr.min.sec, sep = ':')
    {
            if(hr.min.sec %in% "<NA>" | is.na(hr.min.sec))
                    return(NA)
    	    if(length(get.subs(hr.min.sec, sep = sep)) == 1 )
    		     tmp <- as.numeric(c(substr(hr.min.sec, 1, 2), substr(hr.min.sec, 3, 4), substr(hr.min.sec, 5, 6)))
    	    else		 
                 tmp <- as.numeric(get.subs(hr.min.sec, sep = sep))
    			 
            tmp[1] + tmp[2]/60 + tmp[3]/3600
    }
    

   apply(matrix(hr.min.sec, ncol = 1), 1, convert, sep = sep)
  
 } 


