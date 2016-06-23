convert.hr.min.to.decimal.hrs <- function(hr.min) {

  convert <- function(hr.min) {
                if(is.na(hr.min))
                return(NA)
                tmp <- as.numeric(get.subs(hr.min, sep = ":"))
                tmp[1] + tmp[2]/60
          }

  apply(matrix(hr.min, ncol=1), 1, convert)
}
