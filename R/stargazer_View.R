
stargazer_View <- function(..., type = 'text', out = "tmp.html") {
    require(stargazer)
    stargazer(..., type = type, out = out)
    browseURL(paste0(getwd(), "/", out))
}



