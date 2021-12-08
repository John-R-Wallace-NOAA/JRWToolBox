
stargazer_View <- function(..., type = 'text', out = "tmp.html") {
    # https://stackoverflow.com/questions/22244936/output-for-stargazer-in-r
    require(stargazer)
    stargazer(..., type = type, out = out)
    browseURL(paste0(getwd(), "/", out))
}


