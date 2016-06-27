WS <- function () 
{
    browseURL("http://www.atmos.washington.edu/cgi-bin/latest.cgi?ir")
    Sys.sleep(1)
    browseURL("http://www.atmos.washington.edu/cgi-bin/latest.cgi?fronts-ir")
}
