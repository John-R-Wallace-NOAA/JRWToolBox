check.dump.file <- function (dump.location) 
{
    local({
        load(dump.location)
        base::ls()
    })
}
