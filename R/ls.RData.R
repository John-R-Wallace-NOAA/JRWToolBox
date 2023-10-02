ls.RData <- 
function (file) 
{
    local({
        base::load(file)
        base::ls()
    })
}
