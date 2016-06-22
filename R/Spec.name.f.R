Spec.name.f <-
function(RACE.num, year = "052002")
{
        Spec.code.name <- paste("Spec.code.", year, sep = "")
        match.f(data.frame(RACE.num), eval(parse(text = Spec.code.name)), 1, 1, 2:3)
}



