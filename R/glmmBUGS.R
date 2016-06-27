glmmBUGS <- function (formula, data, effects, modelFile = "model.bug", initFile = "getInits.R", 
    family = c("bernoulli", "binomial", "poisson", "gaussian"), link = "", invlink = "inverse",
    spatial = NULL, spatialEffect = NULL, reparam = NULL, prefix = NULL, program = "WinBUGS", ...) 
{

   catf <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {
      cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
      flush.console()
      invisible()
   }


    if(family == "lnorm") {
       family.PQL <- "Gamma"
       link.PQL <- "log"
       catf("\nWhen using the OpenBUGS error distribution of 'dlnorm', the initial values will be from a glmmPQL fit with gamma errors and a log link.\n\n")
       
    } else if(family == "bernoulli") {
       family.PQL <- "binomial"
       link.PQL <- "logit"

    } else {
       family.PQL <- family
       link.PQL <- link
    } 

    if(tolower(program) %in% "openbugs")

    data = getDesignMatrix(formula, data, effects)
    data = na.omit(data)

    covariates = attributes(data)$covariates
    observations = attributes(data)$response

    ragged = winBugsRaggedArray(data, effects = effects, covariates = covariates, 
        observations = observations, prefix = prefix, reparam = reparam)

    if (!is.null(spatial)) 
        ragged = addSpatial(spatial, ragged, spatialEffect, prefix = prefix)

    thepql = glmmPQLstrings(effects = effects, covariates = covariates, 
        observations = observations, data = data, family = eval(parse(text=paste(family.PQL, "(", link.PQL, ")", sep=""))), ...)

    startingValues = getStartingValues(pql = thepql, ragged = ragged, 
        prefix = prefix, reparam = reparam)
    
    if(family == "Gamma")
        startingValues$r <- 1  

    startingFunction(startingValues, file = "getInits.R")

    spatialEffect = grep("^N[[:graph:]]+Spatial$", names(ragged), 
        value = T)
    spatialEffect = gsub("^N", "", spatialEffect)
    spatialEffect = gsub("Spatial$", "", spatialEffect)
    effects = paste(prefix, effects, sep = "")
# ***
    program <- match.arg(tolower(program), c("winbugs", "openbugs"))

    writeBugsModel(file = modelFile, effects = effects, covariates = covariates, 
        observations = observations, family = family, link = link, invlink = invlink, spatial = spatialEffect, 
        prefix = attributes(ragged)$prefix, reparam = reparam, brugs = program %in% "openbugs")

    source("getInits.R")
    assign('startingValues', startingValues, pos = 1)
# ***
    return(list(ragged = ragged, startingValues = startingValues, 
        pql = thepql, program = program))

}
