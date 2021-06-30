lsp <- function(package, what, pattern) {

  # Adapted from: https://stackoverflow.com/questions/12114355/show-names-of-everything-in-a-package
  
  if (!is.character(substitute(package)))
    package <- deparse(substitute(package))
    
  suppressWarnings(package <- ifelse(is.na(as.numeric(package)), package, as.numeric(package)))
 
  if (is.numeric(package))
    package <- JRWToolBox::get.subs(search()[package], ':')[2]
   
  ns <- asNamespace(package)
  if (missing(pattern))
    pattern <- '.*'

  ## base package does not have NAMESPACE
  if (isBaseNamespace(ns)) {
    res <- ls(.BaseNamespaceEnv, all.names = TRUE)
    res_pat <- res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)]
    print(res_pat, quote = FALSE)
    return(invisible(res_pat))
  } else {
    ## for non base packages
    if (exists('.__NAMESPACE__.', envir = ns, inherits = FALSE)) {
      wh <- get('.__NAMESPACE__.', inherits = FALSE,
                envir = asNamespace(package, base.OK = FALSE))
      if (missing(what)) # Fixed from the Stack Over Flow reference
         what <- 'all'
      else if ('?' %in% what) return(ls(wh)) 
      else ls(wh)[pmatch(what[1], ls(wh))]
      if (!is.null(what) && !any(what %in% c('all', ls(wh))))
        stop('\'what\' should be one of ',
             paste0(shQuote(ls(wh)), collapse = ', '),
             ', or \'all\'', domain = NA)
      res <- sapply(ls(wh), function(x) getNamespaceInfo(ns, x))
      res <- rapply(res, ls, classes = 'environment',
                    how = 'replace', all.names = TRUE)
      if (is.null(what)) {
        res_pat <- res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)]
        print(res_pat, quote = FALSE)
        invisible(res_pat)
      }  
      if (what %in% 'all') {
        res <- ls(getNamespace(package), all.names = TRUE)
        res_pat <- res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)]
        print(res_pat, quote = FALSE)
        return(invisible(res_pat))
      }
      if (any(what %in% ls(wh))) {
        res <- res[[what]]
        res_pat <- res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)]
        print(res_pat, quote = FALSE)
        return(invisible(res_pat))
      }
    } else stop(sprintf('no NAMESPACE file found for package %s', package))
  }
}

