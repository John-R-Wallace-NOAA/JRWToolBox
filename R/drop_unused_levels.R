function (data) 
as.data.frame(lapply(data, function(x) if (is.factor(x)) factor(x) else x))
