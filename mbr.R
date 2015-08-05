

mbr = read.csv(file.choose(), header=TRUE, na.strings=c("","NA"))

ds=mbr
#ds = mbr[c("gender","marital_status","occupation", "total_visits")]

df = data.frame(ds)

propmiss = function(df) {
  m = sapply(df, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d = data.frame(t(m))
  d = sapply(d, unlist)
  d = as.data.frame(d)
  d$variable = row.names(d)
  row.names(d) =NULL
  d = cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])

}

propmiss(df)





