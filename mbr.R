

mbr = read.csv(file.choose(), header=TRUE)
ds = mbr[c("gender","marital_status","occupation", "total_visits")]
summary(ds)
  


