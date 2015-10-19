summarycol<-function(c){
  
print(paste("dt", dim(dt)[1]))
  print(summary(dt[c]))  
print(paste("sub.0", dim(sub.0)[1]))
  print(summary(sub.0[c]))
print(paste("sub.1", dim(sub.1)[1]))
  print(summary(sub.1[c]))
print(paste("sub.2", dim(sub.2)[1]))
  print(summary(sub.2[c]))
print(paste("sub.3", dim(sub.3)[1]))
  print(summary(sub.3[c]))
print(paste("sub.4", dim(sub.4)[1]))
  print(summary(sub.4[c]))
print(paste("sub.5", dim(sub.5)[1]))
  print(summary(sub.5[c]))
print(paste("sub.6", dim(sub.6)[1]))
  print(summary(sub.6[c]) )
}

barplotall<-function(c){
  summarycol(c)
  layout(matrix(1:8, ncol = 4))
  barplot(table(dt[c]), main = paste("dt$",c))
  barplot(table(sub.0[c]), main = paste("sub.0$",c))
  barplot(table(sub.1[c]), main = paste("sub.1$",c))
  barplot(table(sub.2[c]), main = paste("sub.2$",c))
  barplot(table(sub.3[c]), main = paste("sub.3$",c))
  barplot(table(sub.4[c]), main = paste("sub.4$",c))
  barplot(table(sub.5[c]), main = paste("sub.5$",c))
  barplot(table(sub.6[c]), main = paste("sub.6$",c))
}

factorToIndicators<-function(df, f){
  fct<-df[,f]
  lev<-levels(fct)
  l<-length(lev)
  for(i in 1:l){
    levelName<-lev[i]
    cond<-df[f] == levelName
    df[paste("l_",levelName, sep = "")]<-if(cond) {
      1
    } else {
      0
    }
  }
}

dropBind<-function(df, m){
  ss<-data.frame(m)
  drops <- c("X.Intercept.")
  ss<-ss[,!(names(ss) %in% drops)]
  df<-cbind(df,ss)
  df
}

toIndicatorsEx<-function(df, f, v){
  cond<-df[f] == v
  sapply(df,comparer(v))
}


comparer<-function(a,b){if(a == b) {
  1
} else {
  0
}}

