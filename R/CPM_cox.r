CPM_cox = function(selected_features){
    library("survival")
    myoutf1 = output_path
	  load(myoutf1)
	  data = myList[["data"]]
    dropna = apply(!is.na(data[,c(1,2,selected_features)]), 1, all)
    data = data[dropna, ]
    colnames(data)[selected_features] = "mytf"
    mycox = coxph(Surv(t.rfs, e.rfs)~mytf, data)
    cat(dim(data))

    xx = cbind(data$t.rfs, mycox$linear.predictors, data$e.rfs)
    row.names(xx) = row.names(data)
    colnames(xx) = c("time", "score", "event")
    xx[, "score"] = -xx[, "score"]

    ##
    cc = 0
    dd = 0
    tt = 0
    cnum = nrow(xx)
    f.mat= matrix(0, cnum, 3)
    colnames(f.mat) = c("concordance", "discordance", "tie")
    row.names(f.mat) = row.names(data)
    f.mat= as.data.frame(f.mat)
    for(i in 1:(cnum-1))
      for(j in (i+1):cnum)
      {
        if(xx[i,"event"]==0 & xx[j,"event"]==0) next
        x1 = xx[i,"time"]-xx[j,"time"]
        x2 = xx[i,"score"]-xx[j,"score"]

        if(xx[i,"event"]==1 & xx[j,"event"]==1)
        {
            if(x2==0)
            {
                tt=tt+1
                f.mat[i,"tie"] = f.mat[i,"tie"] +1
                f.mat[j,"tie"] = f.mat[j,"tie"] +1
            }
            if(x1*x2>0)
            {
                cc = cc+1
                f.mat[i,"concordance"] = f.mat[i,"concordance"] +1
                f.mat[j,"concordance"] = f.mat[j,"concordance"] +1
            }
            if(x1*x2<0)
            {
                dd = dd+1
                f.mat[i,"discordance"] = f.mat[i,"discordance"] +1
                f.mat[j,"discordance"] = f.mat[j,"discordance"] +1
            }
            next
        }
        if(xx[i,"event"]==1 & xx[j,"event"]==0)
        {
            if(x1>0)  next
            if(x2==0)
            {
                tt=tt+1
                f.mat[i,"tie"] = f.mat[i,"tie"] +1
                f.mat[j,"tie"] = f.mat[j,"tie"] +1
            }
            if(x2<0)
            {
                cc = cc+1
                f.mat[i,"concordance"] = f.mat[i,"concordance"] +1
                f.mat[j,"concordance"] = f.mat[j,"concordance"] +1
            }
            if(x2>0)
            {
                dd = dd+1
                f.mat[i,"discordance"] = f.mat[i,"discordance"] +1
                f.mat[j,"discordance"] = f.mat[j,"discordance"] +1
            }
            next
        }
        if(xx[i,"event"]==0 & xx[j,"event"]==1)
        {
            if(x1<0)  next
            if(x2==0)
            {
                tt=tt+1
                f.mat[i,"tie"] = f.mat[i,"tie"] +1
                f.mat[j,"tie"] = f.mat[j,"tie"] +1
            }
            if(x2>0)
            {
                cc = cc+1
                f.mat[i,"concordance"] = f.mat[i,"concordance"] +1
                f.mat[j,"concordance"] = f.mat[j,"concordance"] +1
            }
            if(x2<0)
            {
                dd = dd+1
                f.mat[i,"discordance"] = f.mat[i,"discordance"] +1
                f.mat[j,"discordance"] = f.mat[j,"discordance"] +1
            }
        }
      }
    res = f.mat
    count = apply(f.mat, 1, sum)
    accuracy = (f.mat[, "concordance"] + 0.5 * f.mat[, "tie"])/count
    res = cbind(f.mat, count, accuracy)

    se = which(res[, "count"]>nrow(data)*0.2)
    res = res[se,]

    comxx = intersect(row.names(data), row.names(res))
    acc = res[comxx, "accuracy"]
    thr = summary(mycox)$concordance[1]
    Class = ifelse(acc>=thr, "Y", "N")
    data = cbind(data[comxx,], Class)
    data=rfImpute(Class~., data=data)
    data = cbind(subset(data, select=-1), subset(data, select=1))
    data = cbind(data, acc)
    myList[[2]] = thr
    names(myList)[2] = "threshold"
    myList[[3]] = data
    names(myList)[3] = "CPM_dataset"
    save(myList, file=myoutf1)
}
