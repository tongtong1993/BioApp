CPM_RF = function(selected_features){
    library("randomForest")
	  myoutf1 = output_path
	  load(myoutf1)
	  data = myList[["data"]]
	  dropna = apply(!is.na(data[,c(1,selected_features)]), 1, all)
	  data = data[dropna, ]
	  xx = table(data[, 1])
    sam.siz = min(xx)
    y = as.factor(data[, 1])
    x = data[, selected_features]
    ##clinical_input = rbind(clinical_input[, class_index],clinical_input[, selected_features])
    myfit_RF_CPM = randomForest(x, y, sampsize=c(sam.siz, sam.siz), replace=T, ntree=10000)
    Class = ifelse(predict(myfit_RF_CPM, type = "response") == y, "Y", "N")
    Prediction = predict(myfit_RF_CPM, type = "response")
    data = cbind(data, Prediction, Class)
    myList[[2]] = data
    names(myList)[2] = "CPM_dataset"

    ## 10 fold cross validation for cpm
    label = levels(data[, 1])
    pdat = data[data[, 1]==label[1], ]
    #print(pdat)
    ndat = data[data[, 1]==label[2], ]
    #print(ndat)
    kk = 10
    psiz = floor(nrow(pdat)/kk)
    nsiz = floor(nrow(ndat)/kk)
    myauc = rep(0, 10)
    myx = myy = rep(0, 101)
    for(p in 1:10)
    {
        myres = NULL
        pdat = pdat[sample(1:nrow(pdat)),]
        ndat = ndat[sample(1:nrow(ndat)),]
        for(k in 1:kk)
        {
            cat("\r\r\r", p, "-->", k)
            if(k==kk)
            {
                se = ((k-1)*psiz+1):nrow(pdat)
            }else
            {
                se = ((k-1)*psiz+1):(k*psiz)
            }
            ptr = pdat[-se,]
            pte = pdat[se,]
            if(k==kk)
            {
                se = ((k-1)*nsiz+1):nrow(ndat)
            }else
            {
                se = ((k-1)*nsiz+1):(k*nsiz)
            }
            ntr = ndat[-se,]
            nte = ndat[se,]

            tr = rbind(ptr, ntr)
            te = rbind(pte, nte)
            sam.siz = min(nrow(ptr), nrow(ntr))
            x = tr[,selected_features]
            y = as.factor(tr[,1])
            fit = randomForest(x, y, sampsize=c(sam.siz, sam.siz), replace=T, ntree=10000)
            tmp = predict(fit, te[,selected_features], type="prob")
            tmp = data.frame(te[,1], tmp)
            myres = rbind(myres, tmp)
            }
        print(colnames(myres))
        print(myres)
        #break
        thr = (1:99)*0.01
        yy =  xx =  rep(0, length(thr))
        fdr = rep(0,99)
        for(i in 1:length(thr))
        {
            aa = sum(myres[,label[1]]>=thr[i] & myres[,1]==label[1]) #true positive
            bb = sum(myres[,label[1]]<thr[i] & myres[,1]==label[1] ) #false negative
            cc = sum(myres[,label[1]]>=thr[i] & myres[,1]==label[2]) #false positive
            dd = sum(myres[,label[1]]<thr[i] & myres[,1]==label[2]) #true negative
            fdr[i] = aa/sum(myres[,2]>=thr[i])
            yy[i] = aa/(aa+bb) #sensitivity
            xx[i] = cc/(cc+dd) #false positive rate
        }
        xx = c(1, xx, 0)
        yy = c(1, yy, 0)
        tmp1 = tmp2 = rep(0,100)
        for(i in 1:100)
        {
            tmp1[i] = xx[i]-xx[i+1]
            tmp2[i] = (yy[i+1]+yy[i])/2
        }
        myauc[p] = sum(tmp1*tmp2)
        myx = myx+xx
        myy = myy+yy
    }
    myauc1 = mean(myauc)
    myauc1
    myx1 = myx/10
    myy1 = myy/10

    myList[[3]] = myauc1
    names(myList)[3] = "AUC.CPM.RF"
    myList[[4]] = cbind(myx1, myy1)
    names(myList)[4] = "ROC.CPM.RF"
    myList[[5]] = importance(myfit_RF_CPM)
    names(myList)[5] = "CPM.relative.importance"
    save(myList, file=myoutf1)
}
