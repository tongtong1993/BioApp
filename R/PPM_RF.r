PPM_RF = function(selected_features){
    library("randomForest")
    myoutf1 = output_path
    load(myoutf1)
    data = myList[["CPM_dataset"]]
    dropna = apply(!is.na(data[, selected_features]), 1, all)
    x = data[dropna, selected_features]
    y = as.factor(data[dropna, "Class"])
    xx = table(y)
    sam.siz = min(xx)
    myfit_RF_PPM = randomForest(x, y, sampsize = c(sam.siz, sam.siz),
        replace = T, ntree = 10000)
    Class = ifelse(predict(myfit_RF_PPM, type="response") == y, "Y","N")
    C_score = predict(myfit_RF_PPM, type="prob")
    C_score = C_score[,"Y"]
    data = cbind(Class,x,C_score)
    print("#########")
    print(data)
    len = length(myList)
    myList[[len+1]] = data
    names(myList)[len+1] = "PPM_dataset"
    save(myList, file=myoutf1)

    pdat = x[y=="Y", ]
    Class = rep("Y",nrow(pdat))
    pdat = cbind(Class, pdat)
    ndat = x[y=="N", ]
    Class = rep("N", nrow(ndat))
    ndat = cbind(Class, ndat)
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

            fit <- randomForest(Class~., sampsize=c(sam.siz, sam.siz), replace=T, data=tr, ntree=10000)
            tmp = predict(fit, te[,-1], type="prob")
            tmp = data.frame(te[,1], tmp)
            myres = rbind(myres, tmp)
        }


        thr = (1:99)*0.01
        yy =  xx =  rep(0, length(thr))
        fdr = rep(0,99)
        for(i in 1:length(thr))
        {
                aa = sum(myres[,"Y"]>=thr[i] & myres[,1]=="Y")
                bb = sum(myres[,"Y"]<thr[i] & myres[,1]=="Y" )
                cc = sum(myres[,"Y"]>=thr[i] & myres[,1]=="N")
                dd = sum(myres[,"Y"]<thr[i] & myres[,1]=="N")
                fdr[i] = aa/sum(myres[,2]>=thr[i])
                yy[i] = aa/(aa+bb)
                xx[i] = cc/(cc+dd)
        }
        xx = c(1, xx, 0)
        yy = c(1, yy, 0)
        tmp1 = tmp2 = rep(0,100)
        for(i in 1:100)
        {
            tmp1[i] = xx[i]-xx[i+1]
            tmp2[i] = (yy[i+1]+yy[i])/2
        }
        myauc[p] =  sum(tmp1*tmp2)
        myx = myx+xx
        myy = myy+yy
    }
    myauc2 = mean(myauc)
    myx2 = myx/10
    myy2 = myy/10
    ##  AUC = 0.8457247

    load(file= myoutf1)
    myList[[len+2]] = myauc2
    names(myList)[len+2] = "AUC.PPM.RF"
    myList[[len+3]] = cbind(myx2, myy2)
    names(myList)[len+3] = "ROC.PPM.RF"
    save(myList, file=myoutf1)


    load(file= myoutf1)
    myList[[len+4]] = myfit_RF_PPM[["importance"]]
    names(myList)[len+4] = "PPM.RF.RelativeImportance"
    save(myList, file=myoutf1)
}
