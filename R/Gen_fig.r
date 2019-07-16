Gen_fig = function(){
    f = c()
    c = c()
    load(output_path)
    path = output_path
    path = unlist(strsplit(path, "/"))
    len = length(path)
    path = path[1:len-1]
    path = paste(path, collapse = "/")

    for(x in 1:ncol(myList[["PPM_dataset"]])){
        Class = class(myList[["PPM_dataset"]][,x])
        print(Class)
        if(Class == "factor"){
            f = c(f,x)
        } else
            c = c(c,x)
    }
    f = f[-1]
    if (length(c)>=1){
        for(ind in c){
            load(output_path)
            name = colnames(myList[["PPM_dataset"]])[ind]
            name = paste("fig:",name, sep="")
            data = myList[["PPM_dataset"]][,c(1,ind)]
            ggplot(data, aes(x=as.factor(data$Class), y=data[,2])) +
            geom_boxplot(fill="slateblue", alpha=0.2) +
            xlab("correct/incorrect prediction")+
            ylab(colnames(data)[2])+
            ggsave(paste(name,".jpeg",sep=""), device="jpeg", path=path)
        }
    }

    if (length(f)>=1){
        for (ind in f){
            load(output_path)
            name = colnames(myList[["PPM_dataset"]])[ind]
            name = paste("fig:",name, sep="")
            data = myList[["PPM_dataset"]][,c(1,ind)]

            data_Y = data[data$Class == "Y",]
            data_N = data[data$Class == "N",]
            df = rbind(table(data_Y[,2]),table(data_N[,2]))
            df = t(df)
            df = as.data.frame(df)
            colnames(df) = c("Y","N")
            df = cbind(rownames(df),df)
            colnames(df)[1] = "Class"
            rownames(df) = NULL
            df = melt(df, id.var="Class")
            df = arrange(df, Class, desc(variable))
            #print(df)
            df = ddply(df, .(Class), transform, percent = value/sum(value) * 100)
            df = ddply(df, .(Class), transform, pos = cumsum(value) - 0.5 * value)
            #print(df)
            df$label = paste0(sprintf("%.0f", df$percent), "%")
            print(df)

            ggplot(df, aes(x = Class, y = value, fill=variable))+
            geom_bar(stat = "identity")+
            geom_text(aes(y = pos, label = label), size = 2)+
            ggtitle(name)+
            ggsave(paste(name,".jpeg",sep=""), path=path)
        }
    }
    df = myList[["PPM.RF.RelativeImportance"]]
    df = as.data.frame(df)
    Class = rownames(df)
    df = cbind(df,Class)

    ggplot(df, aes(x=rownames(df), y=MeanDecreaseGini))+
    geom_bar(stat="identity")+
    xlab("Class")+
    ylab("relative_importance")+
    ggsave("PPM.RF.RelativeImportance.jpeg", path=path)
}
