preprocess = function(CPM_type, raw_data, class_index){
    myList = list(NULL)
    myoutf1 = output_path
    data = cbind(subset(raw_data, select=class_index), subset(raw_data, select=-class_index))
    if (CPM_type == "cox"){
        colnames(data)[c(1,2)] = c("t.rfs", "e.rfs")
    }
    myList[[1]] = data
    names(myList)[1] = "data"
    save(myList, file=myoutf1)
    for(x in 1:ncol(data)){
        txt = paste(x,": ",colnames(data)[x])
        print(txt)
    }
}