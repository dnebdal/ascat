compareDF = function(df1,df2) {
  if (ncol(df1) != ncol(df2)) {
    printf("[FAIL]\tColcount differs: %d vs %d", ncol(df1), ncol(df2))
    return()
  }
  
  if (all(colnames(df1) == colnames(df2))) {
    printf("[ OK ]\t Colnames are identical\n")
  } else {
    printf("[FAIL]\tColnames differ\n")
  }
  
  if (nrow(df1) != nrow(df2)) {
    printf("[FAIL]\tRowcount differs: %d vs %d", nrow(df1), nrow(df2))
    return()
  }
  
  colcompare = colSums(df1==df2)
  rows=nrow(df1)
  
  if (all(colcompare == rows)) {
    printf("[ OK ]\t Contents are identical\n")
  } else {
    printf("[FAIL]\t%d/%d columns are different\n",
           sum(colcompare!=rows), rows)
  }
  
}



compareSegmentList = function(l1, l2) {
  if (length(l1) != length(l2)) {
    printf("[FAIL]\tLengths differ: %d vs %d", length(l1), length(l2))
    return()
  } 
  
  printf("[ OK ]\tSame number of samples\n")
  lengths1 = as.numeric(lapply(l1, length))
  lengths2 = as.numeric(lapply(l2, length))
  
  if (! all(lengths1==lengths2)) {
    printf("[FAIL]\tLenths differ in %d of %d samples\n", 
           sum(lengths1!=lengths2), length(l1))
    return()
  }
  printf("[ OK ]\tSamples have the same lengths\n")
  
  identical=0
  for (i in 1:length(l1)) {
    if (all(l1[[i]][,1]==l2[[i]][,1]) & 
        all(rownames(l1[[i]]) == rownames(l2[[i]]))) {
      identical = identical + 1
    }
  }
  
  if (identical != length(l1)) {
    printf("[FAIL]\t%d of %d samples are different\n",
           length(l1)-identical, length(l1))
  } else {
    printf("[ OK ]\tAll samples are identical\n")
  }
}

ascat.compareObj = function(obj1, obj2) {
  printf("Comparing Tumor_LogR:\n")
  compareDF(obj1$Tumor_LogR, obj2$Tumor_LogR)
  
  printf("Comparing Tumor_BAF:\n")
  compareDF(obj1$Tumor_BAF, obj2$Tumor_BAF)
  
  if (is.null(obj1$Tumor_LogR_segmented) != 
      is.null(obj2$Tumor_LogR_segmented)) {
    printf("[FAIL]\tOnly one has Tumor_LogR_segmented\n") 
  } else if (!is.null(obj1$Tumor_LogR_segmented)) {
    printf("Comparing Tumor_LogR_segmented:\n")
    compareSegmentList(obj1$Tumor_LogR_segmented, obj2$Tumor_LogR_segmented)
  } else {printf("Neither has Tumor_LogR_segmented.\n")}
  
  if (is.null(obj1$Tumor_BAF_segmented) != 
      is.null(obj2$Tumor_BAF_segmented)) {
    printf("[FAIL]\tOnly one has Tumor_BAF_segmented\n") 
  } else if (!is.null(obj1$Tumor_BAF_segmented)) {
    printf("Comparing Tumor_BAF_segmented:\n")
    compareSegmentList(obj1$Tumor_BAF_segmented, obj2$Tumor_BAF_segmented)
  } else {printf("Neither has Tumor_BAF_segmented.\n")}
  
}

ascat.compareResult = function(obj1, obj2) {
  printf("Testing nA:\n")
  compareDF(obj1$nA, obj2$nA)
  
  printf("Testing nB:\n")
  compareDF(obj1$nB, obj2$nB)
  
  printf("Testing ploidy:\n")
  if(all(obj1$ploidy == obj2$ploidy)) {
    printf("[ OK ]\tIdentical\n")
  } else {
    printf("[FAIL]\tDifferent")
  }
}
