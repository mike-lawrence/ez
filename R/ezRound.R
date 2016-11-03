ezROUND=function(res,digits = 5){
  tables=names(res)
  if ("ANOVA" %in% tables){
    cols=names(res$ANOVA)
    cols=cols[!cols=="p<.05" &!cols=="Effect"]
    for (icol in cols){
      res$ANOVA[[icol]]=round(res$ANOVA[[icol]],digits = digits)
    }
  }
  
  if ("Mauchly's Test for Sphericity" %in% tables){
    cols=names(res$"Mauchly's Test for Sphericity")
    cols=cols[!cols=="p<.05" &!cols=="Effect"]
    for (icol in cols){
      res$"Mauchly's Test for Sphericity"[[icol]]=round(res$"Mauchly's Test for Sphericity"[[icol]],digits = digits)
    }
  }
  
  if ("Sphericity Corrections" %in% tables){
    cols=names(res$"Sphericity Corrections")
    cols=cols[!cols=="p[GG]<.05" &!cols=="p[HF]<.05" &!cols=="Effect"]
    for (icol in cols){
      res$"Sphericity Corrections"[[icol]]=round(res$"Sphericity Corrections"[[icol]],digits = digits)
    }
  }
  
  if ("Levene's Test for Homogeneity of Variance" %in% tables){
    cols=names(res$"Levene's Test for Homogeneity of Variance")
    cols=cols[!cols=="p<.05" & !cols=="DFn" &!cols == "DFd"]
    for (icol in cols){
      res$"Levene's Test for Homogeneity of Variance"[[icol]]=round(res$"Levene's Test for Homogeneity of Variance"[[icol]],digits = digits)
    }
  }
return(res)
}



