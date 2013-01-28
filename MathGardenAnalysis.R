# an attempt to analyze data sets resulting from the Maths Garden - spacing test

# NOTE: this entirely eliminates from consideration 3 potentially important vectors from the original data set
# log27 (which had many other problems than those in log27Reduced) 
# itemlog27
# userswide

# libraries
require(sciplot)
require(plyr)
require(data.table)
wd <- "/Users/dlandy/Dropbox/Projects/Math\ Garden/"
setwd(wd)

source(paste(wd, "loadMathGardenDataSets.R", sep="/"))

# Load Data:
c(log27Reduced, items, itemsBySexAndGrade, itemsByGrade) := loadMathGardenDataSets()

#if(exists("itemsOrig")){ rm(itemsOrig) }
# Figures and Analyses

plotThoseDatas <- function(items, type, pch=c(5,6), x.leg=1.6, mainTitle="", foilList = c(), foilDescList=c(), fill=F){
  # Foil list should be a list of literal column names, like "Foil3Rate".  
  with(items[(items$type==type) ,], {
    lineplot.CI(feature1, rating, feature2, type="b", main="Rating", ylab="Rating",  pch=pch, x.leg=x.leg)
    lineplot.CI(feature1, rating-start_rating, feature2, type="b", main="Ratings Shift", pch=pch, x.leg=x.leg)
    lineplot.CI(feature1, 1-correctRate,  feature2,type="b", main="Error Rate", pch=pch, x.leg=x.leg)
    lineplot.CI(feature1, foilRate,  feature2,type="b", main="Foil Rate", pch=pch, x.leg=x.leg)
  })
  title(mainTitle, outer=TRUE)
  if(length(foilDescList) < length(foilList)){
    foilDescList <- foilList
    cat("foilDescList was too short: replacing with column descriptors")
  }
  if(length(foilList>0)){
    for(i in 1:length(foilList)){
      lineplot.CI(items[(items$type==type), "feature1"],
                  items[(items$type==type), foilList[i]],
                  items[(items$type==type), "feature2"],
                  type="b", 
                  main=foilDescList[i],
                  pch=pch, x.leg=x.leg
      )
    }
  }
  if(fill){ #Fill in the rest of the page with empty plots
    if(((4-(length(foilList) %% 4)) %% 4)>0){
    for (i in 1:((4-(length(foilList) %% 4)) %% 4)){
      cat("i:", i, " l(f):", length(foilList))
      lineplot.CI(items[(items$type==type), "feature1"],
                  items[(items$type==type), "feature1"]==items[(items$type==type), "feature1"],
                  items[(items$type==type), "feature2"],
                  type="b", 
                  main="filler plot",
                  pch=pch, x.leg=x.leg)
    }
    }
  }
}



pdf("FiguresWithFoils.pdf")
par(mfrow=c(2,2), oma=c(0,0,1,0))
pch <- c(5,6)
x.leg <- 1.6

plotThoseDatas(items, "1", mainTitle="Family 1: +x vs. x+ and Spacing", 
               foilList = c("foil2Rate", "foil3Rate", "foil4Rate", "foil5Rate"),
               foilDescList = c("Multiplying all terms", "Adding all terms", "+x Swaps", "x+ Swaps"),
               fill=T)
plotThoseDatas(items, "2", mainTitle="Family 2: a+a+a : 3 vs a + b + c : 3")

with(items[(items$type=="3") ,], {
  feature_1 <- factor(feature1, levels=c("plus_double", "all_diff", "times_double"))  
  lineplot.CI(feature_1, rating, feature2, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, rating-start_rating, feature2,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, 1-correctRate, feature2,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, foilRate, feature2,type="b", main="Foil Rate",  pch=pch, x.leg=x.leg)
  title("Family 3: + and x, paired terms", outer=TRUE)
  lineplot.CI(feature_1, foil2Rate, feature2,type="b", main="Multiplying all terms",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, foil3Rate, feature2,type="b", main="Adding all terms",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, foil2Rate==foil2Rate, feature2,type="b", main="Filler",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, foil2Rate==foil2Rate, feature2,type="b", main="Filler",  pch=pch, x.leg=x.leg)
  
})
title("Family 3: + and x, paired terms", outer=TRUE)

plotThoseDatas(items, "4", mainTitle="Family 4: spatial congruency, division and subtraction style")


plotThoseDatas(items, "5", mainTitle="Family 5: Does ones digit simplicity lure?",
               foilList = c("foil2Rate", "foil3Rate"),
               foilDescList = c("A-B-C", "A-B+C"),
               fill=T)
plotThoseDatas(items, "6", mainTitle="Family 6: Does ones digit simplicity lure in double subtractions and double divisions?")

plotThoseDatas(items, "7", mainTitle="Family 7: numeric identity and /X problems")

with(items[(items$type=="8") ,], {
  type_2 <- factor(floor((type2-1)/2), ordered=F)
  pch<-c(5,6,7,8)
  lineplot.CI(feature1, rating, type_2, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature1, rating-start_rating, type_2,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature1, 1-correctRate, type_2,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature1, foilRate, type_2,type="b", main="Foil Rate",  pch=pch, x.leg=x.leg)
})
title("Family 8.  Balance and perceptual symmetry overriding left-to-right order of evaluation.", outer=TRUE)

plotThoseDatas(items, "9", mainTitle="Family 9: Magnitude Proximity and +X",
               foilList = c("foil2Rate", "foil3Rate"),
               foilDescList = c("Multiplying all terms", "Adding all terms"),
               fill=T)

with(items[(items$type=="10") ,], {
  pch<-c(5,6,7,8)
  lineplot.CI(feature2, rating, feature1, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature2, rating-start_rating, feature1,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature2, 1-correctRate, feature1,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature2, foilRate, feature1,type="b", main="Foil Rate",  pch=pch, x.leg=x.leg) 
  title("Family 10: a*b+c*d", outer=TRUE)
  
  lineplot.CI(feature1, foil2Rate, feature2,type="b", main="Left-Right Order",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature1, foil3Rate, feature2,type="b", main="Adding all terms",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature1, foil2Rate==foil2Rate, feature2,type="b", main="Filler",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature1, foil2Rate==foil2Rate, feature2,type="b", main="Filler",  pch=pch, x.leg=x.leg)
  
})
title("Family 10: a*b+c*d", outer=TRUE)


plotThoseDatas(items, "11", mainTitle="Family 11: Spacing and Mal-Rules", 
               foilList = c("foil2Rate", "foil3Rate"),
               foilDescList = c("Left-Right", "Adding all terms"),
               fill=T)

with(items[((items$type=="12") & (items$feature2=="multdiv")) ,], {
  feature_1 <- factor(feature1, levels=c("cong", "neutral", "incong"), ordered=T)
  lineplot.CI(feature_1, rating, feature2, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, rating-start_rating, feature2,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, 1-correctRate, feature2,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, foil2Rate, feature2,type="b", main="Timeout",  pch=pch, x.leg=x.leg)
})
title("Family 12A.  Spacing, associativity and efficiency, */ formats: congruency mislabeled.", outer=TRUE)

items$feature3 <- items$feature1
items$feature3[items$feature3=="leftbig"] <- "cong"
items$feature3[items$feature3=="rightbig"] <- "incong"
items$feature3 <- factor(items$feature3, levels=c("incong", "neutral", "cong"), ordered=T)


itemsBySexAndGrade$feature3 <- itemsBySexAndGrade$feature1
itemsBySexAndGrade$feature3[itemsBySexAndGrade$feature3=="leftbig"] <- "cong"
itemsBySexAndGrade$feature3[itemsBySexAndGrade$feature3=="rightbig"] <- "incong"
itemsBySexAndGrade$feature3 <- factor(itemsBySexAndGrade$feature3, levels=c("incong", "neutral", "cong"), ordered=T)

itemsByGrade$feature3 <- itemsByGrade$feature1
itemsByGrade$feature3[itemsByGrade$feature3=="leftbig"] <- "cong"
itemsByGrade$feature3[itemsByGrade$feature3=="rightbig"] <- "incong"
itemsByGrade$feature3 <- factor(itemsByGrade$feature3, levels=c("incong", "neutral", "cong"), ordered=T)


with(items[((items$type=="12") & (items$feature2=="addsub") & !(items$type2 %in% c(4,6,8))) ,], {
  x.leg=1
  pch<-c(4,5,6,7,8)
  feature2 <- type2 > 9
  lineplot.CI(feature3, rating, feature2, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature3, rating-start_rating, feature2,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature3, 1-correctRate, feature2,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature3, foil2Rate, feature2,type="b", main="Timeout Rate",  pch=pch, x.leg=x.leg)
})
title("Family 12B.  Spacing, associativity and efficiency, +* formats", outer=TRUE)


plotThoseDatas(items, "13", mainTitle="Family 13: (a+b)/(c+d)",
               foilList = c("foil2Rate", "foil3Rate", "foil4Rate"),
               foilDescList = c("Sum All Operands", "Sum Numerator", "Sum Denominator"),
               fill=T)

plotThoseDatas(items, "14", mainTitle="Family 14: (a/b)[+|*](c/d)",
               foilList = c("foil2Rate", "foil3Rate", "foil4Rate", "foil5Rate", "foil6Rate", "foil7Rate", "foil8Rate"),
               foilDescList = c("Sum All Operands", "Sum Numerator", "Sum Denominator",
                                "(a/b)/(c/d)", "a-b", "(a/b)*(c/d)", "(a/b)+(a/d)"),
               fill=T)

with(items[((items$type=="14")) ,], {
  x.leg=1
  pch<-c(4,5,6,7,8)
  feature4 <- type2 > 4
  lineplot.CI(feature2, rating, feature4, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature2, rating-start_rating, feature4,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature2, 1-correctRate, feature4,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature2, foil2Rate, feature4,type="b", main="Timeout Rate",  pch=pch, x.leg=x.leg)
})


plotThoseDatas(items, "15", mainTitle="Family 15: One op asymmetry,+",
               foilList = c("foil2Rate", "foil3Rate", "foil4Rate", "foil5Rate", "foil6Rate", "foil7Rate"),
               foilDescList = c("Sum Error", "Product Error", "Subtraction Error", "Division Error",
                                "Operand 1 Error" ,"Operand 2 Error"),
               fill=T)






plotThoseDatas(items, "16", mainTitle="Family 16: One op asymmetry,-",
               foilList = c("foil2Rate", "foil3Rate", "foil4Rate", "foil5Rate", "foil6Rate", "foil7Rate"),
               foilDescList = c("Sum Error", "Product Error", "Subtraction Error", "Division Error",
                                "Operand 1 Error" ,"Operand 2 Error"),
               fill=T)

plotThoseDatas(items, "17", mainTitle="Family 17: One op asymmetry,*",
               foilList = c("foil2Rate", "foil3Rate", "foil4Rate", "foil5Rate", "foil6Rate", "foil7Rate"),
               foilDescList = c("Sum Error", "Product Error", "Subtraction Error", "Division Error",
                                "Operand 1 Error" ,"Operand 2 Error"),
               fill=T)

plotThoseDatas(items, "18", mainTitle="Family 18: One op asymmetry,/",
               foilList = c("foil2Rate", "foil3Rate", "foil4Rate", "foil5Rate", "foil6Rate", "foil7Rate"),
               foilDescList = c("Sum Error", "Product Error", "Subtraction Error", "Division Error",
                                "Operand 1 Error" ,"Operand 2 Error"),
               fill=T)

plotThoseDatas(items, "21", mainTitle="Family 21: The See-Saw effect")

with(items[(items$type=="22") ,], {
  feature_1 <- factor(feature1, levels=c("subleft", "neutral", "subright"))  
  lineplot.CI(feature_1, rating, feature2, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, rating-start_rating, feature2,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, 1-correctRate, feature2,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, foilRate, feature2,type="b", main="Foil Rate",  pch=pch, x.leg=x.leg)
})
title("Family 22: Asymmetry and Clumping", outer=T)

dev.off()

pdf("MattMartha.pdf")
par(mfrow=c(2,2), oma=c(0,0,1,0))
pch <- c(5,6)
x.leg <- 1.6
plotThoseDatas(items, "21", mainTitle="Family 21: The See-Saw effect")

with(items[(items$type=="22") ,], {
  feature_1 <- factor(feature1, levels=c("subleft", "neutral", "subright"))  
  lineplot.CI(feature_1, rating, feature2, type="b", main="Rating",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, rating-start_rating, feature2,type="b", main="Ratings Shift",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, 1-correctRate, feature2,type="b", main="Error Rate",  pch=pch, x.leg=x.leg)
  lineplot.CI(feature_1, foilRate, feature2,type="b", main="Foil Rate",  pch=pch, x.leg=x.leg)
})
title("Family 22: Asymmetry and Clumping", outer=T)

dev.off()

reportANOVAsBothWithin <- function(i) {
  with(items[(items$type==i),], {
    feature_1 <- factor(feature1)
    feature_2 <- factor(feature2)
    cat("\n\nratings\n")
    print(summary(aov(rating~feature_1*feature_2 + Error(family/(feature_1*feature_2)))))
    cat("\n\nerrors\n")
    print(summary(aov(correctRate~feature_1*feature_2 + Error(family/(feature_1*feature_2)))))
    cat("\n\nfoils\n")
    print(summary(aov(foilRate~feature_1*feature_2 + Error(family/(feature_1*feature_2)))))
  })
}

reportANOVAs <- function(i) {
  with(items[(items$type==i),], {
    feature_1 <- factor(feature1)
    feature_2 <- factor(feature2)
    cat("\n\nratings\n")
    print(summary(aov(rating~feature_1*feature_2 + Error(family/(feature_1)))))
    cat("\n\nerrors\n")
    print(summary(aov(correctRate~feature_1*feature_2 + Error(family/(feature_1)))))
    cat("\n\nfoils\n")
    print(summary(aov(foilRate~feature_1*feature_2 + Error(family/(feature_1)))))
  })
}


reportSpecialANOVAs <- function(i, dependentName, printName) {
  cat("hi")
  x <- items[(items$type==i),dependentName]
  with(items[(items$type==i),], {
    feature_1 <- factor(feature1)
    feature_2 <- factor(feature2)
    interaction.plot(feature_1, feature_2, x, type="b", main=printName)
    cat(paste("\n", printName, "\n", sep=""))
    print(summary(aov(x~feature_1*feature_2 + Error(family/(feature_1*feature_2)))))
  })
}
#Analyses
analyses <- FALSE
#with(items[(items$type=="12") & ((items$feature1=="leftbig") | (items$feature1=="rightbig")),], summary(aov(rating~feature1*even2 + Error(as.factor(family)/(feature1*even2)))))

#Family 1
if(analyses){
  with(items[(items$type=="1"),], summary(aov(rating~feature1*feature2 + Error(family/(feature1*feature2)))))
  with(items[(items$type=="1"),], summary(aov(correctRate~feature1*feature2 + Error(family/(feature1*feature2)))))
  with(items[(items$type=="1"),], summary(aov(foilRate~feature1*feature2 + Error(family/(feature1*feature2)))))
  with(items[(items$type=="1"),], summary(aov(foil2Rate~feature1*feature2 + Error(family/(feature1*feature2)))))
  with(items[(items$type=="1"),], summary(aov(foil3Rate~feature1*feature2 + Error(family/(feature1*feature2)))))
  
  i <- "2" #2
  with(items[(items$type==i),], summary(aov(rating~feature1*feature2 + Error(family/(feature1*feature2)))))
  with(items[(items$type==i),], summary(aov(correctRate~feature1*feature2 + Error(family/(feature1*feature2)))))
  with(items[(items$type==i),], summary(aov(foilRate~feature1*feature2 + Error(family/(feature1*feature2)))))
  
  i <- "3" #3
  with(items[(items$type==i),], {
    feature_1 <- factor(feature1, levels=c("plus_double", "all_diff", "times_double"), ordered=T)  
    print(summary(aov(rating~feature_1*feature2 + Error(family/(feature_1*feature2)))))
    print(summary(aov(correctRate~feature_1*feature2 + Error(family/(feature_1*feature2)))))
    print(summary(aov(foilRate~feature_1*feature2 + Error(family/(feature_1*feature2)))))
  })
  
  reportANOVAsBothWithin("4")
  
  reportANOVAsBothWithin("5")
  
  reportANOVAs("6")
  
  i <- "7" #7
  with(items[(items$type==i),], {
    feature_1 <- factor(feature1)  
    print(summary(aov(rating~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(correctRate~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(foilRate~feature_1 + Error(family/(feature_1)))))
  })  
  
  i <- "8" #8
  with(items[(items$type==i),], {
    feature_1 <- factor(feature1,ordered=F)  
    print(summary(aov(rating~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(correctRate~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(foilRate~feature_1 + Error(family/(feature_1)))))
  })  
  
  reportANOVAsBothWithin("9")
  
  i <- "10" #10
  with(items[(items$type==i),], {
    feature_2 <- factor(feature2,ordered=F)  
    print(summary(aov(rating~feature_2 + Error(family/(feature_2)))))
    print(summary(aov(correctRate~feature_2 + Error(family/(feature_2)))))
    print(summary(aov(foilRate~feature_2 + Error(family/(feature_2)))))
  })  
  i <- "11" #11
  with(items[(items$type==i),], {
    feature_1 <- factor(feature1,ordered=F)  
    print(summary(aov(rating~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(correctRate~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(foilRate~feature_1 + Error(family/(feature_1)))))
  })  
  
  i<-"12" #12
  with(items[((items$type==i) & (items$feature2=="multdiv")) ,], {
    feature_1 <- factor(feature1,ordered=T)  
    print(summary(aov(rating~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(correctRate~feature_1 + Error(family/(feature_1)))))
    print(summary(aov(timeoutRate~feature_1 + Error(family/(feature_1)))))
  })  
  
  i<-"12" #12
  with(items[((items$type==i) & (items$feature2=="addsub") & (!items$feature1=="neutral")) ,], {
    feature2 <- type2 > 9
    feature3 <- feature1
    feature3[feature3=="leftbig"] <- "incong" 
    feature3[feature3=="rightbig"] <- "cong" 
    feature3 <- factor(feature3, levels=c("incong", "cong"), ordered=T)
    print(summary(aov(rating~feature3*feature2 + Error(family/(feature3*feature2)))))
    print(summary(aov(correctRate~feature3*feature2 + Error(family/(feature3*feature2)))))
    print(summary(aov(timeoutRate~feature3*feature2 + Error(family/(feature3*feature2)))))
  })
  
  
  i<-"13" #13
  with(items[((items$type==i) & (items$feature2=="neutral")),], {
    print(summary(aov(correctRate~feature1 + Error(family/(feature1)))))
    print(summary(aov(foilRate~feature1 + Error(family/(feature1)))))
    
  })
  
  with(items[((items$type==i) & (items$feature1=="parallel")),], {
    print(summary(aov(correctRate~feature2 + Error(family/(feature2)))))
    print(summary(aov(foilRate~feature2 + Error(family/(feature2)))))
    
  })
  
  i <- "15" #15
  with(items[((items$type==i) & (items$feature1=="close" | items$feature1=="spaced")),], {
    # print(summary(aov(foil3Rate~feature1 + Error(family/(feature1)))))
    print(summary(aov(overEstimateRate~feature1 + Error(family/(feature1)))))
    
  })
  
  i <- "17" #17
  with(items[((items$type==i) & (items$feature1=="close" | items$feature1=="spaced")),], {
    # print(summary(aov(foil2Rate~feature1 + Error(family/(feature1)))))
    
    print(summary(aov(overEstimateRate~feature1 + Error(family/(feature1)))))
    
  })
  reportANOVAsBothWithin("21")
  
  
  reportANOVAsBothWithin("22")
  
  reportSpecialANOVAs(1, "foil2Rate", "Family 1 Times Times Errors")
  
  reportSpecialANOVAs(1, "foil3Rate", "Family 1 Plus Plus Errors")
  
  
  reportSpecialANOVAs(3, "foil2Rate", "Family 3 Times Times Errors")
  
  reportSpecialANOVAs(3, "foil3Rate", "Family 3 Plus Plus Errors")
  
  
  with(items[((items$type=="15") & (items$operand1>items$operand2)),], {
    print(mean(foil2Rate)) #Sum Errors
    print(mean(foil3Rate)) #Product Errors
    print(mean(foil4Rate)) #Difference Errors
    print(mean(foil5Rate)) #Division Errors
  })
  
  with(items[((items$type=="16") & (items$operand1<items$operand2)),], {
    print(mean(foil2Rate)) #Sum Errors
    print(mean(foil3Rate)) #Product Errors
    print(mean(foil4Rate)) #Difference Errors
    print(mean(foil5Rate)) #Division Errors
  })
  with(items[((items$type=="17") & (items$operand1>items$operand2)),], {
    print(mean(foil2Rate)) #Sum Errors
    print(mean(foil3Rate)) #Product Errors
    print(mean(foil4Rate)) #Difference Errors
    print(mean(foil5Rate)) #Division Errors
  })
  with(items[((items$type=="18") & (items$operand1<items$operand2)),], {
    print(mean(foil2Rate)) #Sum Errors
    print(mean(foil3Rate)) #Product Errors
    print(mean(foil4Rate)) #Difference Errors
    print(mean(foil5Rate)) #Division Errors
  })
}




