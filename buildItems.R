buildComputeRates <- function(ll){
  function(id, correct, foil, foil2, foil3, foil4, foil5, foil6, foil7, foil8){
    cat( id, " ")
    cat(length(ll[[as.character(id)]][,1]))
    item <- data.table(id = id)
    correct <- as.character(correct)
    foil <- as.character(foil)
    foil2 <- as.character(foil2)
    foil3 <- as.character(foil3)
    foil4 <- as.character(foil4)
    foil5 <- as.character(foil5)
    foil6 <- as.character(foil6)
    foil7 <- as.character(foil7)
    foil8 <- as.character(foil8)
    
    accVec <- ll[[as.character(id)]]$answer==correct
    
    item$correctRate <- (mean(accVec, na.rm=T))
    item$timeoutRate <- (mean(ll[[as.character(id)]]$answer=="…", na.rm=T))
    item$questionRate <- (mean(ll[[as.character(id)]]$answer=="¿", na.rm=T))
    
    
   # item$overEstimateRate <- with(ll[[as.character(id)]][!accVec & 
  #    !ll[[as.character(id)]]$answer=="…" &
  #    !ll[[as.character(id)]]$answer=="¿",],
  #                                mean(answer > correct, na.rm=T)
  #  )
    
    item$positiveErrorRate <- (1-item$correctRate-item$timeoutRate - item$questionRate) / (1-item$timeoutRate - item$questionRate)
    
    item$foilRate <- (mean(ll[[as.character(id)]]$answer==foil, na.rm=T)) 
    item$foil2Rate <- (mean((ll[[as.character(id)]]$answer==foil2) & (!accVec), na.rm=T))
    item$foil3Rate <- (mean((ll[[as.character(id)]]$answer==foil3) & (!accVec), na.rm=T))
    item$foil4Rate <- (mean((ll[[as.character(id)]]$answer==foil4) & (!accVec), na.rm=T))
    item$foil5Rate <- (mean((ll[[as.character(id)]]$answer==foil5) & (!accVec), na.rm=T))
    item$foil6Rate <- (mean((ll[[as.character(id)]]$answer==foil6) & (!accVec), na.rm=T))
    item$foil7Rate <- (mean((ll[[as.character(id)]]$answer==foil7) & (!accVec), na.rm=T))
    item$foil8Rate <- (mean((ll[[as.character(id)]]$answer==foil8) & (!accVec), na.rm=T))
    item$residualErrors <- 1-with(item, (correctRate + timeoutRate + questionRate+ foilRate + foil2Rate+foil3Rate + foil4Rate+foil5Rate+foil6Rate+foil7Rate+foil8Rate))
    
    item$foilCount <- sum((ll[[as.character(id)]]$answer==foil & (!accVec)), na.rm=T)
    item$correctCount <- sum(accVec, na.rm=T)
    item$nSamples <- length(accVec)
    
    
    
    cat("\n")
    item
  }
}




buildItems <- function(items){
  # build special foil responses for Rob's requested followup analysis
  items$foil2 <- -100
  items$foil3 <- -100
  items$foil4 <- -100
  items$foil5 <- -100
  items$foil6 <- -100
  items$foil7 <- -100
  items$foil8 <- -100
  items$residualErrors <- 0
  items$overEstimateRate <- -100
  # Item type 1,3,9: multiplying all terms together (foil 2), and adding all (foil 3).  Hypothesis is that this is common with +x, but not x+
  items$foil2[items$type==1 | items$type==3 | items$type==9] <- with(items[items$type==1 | items$type==3 | items$type==9,], (operand1 * operand2 * operand3))
  items$foil3[items$type==1 | items$type==3 | items$type==9] <- with(items[items$type==1 | items$type==3 | items$type==9,], (operand1 + operand2 + operand3))
  
  
  items$foil4[items$type==1 & items$feature2=="mult_first" ] <- with(items[items$type==1 & items$feature2=="mult_first",], (operand1 + operand2 * operand3))
  items$foil5[items$type==1 & items$feature2=="plus_first" ] <- with(items[items$type==1 & items$feature2=="plus_first",], (operand3 * operand2 + operand1))
  
  # Item types 2, 4,6,7 have no alternative foil of interest
  # Item type 5: Mistaking the x for +, - (foils 2, 3) (never seems to happen. interesting.  but lots of possible reasons)
  items$foil2[items$type==5] <- with(items[items$type==5,], (operand1 - operand2 - operand3))
  items$foil3[items$type==5] <- with(items[items$type==5,], (operand1 - operand2 + operand3))
  
  # Item types 10, 11: using a strict left-right ordering (foil 2), and just adding all terms (foil 3)
  items$foil2[items$type==10] <- with(items[items$type==10,], ((operand1 * operand2 + operand3)*operand4))
  items$foil2[items$type==11] <- with(items[items$type==11,], (((operand1 + operand2) / operand3)+operand4))
  
  items$foil3[items$type==10 | items$type==11] <- with(items[items$type==10 | items$type==11,], ((operand1 + operand2 + operand3 + operand4)))
  
  
  #With 12, we just expect lots of timing out.
  items$foil2[items$type==12] <- with(items[items$type==12,], "…")
  
  
  
  # Item type 13 & 14: adding all terms (foil 2), just numerator (foil 3), or just the denominator (foil 4)
  # No real hypotheses, except that some people might just not know what to do, and will just add  
  # However, DL notes that if so, the same thing should happen with foil 10
  # Note; operand 1 is doubled
  items$foil2[items$type==13] <- with(items[items$type==13,], ((operand1 + operand2 + operand3 + operand1)))
  items$foil3[items$type==13] <- with(items[items$type==13,], ((operand1 + operand2 )))
  items$foil4[items$type==13] <- with(items[items$type==13,], ((operand3 + operand1)))
  
  # There are some special hypotheses for item type 14
  # Here Operand *2* is doubled!!!
  items$foil2[items$type==14] <- with(items[items$type==14,], ((operand1 + operand2 + operand3 + operand2)))
  items$foil3[items$type==14] <- with(items[items$type==14,], ((operand1 + operand2 )))
  items$foil4[items$type==14] <- with(items[items$type==14,], ((operand3 + operand2)))
  items$foil5[items$type==14] <- with(items[items$type==14,], ((operand3 / operand2) / (operand1 / operand2)))
  items$foil6[items$type==14] <- with(items[items$type==14,], ((operand1 - operand2)))
  
  items$foil7[items$type==14] <- with(items[items$type==14,], ((operand1 / operand2) * (operand3 / operand2)))
  items$foil8[items$type==14] <- with(items[items$type==14,], ((operand1 / operand2) + (operand3 / operand2)))
  
  
  # Items 15, 16, 17, 18: all operator confusions are of interest. Right?  Of course, operand overlap makes certain kinds of confusions less likely.  
  # We should have made two or three common problems, that appeared (and appeared plausibly) in all forms, like 8 op 4.
  items$foil2[items$type>=15 & items$type<=18] <- with(items[items$type>=15 & items$type<=18,], ((operand1 + operand2)))
  items$foil3[items$type>=15 & items$type<=18] <- with(items[items$type>=15 & items$type<=18,], ((operand1 * operand2)))
  
  
  items$foil4[items$type==15 | items$type==17] <- with(items[items$type==15 | items$type==17,], ((operand1 - operand2)))
  items$foil4[items$type==16 | items$type==18] <- with(items[items$type==16 | items$type==18,], ((operand2 - operand1)))
  
  
  items$foil5[items$type==15 | items$type==17] <- with(items[items$type==15 | items$type==17,], ((operand1 / operand2)))
  items$foil4[items$type==16 | items$type==18] <- with(items[items$type==16 | items$type==18,], ((operand2 / operand1)))
  
  items$foil6[items$type>=15 & items$type<=18] <- with(items[items$type>=15 & items$type<=18,], ((operand1)))
  items$foil7[items$type>=15 & items$type<=18] <- with(items[items$type>=15 & items$type<=18,], ((operand2)))
  
  items$foil7[items$type==16] <- with(items[items$type==16,], ((operand1 - (operand2 %% 10))))
  
  #Item types 19, 20:
  
  items$foil2[items$type==19 | items$type==20] <- with(items[items$type==19 | items$type==20,],   ((operand1 * operand2 * operand3)))
  items$foil3[items$type==19 | items$type==20] <- with(items[items$type==19 | items$type==20,],   ((operand1 + operand2 + operand3)))
  items$foil2[items$type==19 | items$type==20] <- with(items[items$type==19 | items$type==20,],   ((operand1 * operand2 + operand3)))
  items$foil2[items$type==19 | items$type==20] <- with(items[items$type==19 | items$type==20,],   ((operand1 + operand2 * operand3)))
  
  items$foil2[items$type==19 | items$type==20] <- with(items[items$type==19 | items$type==20,],   (((operand1 + operand3) * operand2)))
  
  
  items$foil2[items$type==22] <- with(items[items$type==22,],   (operand1 - (operand2+operand3)*operand4))
  items$foil3[items$type==22] <- with(items[items$type==22,],   (operand1 + ((0-operand2)+operand3)*operand4))
  items$foil4[items$type==22] <- with(items[items$type==22,],   ((operand1 - (operand2+operand3))*operand4))
  items$foil5[items$type==22] <- with(items[items$type==22,],   ((operand1-operand2)+operand3)*operand4)
  
  
  
  items
}