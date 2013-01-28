# Load Math Garden Data

# This set of scripts should be sourced with the variable porportionMathGardenToLoad set to some value between 0 and 1.  If it is, then only that proportion of the data will be set
# porportionMathGardenToLoad: The proportion of math garden data to load!  Defaults to 1


require(sciplot)
require(plyr)
require(data.table)


if(!exists("proportionMathGardenToLoad")){
  proportionMathGardenToLoad = 1
}



# Functions for creating data
loadMathGardenDataSets <- function(proportionMathGardenToLoad=1, randomSubset=TRUE, wd="/Users/dlandy/Dropbox/Projects/Math\ Garden/"){
  #source("/Users/dlandy/Dropbox/Operation Order (shared)/rekenvolgorde.r")  
  source(paste(wd, "buildItems.R", sep=""))
  # Load raw data
  if(!(exists("log27Reduced"))){
    cat("Loading the item log\n")
    #  infile <- paste(wd, "rekenvolgordeOct292012.Rdata", sep="")
    #infile <- "/Users/dlandy/Dropbox/Operation Order (shared)/rekenvolgordeOct291012.Rdata"
    infile <- paste(wd, "rekenvolgordeDec062012.Rdata", sep="")
    load(infile)
    print(length(log27))
    print(length(log27$item_id))
    
    if(proportionMathGardenToLoad<1){
      if(randomSubset){
        log27 <- log27[sample(length(log27$user_id), floor(proportionMathGardenToLoad*length(log27$user_id))),]
      } else {
        log27 <- log27[1:floor(proportionMathGardenToLoad*length(log27$user_id)),]
      }
    }
    print(length(log27))
    print(length(log27$item_id))
    print(length(sample(length(log27$user_id), floor(proportionMathGardenToLoad*length(log27$user_id)))))
    
      
    male_ids <- users27$user_id[users27$gender=="m"]
    female_ids <- users27$user_id[users27$gender=="f"]
    
    log27$sex <- 0
    
    log27$sex[log27$user_id %in% male_ids] <- "m"
    log27$sex[log27$user_id %in% female_ids] <- "f"
    
    items27$true_id <- items27$id
    items27$true_id[items27$predecessor_id>0] <- items27$predecessor_id[items27$predecessor_id>0]
    items27 <- items27[order(items27$true_id),]
    
    rm(male_ids, female_ids)
  } else{
    cat("Item log already loaded\n")
    
  }
  
  # Load item definitions
  
  
  if(!(exists("itemDefinitions"))){
    cat("Loading the item definitions\n")
    itemDefinitions <-read.csv("/Users/dlandy/Dropbox/Projects/Math Garden/problem_list_4_1_12.csv", 1)
  }else{
    cat("Item definitions already loaded\n")
    
  }
  
  
  if(!(exists("items"))){
    cat("Building the items\n")
    items <- cbind(items27, itemDefinitions)
    items$feature1 <- as.character(items$feature1)
    items$feature2 <- as.character(items$feature2)
    items$family <- as.factor(items$family)
    if(!exists("log27Reduced")){
      print(length(log27))
      print(length(log27$item_id))
      log27Reduced <<- data.table(log27[log27$item_id %in% items$id,], key="user_id")
      users27 <- data.table(users27, key="user_id")
      #   log27Reduced <<- log27Reduced[log27Reduced,]\
      cat("Starting to remove elements of uncertain birth date")
      log27Reduced$birth_date_reliability <- tapply(1:length(log27Reduced$user_id), 1:length(log27Reduced$user_id), function(x){users27$birth_date_reliability[users27$user_id==log27Reduced$user_id[x]]})
      cat("Done removing elements of uncertain birthdate")
      log27Reduced <- log27Reduced[log27Reduced$birth_date_reliability > 0,]
      log27Reduced$correct==0
      log27Reduced$foil==0
      if(!exists("ll27")){    
        ll27 <- split(log27Reduced, log27Reduced$item_id)
      }
      rm(log27)
      rm(itemlog27)
      rm(userswide)
    }
    items$identifier <- items$id
    
    items <- buildItems(items)
    itemsOrig <- items
    
    itemsDT <- data.table(items)
    computeRates <- buildComputeRates(ll27)
    itemsList <- itemsDT[, computeRates(as.character(id),  correct, 
                                        foil, foil2, foil3, foil4, foil5,
                                        foil6, foil7, foil8), by=as.character(id)]
    items <- cbind(items, itemsList)
  } else{
    cat("Items already loaded\n")
    
  }
  
  if(!exists("itemsBySexAndGrade")){
    # 19 grades, 2 sexes
    cat("Creating Items by Sex and Grade\n")
    itemsBySexAndGrade <- rbind(itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig,
                                itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig) 
    itemsBySexAndGrade$sex <- c(rep("f", length(itemsOrig$id) * 19), rep("m", length(itemsOrig$id) * 19) )
    itemsBySexAndGrade$grade <- rep(unlist(tapply(sort(unique(users27$grade)), sort(unique(users27$grade)), function(x) { rep(x, length(itemsOrig$id))})), 2)
    itemsBySexAndGrade$isg <- interaction(itemsBySexAndGrade$grade, itemsBySexAndGrade$sex, itemsBySexAndGrade$id)
    cat("   Splitting the log by grade, sex, and item\n")
    if(!exists("ll27Sex")){  
      ll27Sex <<- split(log27Reduced, interaction(log27Reduced$grade, log27Reduced$sex, log27Reduced$item_id))
    }
    
    cat("   calculating rates for each item, by sex and grade\n")
    computeSexRates <- buildComputeRates(ll27Sex)
    cat("Creating ...\n")
    
    itemsBySexAndGrade$identifier <- itemsBySexAndGrade$isg
    cat("Creating ...\n")
    itemsBySexAndGradeDT <- data.table(itemsBySexAndGrade)
    
    result <- itemsBySexAndGradeDT[, computeSexRates(as.character(identifier), correct, foil, foil2, foil3, foil4, foil5, foil6, foil7, foil8), by=isg]
    
    itemsBySexAndGrade <- cbind(itemsBySexAndGrade, result)
    
  } else{
    cat("Items by sex and grade already loaded\n")
    
  }
  
  if(!exists("itemsByGrade")){
    # 17 grades
    cat("Creating Items by Grade\n")
    itemsByGrade <- rbind( itemsOrig,itemsOrig,itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig, itemsOrig) 
    itemsByGrade$grade <- rep(unlist(tapply(sort(unique(users27$grade)), sort(unique(users27$grade)), function(x) { rep(x, length(itemsOrig$id))})), 1)
    itemsByGrade$ig <- interaction(itemsByGrade$grade,itemsByGrade$id)
    cat("   Splitting the log by grade, and item\n")
    if(!exists("ll27Grade")){  
      ll27Grade <<- split(log27Reduced, interaction(log27Reduced$grade, log27Reduced$item_id))
    }
    
    cat("   calculating rates for each item and grade\n")
    computeGradeRates <- buildComputeRates(ll27Grade)
    
    itemsByGrade$identifier <- itemsByGrade$ig
    itemsByGradeDT <- data.table(itemsByGrade)
    
    result <- itemsByGradeDT[, computeGradeRates(as.character(identifier), correct, foil, foil2, foil3, foil4, foil5, foil6, foil7, foil8), by=ig]
    
    itemsByGrade <- cbind(itemsByGrade, result)
    
  } else{
    cat("Items by  grade already loaded\n")
    
  }
  
  
  return(c(log27Reduced, items, itemsBySexAndGrade, itemsByGrade))
  
}


