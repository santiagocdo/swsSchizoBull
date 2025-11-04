### Collaborators (auto-populated):
###
# prepare descriptive table
f_suppTables <- function (quest,descrGuide) {
  # change nature of the next variables
  quest$demo_gender <- as.factor(quest$demo_gender)
  
  # create continuous variables 
  for (i in 1:nrow(descrGuide)) {
    if (descrGuide[i,2] == 1) {
      quest[,descrGuide[i,1]] <- as.numeric(quest[,descrGuide[i,1]])
    }
  }
  
  # summarized
  for (i in 1:nrow(descrGuide)) {
    # if the variable is continuous
    if (descrGuide[i,2] == 1) {
      temp <- f_descrContinuous(quest[,descrGuide[i,1]])
      temp <- matrix(c(descrGuide[i,1],descrGuide[i,2],rep(NA,3),temp),nrow=1)
    } else { # or the variable is categorical
      temp <- f_descrCategorical(quest[,descrGuide[i,1]])
      temp <- cbind(descrGuide[i,1],descrGuide[i,2],temp,matrix(NA,ncol=5,nrow=nrow(temp)))
    }
    if (i == 1) {
      outputTable <- temp
    } else {
      outputTable <- rbind(outputTable,temp)
    }
  }
  # add column names
  colnames(outputTable) <- c("var","type","factor","frequency","percentage","N","mean","sd","min","max")
  
  # function output
  return(outputTable)
}
# categorical descriptive variable
f_descrCategorical <- function(vec) {return(matrix(c(levels(as.factor(vec)),table(vec),(table(vec) / length(vec)) * 100),
                                                   nrow=length(levels(as.factor(vec)))))}
# continuous descriptive variable
f_descrContinuous <- function(vec) {return(c(sum(!is.na(vec)),mean(vec, na.rm = T),sd(vec, na.rm = T),range(vec, na.rm = T)))}
