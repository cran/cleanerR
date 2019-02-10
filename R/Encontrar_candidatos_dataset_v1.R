# This set of functions for the cleanerR package were produced by Rafael Silva Pereira
#The Institution that provided the resources for the development was the Láboratorio Nacional de Computação Cientifica (LNCC)
#The institution is located in Brazil
#In special the maintainer thanks the DEXL Reseach Group, from where he is doing his research for his master thesis
#in the field of machine learning and Data Science

#The maintainer thanks CNPQ as well for the funding that makes him able to dedicate himself to his research

#This set of functions have the purpose to deal with missing data ,be it by showing it via NA_VALUES or even trying to predict it
#By using the concept in databases of functional dependencies we can maximize all possible information about a goal collumn
#And with this we can maximize the prediction accuracy.




loadNamespace("plyr")
loadNamespace("data.table")
require(plyr)
require(data.table)
#'\code{Candidates} Asks for a dataframe and some parameters and returns how close the collums chosen can predict the goal collum
#' Should be used mostly with generate_candidates or preferably BestVector in case you only want the best combination possible for prediction
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param vec a vector of collums you wish to test if can be used to predict the values
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
Candidates<-function(df,goal,vec,repetitions,trigger=1){
  y<-list()

  if(class(df)[1] == "data.table"){
	  CandidatesTable(df,goal,vec,repetitions,trigger)
  }else{
    df<-df[which(!is.na(df[,goal]) ),]  # First we drop the NA lines of goal to build the frequency table

    if(length(vec) == 1){
      x<-length(unique(df[,vec] )   )
    }else{
	    x<-nrow(unique(df[,vec] )   ) # To calculate repetitions we must have the minimum value of the frequency table and the actual
    }

    vec<-append(vec,goal)
    m<-plyr::count(df[,vec])
    z<-nrow(m) # Actual frequency table number of lines
    step1<-as.numeric(table(plyr::count(df[,vec])$freq)[which(names(table(plyr::count(df[,vec])$freq)) == 1)])
    step2<-sum(as.numeric(table(plyr::count(df[,vec])$freq)))
    #AuxTrigger<-as.numeric(table(plyr::count(df[,vec])$freq)[which(names(table(plyr::count(df[,vec])$freq)) == 1)])/sum(as.numeric(table(plyr::count(df[,vec])$freq))) # percentage of tuples that show only once
    AuxTrigger<-step1/step2
    if(length(AuxTrigger > 0)){
      acceptance<-as.numeric( (1-AuxTrigger) < trigger  )
    }else{
      acceptance<-0
    }

    y[[2]] <- acceptance
    # A candidate shall be accepted if it satisfies AuxTrigger and the repetitions value

    w<-abs(z-x)

    y[[1]]=w

    return(y)
  }
# apenas valor 0 aceita vec para esta goal
}

#'\code{CandidatesTable} candidates implementation that asks for a data.table object
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param vec a vector of collums you wish to test if can be used to predict the values
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
CandidatesTable<-function(df,goal,vec,repetitions,trigger=1){
  #This function is a data.table implementation of candidates, refer to it for comments
  y<-list()

  auxiliar<-which( !is.na(df[,..goal]) )
  df<-df[auxiliar,]

  x<-nrow(unique(df[,..vec] )   )

  vec<-append(vec,goal)
  m<-plyr::count(df[,..vec])
  z<-nrow(m)
  step1=as.numeric(table(plyr::count(df[,..vec])$freq)[which(names(table(plyr::count(df[,..vec])$freq)) == 1)])
  step2=sum(as.numeric(table(plyr::count(df[,..vec])$freq)))
  #AuxTrigger<-as.numeric(table(plyr::count(df[,..vec])$freq)[which(names(table(plyr::count(df[,..vec])$freq)) == 1)])/sum(as.numeric(table(plyr::count(df[,..vec])$freq)))
  AuxTrigger<-step1/step2
  if(length(AuxTrigger > 0)){
    acceptance<-as.numeric( (1-AuxTrigger) < trigger  )
  }else{
    acceptance<-0
  }
  y[[2]]<-acceptance


  w<-abs(z-x)

  y[[1]]<-w
  w<-as.numeric(w>repetitions)
#return(w)
  return(y)


}

#'\code{GenerateCandidates} Asks for a dataframe and some parameters and returns all possible combinations of collums for prediction that satisfy a given error in input
#'in a list the first element of the list are the combinations while the second is its measure of error,to get the best parameters call BestVector
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @examples
#' #The GenerateCandidates function generates all sets of maximum length maxi.
#' #Maxi is a measure of error.
#' #This measure of error is related to the repetitions parameter.
#' #This parameter should range from 0 (rejects anything less to 100 percent accuracy)
#' #To number of rows of the dataframe to accept all.

#' #Lets generate a dataset
#' e<-sample(1:5,1e4,replace<-TRUE)
#' e1<-sample(1:5,1e4,replace<-TRUE)
#' e2<-sample(1:5,1e4,replace<-TRUE)
#' e<-data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' names(e)=c("random1","random2","random3","2randoms","3randoms")
#' #We can then generate all candidates to predict the 5 collumn
#' #We shall determine the reject part to 80 percent of the dataframe length
#' z<-GenerateCandidates(df<-e, goal<-5, maxi<-4, repetitions<-0.8*nrow(e), trigger = 1)
#' #We can see z is a list
#' #z[[1]] is another list that contains all sets that satisfy our request
#' #z[[2]] is a measure of error, the smaller the more accurate
#' #Lets then order z[[1]] by z[[2]]
#' m<-z[[1]][order(z[[2]])]
#' print(m)
#' #We can see then that m[[1]] holds the best set for prediction, while m[[length(m)]] the worst
#' #To prove it we can do the following
#' cat("The best set to predict",names(e)[5],"is ",names(e)[m[[1]]],"\n"   )
#' cat("Its expected accuracy is",MeanAccuracy(e,m[[1]],5),"\n"  )
#' cat("The worst set to predict",names(e)[5],"is ",names(e)[m[[length(m)]]],"\n"   )
#' cat("Its expected accuracy is",MeanAccuracy(e,m[[length(m)]],5),"\n"  )

#'@export
GenerateCandidates<-function(df,goal,maxi,repetitions,trigger=1){
  if(class(df)[1] == "data.table"){
	  GenerateCandidatesTable(df,goal,maxi,repetitions,trigger)
  }else{
    groups<-1:ncol(df)
    groups<-groups[-goal] # Set of values that shall be tested against goal
    if(length(groups) > maxi){
      loop<-maxi
    }else{
      loop<-length(groups)  # To be sure that we dont try to acess collumns that dont exist
    }


    z<-lapply(1:loop, function(x) combn(groups, x))

    CandidateVectors<-list()
    errors<-c()
    cont<-1
    if(length(z)>maxi){
	    maximum<-length(z)
    }else{
	    maximum<-maxi
    }
    for(i in 1:maximum){
	    print(i)
	    data<-data.frame(z[[i]])
	    for(j in 1:ncol(data)){
		    vetor<-data[,j]

		    auxiliar_list<-Candidates(df,goal,vetor,repetitions,trigger) # tests for a candidate, if it satisfies we will store the set and error
		    if(auxiliar_list[[1]] <= repetitions & auxiliar_list[[2]]==0){
			    CandidateVectors[[cont]]<-vetor
			    errors[cont]<-auxiliar_list[[1]]
			    cont<-cont+1

		    }

	   }


    }
    retorno<-list(sets=CandidateVectors,error=errors)
    OrderedList=order(retorno[[2]])
    retorno[[2]]=retorno[[2]][OrderedList]
    retorno[[1]]=retorno[[1]][OrderedList]
    return(retorno) #Returns a list that contains all sets that satisfied the parameters as well as their error
  }
}
#'\code{GenerateCandidatesTable} Asks for a data.table and some parameters and returns all possible combinations of collums for prediction that satisfy a given error in input
#'in a list the first element of the list are the combinations while the second is its measure of error,to get the best parameters call BestVector
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
GenerateCandidatesTable<-function(df,goal,maxi,repetitions,trigger=1){
  #This function is a data table implementation to GenerateCandidates refer to it for comments
  groups<-1:ncol(df)
  groups<-groups[-goal]
  if(length(groups) > maxi){
    loop<-maxi
  }else{
    loop<-length(groups)
  }

  z<-lapply(1:loop, function(x) combn(groups, x))



  CandidateVectors<-list()
  errors<-c()
  cont<-1
  if(length(z)>maxi){
  	maximum <- length(z)
  }else{
	  maximum <- maxi
  }
  for(i in 1:maximum){
	  print(i)
	  data<-data.table(z[[i]])
	  for(j in 1:ncol(data)){
		  vetor<-unlist(data[,..j])

		  auxiliar_list<-CandidatesTable(df,goal,vetor,repetitions,trigger)
		  if(auxiliar_list[[1]]<=repetitions & auxiliar_list[[2]]==0){
			  CandidateVectors[[cont]]<-vetor
			  errors[cont]=auxiliar_list[[1]]
			  cont<-cont+1

		  }

	  }


  }
  retorno<-list(sets=CandidateVectors,error=errors)
  OrderedList=order(retorno[[2]])
  retorno[[2]]=retorno[[2]][OrderedList]
  retorno[[1]]=retorno[[1]][OrderedList]

  return(retorno)
}



#'\code{BestVector} Asks for a dataframe and some parameters and returns the best combination of collums to predict the missing value
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#' @examples
#' #The Best Vector Function shall do the following
#' #Take a dataframe and a goal collumn to predict
#' #Tests every combination of vectors limited by a parameter length
#' #Returns the best set to predict the goal
#' #Then to run some experiments first lets build a dataframe
#' e<-sample(1:2,1e2,replace<-TRUE)
#' e1<-sample(1:2,1e2,replace<-TRUE)
#' e2<-sample(1:2,1e2,replace<-TRUE)
#' e<-data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' #We can easily see that to predict the last collumn you need the first three.
#' #Lets Check what the function will find
#' z<-BestVector(e,5,3,nrow(e),1)
#' print(z)
#' #Lets now check what is the best set if we use only 2 collumns maximum
#' z1<-BestVector(e,5,2,nrow(e),1)
#' print(z1)
#' #We could also predict which collumn is best to predict the fourth one
#' z2<-BestVector(e,4,2,nrow(e),1)
#' print(z2)
#' #We could also take a look at the dataset iris.
#' #Since this dataset does not repeat lines we must use trigger<-0
#' #To predict Species
#' z3<-BestVector(iris,5,2,nrow(iris),0)
#' print(names(iris))[z3]
#' #We can check the accuracy of these predictions with the accuracy functions
#' print(MeanAccuracy(iris,z3,5))
#' print(MeanAccuracy(e,z2,4))
#' print(MeanAccuracy(e,z1,5))
#' print(MeanAccuracy(iris,z,5))
#'@export
BestVector<-function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
  meta<-names(df)[goal]
  lista<-as.numeric(which(sapply(sapply(df,unique),length)/nrow(df) < ratio))
  df_aux<-df # copy to recover the collumn later
  if(class(df)[1] == "data.table"){


	  BestVectorTable(df,goal,maxi,repetitions,trigger,ratio)
	}else{
    df<-df[,lista] # dropping candidates that dont satisfy ratio
    goal<-which(names(df) == meta)
    z<-GenerateCandidates(df,goal,maxi,repetitions,trigger)
    if(length(z[[1]]) > 0){
      solution<-unlist(z[[1]][min(which(z[[2]] == min(z[[2]])))])

      return( which(names(df)[solution] == names(df_aux)) ) # recovering the actual collumn


    }else{
      cat("Could not find any candidate, please use a smaller trigger value\n") # In case the conditions are too restrictive
      return(0)

    }
  }
}
#'\code{BestVectorTable} Asks for a data.table and some parameters and returns the best combination of collums to predict the missing value
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#'@export
BestVectorTable<-function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
  #This function is a data table implementation of BestVector refer to it for comments
  meta<-names(df)[goal]
  lista<-as.numeric(which(sapply(sapply(df,unique),length)/nrow(df) < ratio))
  df_aux<-df
  df<-df[,..lista]
  goal<-which(names(df) == meta)
  z<-GenerateCandidatesTable(df,goal,maxi,repetitions,trigger)
  if(length(z[[1]])>0){
	  solution<-unlist(z[[1]][min(which(z[[2]] == min(z[[2]])))])
    return( which(names(df)[solution] == names(df_aux)) )

  }else{
    cat("Could not find any candidate, please use smaller trigger\n")
    return(0)

  }

}



#'\code{NA_VALUES} Asks for a dataframe and returns a table of how many missing values are in each collum
#' @param df A dataframe with the missing values you wish to fill
#' @examples
#' #This function is used to detect how many NA values are in a dataframe
#' # the use is pretty much always the same
#' #Lets consider the dataset iris
#' i<-iris
#' print(NA_VALUES(i)  )
#' #Since it has no missing values it shows none, now lets insert some NA_VALUES there
#' i[sample(1:nrow(i),0.3*nrow(i)),1]<-NA
#' i[sample(1:nrow(i),0.2*nrow(i)),2]<-NA
#' i[sample(1:nrow(i),0.5*nrow(i)),3]<-NA

#' print(NA_VALUES(i))
#' #For every dataframe the user just uses this

#' @export
NA_VALUES<-function(df){
  return(apply(apply(df,2,is.na),2,sum) )

}



#'\code{CompleteDataset} Asks for a dataframe, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param rows The collumns you wish to use to predict the missing values
#' @examples
#' #The CompleteDataset Function shall do the following
#' #Take a dataframe and a goal collumn to predict
#' #Takes a set of vectors to use for prediction
#' #Use this set to predict with accuracy given by MeanAccuracy function
#' #Then to run some experiments first lets build a dataframe
#' e<-sample(1:5,1e4,replace<-TRUE)
#' e1<-sample(1:5,1e4,replace<-TRUE)
#' e2<-sample(1:5,1e4,replace<-TRUE)
#' e<-data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' #Now we got a dataframe lets create a copy of it
#' ce<-e
#' ce[sample(1:nrow(e),0.3*nrow(e)),5]<-NA
#' #So 30 percent of the data is now missing
#' #Lets try to recover it then with CompleteDataset
#' #First we must choose a set of vectors to use
#' #Lets first try with BestVector
#' vector_c<-BestVector(ce,5,4,nrow(ce),1)
#' ce1<-CompleteDataset(ce,rows<-vector_c,goal<-5)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,5]!=e[,5]) )

#' #If the user wanted he of course could choose a set of his own, for example
#' user_set<-c(1,3)
#' ce1<-CompleteDataset(ce,rows<-user_set,goal<-5)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,5]!=e[,5]) )
#' #But we can see that is not the best solution
#' #To see how to check the best sets take a look at GenerateCandidates

#' # The process could be done for the 4 collum as well

#' ce<-e
#' ce[sample(1:nrow(e),0.5*nrow(e)),4]<-NA
#' #So 50 percent of the data is now missing
#' #Lets try to recover it then with CompleteDataset
#' vector_c<-BestVector(ce,4,4,nrow(ce),1)
#' ce1<-CompleteDataset(ce,rows<-vector_c,goal<-4)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,4]!=e[,4]) )
#' #Here we can easily see e holds the original data
#' #ce1 is the recovered data

#'@export
CompleteDataset<-function(df,rows,goal){
  if(class(df)[1] == "data.table"){
  	CompleteDatasetTable(df,rows,goal)
  }else{
  #lets suppose df has missing values on only goal collum
    test<-list()
    df2<-df
    vectors<-which(is.na(df[,goal]))
    df1<-df[-vectors,] # for plyr::count
    aux<-c(rows,goal)
    frequency_table<-plyr::count(df1[,aux]) # Builds the frequency table to help fill missing data
    for(i in vectors){
      test<-list()
      for(j in 1:length(rows)  )
      {
        test[[j]]<-which(df[i,rows[j]] == frequency_table[,j]   )

      }

      prob_table<-frequency_table[Reduce(intersect,test),c(ncol(frequency_table)-1,ncol(frequency_table))] #For a specific missing value builds its probability table based on all gathered knowledge
      if(nrow(prob_table)>0){
        prob<-cumsum(prob_table[,2])
        prob<-prob/max(prob)

        df2[i,goal]<-prob_table[min(which(runif(1) < prob)),1] # assignment of value
      }
    }
    return(df2)
  }
}

#'\code{CompleteDatasetTable} Asks for a data.table, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param rows The collumns you wish to use to predict the missing values
#'@export
CompleteDatasetTable<-function(df,rows,goal){
  #This function is a data table implementation of CompleteDataset check it for comments
  #lets suppose df has missing values on only goal collum
  test<-list()
  df2<-df
  vectors<-which(is.na(df[,..goal]))
  df1<-df[-vectors,] # for plyr::count
  aux<-c(rows,goal)
  frequency_table<-plyr::count(df1[,..aux])
  for(i in vectors){
    test<-list()
    for(j in 1:length(rows)  )
    {
      forget_this<-rows[j]
      test[[j]]=which(unlist(df[i,..forget_this]) == frequency_table[,j]   )

    }

    prob_table<-frequency_table[Reduce(intersect,test),c(ncol(frequency_table)-1,ncol(frequency_table))]
    if(nrow(prob_table)>0){
      prob<-cumsum(prob_table[,2])
      prob<-prob/max(prob)

      df2[i,names(df2)[goal] := prob_table[min(which(runif(1)<prob)),1] ]
    }
  }
  return(df2)
}

#'\code{AutoComplete} Asks for a dataframe, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A dataframe with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#' @examples
#' #The Auto Complete Function shall do the following
#' #Take a dataframe and a goal collumn to predict
#' #Tests every combination of vectors limited by a parameter length
#' #Use the best set to predict with accuracy given by MeanAccuracy function
#' #Then to run some experiments first lets build a dataframe
#' e<-sample(1:5,1e4,replace<-TRUE)
#' e1<-sample(1:5,1e4,replace<-TRUE)
#' e2<-sample(1:5,1e4,replace<-TRUE)
#' e<-data.frame(e,e1,e2,paste(LETTERS[e],LETTERS[e1]),paste(LETTERS[e],LETTERS[e1],LETTERS[e2])   )
#' #Now we got a dataframe lets create a copy of it
#' ce<-e
#' ce[sample(1:nrow(e),0.3*nrow(e)),5]=NA
#' #So 30 percent of the data is now missing
#' #Lets try to recover it then with autocomplete
#' ce1<-AutoComplete(df<-ce,goal<-5,maxi<-3,repetitions<-nrow(ce),trigger<-1)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,5]!=e[,5]) )

#' # The process could be done for the 4 collum as well

#' ce<-e
#' ce[sample(1:nrow(e),0.5*nrow(e)),4]=NA
#' #So 50 percent of the data is now missing
#' #Lets try to recover it then with autocomplete
#' ce1<-AutoComplete(df<-ce,goal<-4,maxi<-4,repetitions<-nrow(ce),trigger<-1)
#' #We can see how many values are still missing with NA_VALUES
#' print(NA_VALUES(ce1) )
#' #And check how many we got wrong by
#' print(sum(ce1[,4]!=e[,4]) )
#' #Here we can easily see e holds the original data
#' #ce1 is the recovered data

#'@export
AutoComplete<-function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
  # The function AutoComplete was made for people who dont wish to explore and check the expected accuracy just wants their data filled
  # It automates the process of filling the data finding the set and filling the data frame
  if(class(df)[1]=="data.table"){
	  AutoCompleteTable(df,goal,maxi,repetitions,trigger,ratio)
  }else{
    z1<-BestVector(df,goal = goal,maxi = maxi,repetitions = repetitions,trigger = trigger,ratio=ratio)
    df2<-CompleteDataset(df = df,rows = z1,goal = goal)
    return(df2)
  }
}


#'\code{AutoCompleteTable} Asks for a data.table, a vector of collumn indices and the goal collumn and  returns the data frame with the values filled
#' @param df A data.table with the missing values you wish to fill
#' @param goal The collum with the missing values you wish to fill
#' @param maxi What will be the length of possible combinations you will test example if 2 they will test up to all possible pairs of collums
#' @param repetitions Measure of error, the bigger the less likely you will get the right prediction
#' @param ratio Rejects collumns that the ratio of unique values to total values is higher than this value, primary keys have ratio equal to 1
#' @param trigger When you pair all possible combination of tuples a percentage of them will show only once, trigger rejects the set if this percentage is higher than this value
#'@export
AutoCompleteTable<-function(df,goal,maxi,repetitions,trigger=1,ratio=0.99){
  #This function is a data.table implementation of AutoComplete refer to it for comments
  z1<-BestVectorTable(df,goal = goal,maxi = maxi,repetitions = repetitions,trigger = trigger,ratio = ratio)
  df2<-CompleteDatasetTable(df = df,rows = z1,goal = goal)
  return(df2)
}

#'\code{MeanAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn the expected value of accuracy of filling missing values if the dataset is representative
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#' @examples
#' #The Mean accuracy function tells its user the expected accuracy.
#' #Code with two ## is working code but takes longer than 5 seconds
#' #Given a set and a goal to predict it supposes the following.
#' #All missing values are representative of the dataset.
#' #Lets first Consider the iris dataset
#' #It has the following parameters
#' print(names(iris))
#' #As we can see the 5 collumn is species
#' #Lets use Sepal.Length to predict Species and see Mean accuracy
#' print(MeanAccuracy(iris,1,5))
#' #Now lets use both sepal parameters
#' ##print(MeanAccuracy(iris,1:2,5))
#' #And when using a Petal parameter as well
#' print(MeanAccuracy(iris,1:3,5))
#' #We can see that iris even in the Mean case scenario species can be defined by these 3
#' #Now lets take a look at the mtcars dataset
#' ##print(names(mtcars))
#' #Predicting gear using mpg
#' ##print(MeanAccuracy(mtcars,1,10))
#' #But if we try to predict mpg using gear
#' ##print(MeanAccuracy(mtcars,10,1))
#' #So using the Mean accuracy function we can know whats the mean case accuracy
#' #If the user requires he can also predict more than 1 goal for example
#' ##print(MeanAccuracy(mtcars,c(1,3,5),c(10,11)))
#' #In this case we want to use mpg,disp,drat to predict a pair gear,carb
#' #To check the confidence of predicted values the user should use all three accuracy functions

#'@export
MeanAccuracy<-function(df,VECTORS,goal){
  if(class(df)[1] == "data.table"){
    MeanAccuracyTable(df,VECTORS,goal)
  }else{
    Distributions<-plyr::count(df[,c(VECTORS,goal)  ]) # Builds frequency table
    unicos<-unique(df[,VECTORS]) #Builds unique table
    lista<-list()
    if(length(VECTORS) > 1){
      Size<-nrow(unicos)
    }else{
      Size<-length(unicos)
    }
    for(j in 1:Size){
      vec<-c()
      for(i in 1:nrow(Distributions)){
        if(length(VECTORS) > 1   ){
          vec[i]=sum(Distributions[i,1:ncol(unicos)] == unicos[j,1:ncol(unicos)]) == ncol(unicos)
        }else{
          vec[i]=Distributions[i,1] == unicos[j]
        }
      }
      lista[[j]]<-vec
    }
    lista<-lapply(lista,which)
    maximum<-sum(Distributions$freq) # Calculates total number of elements
    prob<-0
  #print()
    for(i in 1:length(lista)){
      if(length(lista[[i]]) == 1 ){
        prob<-prob+Distributions$freq[lista[[i]]]/maximum # probability of chosing the element is 100%
      }else{
        prob_auxiliar<-Distributions$freq[unlist(lista[[i]])]
        acumulated<-sum(prob_auxiliar)
        Pe<-prob_auxiliar/acumulated #probabilidade of being chosen
        Pa<-prob_auxiliar/maximum # Probability of appearing in the dataset
        prob<-prob+sum(Pe*Pa)
      }
    }
    return(prob)
  }
}


#'\code{MeanAccuracyTable} Asks for a data.table, a vector of collumn indices and the goal collumn the expected value of accuracy of filling missing values if the dataset is representative
#' @param df A data.table that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
MeanAccuracyTable<-function(df,VECTORS,goal){
  #This function is a data.table implementation of MeanAccuracy refer to it for comments
  auxiliar_table<-c(VECTORS,goal)
  Distributions<-plyr::count(df[,..auxiliar_table ])
  unicos<-unique(df[,..VECTORS])
  lista<-list()

  Size<-nrow(unicos)


  for(j in 1:Size){
    vec<-c()
    for(i in 1:nrow(Distributions)){

      vec[i]<-sum(Distributions[i,1:ncol(unicos)] == unicos[j,1:ncol(unicos)]) == ncol(unicos)

    }
     lista[[j]]<-vec
  }

  lista<-lapply(lista,which)
  maximum<-sum(Distributions$freq)
  prob<-0

  for(i in 1:length(lista)){
    if(length(lista[[i]]) == 1 ){
      prob<-prob+Distributions$freq[lista[[i]]]/maximum # probabilidade de escolhelo 100%
    }else{
      prob_auxiliar<-Distributions$freq[unlist(lista[[i]])]
      acumulated<-sum(prob_auxiliar)
      Pe<-prob_auxiliar/acumulated #probabilidade de ser escolhido
      Pa<-prob_auxiliar/maximum # probabilidade global
      prob<-prob+sum(Pe*Pa)
    }
  }
  return(prob)
}

#'\code{BestAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn and returns the maximum possible value of accuracy of filling missing values
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#' @examples
#' #The Best accuracy function tells its user the best accuracy possible.
#' #Code with two ## is working code but takes longer than 5 seconds
#' #Given a set and a goal to predict it supposes the following.
#' #All missing values are contained in the possible values with lowest uncertainty.
#' #Lets first Consider the iris dataset
#' #It has the following parameters
#' print(names(iris))
#' #As we can see the 5 collumn is species
#' #Lets use Sepal.Length to predict Species and see Best accuracy
#' print(BestAccuracy(iris,1,5))
#' #Now lets use both sepal parameters
#' print(BestAccuracy(iris,1:2,5))
#' #And when using a Petal parameter as well
#' ##print(BestAccuracy(iris,1:3,5))
#' #We can see that iris even in the best case scenario species can be defined by these 3
#' #Now lets take a look at the mtcars dataset
#' ##print(names(mtcars))
#' #Predicting gear using mpg
#' ##print(BestAccuracy(mtcars,1,10))
#' #But if we try to predict mpg using gear
#' ##print(BestAccuracy(mtcars,10,1))
#' #So using the best accuracy function we can know whats the best case accuracy
#' #If the user requires he can also predict more than 1 goal for example
#' ##print(BestAccuracy(mtcars,c(1,3,5),c(10,11)))
#' #In this case we want to use mpg,disp,drat to predict a pair gear,carb
#' #To check the confidence of predicted values the user should use all three accuracy functions

#'@export
BestAccuracy<-function(df,VECTORS,goal){
  #The best accuracy function works like the mean accuracy function, its main difference is it will find the value with highest confidence and return the accuracy to predict that.
  if(class(df)[1] == "data.table"){
	BestAccuracyTable(df,VECTORS,goal)
  }else{
    AuxiliarVector<-c()
    Distributions<-plyr::count(df[,c(VECTORS,goal)  ])
    unicos<-unique(df[,VECTORS])
    lista<-list()
    if(length(VECTORS)>1){
      Size<-nrow(unicos)
    }else{
      Size<-length(unicos)
    }
    for(j in 1:Size){
      vec<-c()
      for(i in 1:nrow(Distributions)){
        if(length(VECTORS)>1   ){
          vec[i] <- sum(Distributions[i,1:ncol(unicos)] == unicos[j,1:ncol(unicos)]) == ncol(unicos)
        }else{
          vec[i] <- Distributions[i,1] == unicos[j]
        }
      }
      lista[[j]] <- vec
    }
    lista<-lapply(lista,which)
    maximum<-sum(Distributions$freq)
    prob<-0

    for(i in 1:length(lista)){
      if(length(lista[[i]]) == 1 ){

        AuxiliarVector[i]<-1
      }else{
        prob_auxiliar<-Distributions$freq[unlist(lista[[i]])]
        acumulated<-sum(prob_auxiliar)
        Pe<-prob_auxiliar/acumulated # Probability of being chosen
        Pa<-prob_auxiliar/maximum # Probability of being in the dataset
        prob<-prob+sum(Pe*Pa)
        AuxiliarVector[i]<-sum(Pe*Pe)
      }
    }
    return(max(AuxiliarVector)) # Returns the max Accuracy
  }
}

#'\code{BestAccuracyTable} Asks for a data.table, a vector of collumn indices and the goal collumn and returns the maximum possible value of accuracy of filling missing values
#' @param df A data.table that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
BestAccuracyTable<-function(df,VECTORS,goal){
  # This function is a data table implementation of BestAccuracy refer to it for comments
  AuxiliarVector<-c()
  auxiliar_table<-c(VECTORS,goal)
  Distributions<-plyr::count(df[,..auxiliar_table ])
  unicos<-unique(df[,..VECTORS])
  lista<-list()

  Size<-nrow(unicos)


  for(j in 1:Size){
    vec<-c()
    for(i in 1:nrow(Distributions)){

      vec[i]=sum(Distributions[i,1:ncol(unicos)] == unicos[j,1:ncol(unicos)]) == ncol(unicos)

    }
    lista[[j]]<-vec
  }

  lista<-lapply(lista,which)
  maximum<-sum(Distributions$freq)
  prob<-0

  for(i in 1:length(lista)){
    if(length(lista[[i]]) == 1 ){

      AuxiliarVector[i]<-1
    }else{
      prob_auxiliar<-Distributions$freq[unlist(lista[[i]])]
      acumulated<-sum(prob_auxiliar)
      Pe<-prob_auxiliar/acumulated #probabilidade de ser escolhido
      Pa<-prob_auxiliar/maximum # probabilidade global
      prob<-prob+sum(Pe*Pa)
      AuxiliarVector[i]<-sum(Pe*Pe)
    }
  }
  return(max(AuxiliarVector))
}

#'\code{WorstAccuracy} Asks for a dataframe, a vector of collumn indices and the goal collumn and returns the minimum possible value of accuracy of filling missing values
#' @param df A dataframe that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#' @examples
#' #The Worst accuracy function tells its user the worst case accuracy possiblle.
#' #Code with two ## is working code but takes longer than 5 seconds
#' #Given a set and a goal to predict it supposes the following.
#' #All missing values are contained in the possible values with highest uncertainty.
#' #Lets first Consider the iris dataset
#' #It has the following parameters
#' print(names(iris))
#' #As we can see the 5 collumn is species
#' #Lets use Sepal.Length to predict Species and see Worst accuracy
#' print(WorstAccuracy(iris,1,5))
#' #Now lets use both sepal parameters
#' print(WorstAccuracy(iris,1:2,5))
#' #And when using a Petal parameter as well
#' ##print(WorstAccuracy(iris,1:3,5))
#' #We can see that iris even in the worst case scenario species can be defined by these 3
#' #Now lets take a look at the mtcars dataset
#' ##print(names(mtcars))
#' #Predicting gear using mpg
#' ##print(WorstAccuracy(mtcars,1,10))
#' #But if we try to predict mpg using gear
#' ##print(WorstAccuracy(mtcars,10,1))
#' #So using the Worst accuracy function we can know whats the worst case accuracy
#' #If the user requires he can also predict more than 1 goal for example
#' ##print(WorstAccuracy(mtcars,c(1,3,5),c(10,11)))
#' #In this case we want to use mpg,disp,drat to predict a pair gear,carb
#' #To check the confidence of predicted values the user should use all three accuracy functions

#'@export
WorstAccuracy<-function(df,VECTORS,goal){
  #The worst accuracy function shall behave exactly like best accuracy but return the accuracy of the set with lowest confidence
  #Refer to best and mean accuracy for details of implementation
  if(class(df)[1] == "data.table"){
	  WorstAccuracyTable(df,VECTORS,goal)
  }else{
    AuxiliarVector<-c()
    Distributions<-plyr::count(df[,c(VECTORS,goal)  ])
    unicos<-unique(df[,VECTORS])
    lista<-list()
    if(length(VECTORS) > 1){
      Size<-nrow(unicos)
    }else{
      Size<-length(unicos)
    }
    for(j in 1:Size){
      vec<-c()
      for(i in 1:nrow(Distributions)){
        if(length(VECTORS) > 1   ){
          vec[i]=sum(Distributions[i,1:ncol(unicos)] == unicos[j,1:ncol(unicos)]) == ncol(unicos)
        }else{
          vec[i]=Distributions[i,1] == unicos[j]
        }
      }
      lista[[j]]<-vec
    }
    lista<-lapply(lista,which)
    maximum<-sum(Distributions$freq)
    prob<-0

    for(i in 1:length(lista)){
      if(length(lista[[i]]) == 1 ){

        AuxiliarVector[i]<-1
      }else{
        prob_auxiliar<-Distributions$freq[unlist(lista[[i]])]
        acumulated<-sum(prob_auxiliar)
        Pe<-prob_auxiliar/acumulated #Probability of being choosen
        Pa<-prob_auxiliar/maximum # Probability of it appearing in the dataset
        prob<-prob+sum(Pe*Pa)
        AuxiliarVector[i]<-sum(Pe*Pe)
      }
    }
    return(min(AuxiliarVector))
  }
}



#'\code{WorstAccuracyTable} Asks for a data.table, a vector of collumn indices and the goal collumn and returns the minimum possible value of accuracy of filling missing values
#' @param df A data.table that you intend to fill missing values, warning this dataframe shall contain no missing values so the user must drop the lines it happens
#' @param goal The collum with the missing values you wish to fill
#' @param VECTORS The collumns you wish to use to predict the missing values
#'@export
WorstAccuracyTable<-function(df,VECTORS,goal){
  #This function is a data table implementation of Worst Accuracy refer to it for comments
  AuxiliarVector<-c()
  auxiliar_table<-c(VECTORS,goal)
  Distributions<-plyr::count(df[,..auxiliar_table ])
  unicos<-unique(df[,..VECTORS])
  lista<-list()

  Size<-nrow(unicos)


  for(j in 1:Size){
    vec<-c()
    for(i in 1:nrow(Distributions)){

     vec[i]=sum(Distributions[i,1:ncol(unicos)] == unicos[j,1:ncol(unicos)]) == ncol(unicos)

    }
    lista[[j]]<-vec
  }

  lista<-lapply(lista,which)
  maximum<-sum(Distributions$freq)
  prob<-0

  for(i in 1:length(lista)){
    if(length(lista[[i]]) == 1 ){

      AuxiliarVector[i]<-1
    }else{
      prob_auxiliar<-Distributions$freq[unlist(lista[[i]])]
      acumulated<-sum(prob_auxiliar)
      Pe<-prob_auxiliar/acumulated #Probability of being chosen
      Pa<-prob_auxiliar/maximum # Probability of appearing in the dataset
      prob<-prob+sum(Pe*Pa)
      AuxiliarVector[i]<-sum(Pe*Pe)
    }
  }
  return(min(AuxiliarVector))
}
