
#install.packages("hash")
library("hash")

hurrdata <- read.csv("~/Desktop/Junior_2014_Fall/Stat31/Stat31ProjectStuff/dataset.csv")

#trim function gets rid of white space at the start and end of word"
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#initialize our variables and hash-table used to store our transitions
prev <- "GO"
transitions <- hash()
counter<-0
# ------------------------------------------------------- 

# Make transitions dictionary with all transitions and their 
# counts from date 2000 onwards.

for (i in 1:49569) { #for all of the lines in our data
  name <- hurrdata[i,1]
  AtlanticCat <- substr(name, 1,2) #we create a substring of our
  #Storm data's name-line, as the first two letters of the name-line
  #represents the category of the storm, in particular, an Atlantic
  #storm
  if (AtlanticCat == "AL") { #The beginning of each new storm
    #represents the end of a previous storm and the start of a new
    #one
    subDateStr <- substr(name, 5,8)
    subDate <- as.integer(subDateStr)
    if (subDate >= 2000) { #we omit any hurricane before year 2000
      curr <- "NO"
      trans <- paste(prev, curr, sep="->") #store key as 
      #start_state->transitioned_state
      counter <- counter+1
      if (has.key(trans, transitions)){ #if we already stored the
        #key, just update the value by incrementing 1 every time we
        #see key again.
        setVal <- transitions[[trans]] + 1
        .set(transitions, keys=trans, values=setVal)
      } else { #if key is not already stored, make a new key-value
        #pair storing key as "xx->yy" format and value as 1.
        .set(transitions, keys=trans, values = 1.0)
      }
      prev <- "GO" 
      #at the beginning of storm, set start_state to "GO"
    }
  } else { #If we are not at name-line
    subDateStr <- substr(name, 1, 4)
    subDate <- as.integer(subDateStr) #this is the year of hurricane
    if (subDate >= 2000) { #omit all hurricanes before 2000
      counter<-counter+1
      curr = trim(hurrdata[i,4])
      trans = paste(prev,curr, sep="->")
      if (has.key(trans, transitions)) { # if key already stored
        #rinse and repeat steps outlined above
        setVal <- transitions[[trans]] + 1
        .set(transitions, keys = trans, values = setVal)
      } else {
        .set(transitions, keys = trans, values = 1.0)
      }
      prev = trim(hurrdata[i,4])
    }
  }
}


# get the denominator counts to create the transition probabilities
# ------------------------------------------------------- 
#first initialize our hash-table and source vector


denominators <- hash()
srcVector<-c("NO") #source vector indicates the previous state of the storm, initialize "NO" to indicate absorbing state

transitionsVec <- values(transitions, keys=NULL) #grabs the values 
#of the transitions hash-table and stores them as a vector
endNum <- length(transitionsVec)

for (i in 1:endNum) { #we conglomerate all our data from each 
  #start_state into a denominator, so we can construct probabilities
  #of each transition given a starting state.
  g<-names(transitionsVec[i])
  count<-transitions[[g]]
  src<-substr(g,1,2)
  if (has.key(src, denominators)) { #update start_state
    count<-count+denominators[[src]]
    .set(denominators, keys = src, values = count)
  } else { #store start_state
    singleSrc <- c(src)
    srcVector<-c(singleSrc, srcVector)
    #append to our start_state vector
    .set(denominators, keys = src, values = count)
  }
}

names=as.list(srcVector)

#Here lies a matrix keeping track of start_state to 
#transition_state. rows indicate start_state, and columns indicate
#the transition_state. Corresponding probabilities for each 
#start_state->transition_state can be found by row-column indexing.
transitionsProb <- matrix(nrow=11, ncol=11,data=0, dimnames=list(names,names))

transitionsProb[11,11]=1 #set "NO->NO as 1, since it is an absorbing state"

for (i in 1:length(srcVector)){ #these are our source vectors
  for (k in 1:length(srcVector)){ #these are our next state vectors
    checkKey <- paste(srcVector[i],srcVector[k],sep="->")
    if (has.key(checkKey,transitions)) {
      transitionsProb[i,k]=transitions[[checkKey]]/denominators[[(srcVector[i])]]
    }
  }
}

#this is our transition matrix, it calculates the probability of 
#transitioning to a different state given a particular state in a 
#tropical storm.
print("Here is your transition matrix!")
print("It is labelled: transitionProb")
print(transitionsProb) 

clear(denominators)
rm(denominators)
clear(transitions)
rm(transitions)