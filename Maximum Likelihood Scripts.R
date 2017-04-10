##################
# Maximum Likelihood estimate
# Companion R-Script
# For the TBRS2 model
# R-script by Nicolas Gauvrit
# TBRS2 model by Nicolas Gauvrit and Fabien Mathy
##################


## Function recode below change the code of
## the task, from a letter notation
## i.e. LL01001 where L is a letter
## 1 means concurrent tasks
## and 0 means spare time. Each corresponding
## to 1 second.
## The function returns a vectors coded like a Task
## function, each column corresponding to 0.1 second

recode = function(string){
  n=nchar(string)
  task=rep(0,n)
  item=1
  string=strsplit(string,split="")
  string=unlist(string)
  for (i in 1:n) {
    x=string[i]
    if (x=="L"){task[i]=item; item=item+1}
    if (x=="0"){task[i]=0}
    if (x=="1"){task[i]=-1}
  }
  return(task)
}




## Function taskToActivation compute the log-odds of recall
## i.e. activation, from a task function.
## it returns a matrix of activations. Each column corresponds
## to a tenth of second, each row to an item.
## The last column gives the log-odds used in the
## computation of the LL (see below).

taskToActivation=function(task,startType="first",type="steady",baseline=2,duration=2,d=1,r=3){
  # task = task vector by second. Is expanded.
  # startType= "first", "next" or "lowest"
  # type = "steady" or "threshold"
  # all computations made in log-odds
  # duration = length of focus (steady) in tenth of seconds or
  # threshold = log-odds (threshold)
  # baseline = the log-odds of activation at the end of
  # presentation
  # d = decay rate in point of log-odds by second
  # r = refreshment rate
  
  # init focus-matrix
  expand=10 # change if you want anything else than tenth of seconds
  expand.task=rep(task,each=expand)
  n=length(expand.task)
  m=max(task)
  focus.matrix=matrix(NA,ncol=n,nrow=m)
  
  # init LO matrix (log-odds)
  lo.matrix=focus.matrix
  
  # init items stuffs
  currentItem=1 # current item to refresh
  repItem=0 # how many time is has already been refreshed
  numItem=0 # number of item already seen
  restart=F # are we currently restarting after dual tasks or presentation ?
  if (expand.task[1]>0){focus.matrix[expand.task[1],1]=2; lo.matrix[expand.task[1],1]=baseline}
  
  # starting the loop
  for (i in 2:n){
    
    # init
    k=expand.task[i] # current task state
    focus.matrix[,i]=focus.matrix[,(i-1)]-focus.matrix[,(i-1)] # set current column at 0/NAs in focus matrix
    
    # case k=-1, that is no refreshment at all
    if (k==-1){
      restart=T # restart next time
      lo.matrix[,i]=lo.matrix[,(i-1)]-1*d/expand # everybody down
    }
    
    # case k>0, that is item k is presented
    if (k>0){
      focus.matrix[k,i]=2 # focus.matrix OK
      numItem=k # items must be in orders
      restart=T # restart next time
      lo.matrix[,i]=lo.matrix[,(i-1)]-1*d/expand # everybody down
      lo.matrix[k,i]=baseline # except for item k
      currentItem=k} 
    
    # now the hard case : k==0, that is spare time
    if (k==0){
      
      # changing repItem and currentItem in case we restart
      if (restart){
        repItem=1
        if (startType=="first"){currentItem=1}
        if (startType=="next"){currentItem=currentItem+1; if (currentItem>numItem){currentItem=1}}
        if (startType=="lowest"){currentItem=which(lo.matrix[,(i-1)]==min(lo.matrix[,(i-1)],na.rm=T))[1]}}
      
      # updating matrices
      focus.matrix[currentItem,i]=1
      lo.matrix[,i]=lo.matrix[,(i-1)]-1*d/expand # everybody down
      lo.matrix[currentItem,i]=lo.matrix[currentItem,(i-1)]+r/expand # except currentItem
      
      # updating currentItem and repItem
      repItem=repItem+1
      
      # in the steady case
      if (type=="steady" && repItem>duration) {
        repItem=1
        currentItem=currentItem+1
        if (currentItem>numItem){currentItem=1}}
      
      # in the threshold case
      if (type=="threshold" && lo.matrix[currentItem,i]>=duration) # parameter is the threshold
      {
        repItem=0
        if (startType=="lowest"){currentItem=which(lo.matrix[,i]==min(lo.matrix[,i],na.rm=T))}
        if (startType!="lowest"){
          currentItem=currentItem+1
          if (currentItem>numItem){currentItem=1}
        }}
      restart=F
    } # end of "k==0" case    
  } # end of loop for i
  return(lo.matrix)}





## Function LL computes the log-likelihood of
## a particular task and an observation
## being a vector of binary digits, 0 meaning not correctly
## recalled, 1 meaning correctly recalled.

LL=function(task,obs,LLstartType="first",LLduration=3,LLtype="steady",LLd=1,LLr=3,LLbaseline=2){
  # parameter = focus duration or threshold
  # startType= 'first', 'lowest' or 'next'
  # type = "steady" or "threshold"
  # d = decay rate
  # r = refreshment rate
  x=taskToActivation(task,d=LLd,r=LLr,duration=LLduration,type=LLtype,startType=LLstartType,baseline=LLbaseline)
  y=x[,dim(x)[2]]
  y=exp(y)/(1+exp(y))
  proba=prod(y*obs+(1-y)*(1-obs))
  L=log(proba)
  return(L)}






## Function LLdf takes a dataframe as input. The dataframe
## the dataframe corresponds to a partiticipant set of
## trials. The first columns must be the tasks in the letter
## form, e.g., "LL0101". There must be variable "obs"
## giving correct answers. This variable must be declared
## as characters. For instance, "0011" indicates that there
## was 4 items to recall. The two last were recalled, but not
## the first two.
## The function return the log-likelihood associated with
## the complete dataset.

LLdf=function(df,LDstartType="first",LDduration=3,LDtype="steady",LDd=1,LDr=3,LDbaseline=2){
  L=0
  for (i in 1:dim(df)[1]){
    task=recode(df[i,1])
    obs=df$obs[i]
    obs=unlist(strsplit(obs,""))
    obs=as.numeric(obs) L=L+LL(task,obs,LLstartType=LDstartType,LLduration=LDduration,LLtype=LDtype,LLd=LDd,LLr=LDr,LLbaseline=LDbaseline)}
  return(L)}






## Function fun returns -LL of a dataframe called df
## values for duration, type and start type can be changed
## if needed. This function is used to fin the maximum
## likelihood estimate.

fun=function(theta){
  # theta[1] = decay rate
  # theta[2] = refreshment rate
  # theta[3] = baseline
  x=-LLdf(df,LDtype="steady",LDstartType="first",LDd=theta[1],
          LDr=theta[2],LDduration=2,LDbaseline=theta[3])
  if (is.na(x)){x=1000}
  if (x>1000){x=1000}
  return(x)}





## To get the maximum likelihood estimate, just apply
## the "nlm" function (Non-linear minimization) to function
## "fun" defined above.

mle=nlm(fun,c(.3,1,1))





