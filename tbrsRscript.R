tbrs=function(taskString,startType="first",type="steady",baseline=2,duration=5,d=.3,r=1){
  # taskString = task string with "L" = letter, "0" = free time, "1" = dual task
  # startType= "first", "next" or "lowest"
  # type = "steady" or "threshold"
  # all computations made in log-odds
  # duration is length of focus (steady) in tenth of seconds or threshold in log-odds (threshold)
  # baseline is the log-odds of activation during presentation
  # d is decay rate in point of log-odds by second
  # r is refreshment rate
  
  
  # transform taskString into task vector, under the name of task
  n=nchar(taskString)
  task=rep(0,n)
  item=1
  string=strsplit(taskString,split="")
  string=unlist(string)
  for (i in 1:n) {
    x=string[i]
    if (x=="L"){task[i]=item; item=item+1}
    if (x=="0"){task[i]=0}
    if (x=="1"){task[i]=-1}
  }
  
  
  # init focus-matrix
  expand=10 # change if you want anything else than tenth of seconds
  expand.task=rep(task,each=expand) # expand.task is the task function in tenth of sec
  n=length(expand.task) # number of tenth of seconds
  m=max(task) # number of items
  focus.matrix=matrix(NA,ncol=n,nrow=m)
  
  # init LO matrix (log-odds)
  lo.matrix=focus.matrix
  
  # init items stuffs
  currentItem=1 # current item to refresh
  repItem=0 # how many time it is has already been refreshed
  numItem=0 # number of item already seen
  restart=F # are we currently restarting after dual tasks or presentation ?
  if (expand.task[1]>0){focus.matrix[expand.task[1],1]=2; lo.matrix[expand.task[1],1]=baseline}
  ### NOTE : in focus matrix 0 means not focused, 1 focused, 2 means an item is presented
  
  
  
  # starting the loop
  for (i in 2:n){
    
    # init
    k=expand.task[i] # current task state
    focus.matrix[,i]=focus.matrix[,(i-1)]-focus.matrix[,(i-1)] # set current column at 0/NAs in focus matrix
    
    # case k=-1, that is no refreshment at all
    if (k==-1){
      restart=T # restart next time
      lo.matrix[,i]=lo.matrix[,(i-1)]-d/expand # everybody down
    }
    
    # case k>0, that is item k is presented
    if (k>0){
      focus.matrix[k,i]=2 # focus.matrix OK
      numItem=k # items must be in orders
      restart=T # restart next time
      lo.matrix[,i]=lo.matrix[,(i-1)]-d/expand # everybody down
      lo.matrix[k,i]=baseline # except for item k
      currentItem=k
    } 
    
    # now the hard case : k==0, that is spare time
    if (k==0){
      
      # changing repItem and currentItem in case we restart now
      if (restart){
        repItem=1
        if (startType=="first"){currentItem=1}
        if (startType=="next"){currentItem=currentItem+1; if (currentItem>numItem){currentItem=1}}
        if (startType=="lowest"){currentItem=which(lo.matrix[,(i-1)]==min(lo.matrix[,(i-1)],na.rm=T))}
      }
      
      # updating matrices
      focus.matrix[currentItem,i]=1
      lo.matrix[,i]=lo.matrix[,(i-1)]-d/expand # everybody down
      lo.matrix[currentItem,i]=lo.matrix[currentItem,(i-1)]+r/expand # except currentItem
      
      # updating currentItem and repItem
      repItem=repItem+1
      
      # in the steady case
      if (type=="steady" && repItem>duration) {
        repItem=1
        currentItem=currentItem+1
        if (currentItem>numItem){currentItem=1}}
      
      # in the threshold case
      if (type=="threshold" && lo.matrix[currentItem,i]>=duration) # duration is the threshold
      {
       # repItem=0
        if (startType=="lowest"){currentItem=which(lo.matrix[,i]==min(lo.matrix[,i],na.rm=T))}
        if (startType!="lowest"){
          currentItem=currentItem+1
          if (currentItem>numItem){currentItem=1}
        }
      }
      
      restart=F
    } # end of "k==0" case
    
    
  } # end of loop for i

  
  
  
  
  ### PLOTING
    # colors... up to 6 iterms (item 1 is red)
    # cols=c("red","blue","darkgrey","darkgreen","magenta","orange","pink","slateblue","violetred")
   

  
    # mat is the log-odds matrix
    mat=exp(lo.matrix) # odds matrix
    mat=mat/(1+mat) # probability matrix
    m=nrow(mat) # number of items
    n=ncol(mat) # time
  
  
    cols=rainbow(m,v=.7)
  
    # calcul des limites
    ymin=min(mat,na.rm=T)
    ymax=max(mat,na.rm=T)
    l=(ymax-ymin)/6
    taskHeight=ymax+2*l
    focusHeight=ymax+l
    ymax=ymax+2*l
  
  
   
    # init by drawing first line
    ruler=seq(1/expand,n/expand,1/expand)
    plot(ruler,mat[1,],type="l",col=cols[1],xlab="time (s.)",ylab="prop. correct",
         main="",ylim=c(ymin,ymax),las=1,yaxt="n")
    axis(side=2, at=c(round(ymin,digits=2),round(ymax-2*l,digits=2),focusHeight,taskHeight), labels=c(round(ymin,digits=2),round(ymax-2*l,digits=2),"focus","task"), las=1)
    
  
    # draw task at taskHeight
    col.task=rep("white",n)
    col.task[expand.task==-1]="black"
    col.task[expand.task>0]=cols[expand.task[expand.task>0]]
    segments(ruler,rep(taskHeight,n),ruler+1/expand,rep(taskHeight,n),col=col.task,lwd=10,lend="square")

    # draw focus at focusHeight
  col.focus=rep("white",n) 
  for(i in 1:n){
    x=max(focus.matrix[,i],na.rm=T)
    if (is.na(x)){x=0}
    if (x>0){col.focus[i]=cols[which(focus.matrix[,i]==x)]}
  }
  segments(ruler,rep(focusHeight,n),ruler+1/expand,rep(focusHeight,n),col=col.focus,lwd=10,lend="square")
  
  
    # add remaining lines
  if (m>1){
    for (i in 2:m){
      points(c(1:n)/10,mat[i,],type="l",col=cols[i])
    }}
    

  
  return(mat[,n]) # to get the entire matrix, type "return(mat)" instead
  
}
