#Optimization for 33 variables
.libPaths("C:/Users/zulfi/userLibs")
library(pracma)
library(hash)
A0<-matrix(rnorm(17*33,mean=0,sd=1),nrow=33,ncol=17)

buckets<-hash()

binarize_hypersurface_evaluations<-function( hyp_evals )
{
  #print(hyp_evals)
  hyp_evals[is.nan(hyp_evals)]<-0
  # hack
  hyp_evals-mean(hyp_evals)
  
  sh<-sign(hyp_evals[1:16])
  # code -1 -> 0
  # 1 -> 1
  shc<-paste(sh)
  #print(shc)
  shd<-gsub("-1","0",shc)
  #print(shd)
  
  bits<-paste(shd,collapse="")
  #print(bits)
  ans<-strtoi( bits, base=2)
  #print(ans)
  ans
}

evaluate_hypersurfaces<-function(p, A){
  ans<-dot(A,c(1,p))
}

#points<-sample_human_personality(N=1000)
#points[is.nan(points)]<-3

fill_buckets<-function( A )
{
  # determine the hypersurface functions
  # for each point evaluate point on 33 hypersurface
  # functions
  # based on whether it is positive or negative 
  # update bucket
  
  for ( pi in 1:nrow(points)){
    p<-points[pi,]
    hyp_evals<-evaluate_hypersurfaces( p, A )
    # index of bucket
    #print(paste('point number ',pi))
    #print(hyp_evals)
    
    index<-binarize_hypersurface_evaluations( hyp_evals )
    #print(index)
    si<-as.character(index)
    #print(si)
    if (is.null(buckets[[si]])){
      buckets[[si]]=1
    } else {
      buckets[[ si ]]<- buckets[[si]]+1
    }
  }
  buckets
}

objective<-function( x ){
  #print('objective')
  #print(x)
  clear(buckets)
  A<-matrix(x,ncol=17,nrow=33)
  buckets<-fill_buckets( A )
  #print(buckets)
  v<-values(buckets)
  ans<-sum( abs(v - 1) ) + dim(points)[2]-length(v)
  print(paste('fn=',ans))
  ans
}

x0<-rep(.1,33*17)

#sol<-optim( x0, objective)
#print(sol$par)

stochastic_optimization<-function(m=100){
  bestx<-rep(0,17*33)
  bestfn<-999999999999999
  for ( it in 1:m){
    x0<-rnorm(17*33)
    print(paste('iteration:',it))
    fn<-objective(x0)
    if ( fn < bestfn ){
      bestfn<-fn
      bestx<-x0
    }
  }
  list(x=bestx,fn=bestfn)
}

personality_dna_metrics<-function(hx, A=A1){
  ans<-A[,1]+A[,2:17] %*% matrix(hx,nrow=16,ncol=1)
  ans
}

names_personality_dna<-function( A ){

  nA<-A
  for (k in 1:33){
    nA[k,]<-A[k,]/norm(matrix(A[k,]),type="2")
  }
  # determine the five column ocean matrix
  Ocean<-matrix(0,nrow=33,ncol=5)
  Ocean[,1]<-rowSums(nA[,2:5])
  Ocean[,2]<-rowSums(nA[,6:8])
  Ocean[,3]<-rowSums(nA[,9:11])
  Ocean[,4]<-rowSums(nA[,12:14])
  Ocean[,5]<-rowSums(nA[,15:17])
  
  s<-sign(Ocean)
  ans<-rep("",33)
  pm<-function(g){
    ans<-""
    if (g>0){
      ans<-"+"
    } else {
      ans<-"-"
    }
    ans
  }
  for(k in 1:33){
    ans[k]<-paste("[",pm(s[k,1]),pm(s[k,2]),pm(s[k,3]),pm(s[k,4]),pm(s[k,5]),"]",collapse=",")
  }
  ans 
}
