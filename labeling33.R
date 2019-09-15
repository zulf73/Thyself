# trait set labels by 33 factors

vector_sign_index<-function(v)
{
  sh<-sign(v)
  shc<-paste(sh)
  #print(shc)
  shd<-gsub("-1","0",shc)
  #print(shd)
  
  bits<-paste(shd,collapse="")
  #print(bits)
  ans<-strtoi( bits, base=2)
  ans
}

sixteen_to_five_factors<-function(xx){
  ans<-rep(0,5)
  ans[1]<-sum(xx[1:4])
  ans[2]<-sum(xx[5:7])
  ans[3]<-sum(xx[8:10])
  ans[4]<-sum(xx[11:13])
  ans[5]<-sum(xx[14:16])
  ans
}

show_histogram<-function( label, trait_num ){
  choice<-which(labels30==label)
  subtraits<-sx_traits_values[ choice, trait_num ]
  hist(subtraits,probability=TRUE)
}

do_pairwise_ttest_on_traits<-function( trait_num)
{
  df<-data.frame( label=labels30, trait_val=sx_traits_values[,trait_num])
  #L=length(all_labels)
  L=100
  diff_matrix<-matrix(0,nrow=L,ncol=L)
  pmatrix<-matrix(0,nrow=L,ncol=L)
  for (j in 1:(L-1)){
    v<-na.omit(df$trait_val[df$label==all_labels[j]])
    for (k in (j+1):L){
      w<-na.omit(df$trait_val[df$label==all_labels[k]])
      if (length(v)>3 && length(w)> 3) {
        out<-t.test( v, w, na.rm=T)
        diff<-out$estimate[1]-out$estimate[2]
        print( paste('j=', j, 'k=',k, 'diff=',diff, 'p=',out$p.value))
        diff_matrix[j,k]<-diff  
        diff_matrix[k,j]<- -diff  
        pmatrix[j,k]<-out$p.value
        pmatrix[k,j]<-out$p.value
      }
    }
  }
  list(D=diff_matrix,P=pmatrix)
}

plot_by_three_params<-function( class_number, ocean, trait_num )
{
  indices<-which(labels30==all_labels[class_number])
  plot( sx[indices,ocean],sx_traits_values[indices,trait_num],type='p',pch='o')
}

plot_by_three_params2<-function( class_number, ocean, trait_num )
{
  indices<-which(labels30==all_labels[class_number])
  x<-sx_personality_dna[indices,ocean]
  y<-sx_traits_values[indices,trait_num]
  m<-lm(y~x)
  plot( x,y,type='p',pch='o', main=summary(m)$r.squared)
  abline(m)
}

rsquared_by_class<-function( class_number, trait_num)
{
  indices<-which(labels30==all_labels[class_number])
  
  mparams<-matrix(0,nrow=33,ncol=3)
  for (ocean in 1:33){ 
    x<-sx_personality_dna[indices,ocean]
    y<-sx_traits_values[indices,trait_num]

    res<-tryCatch( {m<-lm(y~x)},
                   warning=function(e){e},
                   error = function(e){e} )
    
    mparams[ocean,1:2]<-m$coefficients[1:2]
    mparams[ocean,3]<-summary(m)$r.squared
  }
  if (trait_num==1){
    print(paste('class=',class_number, 'trait=', trait_num, 'good values=', sum(mparams[,3]>0.15)))
  }
  mparams
}

best_rsquared_by_class<-function( class_number, trait_num)
{
  indices<-which(labels30==all_labels[class_number])
  
  mparams<-matrix(0,nrow=33,ncol=3)
  for (ocean in 1:33){ 
    x<-sx_personality_dna[indices,ocean]
    y<-sx_traits_values[indices,trait_num]
    
    res<-tryCatch( {m<-lm(y~x)},
                   warning=function(e){e},
                   error = function(e){e} )
    
    mparams[ocean,1:2]<-m$coefficients[1:2]
    mparams[ocean,3]<-summary(m)$r.squared
  }
  if (trait_num==1){
    print(paste('class=',class_number, 'trait=', trait_num, 'good values=', sum(mparams[,3]>0.15)))
  }
  best_rsq_idx<-which.max(mparams[,3])
  ans<-list(rsq=mparams[best_rsq_idx,3], i=best_rsq_idx, a=mparams[best_rsq_idx,1],
       b=mparams[best_rsq_idx,2])
  print(paste('c=',class_number, 't=',trait_num, 'i=', ans$i, 'r=', ans$rsq))
  ans
}


n_classes<-length(all_labels)


fill_class_trait_rsq<-function(){
  class_trait_rsq<-matrix(0, nrow=n_classes, ncol=74)
  for (k in 1:n_classes){
    for ( j in 1:74){
      rsq<-rsquared_by_class(k,j)
      good_rs<-sum( rsq[,3]>0.2)
      class_trait_rsq[ k, j ]<-good_rs
    }
  }
  class_trait_rsq
}

fill_class_trait_best_rsq<-function(){
  data<-c()
  #n_classes<-length(all_labels)
  n_classes<-200
  for (k in 1:n_classes){
    for ( j in 1:74){
      brsq<-best_rsquared_by_class(k,j)
      if (is.null(brsq$rsq) || is.na(brsq$rsq)){
        new_row<-c(k,j, 1, 0,0,0)
      } else {
        new_row<-c( k, j, brsq$i, brsq$a, brsq$b, brsq$rsq)
      }
      data<-rbind(data, new_row)
    }
  }
  data
}


find_sparse_trait_description_by_33_vars<-function( class_trait_best_rsq_matrix, noise_level )
{
  C<-class_trait_best_rsq_matrix
  Ct<-matrix( 0, nrow=74, ncol = 33 )
  for ( g in 1:dim(C)[1] ){
    row<-C[g,]
    tn<-row[2]
    idx<-row[3]
    Ct[tn,idx]<-Ct[tn,idx]+1
  }
  Ct
}
