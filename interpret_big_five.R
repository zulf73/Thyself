library("MASS")
Sigma<-diag(c(1.249,1.248,1.224,1.295,1.194))^2
mu<-c(3.242,3.617,3.536,3.374,3.326)
mvrnorm(n=1,mu,Sigma)

choose_by_percentile<-function( percentile, textblocks)
{
  choice<-""
  # assume binary
  if (percentile < 0.5){
    choice<-textblocks[1]
  }
  if (percentile >= 0.5){
    choice<-textblocks[2]
  }
  choice
}

#The text comes from https://medium.com/better-humans/what-is-your-big-five-personality-score-96a6e40e743f

t = c(
  "inventiveness and intellectual curiosity, having a preference for variety over routine, and seeking fulfillment in intense, euphoric experiences.",
  "consistency and caution, seeking fulfillment in perseverance, tending to be pragmatic and data-driven.",
  "efficiency and organization. Tendency to be self-disciplined and dependable.",
  'easy-going and perhaps careless. Flexible and spontaneous, but can be perceived as unreliable, sloppy.',
  'a tendency towards being outgoing and energetic. Assertiveness, talkativeness, positive emotions, and a tendency to seek the company of others. High extraversion can be perceived as attention-seeking and domineering.',
  'a tendency towards being solitary and reserved. Often a reflective personality, though low extraversion can be perceived as aloof or self-absorbed.',
  "a tendency to be friendly, compassionate, and cooperative. Indicates ones trusting and helpful nature, and whether one is well-tempered.",
  "an analytic and detached tendency. Often competitive or challenging; can be seen as argumentative or untrustworthy.",
  "tendency to be calm, emotionally stable, and free from persistent negative feelings. However, freedom from negative feelings does not mean that low-scorers experience a lot of positive feelings.",
  "a high emotional reactiveness and vulnerability to stress. Neuroticism may correlate with perceiving many situations as threatening."
)

openness_text<-c( t[2],t[1])
conscientiousness_text<-c(t[4],t[3])
extraversion_text<-c(t[6],t[5])
agreeableness_text<-c(t[8],t[7])
emotional_stability_text<-c(t[10],t[9])

interpret<-function( percentile, textblock){
  choose_by_percentile( percentile, textblock )
}

interpret_big_five_vec<-function( v )
{
  sigma<-sqrt(diag(Sigma))
  p<-pnorm( (v-mu)/sigma, 0, 1 )
  a1<-interpret(p[1], openness_text)
  a2<-interpret(p[2], conscientiousness_text )
  a3<-interpret(p[3], extraversion_text )
  a4<-interpret(p[4], agreeableness_text )
  a5<-interpret(p[5], emotional_stability_text )

  c(a1,a2,a3,a4,a5)  
}

simulate_personality<-function()
{
  v<-mvrnorm(n=1,mu,Sigma)
  d<-interpret_big_five_vec(v)
  
  print('-----------------')
  print('Simulated personality')
  print(paste('o=',v[1],'c=',v[2],'e=',v[3],
              'a=',v[4],'s=',v[5]))
  print('------------------')
  print("Percentiles")
  sigma<-sqrt(diag(Sigma))
  p<-pnorm( (v-mu)/sigma, 0, 1 )
  print(paste('o=',p[1],'c=',p[2],'e=',p[3],
              'a=',p[4],'s=',p[5]))
  
  print(paste('Openness:',d[1]))
  print(paste('Conscientiousness:',d[2]))
  print(paste('Extraversion:',d[3]))
  print(paste('Agreeableness:',d[4]))
  print(paste('Stability:',d[5]))
  
}

subclasses<-function( diophantine_coefficients, val )
{
  n<-sum(diophantine_coefficients)
  qsum<-round( val*n)
  a<-diophantine_coefficients
  sols<-nlde( a=a, n=qsum )
  msols<-sols$solutions
  N<-dim(msols)[2]
  mtx<-matrix(nrow=length(a),ncol=0)
  check_bounds<-function(y){
    out_of_bounds<-FALSE 
    r<-length(y)
    for (idx in 1:r){
      if (y[idx]<1 | y[idx]>5){
        out_of_bounds<-TRUE
        break
      }
    }
    return (!out_of_bounds)
  }
  for (k in 1: N){
    if ( check_bounds( msols[,k]) ){
      mtx<-cbind( mtx, msols[,k])
    }
  }
  mtx
}

extraversion_a<-c(4,3,1)
agreeableness_a<-c(3,3,2)
conscientiousness_a<-c(2,5,2)
neuroticism_a<-c(4,3,1)
openness_a<-c(3,1,3,1)

e_subclasses<-function( v ){
  subclasses( extraversion_a, v)  
}

a_subclasses<-function( v ){
  subclasses( agreeableness_a, v)  
}
c_subclasses<-function( v ){
  subclasses( conscientiousness_a, v)  
}
n_subclasses<-function( v ){
  subclasses( neuroticism_a, v)  
}
o_subclasses<-function( v ){
  subclasses( openness_a, v)  
}

numbered_subclasses<-function(v, k){
  ans<-NULL 
  if (k==1){
    ans<-o_subclasses(v)
  }
  if (k==2){
    ans<-c_subclasses(v)
  }
  if (k==3){
    ans<-e_subclasses(v)
  }
  if (k==4){
    ans<-a_subclasses(v)
  }
  if (k==5){
    ans<-n_subclasses(v)
  }
  ans 
}

monte_carlo_sixteen_traits<-function(N){
  v<-mvrnorm(n=N,mu,Sigma=Sigma)
  sixteen<-c()
  for (k in 1:N){
    if (k %% 100 == 0 ){
      print(paste('iteration',k))
    }
    sixteen_v<-rep(0,16)
    start<-1
    for( g in 1:5){
      val<-v[k,g]
      if (val<1){
        val=1
      }
      if (val>5){
        val=5
      }
      sv<-numbered_subclasses(val,g)
      cm<-rowMeans(sv)
      sixteen_v[start:(start+length(cm)-1)]<-cm
      start<-start+length(cm)
    }
    sixteen<-rbind(sixteen,sixteen_v)
  }
  sixteen
}

sample_empirical_density<-function(x, N=1)
{
  den<-density( x, na.rm=TRUE )
  newx<-sample(x, N, replace=T) + rnorm( N, 0, den$bw)
  newx
}

# sx will be the monte carlo distribution of 16 factor
# personality
sample_human_personality<-function(N=1)
{
  v<-matrix(nrow=N,ncol=16,0)
  for ( n in 1:N){
    for (k in 1:16){
      x<-sx[,k]
      y<-sample_empirical_density( x,N=1)
      v[n,k]<-y
    }
  }
  v
}

jung_bigfive<-matrix( nrow=4, ncol=5)

jung_bigfive[1,]<-c( 0.16,-0.74, 0.03, -0.03, 0.08)
jung_bigfive[2,]<-c( -0.06,0.1, 0.72, 0.04, -0.15)
jung_bigfive[3,]<-c( 0.06,0.19, 0.02, 0.44, -0.15)
jung_bigfive[4,]<-c( 0.11,0.15, 0.30, -0.06, -0.49)

colnames(jung_bigfive)<-c("n","e","o","a","c")
rownames(jung_bigfive)<-c("I","N","F","P")

jung_variables_from_sixteen<-function(v)
{
  A<-jung_bigfive
  B<-matrix(nrow=4,ncol=16)
  Ao<-A[,"o"]
  Ac<-A[,"c"]
  Ae<-A[,"e"]
  Aa<-A[,"a"]
  An<-A[,'n']
  B[,1:4]<-cbind(Ao,Ao,Ao,Ao)
  B[,5:7]<-cbind(Ac,Ac,Ac)
  B[,8:10]<-cbind(Ae,Ae,Ae)
  B[,11:13]<-cbind(Aa,Aa,Aa)
  B[,14:16]<-cbind(An,An,An)

  # normalize row sums
  for (k in 1:dim(B)[1]){
    B[k,]<-B[k,]/sum(B[k,])
  }
  y<-B %*% v
  (y-1)/5.
}

myers_briggs_type<-function(hx)
{
  jv<-jung_variables_from_sixteen(hx)
  ans<-c("a","a","a","a")
  if ( jv[1] < 0.5 ) {
    ans[1]<-"I"
  } else {
    ans[1]<-"E"
  }
  if ( jv[2] < 0.5 ) {
    ans[2]<-"S"
  } else {
    ans[2]<-"N"
  }
  if ( jv[3] < 0.5 ) {
    ans[3]<-"T"
  } else {
    ans[3]<-"F"
  }
  if ( jv[2] < 0.5 ) {
    ans[4]<-"P"
  } else {
    ans[4]<-"J"
  }
  ans
}

bfi<-matrix(nrow=44,ncol=5)
bfi[1,]<-c( .62, .04, -0.04, .11, .08)
bfi[2,]<-c( .62, .24, .08, -0.04, 0.14)
bfi[3,]<-c( .46, .20, .13, -0.05, .32)
bfi[4,]<-c( .44, .20, .20,  -.14, .26 )
bfi[5,]<-c( .33, -0.03, .23, -.15, .26)
bfi[6,]<-c( -.68, .10, .07, .04, .07 )
bfi[7,]<-c( -.53, .08, -.08, .26, .09 )
bfi[8,]<-c( -.5, .03, .09, .12, .07)
bfi[9,]<-c( .00, .59, .15, .05, .13)
bfi[10,]<-c( .03, .51, -.03, -.05, .13)
bfi[11,]<-c( .04, .46, .16, .02, .16)
bfi[12,]<-c( .18, .45, .12, .03, .14)
bfi[13,]<-c( .09, .42, .04, .00, .09)
bfi[14,]<-c( .11, -.41, -.12, .23, .10)
bfi[15,]<-c( .18, -.38, -.09, .22, .04)
bfi[16,]<-c( -.21,-.37, -.04, .12, .15)
bfi[17,]<-c( .06, -.32, -.03, .27, .08)
bfi[18,]<-c( .04, .08, .59, .05, .13)
bfi[19,]<-c( .11,.21, .57, -.05, .20)
bfi[20,]<-c( .02, .08, .53, .01, .17)
bfi[21,]<-c( .04, .20, .52, .08, .12 )
bfi[22,]<-c( .11, .07, .51, -.03, .14)
bfi[23,]<-c( -.1,-.1,-.54,.17,.09)
bfi[24,]<-c( .00, -.03, -.53, .12, .14)
bfi[25,]<-c( .04,-.04,-.46, .11, .17)
bfi[26,]<-c( .01, .01, -.39, .32,.08)
bfi[27,]<-c( -.12, .03, -.03, .63, .03)
bfi[28,]<-c( -.09, -.05, -.07, .58, -.02)
bfi[29,]<-c( -.08, -.06, .04, .58, .06)
bfi[30,]<-c( -.04, -.19, -.06, .48, .11)
bfi[31,]<-c( -.28, -.14, -.14, .46, .03)
bfi[32,]<-c( .05, .13, .02, -.57, .19)
bfi[33,]<-c( .01, .17, .11, -.49, .14)
bfi[34,]<-c( -.02, .13, .15, -.46, .26)
bfi[35,]<-c( .18, -0.05, .12, -.18, .58)
bfi[36,]<-c( .13, .04, .07, .04, .53)
bfi[37,]<-c( .22, .02, .13, -.14, .55)
bfi[38,]<-c( .02, .04, .07,.04, .53)
bfi[39,]<-c( .00, .09, .01, .08, .52)
bfi[40,]<-c( .02, -.01, .19, .00, .47)
bfi[41,]<-c( .01, .01, -.02, -.01, .46)
bfi[42,]<-c( .18, .1, 0.05, .02, .42)
bfi[43,]<-c( -.02, 0.01, .05, .02, -.34)
bfi[44,]<-c( -.1, .06, 0.07, .07, -.21)

colnames(bfi)<-c("e","a","c","n","o")
bfi_desc<-c(
  "Is talkative",
  "Is outgoing, sociable",
  "Generates a lot of enthusiasm",
  "Is full of energy",
  "Has an assertive personality",
  "Tends to be quiet",
  "Is shy, inhibited",
  "Is reserved",
  "Is considerate and kind to almost anyone",
  "Has a forgiving nature",
  "Is helpful and unselfish with others",
  "Likes to cooperate with others",
  "Is generally trusting",
  "Is sometimes rude to others",
  "Starts quarrels with others",
  "Can be cold and aloof",
  "Tends to find fault with others",
  "Does a thorough job",
  "Does things efficiently",
  "Perserveres until the task is finished",
  "Is a reliable worker",
  "Makes plans and follows through",
  "Tends to be lazy",
  "Tends to be disorganized",
  "Can be somewhat careless",
  "Is easily distracted",
  "Worries a lot",
  "Gets nervous easily",
  "Can be tense",
  "Can be moody",
  "Is depressed, blue",
  "Is relaxed, handles stress well",
  "Is emotionally stable, not easily upset",
  "Remains calm in tense situations",
  "Is inventive",
  "Has an active imagination",
  "Is original, has new ideas",
  "Likes to reflect, play with ideas",
  "Values artistic, aesthetic experiences",
  "Is ingenious, deep thinker",
  "Is sophisticated in art, music, or literature",
  "Is curious about many things",
  "Has few artistic interests",
  "Prefers work that is routine"
)

bfi_from_sixteen<-function(v)
{
  A<-bfi
  B<-matrix(nrow=44,ncol=16)
  Ao<-A[,"o"]
  Ac<-A[,"c"]
  Ae<-A[,"e"]
  Aa<-A[,"a"]
  An<-A[,'n']
  B[,1:4]<-cbind(Ao,Ao,Ao,Ao)
  B[,5:7]<-cbind(Ac,Ac,Ac)
  B[,8:10]<-cbind(Ae,Ae,Ae)
  B[,11:13]<-cbind(Aa,Aa,Aa)
  B[,14:16]<-cbind(An,An,An)
  
  # normalize row sums
  for (k in 1:dim(B)[1]){
    B[k,]<-B[k,]/sum(B[k,])
  }
  y<-B %*% v
  (y-1)/5.
}

jung_energy<-c("energy directed outward", "energy directed inward")
jung_gather_data<-c("gather data from senses", 
                    "gather data from possibilities, meaning, big picture")
jung_make_decisions<-c( "make decisions on facts",
                        "step into decision and seek harmony with value system and impact on people")
jung_perceiving_judging<-c("go with the flow", "organize and make decisions")

print_bfi_from_sixteen<-function(v)
{
  y<-bfi_from_sixteen(v)
  for (k in 1:length(y)){
    print(paste(bfi_desc[k], y[k]))
  }
  print('--------------------------')
  mbti<-myers_briggs_type(v)
  
  print('Jung Type:')
  print(mbti)
  if ( mbti[1] == 'E'){
    print( jung_energy[1])
  } else {
    print (jung_energy[2])
  }

  if ( mbti[2] == 'S'){
    print( jung_gather_data[1])
  } else {
    print (jung_gather_data[2])
  }

  if ( mbti[3] == 'T'){
    print( jung_make_decisions[1])
  } else {
    print (jung_make_decisions[2])
  }
  
  if ( mbti[4] == 'P'){
    print( jung_perceiving_judging[1])
  } else {
    print (jung_perceiving_judging[2])
  }
  
}

