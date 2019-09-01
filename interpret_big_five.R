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
