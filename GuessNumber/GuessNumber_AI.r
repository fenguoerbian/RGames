############ GuessNumber AI ############
###### This is a simple AI for GuessNumber ######
### This AI is still under develop. ######

###### pre-define functions ######
clc<-function()  cat("\f");

PrintWelcome<-function()
{
  cat("Hello! Welcome to Guess Number! \n
      This is a trial AI!\n
      Hope it will be helpful!\n")
}

GetDigits<-function()
{
  ###### This function sets the numbers of digits ######
  ### Digits: number of digits of each number ###
  cat("Please set the number of digits of each nubmber(default 4, min 1, max 10).\n")
  DigitsTry<-try(Digits<-scan("",what=integer(),n=1, quiet=T),silent=T)
  if(class(DigitsTry)=="try-error") Digits <- 4
  if(length(Digits) == 0) Digits <- 4
  if(Digits < 1 )  Digits <- 1
  if(Digits > 10)  Digits <- 10
  
  cat(paste("Digits = ",Digits, "\n",sep=""))
  return(Digits)
}


GetGuess<-function()
{
  cat("Please set the number of guesses for each number(default 7, min 1, max 20).\n")
  GuessTry<-try(Guess<-scan("",what=integer(),n=1,quiet=T),silent=T)
  if(class(GuessTry)=="try-error")  Guess<-7
  if(length(Guess) == 0) Guess <- 7
  if(Guess < 1) Guess <- 1
  if(Guess > 20)  Guess <- 20
  
  cat(paste("Guess = ",Guess, "\n", sep=""))
  return(Guess)
}

InputAandB<-function()
{
  cat("Please input A:\n")
  A <- scan("",what=integer(),n=1,quiet=T)
  cat("Please input B:\n")
  B <- scan("",what=integer(),n=1,quiet=T)
  cat(paste("Your input: A= ",A, " B= ",B, "\n", sep=""))
  return(data.frame(A=A,B=B))
}

InputHint<-function()
{
  cat("Please input the hints for every digits.\n")
  cat("If there is no hint for some digits, input *.\n")
  temp <- scan("",what=character(),n=1,quiet=T)
  temp <- strsplit(temp,"")[[1]]
  res <- as.integer(temp)
  return(res)
}

GetAandB_AI<-function(Answer,UserGuess,Digits)
{
  ### This function is used to get A and B of the guess ###
  ### A indicates how many numbers in the guess which are right both in value and position ###
  ### B indicates how many numbers in the guess which are right in value but wrong in position ###
  ### If a number has been check as A, then it will not be check as B ###
  
  ### Here are some examples: ###
  ### Answer: 110, Guess: 211. Then A=1, B=1 ###
  ### Answer: 220, Guess: 112. Then A=0, B=1 ###
  ### Answer: 221, Guess: 112. Then A=0, B=3 ###
  ### Answer: 990, Guess: 912. Then A=1, B=0 ###
  
  Index<-1:Digits
  indicator<-rep(T,Digits)
  A <-0
  B <- 0
  for(i in Index)
  {
    if(Answer[i] == UserGuess[i])
    {
      A <- A+1
      indicator[i] <- F
    }
  }
  
  Index<-Index[indicator]
  for(i in Index)
  {
    for(j in Index)
    {
      if(Answer[j] == UserGuess[i])
      {
        B <- B + 1
        break()
      }
    }
  }
  res<-data.frame(A=A,B=B)
  return(res)
}

###### main body ######
GuessNumber<-function()
{
  ### This is the main body of the game ###
  clc()
  
  ### print welcome information ###
  PrintWelcome()
  
  ### Get some parameters of the game ###
  Digits <- GetDigits()
  GuessNum <- GetGuess()
  MyNA<-as.integer("*")
  
  ### Generate the index, also the possible answer choices ###
  Index <- 1:10^Digits - 1
  ind <- rep(T,10^Digits)    # indicator, T means possible answer choices
  
  ### Fixed first Guess ###
  cat("My guess is: ",1:Digits,"\n")
  iter <- 1
  Result <- InputAandB()
  Hint <- InputHint()    # input the hint
  Success <- (Result$A == Digits)
  LastGuess <- rep(0, Digits)
  for(i in 1:Digits) LastGuess[i] <- i
  
  while( (iter<GuessNum)&&(Success==0) )
  {
    ### update the possible answer ###
    for(i in Index[ind])
    {
      PossibleAnswer <- rep(0, Digits)
      temp<- i
      for(j in 1:Digits)
      {
        PossibleAnswer[j] <- temp %/% 10^(Digits-j)
        temp <- temp - PossibleAnswer[j] * 10^(Digits-j)
      }
      PossibleResult <- GetAandB_AI(PossibleAnswer,LastGuess,Digits)
      HintCheck <- T
      for(j in 1:Digits)
      {
       if(!identical(Hint[j],MyNA)) HintCheck <- HintCheck&&(PossibleAnswer[j]==Hint[j])
      }
      ind[i+1] <- (PossibleResult$A==Result$A) && (PossibleResult$B==Result$B)
      ind[i+1] <- ind[i+1]&&HintCheck
    }
    
    cat(paste("There are(is) ",sum(ind)," possible answer(s).\n",sep=""))
    temp <- sample(Index[ind],1)
    if(length(Index[ind])==1) temp <- Index[ind][1]
    cat(paste("My guess is: ",temp,"\n",sep=""))
    for(i in 1:Digits)
    {
      LastGuess[i] <- temp %/% 10^(Digits-i)
      temp <- temp - LastGuess[i] * 10^(Digits-i)
    }
    Result <- InputAandB()
    Hint <- InputHint()
    iter <- iter + 1
    if(Result$A==Digits) Success <- T
  }
  if(Success==T) cat("I got it!\n")
  else
  {
    cat("Sorry I lose~\n")
    cat("Possible answers are:\n")
    for(i in Index[ind]) cat(i,"\n")
  }
}
