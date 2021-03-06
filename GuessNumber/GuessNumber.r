############ Guess Number ############
###### This is a console version of the game guessing a number ######

############ pre-define function ############
clc<-function()  cat("\f");

PrintWelcome<-function()
{
  cat("Hello! Welcome to Guess Number! \n
      This is a trial programe, have fun!\n")
}

PrintHelp<-function()
{
  cat("Here are some helping information about A and B:\n")
  cat("A means the number of digits in User's guess which match in the answer both value and position. \n")
  cat("B means the number of digits in User's guess which match in the answer only value but not position. \n")
  cat("The digits counted in A will not be counted in B.\n")
  cat("Here are some examples:\n")
  cat("Answer: 110, Guess: 211. Then A=1, B=1. \n")
  cat("Answer: 220, Guess: 112. Then A=0, B=1. \n")
  cat("Answer: 221, Guess: 112. Then A=0, B=3. \n")
  cat("Answer: 990, Guess: 912. Then A=1, B=0. \n")
  cat("\n\n\n")
  cat("Game starts in 3s... ")
  Sys.sleep(1)
  cat("2s... ")
  Sys.sleep(1)
  cat("1s... \n")
  Sys.sleep(1)
}

SetDigits<-function()
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
 

SetGuess<-function()
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


SetRound<-function()
{
  cat("Please set the number of games you want to play(default 3, min 1, max 100).\n")
  RoundTry<-try(Round<-scan("",what=integer(),n=1,quiet=T),silent=T)
  if(class(RoundTry)=="try-error")  Round<-3
  if(length(Round) == 0) Round <- 3
  if(Round < 1) Round<- 1
  if(Round > 10)  Round<- 100
  
  cat(paste("Round = ", Round, "\n", sep=""))
  return(Round)
}

GenAnswer<-function(Digits)
{
  res<-sapply(1:Digits,FUN=function(i) sample(0:9,1))
  return(res)
}

GetUserGuess<-function(Digits)
{
  cat("Please input your guess: \n")
  cat(paste("Caution: default value is 0, and only last ", Digits, " number(s) will be recorded.\n", sep=""))
  cat("Caution: Negative numbers will be treat as 0.\n")
  InputTry<-try(Input<-scan("",what=integer(),n=1,quiet=T),silent=T)
  if( class(InputTry)=="try-error" )
  {
    Input <- 0
    cat("Input error! Set it to 0!\n")
  }
  if(length(Input) == 0) Input <- 0
  if(Input < 0) Input<- 0
  Input <- Input %% (10^Digits)
  res<-rep(0,Digits)
  for(i in 1:Digits)
  {
    res[i] <- Input %/% 10^(Digits-i)
    Input <- Input - res[i]*10^(Digits-i)
  }
  return(res)
}

GetAandB<-function(Answer,UserGuess,Digits)
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
  Show<-rep("*",Digits)
  Show[!indicator]<-Answer[!indicator]
  cat(Show)
  cat("\n")
  res<-data.frame(A=A,B=B)
  return(res)
}

############ main body ############
StartGame<-function()
{
  ### This is the main body of the game ###
  clc()
  ### print welcome information ###
  PrintWelcome()
  
  ### Set Game parameter the first time ###
  Digits<-SetDigits()
  GuessNum<-SetGuess()
  RoundNum<-SetRound()
  
  ### print some help information ###
  PrintHelp()
  
  ### start the main game ###
  Score <- 0
  Round <-1
  
  while(Round <= RoundNum)
  {
    ### print current game state ###
    AllState<-paste("Round: ", Round,", Score: ", Score, sep="")
    AllState<-paste(AllState,"\nGood Luck, you'll need it!\n", sep="")
    cat(AllState)
    
    ### Generate the answer and begin the guessing ###
    Answer <- GenAnswer(Digits)
    AnswerNumber <- 0
    for(i in 1:Digits) AnswerNumber <- AnswerNumber + Answer[i]*10^(Digits-i)
    Guess <- 1
    Win<-0
    while( (Guess <= GuessNum) && (Win == 0))
    {
      GameState<-paste("Guess No.", Guess, "/", GuessNum, ": ", sep="")
      cat(GameState)
      UserGuess<-GetUserGuess(Digits)
      Result<-GetAandB(Answer,UserGuess,Digits)
      cat(paste("A = ",Result$A, "  B = ", Result$B, "\n", sep=""))
      if(Result$A == Digits) Win <- 1
      else Guess <- Guess + 1
    }
    if( Win == 1)
    {
      Score <- Score + 1
      Round <- Round + 1
      cat(paste("Answer is ", AnswerNumber, ".\n",sep=""))
      cat("Congratulations! You win!\n")
    }
    else
    {
      Round <- Round + 1
      cat(paste("Answer is ", AnswerNumber, ".\n",sep=""))
      cat("Sorry, you lose. Better luck next time!\n")
    }
    if(Round <= RoundNum)
    {
      cat("New game starts in 3s... ")
      Sys.sleep(1)
      cat("2s... ")
      Sys.sleep(1)
      cat("1s... \n")
      Sys.sleep(1)
      clc()
    }
    else
    {
      cat("Game ends in 3s... ")
      Sys.sleep(1)
      cat("2s... ")
      Sys.sleep(1)
      cat("1s... \n")
      Sys.sleep(1)
      clc()
    }
  }
  clc()
  cat(paste(RoundNum," round(s) has(have) finished. You score ",Score, " point(s)!\n", sep=""))
  return(Score)
}
