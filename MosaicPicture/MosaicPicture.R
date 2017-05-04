############ My version of drawing masaic figure ############
###### load packages ######
library(jpeg)

###### function definitions ######
ConvertToGray <- function(RGBdata){
  # Convert a rgb picture to a gray picture.
  #
  # Args:
  #   RGBdata: numeric matrix/array of a picture. 
  #            length(dim(RGBdata))= 2 means it's a gray picture itself and  
  #            will be returned without any configuration.
  #            length(dim(RGBdata) = 3 means it's a rgb picture and the 3rd
  #            dimension stores R(red) G(green) B(blue) data of the picture.
  #
  # Returens: 
  #   res: a matrix of a gray picture. Result of each pixel is mean of R, G
  #        and B.
  if (length(dim(RGBdata))==2){
    res <- RGBdata
  }else{
    DIM <- dim(RGBdata)
    res <- array(apply(array(RGBdata, dim=c(DIM[1]*DIM[2],DIM[3])), 1, mean), 
                 dim=c(DIM[1], DIM[2]))
  }
  return(res)
}

###### main body ######
# Some important variables #
# working.directory: The working directory. In this folder there are 
#                    A figure called "fun.jpg" is the target figure.
#                    A sub-folder, ./pool/, saving all your small mosaic 
#                    figures. I recommend the size of each figure should be
#                    no more than 100*100 otherwise the programe would be very
#                    slow. The number of figures should be large, the more the
#                    better resulting figure.
# alpha: numeric value. The size of every small mosaic figure is alpha * alpha.
# gray.res: logical value. TRUE means using gray mosaic figures and FALSE means
#           using original mosaic figures.
# Output of the programe #
# mosaic.jpg: The resulting pixelated figure should be saved at your 
#             working.directory
### read the original figure ###
working.directory <- 'E:/[Document]/[Git]/Personal_Interest/RGames/MosaicPicture/'
setwd(working.directory)
me.origin <- readJPEG('fun.jpg')
me <- ConvertToGray(me.origin)


### read and prepare information of image pool ###
setwd(paste(working.directory, "pool/", sep = ""))
pool.dir <- dir()
tmp <- NULL
num.of.pool <- length(pool.dir)
n <- 0
for(i in pool.dir){
  tmp[[i]] <- mean(readJPEG(i))
  n <- n + 1
  print(paste(n, "/", num.of.pool, ", ", i, sep=""))
}
id <- sort(tmp, decreasing=F)
id <- data.frame(n = names(id), m = id)

### draw the final figure ###
setwd(working.directory)
Height <- dim(me)[1]
Width <- dim(me)[2]
me.round <- round(me * (num.of.pool - 1)) + 1
me.factor <- factor(me.round)
me.levels <- levels(me.factor)
alpha <- 30
gray.res <- T

jpeg("mosaic.jpg", width=Width * alpha, height = Height * alpha)
par(mfcol = c(Height, Width), mar = rep(0, 4), xpd = NA, xaxs = "i", yaxs = "i")
plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", frame.plot = F)
setwd(paste(working.directory, "pool/", sep = ""))
for(i in 1 : length(me.levels)){
  tmp.name <- as.character(id[as.numeric(me.levels[i]), 1])
  tmp <- readJPEG(tmp.name)
  if (gray.res == T){
    tmp <- ConvertToGray(tmp)
  }
  index <- which(me.round == me.levels[i])
  n <- 0
  for(idx in index){
    nc <- (idx - 1) %/% Height + 1
    nr <- (idx - 1) %% Height + 1
    par(mfg = c(nr, nc))
    plot(c(0, alpha), c(0, alpha), xlim = c(0, alpha), ylim=c(0, alpha), 
         type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
         frame.plot = F)
    rasterImage(tmp, 0, 0, alpha, alpha) 
    n <- n + 1
    print(paste("i=", i, "/", length(me.levels), " , n=", n, "/", 
                length(index), sep=""))
  }
}
setwd(working.directory)
dev.off()

