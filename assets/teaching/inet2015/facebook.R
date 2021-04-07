## First log in to Facebook. Then go to
## https://developers.facebook.com/tools/explorer
## Here you will need to generate your own Access Token. Mine is below
## (it won't work, it expires after 1 hour)
## Replace the one below with your own
## I had to mess around with some of the paths below to get it to work
## For instance 6833552 is my Facebook node number. You can find your
## own when you use the link above. If something doesn't work, compare
## the path to the one in your browser.


access_token = 'AAACEdEose0cBANr6DtJeqJ2AUmLlchQ1ES61Q8eGx8hjRvnM1wjNqSth3HvQKAhZA4qKZCIZAwctg74Rtqc40BZCTERtEpiWv16ZBVNAk3QZDZD'

require(RCurl)
require(rjson)
require(Rgraphviz)
require(pixmap)

facebook <-  function( path = "6833552", access_token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=",
                                     unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  data <- getURL( sprintf(
  "https://graph.facebook.com/%s%s&access_token=%s", path,
                          options, access_token, ssl.verifypeer=FALSE ) )
  fromJSON( data )
}

friends <- facebook( path="6833552?fields=friends" ,
                    access_token=access_token) 
## extract Facebook IDs
friends.id <- sapply(friends$friends$data, function(x) x$id)
## extract names 
friends.name <- sapply(friends$friends$data, function(x)
                    iconv(x$name,"UTF-8","ASCII//TRANSLIT")) 
## short names to initials 
initials <- function(x) paste(substr(x,1,1), collapse="")
friends.initial <- sapply(strsplit(friends.name," "), initials) 

## friendship relation matrix
N <- length(friends.id)
friendship.matrix <- matrix(0,N,N)
for (i in 1:N) {
  tmp <- facebook( path=paste("6833552/mutualfriends", friends.id[i], sep="?user=") , 
                   access_token=access_token)
  mutualfriends <- sapply(tmp$data, function(x) x$id)
  friendship.matrix[i,friends.id %in% mutualfriends] <- 1
}

g <- new("graphAM", adjMat=friendship.matrix)

pdf(file="facebook1.pdf", width=125, height=125)
attrs <- list(node=list(shape="ellipse", fixedsize=FALSE))
nAttrs <- list(label=friends.initial)
names(nAttrs$label) <- nodes(g)
plot(g, "neato", attrs=attrs, nodeAttrs=nAttrs)
dev.off()

## download small profile picture of each friend
dir.create("photos")
for (i in 1:length(friends.id))
  download.file(paste("http://graph.facebook.com", friends.id[i], "picture", sep="/"), 
                destfile=paste("photos/",friends.id[i],".jpg",sep=""))
system('for i in `ls photos/*.jpg`; do j=${i%.*}; convert $j.jpg $j.pnm; done',
       wait=TRUE) 

## customized node plotting function
makeNodeDrawFunction <- function(x) {
  force(x)
  function(node, ur, attrs, radConv) {
    photo <- read.pnm(paste("photos/", x, ".pnm", sep=""))
    nc <- getNodeCenter(node)
    addlogo(photo, c(getX(nc)-25, getX(nc)+25), c(getY(nc)-25, getY(nc)+25))
  }
}
drawFuns <- apply(as.array(friends.id), 1, makeNodeDrawFunction)

## a graph with photos
pdf(file="facebook2.pdf", width=25, height=25)
attrs <- list(node=list(shape="box", width=0.75, height=0.75))
plot(g, "fdp", attrs=attrs, drawNode=drawFuns)
dev.off()

## For the talk, I used only a subset of my facebook network
n = 50
samp = sample(1:N, n, prob=rowSums(friendship.matrix))
friend.mat = friendship.matrix[samp,samp]

drawFuns <- apply(as.array(friends.id[samp]), 1, makeNodeDrawFunction)
g <- new("graphAM", adjMat=friend.mat)

pdf(file="facebook3.pdf", width=50, height=50)
attrs <- list(node=list(shape="box", width=.5, height=.5))
plot(g, "fdp", attrs=attrs, drawNode=drawFuns)
dev.off()

