# ML_PROJECT
# REZA ASAD
# START: OCTOPER 31

# Get the Data
rm(list = setdiff(ls(), lsf.str()))
x <- read.table("C:\\Users\\Reza\\OneDrive\\Documents\\ML_Project\\train.csv"
	, sep = ",", header = TRUE)
y <- read.table("C:\\Users\\Reza\\OneDrive\\Documents\\ML_Project\\trainLabels.csv"
	, sep = ",", header = TRUE)

# Creating Train and Test Set
set.seed(23)
ii <- rep(1:2, each=floor(nrow(x)/100));
ii <- sample(ii);
data <- cbind(ii,x);
labels <- cbind(ii,y);
x.train <- data[ii == 1,-c(1,2)];
x.test <- data[ii == 2,-c(1,2)];
labels.tr <- labels[ii == 1,-c(1,2)];
labels.te <- labels[ii == 2,-c(1,2)];

x <- 0;
y <- 0
data <- 0;
labels <- 0;
ii <- 0;

#----------------------------Random-K-Label-(RAKEL)---------------------------------

N.tr <- nrow(x.train);
N.te <- nrow(x.test);
ncols <- ncol(labels.tr)
samp <- 22
len <- 6
index <- matrix(0,len,samp);
y.tr <- matrix(0,N.tr,samp);
y.te <- matrix(0,N.te,samp);

# Each Column of the Matrix Index Contains the
# Indices of the Labels that I Group Togeather. 
# Each Group Contains 6 Labels.
for(i in 1:samp){
	index[,i] <- sample(1:ncols, len, replace = F);
}

# I Build the Response Variable After Grouping the
# Labels. Now This is a Single-Label Problem.
for(i in 1:N.tr){
	for(j in 1:samp){
		y.tr[i,j] <- paste(labels.tr[i,][index[,j]], collapse = "");
		y.te[i,j] <- paste(labels.te[i,][index[,j]], collapse = "");
	}
}

# Running Decision Trees Under Boosting for my Signle-Label
# Problem
library(adabag)
pre.tr <- matrix(0, N.tr, samp);
pre.te <- matrix(0, N.te, samp);
x.tr <- data.frame(data.matrix(x.train));
x.te <- data.frame(data.matrix(x.test));
x.train <- 0;
x.test <- 0;
for(i in 1:samp){
	yhat <- factor(y.tr[,i]);
	train.dat <- cbind(x.tr,yhat);	
	dec.tree <- rpart.control(cp=-1,maxdepth=17,minsplit=0,xval=0);
	bo1 <- boosting(yhat~., data=train.dat, boos=T, mfinal=50,
		control=dec.tree);
	test.dat <- cbind(x.te,yhat)
	pre.tr[,i] <- bo1$class;
	pre.te[,i] <- predict(bo1, newdata = test.dat)$class
	bo1 <- 0;
	yhat <- 0;
	train.dat <- 0;
}

# For Prediction I Take The Majority Votes Using my 
# Individual Models. 
y.pre.tr <- matrix(0, N.tr, ncols);
y.pre.te <- matrix(0, N.te, ncols);
vote.matrix.tr <- matrix(0, N.tr, ncols);
vote.matrix.te <- matrix(0, N.te, ncols);
for(j in 1:samp){
	y.vect.tr <- 2*as.numeric(unlist(strsplit(pre.tr[,j],"")))-1;
	y.vect.te <- 2*as.numeric(unlist(strsplit(pre.te[,j],"")))-1;
	new.y.tr <- matrix(y.vect.tr, nrow = N.tr, ncol = len, byrow = T);
	new.y.te <- matrix(y.vect.te, nrow = N.te, ncol = len, byrow = T);
	y.pre.tr[,index[,j]] <- new.y.tr;
	y.pre.te[,index[,j]] <- new.y.te;
	vote.matrix.tr <- vote.matrix.tr + y.pre.tr;
	vote.matrix.te <- vote.matrix.te + y.pre.te;
}

predic.tr <- sign(vote.matrix.tr);
predic.te <- sign(vote.matrix.te);
mean(predic.tr!= 2*labels.tr-1)
mean(predic.te!= 2*labels.te-1)

