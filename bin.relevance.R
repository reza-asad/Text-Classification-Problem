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

#------------------------------Binary-Relevance---------------------------------

numb.labels <- ncol(labels.tr)
N.tr <- nrow(x.train);
N.te <- nrow(x.test);
pre.tr <- matrix(0, N.tr, numb.labels);
pre.te <- matrix(0, N.te, numb.labels);
x.tr <- data.frame(data.matrix(x.train));
x.te <- data.frame(data.matrix(x.test));

# Empty Some Memory
x.train <- 0;
x.test <- 0;

# Running Decisoin Trees Under Boosting for Each Label
library(adabag)
for(i in 1:numb.labels){
	yhat <- factor(labels.tr[,i]);
	train.dat <- cbind(x.tr,yhat);	
	dec.tree <- rpart.control(cp=-1,maxdepth=17,minsplit=0,xval=0);
	bo1 <- boosting(yhat~., data=train.dat, boos=F, mfinal=50,
		control=dec.tree);
	test.dat <- cbind(x.te,yhat)
	pre.tr[,i] <- bo1$class;
	pr.te[,i] <- predict(bo1, newdata = test.dat)$class
	bo1 <- 0;
	yhat <- 0;
	train.dat <- 0;
}

mean(pre.tr!= labels.tr)
mean(pre.te!= labels.te)

