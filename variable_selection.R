#############################
# Ziang Jia
# STAT W4240 
# Homework 05 
# Nov 25th
#

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("D://R Workspace//hw04");

# first include the relevant libraries
library(rpart);
library(Matrix);
library(glmnet);
## define a funcion 
# This code uses tm to preprocess the papers into a format useful for NB
preprocess.dir <- function(dirName){
  
  # the directory must have all the relevant text files
  ds = DirSource(dirName)
  # Corpus will make a tm document corpus from this directory
  fp = Corpus( ds )
  # make all words lower case
  fp = tm_map( fp , content_transformer(tolower));
  # remove all punctuation
  fp = tm_map( fp , removePunctuation);
  # remove stopwords like the, a, and so on.  
  fp = tm_map( fp, removeWords, stopwords("english"));
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument)
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace)  
  # now write the corpus out to the files for our future use.
  # MAKE SURE THE _CLEAN DIRECTORY EXISTS
  writeCorpus( fp , sprintf('%s_clean',dirName) )
}

## define a function
# read the files into a file list
read.dir <- function(dirName) {
  # Initial a filenames list
  files_list = list();
  # Get a list of filenames in the directory
  filenames = dir(dirName,full.names=TRUE);
  for (i in 1:length(filenames)){
    files_list[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(files_list)
}

## define a function
# Make dictionary sorted by number of times a word appears in corpus 
# This returns a dataframe that is sorted by the number of times a word appears 
sorted.dic.df <- function(files_list){
  
  # initial a list of vectors to one big vetor
  full_dic = unlist(files_list) 
  # Tabulates the full dictionary
  tab_dic = tabulate(factor(full_dic)) 
  # Find unique values
  dictionary = unique(full_dic) 
  # Sort them alphabetically
  dictionary = sort(dictionary)
  dictionary.df = data.frame(word = dictionary, count = tab_dic)
  sort.dictionary.df = dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
  return(sort.dictionary.df)
}

# define a function
# Make a document-term matrix, which counts the number of times each dictionary element is used in a document
document.term.matrix <- function(files_list,dictionary){
  # initial document term matrix(rows are documents and columns are words)
  dtm = mat.or.vec(length(files_list),nrow(dictionary)); # A matrix filled with zeros
  # count each word in each file in files_list
  for (i in 1:nrow(dtm)){
    temp.file = files_list[[i]]
    for (j in 1:length(temp.file)){
      ind = which(dictionary == temp.file[j])[[1]]
      dtm[i,ind] = dtm[i,ind] + 1
    }
  }
  return(dtm);
}

#################
# Problem 4
#################

##########################################
## this are formula of Gini index, classification error, entropy
pm1 = seq(0,1,by=0.01);
# Gini index
gini = pm1*(1-pm1)+(1-pm1)*pm1;
# classification error
e = c();
for(i in 1:length(pm1)){
  e[i] = 1-max(c(pm1[i],1-pm1[i]));
}
# entropy
entropy = -(pm1*log(pm1)+(1-pm1)*log(1-pm1));
entropy[1] = 0;

plot(pm1, e, main="Pm1-Classification error");
plot(pm1, gini, main="Pm1-Gini Index");
plot(pm1, entropy, main="Pm1-Entropy");


#################
# Problem 6
#################

##########################################
## use function preprocess.directory to clean up corpus
# hamilton_test
preprocess.dir("fp_hamilton_test");
# hamilton_train
preprocess.dir("fp_hamilton_train");
# madison_test
preprocess.dir("fp_madison_test");
# madison_train
preprocess.dir("fp_madison_train");

## To read in data from the directories:
# hamilton.test
hamilton.test = read.dir("fp_hamilton_test_clean");
# hamilton.train
hamilton.train = read.dir("fp_hamilton_train_clean");
# madison.test
madison.test = read.dir("fp_madison_test_clean");
# madison.train
madison.train = read.dir("fp_madison_train_clean");

## concatenating the individual lists into a single large one for all files
allfiles_list = list();
allfiles_list = c(hamilton.test, hamilton.train,madison.test, madison.train);

## create a dictionary with allfiles_list
dictionary = sorted.dic.df(allfiles_list);

## create document.term.matrix for each files_list
# dtm.hamilton.test
dtm.hamilton.test = document.term.matrix(hamilton.test,dictionary);
colnames(dtm.hamilton.test) = as.vector(dictionary$word);
# dtm.hamilton.train
dtm.hamilton.train = document.term.matrix(hamilton.train,dictionary);
colnames(dtm.hamilton.train) = as.vector(dictionary$word);
# dtm.hamilton.test
dtm.madison.test = document.term.matrix(madison.test,dictionary);
colnames(dtm.madison.test) = as.vector(dictionary$word);
# dtm.hamilton.test
dtm.madison.train = document.term.matrix(madison.train,dictionary);
colnames(dtm.madison.train) = as.vector(dictionary$word);

## create labels with Hamilton = 1, Madison = 0
matrix.hamilton.test = data.frame(y = rep(1,nrow(dtm.hamilton.test)),dtm.hamilton.test);
matrix.hamilton.train = data.frame(y = rep(1,nrow(dtm.hamilton.train)),dtm.hamilton.train);
matrix.madison.test = data.frame(y = rep(0,nrow(dtm.madison.test)),dtm.madison.test);
matrix.madison.train = data.frame(y = rep(0,nrow(dtm.madison.train)),dtm.madison.train);

## generate training sets and testing sets
matrix.train = rbind(matrix.hamilton.train, matrix.madison.train);
matrix.test = rbind(matrix.hamilton.test, matrix.madison.test);

## build tree on training sets.
##########################################
# Gini split
fit.tree = rpart(y~.,data=matrix.train,parms=list(split="gini"),method = "class");
summary(fit.tree)
print(fit.tree)
plot(fit.tree, uniform=TRUE, margin=0.3,
     main="Classification Tree by Gini split")
text(fit.tree, use.n=TRUE, all=TRUE, cex=1)
fit.gini.predict = predict(fit.tree, matrix.test,type = "class");
table(fit.gini.predict,matrix.test$y)
##########################################
# information entropy split
fit.info.tree = rpart(y~.,data=matrix.train,parms=list(split="information"),method = "class");
summary(fit.info.tree)
print(fit.info.tree)
plot(fit.info.tree, uniform=TRUE, margin=0.3,
     main="Classification Tree for Info split")
text(fit.info.tree, use.n=TRUE, all=TRUE, cex=1)
fit.info.predict = predict(fit.info.tree, matrix.test,type = "class");
table(fit.info.predict,matrix.test$y)

#################
# Problem 6
#################

##########################################
## scale the data
# combine the train and test
matrix.data = rbind(matrix.train, matrix.test);
matrix.data.scale = scale(matrix.data[,2:4876],center = T, scale = T);
# gain the scaled data matrix
matrix.data.scale = as.matrix(data.frame(y=matrix.data[,1],matrix.data.scale));
matrix.train.scale = matrix.data.scale[1:50,];
matrix.test.scale = matrix.data.scale[51:77,];
rownames(matrix.test.scale)=NULL;

##########################################
## do ridge regression
set.seed(1);
grid = exp(seq(-30,-1,length = 500));
fit.ridge = glmnet(matrix.train.scale[,2:4876],matrix.train.scale[,1],alpha = 0,family="binomial",lambda = grid);
plot(fit.ridge, xvar = "dev", label = T);

# find best lambda
cv.ridge = cv.glmnet(matrix.train.scale[,2:4876],matrix.train.scale[,1],alpha = 0,family="binomial",type.measure = "class",lambda = grid);
plot(cv.ridge);
cv.ridge$lambda.min
log(cv.ridge$lambda.min)
predict.ridge = predict(fit.ridge, matrix.test.scale[,2:4876], s = cv.ridge$lambda.min, type = "class");
predict.ridge
table(predict.ridge, matrix.test$y);

# find the most 10 important variables
coef_10 = as.matrix(coef(fit.ridge, s = cv.ridge$lambda.min));
coef.sort = sort(abs(coef_10),decreasing = T)
coef.ridge = coef.sort[2:11]
for(i in 2:11){
  print(dictionary[which(abs(coef_10)==coef.sort[i])-1,]$word);
}


#############################################
## do lasso regression
grid = exp(seq(-10,1,length = 500));
fit.lasso = glmnet(matrix.train.scale[,2:4876],matrix.train.scale[,1],alpha = 1,family="binomial",lambda = grid);
plot(fit.lasso,label = T);

# find best lambda
cv.lasso = cv.glmnet(matrix.train.scale[,2:4876],matrix.train.scale[,1],alpha = 1,family="binomial",type.measure= "class", lambda = grid);
plot(cv.lasso);
cv.lasso$lambda.min
log(cv.lasso$lambda.min)
predict.lasso = predict(fit.lasso, matrix.test.scale[,2:4876], s = cv.lasso$lambda.min, type = "class");
predict.lasso
table(predict.lasso, matrix.test$y);

# find the most 10 important variables
coef_10 = as.matrix(coef(fit.lasso, s = cv.lasso$lambda.min));
coef.sort = sort(abs(coef_10),decreasing = T )
coef.lasso = coef.sort[2:11]
for(i in 2:11){
  print(dictionary[which(abs(coef_10)==coef.sort[i])-1,]$word);
}

###############################################
## addition: unregularized logistic regression(given lambda = 0)
# regression
data.train = data.frame(matrix.train.scale);
cv.lm = glm(data.train$y~.,data = data.train, family="binomial");
plot(cv.lm);
cv.lm$lambda.min
log(cv.lm$lambda.min)
# prediction 
predict.lm = predict(cv.lm, data.frame(matrix.test.scale)[,2:4876],type = "response");
predict.lm
table(predict.lm, matrix.test$y);
