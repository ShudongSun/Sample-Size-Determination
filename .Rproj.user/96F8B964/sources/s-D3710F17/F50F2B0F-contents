install.packages("devtools")
library(devtools)
install_github("ShudongSun/Sample-Size-Determination")

n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150))
# model=c("svm","randomforest")
model=c("svm","randomforest","logistic","lda","tree")
library(SSD)
load("D:/Dropbox/USC/RA/2020summer/R Package/Sample-Size-Determination/auc_res.Rdata")
calculate_std_of_AUC_and_draw_plot(res,n_train_sets=n_train_sets, model=model)


# res = calculate_AUCs(num_of_seeds=30, random_seeds=TRUE, seeds=NULL, calculate_std_of_AUC_and_produce_plot=TRUE, method="gaussian_copula", ncores = NULL, model=c("svm","randomforest","logistic","lda","tree"))
res = calculate_AUCs(n01_all= c(800,800), n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300),num_of_seeds=20, random_seeds=TRUE, seeds=NULL, calculate_std_of_AUC_and_produce_plot=TRUE, method="pca2_mvnorm", ncores = NULL, model=c("svm","randomforest","logistic","lda","tree"), data_generation = data_generation)


df = 10
rho=0.5
d=5
delta = rep(2,d)
H<-abs(outer(1:d,1:d,"-"))
covxx=rho^H
data_generation=list(dist="t-distribution",sigma=list(class_0=covxx,class_1=covxx),df=c(10,10),delta=c(rep(0,5),rep(2,5)))
