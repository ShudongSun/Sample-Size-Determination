#' @title Calculate std and draw the plots
#'
#' @param res the result produced by cal_parallel
#' @param n_train_sets size sets of training data labeled as class 0/1. For example, n_train_sets=c(c(30,30),c(90,90),c(150,150)) represents that we try 3 different sets of training data and the training size of the first set is c(30,30).
#' @param model base classification model set which is corresponding to the result in the "res".
#' \itemize{
#' \item logistic: Logistic regression. \link{glm} function with family = 'binomial'
#' \item penlog: Penalized logistic regression with LASSO penalty. \code{\link[glmnet]{glmnet}} in \code{glmnet} package
#' \item svm: Support Vector Machines. \code{\link[e1071]{svm}} in \code{e1071} package
#' \item randomforest: Random Forest. \code{\link[randomForest]{randomForest}} in \code{randomForest} package
#' \item lda: Linear Discriminant Analysis. \code{\link[MASS]{lda}} in \code{MASS} package
#' \item slda: Sparse Linear Discriminant Analysis with LASSO penalty.
#' \item nb: Naive Bayes. \code{\link[e1071]{naiveBayes}} in \code{e1071} package
#' \item nnb: Nonparametric Naive Bayes. \code{\link[naivebayes]{naive_bayes}} in \code{naivebayes} package
#' \item ada: Ada-Boost. \code{\link[ada]{ada}} in \code{ada} package
#' \item xgboost: XGBboost. \code{\link[xgboost]{xgboost}} in \code{xgboost} package
#' \item tree: Classificatin Tree. \code{\link[tree]{tree}} in \code{tree} package
#' }
#'
#' @return It will save all the result to the "result" directory.
#' @export
#'
#' @examples calculate_std_of_AUC_and_draw_plot(res)
calculate_std_of_AUC_and_draw_plot <- function(res, n_train_sets, model)
{
  num_of_seeds = length(res)

  num_of_model = length(model)

  num_of_train_set = length(res[[1]][,1,1])/2

  auc_tfs <- array(0,dim=c(num_of_seeds,length(res[[1]][1,,1]),num_of_train_set,num_of_model))
  # auc_tfs_RF <- array(0,dim=c(num_of_seeds,length(res[[1]]$RF_AUC[1,]),num_of_train_set))


  auc_trainft <- array(0,dim=c(num_of_seeds,length(res[[1]][1,,1]),num_of_train_set,num_of_model))
  # auc_trainft_RF <- array(0,dim=c(num_of_seeds,length(res[[1]]$RF_AUC[1,]),num_of_train_set))


  for (i in 1: num_of_seeds)
  {
    for (j in 1:num_of_train_set){
      for (k in 1:num_of_model){
        auc_tfs[i,,j,k] = (res[[i]][j,,k])
        auc_trainft[i,,j,k] = (res[[i]][j+num_of_train_set,,k])
      }
      # auc_tfs_LR[i,,j] = res[[i]]$LR_AUC[j,]
      # auc_tfs_RF[i,,j] = res[[i]]$RF_AUC[j,]
      # auc_tfs_svm[i,,j] = res[[i]]$SVM_AUC[j,]
      # auc_tfs_xgb[i,,j] = res[[i]]$XGB_AUC[j,]
      #
      # auc_trainft_LR[i,,j] = res[[i]]$LR_AUC[j+num_of_train_set,]
      # auc_trainft_RF[i,,j] = res[[i]]$RF_AUC[j+num_of_train_set,]
      # auc_trainft_svm[i,,j] = res[[i]]$SVM_AUC[j+num_of_train_set,]
      # auc_trainft_xgb[i,,j] = res[[i]]$XGB_AUC[j+num_of_train_set,]
    }
  }

  dim(n_train_sets) = c(2,length(n_train_sets)/2)
  n_train_total = n_train_sets[1,] + n_train_sets[2,]

  group = n_train_total
  if(num_of_seeds<8){
    num_of_seeds_show = num_of_seeds
  }else{
    num_of_seeds_show = 8
  }
  seed=rep(c(1:num_of_seeds_show),num_of_train_set)
  # seed=rep(c(9,10,11,12,13,14,15,16),5)
  seed2=rep(c(1:num_of_seeds),num_of_train_set)

  dir.create("./result")
  setwd("./result")

  for (i in 1:num_of_model){

    AUC_tfs = numeric()
    # AUC_tfs_RF = numeric()
    # AUC_tfs_SVM = numeric()
    # AUC_tfs_XGB = numeric()

    AUC_trainft = numeric()
    # AUC_trainft_RF = numeric()
    # AUC_trainft_SVM = numeric()
    # AUC_trainft_XGB = numeric()

    for (j in 1:num_of_train_set){
      AUC_tfs = c(AUC_tfs, c(t(auc_tfs[,,j,i][1:num_of_seeds_show,])))
      # AUC_tfs_RF = c(AUC_tfs_RF, c(t(auc_tfs_RF[,,j][1:num_of_seeds_show,])))
      # AUC_tfs_SVM = c(AUC_tfs_SVM, c(t(auc_tfs_svm[,,j][1:num_of_seeds_show,])))
      # AUC_tfs_XGB = c(AUC_tfs_XGB, c(t(auc_tfs_xgb[,,j][1:num_of_seeds_show,])))

      AUC_trainft = c(AUC_trainft, c(t(auc_trainft[,,j,i][1:num_of_seeds_show,])))
      # AUC_trainft_RF = c(AUC_trainft_RF, c(t(auc_trainft_RF[,,j][1:num_of_seeds_show,])))
      # AUC_trainft_SVM = c(AUC_trainft_SVM, c(t(auc_trainft_svm[,,j][1:num_of_seeds_show,])))
      # AUC_trainft_XGB = c(AUC_trainft_XGB, c(t(auc_trainft_xgb[,,j][1:num_of_seeds_show,])))

    }

    auc_tfs_df<-data.frame(
      AUC_tfs=AUC_tfs,
      # AUC_tfs_RF=AUC_tfs_RF,
      # AUC_tfs_svm=AUC_tfs_SVM,
      # AUC_tfs_xgb=AUC_tfs_XGB,
      Seed=as.factor(rep(seed,each=100)),
      Class=as.factor(rep(group,each=num_of_seeds_show*100))
    )

    auc_trainft_df<-data.frame(
      AUC_trainft=AUC_trainft,
      # AUC_trainft_RF=AUC_trainft_RF,
      # AUC_trainft_svm=AUC_trainft_SVM,
      # AUC_trainft_xgb=AUC_trainft_XGB,
      Seed=as.factor(rep(seed,each=100)),
      Class=as.factor(rep(group,each=num_of_seeds_show*100))
    )

    ### part 1: draw plot

    library(ggplot2)

    pd <- position_dodge(0.5) # move them .5 to the left and right
    cbPalette<-c("#D55E00","#009E73","#56B4E9","#CC79A7")


    auc_tfs.sum<-summarySE(auc_tfs_df,measurevar = "AUC_tfs",groupvars = c("Seed","Class"))
    fig_tfs_auc=ggplot(auc_tfs.sum, aes(x=Seed, y=AUC_tfs, color=Class,group=Class)) +
      geom_errorbar(aes(ymin=AUC_tfs-sd, ymax=AUC_tfs+sd),width=0.2,position = pd)+
      geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
      geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
      theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

    # fig_tfs_auc

    auc_trainft.sum<-summarySE(auc_trainft_df,measurevar = "AUC_trainft",groupvars = c("Seed","Class"))
    fig_trainft_auc=ggplot(auc_trainft.sum, aes(x=Seed, y=AUC_trainft, color=Class,group=Class)) +
      geom_errorbar(aes(ymin=AUC_trainft-sd, ymax=AUC_trainft+sd),width=0.2,position = pd)+
      geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
      geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
      theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

    # fig_trainft_auc

    # auc_tfs_RF.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_RF",groupvars = c("Seed","Class"))
    # fig_tfs_auc_RF=ggplot(auc_tfs_RF.sum, aes(x=Seed, y=AUC_tfs_RF, color=Class,group=Class)) +
    #   geom_errorbar(aes(ymin=AUC_tfs_RF-sd, ymax=AUC_tfs_RF+sd),width=0.2,position = pd)+
    #   geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    #   geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)
    #
    # # fig_tfs_auc_RF
    #
    # auc_trainft_RF.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_RF",groupvars = c("Seed","Class"))
    # fig_trainft_auc_RF=ggplot(auc_trainft_RF.sum, aes(x=Seed, y=AUC_trainft_RF, color=Class,group=Class)) +
    #   geom_errorbar(aes(ymin=AUC_trainft_RF-sd, ymax=AUC_trainft_RF+sd),width=0.2,position = pd)+
    #   geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    #   geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)
    #
    # # fig_trainft_auc_RF
    #
    # auc_tfs_svm.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_svm",groupvars = c("Seed","Class"))
    # fig_tfs_auc_svm=ggplot(auc_tfs_svm.sum, aes(x=Seed, y=AUC_tfs_svm, color=Class,group=Class)) +
    #   geom_errorbar(aes(ymin=AUC_tfs_svm-sd, ymax=AUC_tfs_svm+sd),width=0.2,position = pd)+
    #   geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    #   geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)
    #
    # # fig_tfs_auc_svm
    #
    # auc_trainft_svm.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_svm",groupvars = c("Seed","Class"))
    # fig_trainft_auc_svm=ggplot(auc_trainft_svm.sum, aes(x=Seed, y=AUC_trainft_svm, color=Class,group=Class)) +
    #   geom_errorbar(aes(ymin=AUC_trainft_svm-sd, ymax=AUC_trainft_svm+sd),width=0.2,position = pd)+
    #   geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    #   geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)
    #
    # # fig_trainft_auc_svm
    #
    # auc_tfs_xgb.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_xgb",groupvars = c("Seed","Class"))
    # fig_tfs_auc_xgb=ggplot(auc_tfs_xgb.sum, aes(x=Seed, y=AUC_tfs_xgb, color=Class,group=Class)) +
    #   geom_errorbar(aes(ymin=AUC_tfs_xgb-sd, ymax=AUC_tfs_xgb+sd),width=0.2,position = pd)+
    #   geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    #   geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)
    #
    # # fig_tfs_auc_xgb
    #
    # auc_trainft_xgb.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_xgb",groupvars = c("Seed","Class"))
    # fig_trainft_auc_xgb=ggplot(auc_trainft_xgb.sum, aes(x=Seed, y=AUC_trainft_xgb, color=Class,group=Class)) +
    #   geom_errorbar(aes(ymin=AUC_trainft_xgb-sd, ymax=AUC_trainft_xgb+sd),width=0.2,position = pd)+
    #   geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    #   geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

    # fig_trainft_auc_xgb


    library(ggpubr)
    fig1=ggarrange(fig_tfs_auc,fig_trainft_auc,
                     nrow=1,ncol=2,common.legend = TRUE, legend="bottom")

    pdf(file=paste(model[i],"plot1.pdf",sep="_"),width=8,height=6)
    plot(fig1)
    dev.off()


    # fig_RF=ggarrange(fig_tfs_auc_RF,fig_trainft_auc_RF,
    #                  nrow=1,ncol=2,common.legend = TRUE, legend="bottom")
    # # fig_RF
    # pin = c(10,3)
    # pdf(file="RF_plot.pdf",width=8,height=6)
    # plot(fig_RF)
    # dev.off()
    #
    # fig_svm=ggarrange(fig_tfs_auc_svm,fig_trainft_auc_svm,
    #                   nrow=1,ncol=2,common.legend = TRUE, legend="bottom")
    # # fig_svm
    # pdf(file="svm_plot.pdf",width=8,height=6)
    # plot(fig_svm)
    # dev.off()
    #
    # fig_xgb=ggarrange(fig_tfs_auc_xgb,fig_trainft_auc_xgb,
    #                   nrow=1,ncol=2,common.legend = TRUE, legend="bottom")
    # # fig_xgb
    # pdf(file="xgb_plot.pdf",width=8,height=6)
    # plot(fig_xgb)
    # dev.off()



  ### part 1: calculate the median
  options(scipen = 200)

  AUC_tfs = numeric()
  # AUC_tfs_RF = numeric()
  # AUC_tfs_SVM = numeric()
  # AUC_tfs_XGB = numeric()

  AUC_trainft = numeric()
  # AUC_trainft_RF = numeric()
  # AUC_trainft_SVM = numeric()
  # AUC_trainft_XGB = numeric()

  for (j in 1:num_of_train_set){
    AUC_tfs = c(AUC_tfs, c(t(auc_tfs[,,j,i][1:num_of_seeds,])))
    # AUC_tfs_LR = c(AUC_tfs_LR, c(t(auc_tfs_LR[,,j][1:num_of_seeds,])))
    # AUC_tfs_RF = c(AUC_tfs_RF, c(t(auc_tfs_RF[,,j][1:num_of_seeds,])))
    # AUC_tfs_SVM = c(AUC_tfs_SVM, c(t(auc_tfs_svm[,,j][1:num_of_seeds,])))
    # AUC_tfs_XGB = c(AUC_tfs_XGB, c(t(auc_tfs_xgb[,,j][1:num_of_seeds,])))

    AUC_trainft = c(AUC_trainft, c(t(auc_trainft[,,j,i][1:num_of_seeds,])))
    # AUC_trainft_LR = c(AUC_trainft_LR, c(t(auc_trainft_LR[,,j][1:num_of_seeds,])))
    # AUC_trainft_RF = c(AUC_trainft_RF, c(t(auc_trainft_RF[,,j][1:num_of_seeds,])))
    # AUC_trainft_SVM = c(AUC_trainft_SVM, c(t(auc_trainft_svm[,,j][1:num_of_seeds,])))
    # AUC_trainft_XGB = c(AUC_trainft_XGB, c(t(auc_trainft_xgb[,,j][1:num_of_seeds,])))

  }

  auc_tfs_df<-data.frame(
    AUC_tfs=AUC_tfs,
    # AUC_tfs_RF=AUC_tfs_RF,
    # AUC_tfs_svm=AUC_tfs_SVM,
    # AUC_tfs_xgb=AUC_tfs_XGB,
    Seed=as.factor(rep(seed2,each=100)),
    Class=as.factor(rep(group,each=num_of_seeds*100))
  )

  auc_trainft_df<-data.frame(
    AUC_trainft=AUC_trainft,
    # AUC_trainft_RF=AUC_trainft_RF,
    # AUC_trainft_svm=AUC_trainft_SVM,
    # AUC_trainft_xgb=AUC_trainft_XGB,
    Seed=as.factor(rep(seed2,each=100)),
    Class=as.factor(rep(group,each=num_of_seeds*100))
  )

  auc_tfs.sum<-summarySE(auc_tfs_df,measurevar = "AUC_tfs",groupvars = c("Seed","Class"))
  auc_trainft.sum<-summarySE(auc_trainft_df,measurevar = "AUC_trainft",groupvars = c("Seed","Class"))

  # auc_tfs_RF.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_RF",groupvars = c("Seed","Class"))
  # auc_trainft_RF.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_RF",groupvars = c("Seed","Class"))
  #
  # auc_tfs_svm.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_svm",groupvars = c("Seed","Class"))
  # auc_trainft_svm.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_svm",groupvars = c("Seed","Class"))
  #
  # auc_tfs_xgb.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_xgb",groupvars = c("Seed","Class"))
  # auc_trainft_xgb.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_xgb",groupvars = c("Seed","Class"))

  auc_median <- data.frame(Seed=group, sdDvByMean_median_tfe=rep(0,length(group)), ratio_tfe=rep(0,length(group)), sdDvByMean_median_trainft=rep(0,length(group)), ratio_trainft=rep(0,length(group)))

  m=1
  for(n0_train in group){
    auc_median[m,2] = median(auc_tfs.sum[auc_tfs.sum[,2]==n0_train,][,5]/auc_tfs.sum[auc_tfs.sum[,2]==n0_train,][,4])
    auc_median[m,4] = median(auc_trainft.sum[auc_trainft.sum[,2]==n0_train,][,5]/auc_trainft.sum[auc_trainft.sum[,2]==n0_train,][,4])

    # auc_median_RF[i,2] = median(auc_tfs_RF.sum[auc_tfs_RF.sum[,2]==n0_train,][,5]/auc_tfs_RF.sum[auc_tfs_RF.sum[,2]==n0_train,][,4])
    # auc_median_RF[i,4] = median(auc_trainft_RF.sum[auc_trainft_RF.sum[,2]==n0_train,][,5]/auc_trainft_RF.sum[auc_trainft_RF.sum[,2]==n0_train,][,4])
    #
    # auc_median_svm[i,2] = median(auc_tfs_svm.sum[auc_tfs_svm.sum[,2]==n0_train,][,5]/auc_tfs_svm.sum[auc_tfs_svm.sum[,2]==n0_train,][,4])
    # auc_median_svm[i,4] = median(auc_trainft_svm.sum[auc_trainft_svm.sum[,2]==n0_train,][,5]/auc_trainft_svm.sum[auc_trainft_svm.sum[,2]==n0_train,][,4])
    #
    # auc_median_xgb[i,2] = median(auc_tfs_xgb.sum[auc_tfs_xgb.sum[,2]==n0_train,][,5]/auc_tfs_xgb.sum[auc_tfs_xgb.sum[,2]==n0_train,][,4])
    # auc_median_xgb[i,4] = median(auc_trainft_xgb.sum[auc_trainft_xgb.sum[,2]==n0_train,][,5]/auc_trainft_xgb.sum[auc_trainft_xgb.sum[,2]==n0_train,][,4])

    auc_median[m,3] = auc_median[m,2]/auc_median[1,2]
    auc_median[m,5] = auc_median[m,4]/auc_median[1,4]

    # auc_median_RF[i,3] = auc_median_RF[i,2]/auc_median_RF[1,2]
    # auc_median_RF[i,5] = auc_median_RF[i,4]/auc_median_RF[1,4]
    #
    # auc_median_svm[i,3] = auc_median_svm[i,2]/auc_median_svm[1,2]
    # auc_median_svm[i,5] = auc_median_svm[i,4]/auc_median_svm[1,4]
    #
    # auc_median_xgb[i,3] = auc_median_xgb[i,2]/auc_median_xgb[1,2]
    # auc_median_xgb[i,5] = auc_median_xgb[i,4]/auc_median_xgb[1,4]

    m=m+1
  }

  # file = paste0("./result/auc_median.Rdata")
  # save(auc_median_LR, auc_median_RF, auc_median_svm, auc_median_xgb, file=file)


  n = n_train_total
  nn=seq(1,max(n_train_sets)*2+50,1)
  group2=c("SD-tfe","SD-True")
  newx = data.frame(n=nn)

  ###LR

  fit.tfe = lm(1/t(auc_median)[2,]~n)
  fit.true = lm(1/t(auc_median)[4,]~n)

  predy.tfe=1/predict(fit.tfe,newx)
  predy.true=1/predict(fit.true,newx)

  SD <-data.frame(
    sd = c(t(auc_median)[2,],t(auc_median)[4,]),
    size = rep(n,2),
    Class = rep(group2,each=num_of_train_set)
  )

  SD.pred <-data.frame(
    sd = c(predy.tfe,predy.true),
    nn = rep(nn,2),
    Class = rep(group2,each=length(nn))
  )
  pdf(file=paste(model[i],"plot2.pdf",sep="_"),width=10,height=6)
  plot(ggplot(data = SD, aes(x = size, y = sd, group=Class,color=Class))+
    geom_point()+
    geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  dev.off()

  # ###RF
  #
  # fit_RF.tfe = lm(1/t(auc_median_RF)[2,]~n)
  # fit_RF.true = lm(1/t(auc_median_RF)[4,]~n)
  #
  # predy_RF.tfe=1/predict(fit_RF.tfe,newx)
  # predy_RF.true=1/predict(fit_RF.true,newx)
  #
  # SD_RF <-data.frame(
  #   sd = c(t(auc_median_RF)[2,],t(auc_median_RF)[4,]),
  #   size = rep(n,2),
  #   Class = rep(group,each=num_of_train_set)
  # )
  #
  # SD.pred <-data.frame(
  #   sd = c(predy_RF.tfe,predy_RF.true),
  #   nn = rep(nn,2),
  #   Class = rep(group,each=length(nn))
  # )
  # pdf(file="table_RF_plot.pdf",width=10,height=6)
  # plot(ggplot(data = SD_RF, aes(x = size, y = sd, group=Class,color=Class))+
  #   geom_point()+
  #   geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
  #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  # dev.off()
  #
  # ###svm
  #
  # fit_svm.tfe = lm(1/t(auc_median_svm)[2,]~n)
  # fit_svm.true = lm(1/t(auc_median_svm)[4,]~n)
  #
  # predy_svm.tfe=1/predict(fit_svm.tfe,newx)
  # predy_svm.true=1/predict(fit_svm.true,newx)
  #
  # SD_svm <-data.frame(
  #   sd = c(t(auc_median_svm)[2,],t(auc_median_svm)[4,]),
  #   size = rep(n,2),
  #   Class = rep(group,each=num_of_train_set)
  # )
  #
  # SD.pred <-data.frame(
  #   sd = c(predy_svm.tfe,predy_svm.true),
  #   nn = rep(nn,2),
  #   Class = rep(group,each=length(nn))
  # )
  # pdf(file="table_svm_plot.pdf",width=10,height=6)
  # plot(ggplot(data = SD_svm, aes(x = size, y = sd, group=Class,color=Class))+
  #   geom_point()+
  #   geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
  #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  # dev.off()
  #
  # ###xgb
  #
  # fit_xgb.tfe = lm(1/t(auc_median_xgb)[2,]~n)
  # fit_xgb.true = lm(1/t(auc_median_xgb)[4,]~n)
  #
  # predy_xgb.tfe=1/predict(fit_xgb.tfe,newx)
  # predy_xgb.true=1/predict(fit_xgb.true,newx)
  #
  # SD_xgb <-data.frame(
  #   sd = c(t(auc_median_xgb)[2,],t(auc_median_xgb)[4,]),
  #   size = rep(n,2),
  #   Class = rep(group,each=num_of_train_set)
  # )
  #
  # SD.pred <-data.frame(
  #   sd = c(predy_xgb.tfe,predy_xgb.true),
  #   nn = rep(nn,2),
  #   Class = rep(group,each=length(nn))
  # )
  # pdf(file="table_xgb_plot.pdf",width=10,height=6)
  # plot(ggplot(data = SD_xgb, aes(x = size, y = sd, group=Class,color=Class))+
  #   geom_point()+
  #   geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
  #   theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  # dev.off()
  #
  #
  # print(t(auc_median_LR))
  # print(coef(fit_LR.tfe)[c(1,2)])
  # print(coef(fit_LR.true)[c(1,2)])
  #
  # print(t(auc_median_RF))
  # print(coef(fit_RF.tfe)[c(1,2)])
  # print(coef(fit_RF.true)[c(1,2)])
  #
  # print(t(auc_median_svm))
  # print(coef(fit_svm.tfe)[c(1,2)])
  # print(coef(fit_svm.true)[c(1,2)])
  #
  # print(t(auc_median_xgb))
  # print(coef(fit_xgb.tfe)[c(1,2)])
  # print(coef(fit_xgb.true)[c(1,2)])
  }

  setwd("../")

}
