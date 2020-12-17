#' @title Calculate std and draw the plots
#'
#' @param res the result produced by cal_parallel
#'
#' @return It will save all the result to the "result" directory.
#' @export
#'
#' @examples calculate_std_of_AUC_and_draw_plot(res)
calculate_std_of_AUC_and_draw_plot <- function(res, n_train_sets = c(15,30,60,120,150))
{
  num_of_seeds = length(res)

  num_of_train_set = length(res[[1]]$LR_AUC[,1])/2

  auc_tfs_LR <- array(0,dim=c(num_of_seeds,length(res[[1]]$LR_AUC[1,]),num_of_train_set))
  auc_tfs_RF <- array(0,dim=c(num_of_seeds,length(res[[1]]$RF_AUC[1,]),num_of_train_set))
  auc_tfs_svm <- array(0,dim=c(num_of_seeds,length(res[[1]]$SVM_AUC[1,]),num_of_train_set))
  auc_tfs_xgb <- array(0,dim=c(num_of_seeds,length(res[[1]]$XGB_AUC[1,]),num_of_train_set))

  auc_trainft_LR <- array(0,dim=c(num_of_seeds,length(res[[1]]$LR_AUC[1,]),num_of_train_set))
  auc_trainft_RF <- array(0,dim=c(num_of_seeds,length(res[[1]]$RF_AUC[1,]),num_of_train_set))
  auc_trainft_svm <- array(0,dim=c(num_of_seeds,length(res[[1]]$SVM_AUC[1,]),num_of_train_set))
  auc_trainft_xgb <- array(0,dim=c(num_of_seeds,length(res[[1]]$XGB_AUC[1,]),num_of_train_set))

  for (i in 1: num_of_seeds)
  {
    for (j in 1:num_of_train_set){
      auc_tfs_LR[i,,j] = res[[i]]$LR_AUC[j,]
      auc_tfs_RF[i,,j] = res[[i]]$RF_AUC[j,]
      auc_tfs_svm[i,,j] = res[[i]]$SVM_AUC[j,]
      auc_tfs_xgb[i,,j] = res[[i]]$XGB_AUC[j,]

      auc_trainft_LR[i,,j] = res[[i]]$LR_AUC[j+num_of_train_set,]
      auc_trainft_RF[i,,j] = res[[i]]$RF_AUC[j+num_of_train_set,]
      auc_trainft_svm[i,,j] = res[[i]]$SVM_AUC[j+num_of_train_set,]
      auc_trainft_xgb[i,,j] = res[[i]]$XGB_AUC[j+num_of_train_set,]
    }
    # auc15_tfs_LR = rbind(auc15_tfs_LR,res[[i]]$LR_AUC[1,])
    # auc15_tfs_RF = rbind(auc15_tfs_RF,res[[i]]$RF_AUC[1,])
    # auc15_tfs_svm = rbind(auc15_tfs_svm,res[[i]]$SVM_AUC[1,])
    # auc15_tfs_xgb = rbind(auc15_tfs_xgb,res[[i]]$XGB_AUC[1,])
    #
    # auc30_tfs_LR = rbind(auc30_tfs_LR,res[[i]]$LR_AUC[2,])
    # auc30_tfs_RF = rbind(auc30_tfs_RF,res[[i]]$RF_AUC[2,])
    # auc30_tfs_svm = rbind(auc30_tfs_svm,res[[i]]$SVM_AUC[2,])
    # auc30_tfs_xgb = rbind(auc30_tfs_xgb,res[[i]]$XGB_AUC[2,])
    #
    # auc60_tfs_LR = rbind(auc60_tfs_LR,res[[i]]$LR_AUC[3,])
    # auc60_tfs_RF = rbind(auc60_tfs_RF,res[[i]]$RF_AUC[3,])
    # auc60_tfs_svm = rbind(auc60_tfs_svm,res[[i]]$SVM_AUC[3,])
    # auc60_tfs_xgb = rbind(auc60_tfs_xgb,res[[i]]$XGB_AUC[3,])
    #
    # auc120_tfs_LR = rbind(auc120_tfs_LR,res[[i]]$LR_AUC[4,])
    # auc120_tfs_RF = rbind(auc120_tfs_RF,res[[i]]$RF_AUC[4,])
    # auc120_tfs_svm = rbind(auc120_tfs_svm,res[[i]]$SVM_AUC[4,])
    # auc120_tfs_xgb = rbind(auc120_tfs_xgb,res[[i]]$XGB_AUC[4,])
    #
    # auc150_tfs_LR = rbind(auc150_tfs_LR,res[[i]]$LR_AUC[5,])
    # auc150_tfs_RF = rbind(auc150_tfs_RF,res[[i]]$RF_AUC[5,])
    # auc150_tfs_svm = rbind(auc150_tfs_svm,res[[i]]$SVM_AUC[5,])
    # auc150_tfs_xgb = rbind(auc150_tfs_xgb,res[[i]]$XGB_AUC[5,])
    #
    # auc15_trainft_LR = rbind(auc15_trainft_LR,res[[i]]$LR_AUC[6,])
    # auc15_trainft_RF = rbind(auc15_trainft_RF,res[[i]]$RF_AUC[6,])
    # auc15_trainft_svm = rbind(auc15_trainft_svm,res[[i]]$SVM_AUC[6,])
    # auc15_trainft_xgb = rbind(auc15_trainft_xgb,res[[i]]$XGB_AUC[6,])
    #
    # auc30_trainft_LR = rbind(auc30_trainft_LR,res[[i]]$LR_AUC[7,])
    # auc30_trainft_RF = rbind(auc30_trainft_RF,res[[i]]$RF_AUC[7,])
    # auc30_trainft_svm = rbind(auc30_trainft_svm,res[[i]]$SVM_AUC[7,])
    # auc30_trainft_xgb = rbind(auc30_trainft_xgb,res[[i]]$XGB_AUC[7,])
    #
    # auc60_trainft_LR = rbind(auc60_trainft_LR,res[[i]]$LR_AUC[8,])
    # auc60_trainft_RF = rbind(auc60_trainft_RF,res[[i]]$RF_AUC[8,])
    # auc60_trainft_svm = rbind(auc60_trainft_svm,res[[i]]$SVM_AUC[8,])
    # auc60_trainft_xgb = rbind(auc60_trainft_xgb,res[[i]]$XGB_AUC[8,])
    #
    # auc120_trainft_LR = rbind(auc120_trainft_LR,res[[i]]$LR_AUC[9,])
    # auc120_trainft_RF = rbind(auc120_trainft_RF,res[[i]]$RF_AUC[9,])
    # auc120_trainft_svm = rbind(auc120_trainft_svm,res[[i]]$SVM_AUC[9,])
    # auc120_trainft_xgb = rbind(auc120_trainft_xgb,res[[i]]$XGB_AUC[9,])
    #
    # auc150_trainft_LR = rbind(auc150_trainft_LR,res[[i]]$LR_AUC[10,])
    # auc150_trainft_RF = rbind(auc150_trainft_RF,res[[i]]$RF_AUC[10,])
    # auc150_trainft_svm = rbind(auc150_trainft_svm,res[[i]]$SVM_AUC[10,])
    # auc150_trainft_xgb = rbind(auc150_trainft_xgb,res[[i]]$XGB_AUC[10,])
  }


  group = n_train_sets
  if(num_of_seeds<8){
    num_of_seeds_show = num_of_seeds
  }else{
    num_of_seeds_show = 8
  }
  seed=rep(c(1:num_of_seeds_show),num_of_train_set)
  # seed=rep(c(9,10,11,12,13,14,15,16),5)

  AUC_tfs_LR = numeric()
  AUC_tfs_RF = numeric()
  AUC_tfs_SVM = numeric()
  AUC_tfs_XGB = numeric()

  AUC_trainft_LR = numeric()
  AUC_trainft_RF = numeric()
  AUC_trainft_SVM = numeric()
  AUC_trainft_XGB = numeric()

  for (j in 1:num_of_train_set){
    AUC_tfs_LR = c(AUC_tfs_LR, c(t(auc_tfs_LR[,,j][1:num_of_seeds_show,])))
    AUC_tfs_RF = c(AUC_tfs_RF, c(t(auc_tfs_RF[,,j][1:num_of_seeds_show,])))
    AUC_tfs_SVM = c(AUC_tfs_SVM, c(t(auc_tfs_svm[,,j][1:num_of_seeds_show,])))
    AUC_tfs_XGB = c(AUC_tfs_XGB, c(t(auc_tfs_xgb[,,j][1:num_of_seeds_show,])))

    AUC_trainft_LR = c(AUC_trainft_LR, c(t(auc_trainft_LR[,,j][1:num_of_seeds_show,])))
    AUC_trainft_RF = c(AUC_trainft_RF, c(t(auc_trainft_RF[,,j][1:num_of_seeds_show,])))
    AUC_trainft_SVM = c(AUC_trainft_SVM, c(t(auc_trainft_svm[,,j][1:num_of_seeds_show,])))
    AUC_trainft_XGB = c(AUC_trainft_XGB, c(t(auc_trainft_xgb[,,j][1:num_of_seeds_show,])))

  }

  auc_tfs<-data.frame(
    AUC_tfs_LR=AUC_tfs_LR,
    AUC_tfs_RF=AUC_tfs_RF,
    AUC_tfs_svm=AUC_tfs_SVM,
    AUC_tfs_xgb=AUC_tfs_XGB,
    Seed=as.factor(rep(seed,each=100)),
    Class=as.factor(rep(group,each=num_of_seeds_show*100))
  )

  auc_trainft<-data.frame(
    AUC_trainft_LR=AUC_trainft_LR,
    AUC_trainft_RF=AUC_trainft_RF,
    AUC_trainft_svm=AUC_trainft_SVM,
    AUC_trainft_xgb=AUC_trainft_XGB,
    Seed=as.factor(rep(seed,each=100)),
    Class=as.factor(rep(group,each=num_of_seeds_show*100))
  )




  ### part 1: draw plot

  library(ggplot2)

  pd <- position_dodge(0.5) # move them .5 to the left and right
  cbPalette<-c("#D55E00","#009E73","#56B4E9","#CC79A7")


  auc_tfs_LR.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_LR",groupvars = c("Seed","Class"))
  fig_tfs_auc_LR=ggplot(auc_tfs_LR.sum, aes(x=Seed, y=AUC_tfs_LR, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_tfs_LR-sd, ymax=AUC_tfs_LR+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_tfs_auc_LR

  auc_trainft_LR.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_LR",groupvars = c("Seed","Class"))
  fig_trainft_auc_LR=ggplot(auc_trainft_LR.sum, aes(x=Seed, y=AUC_trainft_LR, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_trainft_LR-sd, ymax=AUC_trainft_LR+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_trainft_auc_LR

  auc_tfs_RF.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_RF",groupvars = c("Seed","Class"))
  fig_tfs_auc_RF=ggplot(auc_tfs_RF.sum, aes(x=Seed, y=AUC_tfs_RF, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_tfs_RF-sd, ymax=AUC_tfs_RF+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_tfs_auc_RF

  auc_trainft_RF.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_RF",groupvars = c("Seed","Class"))
  fig_trainft_auc_RF=ggplot(auc_trainft_RF.sum, aes(x=Seed, y=AUC_trainft_RF, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_trainft_RF-sd, ymax=AUC_trainft_RF+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_trainft_auc_RF

  auc_tfs_svm.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_svm",groupvars = c("Seed","Class"))
  fig_tfs_auc_svm=ggplot(auc_tfs_svm.sum, aes(x=Seed, y=AUC_tfs_svm, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_tfs_svm-sd, ymax=AUC_tfs_svm+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_tfs_auc_svm

  auc_trainft_svm.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_svm",groupvars = c("Seed","Class"))
  fig_trainft_auc_svm=ggplot(auc_trainft_svm.sum, aes(x=Seed, y=AUC_trainft_svm, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_trainft_svm-sd, ymax=AUC_trainft_svm+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_trainft_auc_svm

  auc_tfs_xgb.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_xgb",groupvars = c("Seed","Class"))
  fig_tfs_auc_xgb=ggplot(auc_tfs_xgb.sum, aes(x=Seed, y=AUC_tfs_xgb, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_tfs_xgb-sd, ymax=AUC_tfs_xgb+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_tfs_auc_xgb

  auc_trainft_xgb.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_xgb",groupvars = c("Seed","Class"))
  fig_trainft_auc_xgb=ggplot(auc_trainft_xgb.sum, aes(x=Seed, y=AUC_trainft_xgb, color=Class,group=Class)) +
    geom_errorbar(aes(ymin=AUC_trainft_xgb-sd, ymax=AUC_trainft_xgb+sd),width=0.2,position = pd)+
    geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
    geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(0.7,1.05)

  # fig_trainft_auc_xgb


  library(ggpubr)
  fig_LR=ggarrange(fig_tfs_auc_LR,fig_trainft_auc_LR,
                   nrow=1,ncol=2,common.legend = TRUE, legend="bottom")

  dir.create("./result")
  setwd("./result")
  pdf(file="LR_plot.pdf",width=8,height=6)
  plot(fig_LR)
  dev.off()


  fig_RF=ggarrange(fig_tfs_auc_RF,fig_trainft_auc_RF,
                   nrow=1,ncol=2,common.legend = TRUE, legend="bottom")
  # fig_RF
  pin = c(10,3)
  pdf(file="RF_plot.pdf",width=8,height=6)
  plot(fig_RF)
  dev.off()

  fig_svm=ggarrange(fig_tfs_auc_svm,fig_trainft_auc_svm,
                    nrow=1,ncol=2,common.legend = TRUE, legend="bottom")
  # fig_svm
  pdf(file="svm_plot.pdf",width=8,height=6)
  plot(fig_svm)
  dev.off()

  fig_xgb=ggarrange(fig_tfs_auc_xgb,fig_trainft_auc_xgb,
                    nrow=1,ncol=2,common.legend = TRUE, legend="bottom")
  # fig_xgb
  pdf(file="xgb_plot.pdf",width=8,height=6)
  plot(fig_xgb)
  dev.off()


  ### part 1: calculate the median
  options(scipen = 200)
  group = n_train_sets
  seed=rep(c(1:num_of_seeds),num_of_train_set)

  AUC_tfs_LR = numeric()
  AUC_tfs_RF = numeric()
  AUC_tfs_SVM = numeric()
  AUC_tfs_XGB = numeric()

  AUC_trainft_LR = numeric()
  AUC_trainft_RF = numeric()
  AUC_trainft_SVM = numeric()
  AUC_trainft_XGB = numeric()

  for (j in 1:num_of_train_set){
    AUC_tfs_LR = c(AUC_tfs_LR, c(t(auc_tfs_LR[,,j][1:num_of_seeds,])))
    AUC_tfs_RF = c(AUC_tfs_RF, c(t(auc_tfs_RF[,,j][1:num_of_seeds,])))
    AUC_tfs_SVM = c(AUC_tfs_SVM, c(t(auc_tfs_svm[,,j][1:num_of_seeds,])))
    AUC_tfs_XGB = c(AUC_tfs_XGB, c(t(auc_tfs_xgb[,,j][1:num_of_seeds,])))

    AUC_trainft_LR = c(AUC_trainft_LR, c(t(auc_trainft_LR[,,j][1:num_of_seeds,])))
    AUC_trainft_RF = c(AUC_trainft_RF, c(t(auc_trainft_RF[,,j][1:num_of_seeds,])))
    AUC_trainft_SVM = c(AUC_trainft_SVM, c(t(auc_trainft_svm[,,j][1:num_of_seeds,])))
    AUC_trainft_XGB = c(AUC_trainft_XGB, c(t(auc_trainft_xgb[,,j][1:num_of_seeds,])))

  }

  auc_tfs<-data.frame(
    AUC_tfs_LR=AUC_tfs_LR,
    AUC_tfs_RF=AUC_tfs_RF,
    AUC_tfs_svm=AUC_tfs_SVM,
    AUC_tfs_xgb=AUC_tfs_XGB,
    Seed=as.factor(rep(seed,each=100)),
    Class=as.factor(rep(group,each=num_of_seeds*100))
  )

  auc_trainft<-data.frame(
    AUC_trainft_LR=AUC_trainft_LR,
    AUC_trainft_RF=AUC_trainft_RF,
    AUC_trainft_svm=AUC_trainft_SVM,
    AUC_trainft_xgb=AUC_trainft_XGB,
    Seed=as.factor(rep(seed,each=100)),
    Class=as.factor(rep(group,each=num_of_seeds*100))
  )

  auc_tfs_LR.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_LR",groupvars = c("Seed","Class"))
  auc_trainft_LR.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_LR",groupvars = c("Seed","Class"))

  auc_tfs_RF.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_RF",groupvars = c("Seed","Class"))
  auc_trainft_RF.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_RF",groupvars = c("Seed","Class"))

  auc_tfs_svm.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_svm",groupvars = c("Seed","Class"))
  auc_trainft_svm.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_svm",groupvars = c("Seed","Class"))

  auc_tfs_xgb.sum<-summarySE(auc_tfs,measurevar = "AUC_tfs_xgb",groupvars = c("Seed","Class"))
  auc_trainft_xgb.sum<-summarySE(auc_trainft,measurevar = "AUC_trainft_xgb",groupvars = c("Seed","Class"))

  auc_median_LR <- auc_median_RF <- auc_median_svm <- auc_median_xgb <- data.frame(Seed=group, sdDvByMean_median_tfe=rep(0,length(group)), ratio_tfe=rep(0,length(group)), sdDvByMean_median_trainft=rep(0,length(group)), ratio_trainft=rep(0,length(group)))

  i=1
  for(n0_train in group){
    auc_median_LR[i,2] = median(auc_tfs_LR.sum[auc_tfs_LR.sum[,2]==n0_train,][,5]/auc_tfs_LR.sum[auc_tfs_LR.sum[,2]==n0_train,][,4])
    auc_median_LR[i,4] = median(auc_trainft_LR.sum[auc_trainft_LR.sum[,2]==n0_train,][,5]/auc_trainft_LR.sum[auc_trainft_LR.sum[,2]==n0_train,][,4])

    auc_median_RF[i,2] = median(auc_tfs_RF.sum[auc_tfs_RF.sum[,2]==n0_train,][,5]/auc_tfs_RF.sum[auc_tfs_RF.sum[,2]==n0_train,][,4])
    auc_median_RF[i,4] = median(auc_trainft_RF.sum[auc_trainft_RF.sum[,2]==n0_train,][,5]/auc_trainft_RF.sum[auc_trainft_RF.sum[,2]==n0_train,][,4])

    auc_median_svm[i,2] = median(auc_tfs_svm.sum[auc_tfs_svm.sum[,2]==n0_train,][,5]/auc_tfs_svm.sum[auc_tfs_svm.sum[,2]==n0_train,][,4])
    auc_median_svm[i,4] = median(auc_trainft_svm.sum[auc_trainft_svm.sum[,2]==n0_train,][,5]/auc_trainft_svm.sum[auc_trainft_svm.sum[,2]==n0_train,][,4])

    auc_median_xgb[i,2] = median(auc_tfs_xgb.sum[auc_tfs_xgb.sum[,2]==n0_train,][,5]/auc_tfs_xgb.sum[auc_tfs_xgb.sum[,2]==n0_train,][,4])
    auc_median_xgb[i,4] = median(auc_trainft_xgb.sum[auc_trainft_xgb.sum[,2]==n0_train,][,5]/auc_trainft_xgb.sum[auc_trainft_xgb.sum[,2]==n0_train,][,4])

    auc_median_LR[i,3] = auc_median_LR[i,2]/auc_median_LR[1,2]
    auc_median_LR[i,5] = auc_median_LR[i,4]/auc_median_LR[1,4]

    auc_median_RF[i,3] = auc_median_RF[i,2]/auc_median_RF[1,2]
    auc_median_RF[i,5] = auc_median_RF[i,4]/auc_median_RF[1,4]

    auc_median_svm[i,3] = auc_median_svm[i,2]/auc_median_svm[1,2]
    auc_median_svm[i,5] = auc_median_svm[i,4]/auc_median_svm[1,4]

    auc_median_xgb[i,3] = auc_median_xgb[i,2]/auc_median_xgb[1,2]
    auc_median_xgb[i,5] = auc_median_xgb[i,4]/auc_median_xgb[1,4]

    i=i+1
  }

  # file = paste0("./result/auc_median.Rdata")
  # save(auc_median_LR, auc_median_RF, auc_median_svm, auc_median_xgb, file=file)


  n = n_train_sets
  nn=seq(1,200,1)
  group=c("SD-tfe","SD-True")
  newx = data.frame(n=nn)

  ###LR

  fit_LR.tfe = lm(1/t(auc_median_LR)[2,]~n)
  fit_LR.true = lm(1/t(auc_median_LR)[4,]~n)

  predy_LR.tfe=1/predict(fit_LR.tfe,newx)
  predy_LR.true=1/predict(fit_LR.true,newx)

  SD_LR <-data.frame(
    sd = c(t(auc_median_LR)[2,],t(auc_median_LR)[4,]),
    size = rep(n,2),
    Class = rep(group,each=num_of_train_set)
  )

  SD.pred <-data.frame(
    sd = c(predy_LR.tfe,predy_LR.true),
    nn = rep(nn,2),
    Class = rep(group,each=length(nn))
  )
  pdf(file="table_LR_plot.pdf",width=10,height=6)
  plot(ggplot(data = SD_LR, aes(x = size, y = sd, group=Class,color=Class))+
    geom_point()+
    geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  dev.off()

  ###RF

  fit_RF.tfe = lm(1/t(auc_median_RF)[2,]~n)
  fit_RF.true = lm(1/t(auc_median_RF)[4,]~n)

  predy_RF.tfe=1/predict(fit_RF.tfe,newx)
  predy_RF.true=1/predict(fit_RF.true,newx)

  SD_RF <-data.frame(
    sd = c(t(auc_median_RF)[2,],t(auc_median_RF)[4,]),
    size = rep(n,2),
    Class = rep(group,each=num_of_train_set)
  )

  SD.pred <-data.frame(
    sd = c(predy_RF.tfe,predy_RF.true),
    nn = rep(nn,2),
    Class = rep(group,each=length(nn))
  )
  pdf(file="table_RF_plot.pdf",width=10,height=6)
  plot(ggplot(data = SD_RF, aes(x = size, y = sd, group=Class,color=Class))+
    geom_point()+
    geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  dev.off()

  ###svm

  fit_svm.tfe = lm(1/t(auc_median_svm)[2,]~n)
  fit_svm.true = lm(1/t(auc_median_svm)[4,]~n)

  predy_svm.tfe=1/predict(fit_svm.tfe,newx)
  predy_svm.true=1/predict(fit_svm.true,newx)

  SD_svm <-data.frame(
    sd = c(t(auc_median_svm)[2,],t(auc_median_svm)[4,]),
    size = rep(n,2),
    Class = rep(group,each=num_of_train_set)
  )

  SD.pred <-data.frame(
    sd = c(predy_svm.tfe,predy_svm.true),
    nn = rep(nn,2),
    Class = rep(group,each=length(nn))
  )
  pdf(file="table_svm_plot.pdf",width=10,height=6)
  plot(ggplot(data = SD_svm, aes(x = size, y = sd, group=Class,color=Class))+
    geom_point()+
    geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  dev.off()

  ###xgb

  fit_xgb.tfe = lm(1/t(auc_median_xgb)[2,]~n)
  fit_xgb.true = lm(1/t(auc_median_xgb)[4,]~n)

  predy_xgb.tfe=1/predict(fit_xgb.tfe,newx)
  predy_xgb.true=1/predict(fit_xgb.true,newx)

  SD_xgb <-data.frame(
    sd = c(t(auc_median_xgb)[2,],t(auc_median_xgb)[4,]),
    size = rep(n,2),
    Class = rep(group,each=num_of_train_set)
  )

  SD.pred <-data.frame(
    sd = c(predy_xgb.tfe,predy_xgb.true),
    nn = rep(nn,2),
    Class = rep(group,each=length(nn))
  )
  pdf(file="table_xgb_plot.pdf",width=10,height=6)
  plot(ggplot(data = SD_xgb, aes(x = size, y = sd, group=Class,color=Class))+
    geom_point()+
    geom_line(data = SD.pred, aes(x=nn, y=sd,group=Class,color=Class))+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom"))
  dev.off()

  setwd("../")

  print(t(auc_median_LR))
  print(coef(fit_LR.tfe)[c(1,2)])
  print(coef(fit_LR.true)[c(1,2)])

  print(t(auc_median_RF))
  print(coef(fit_RF.tfe)[c(1,2)])
  print(coef(fit_RF.true)[c(1,2)])

  print(t(auc_median_svm))
  print(coef(fit_svm.tfe)[c(1,2)])
  print(coef(fit_svm.true)[c(1,2)])

  print(t(auc_median_xgb))
  print(coef(fit_xgb.tfe)[c(1,2)])
  print(coef(fit_xgb.true)[c(1,2)])

}
