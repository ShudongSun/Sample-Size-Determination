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
#' @param true_data_to_compare 0/1 variable, whether to have the true data to compare with generated data (which could make the output matrix double): if input_data==NULL(run the simulations), we will generate the true data automatically and calculate the AUCs; if you give us the input_data, please make sure that you give us sufficient input data.
#'
#' @return It will save all the result to the new "result" folder created in your current directory.
#' @export
#'
#' @examples
#' AUC_1 = calculate_AUC_base(n01_p=c(15,15), n01_test=c(300,300), seed=1, model=c("svm","randomforest"), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)))
#' AUC_2 = calculate_AUC_base(n01_p=c(15,15), n01_test=c(300,300), seed=1, model=c("svm","randomforest"), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)))
#' res = {}
#' res[[1]] = AUC_1
#' res[[2]] = AUC_2
#' calculate_std_of_AUC_and_draw_plot(res, n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), model=c("svm","randomforest"), true_data_to_compare=1)
calculate_std_of_AUC_and_draw_plot <- function(res, n_train_sets, model, true_data_to_compare=1)
{
  num_of_seeds = length(res)

  num_of_model = length(model)

  num_of_train_set = length(res[[1]][,1,1])/2
  if(true_data_to_compare==1){
    num_of_train_set = length(res[[1]][,1,1])/2
  }else{
    num_of_train_set = length(res[[1]][,1,1])
  }

  auc_tfs <- array(0,dim=c(num_of_seeds,length(res[[1]][1,,1]),num_of_train_set,num_of_model))

  if(true_data_to_compare==1){
    auc_trainft <- array(0,dim=c(num_of_seeds,length(res[[1]][1,,1]),num_of_train_set,num_of_model))
  }

  for (i in 1: num_of_seeds){
    for (j in 1:num_of_train_set){
      for (k in 1:num_of_model){
        auc_tfs[i,,j,k] = (res[[i]][j,,k])
        if(true_data_to_compare==1){
          auc_trainft[i,,j,k] = (res[[i]][j+num_of_train_set,,k])
        }
      }
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
  seed2=rep(c(1:num_of_seeds),num_of_train_set)

  dir.create("./result")
  setwd("./result")

  for (i in 1:num_of_model){

    AUC_tfs = numeric()

    if(true_data_to_compare==1){
      AUC_trainft = numeric()
    }


    for (j in 1:num_of_train_set){
      AUC_tfs = c(AUC_tfs, c(t(auc_tfs[,,j,i][1:num_of_seeds_show,])))

      if(true_data_to_compare==1){
        AUC_trainft = c(AUC_trainft, c(t(auc_trainft[,,j,i][1:num_of_seeds_show,])))
      }

    }

    auc_tfs_df<-data.frame(
      AUC_tfs=AUC_tfs,
      Seed=as.factor(rep(seed,each=100)),
      Class=as.factor(rep(group,each=num_of_seeds_show*100))
    )

    if(true_data_to_compare==1){
      auc_trainft_df<-data.frame(
        AUC_trainft=AUC_trainft,
        Seed=as.factor(rep(seed,each=100)),
        Class=as.factor(rep(group,each=num_of_seeds_show*100))
          )
      }


    ### part 1: draw plot

    library(ggplot2)

    pd <- position_dodge(0.5) # move them .5 to the left and right
    cbPalette<-c("#D55E00","#009E73","#56B4E9","#CC79A7")

    y_min=1
    y_max=0
    for (k in 1:num_of_seeds){
      y_min = min(y_min,min(res[[k]]))
      y_max = max(y_max,max(res[[k]]))
    }


    auc_tfs.sum<-summarySE(auc_tfs_df,measurevar = "AUC_tfs",groupvars = c("Seed","Class"))
    sd_max = max(auc_tfs.sum$sd)
    if(true_data_to_compare==1){
      auc_trainft.sum<-summarySE(auc_trainft_df,measurevar = "AUC_trainft",groupvars = c("Seed","Class"))
      sd_max = max(max(auc_trainft.sum$sd),sd_max)
    }

    fig_tfs_auc=ggplot(auc_tfs.sum, aes(x=Seed, y=AUC_tfs, color=Class,group=Class)) +
      geom_errorbar(aes(ymin=AUC_tfs-sd, ymax=AUC_tfs+sd),width=0.2,position = pd)+
      geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
      geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC_tfe")+
      theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(y_min,y_max+sd_max)

    # fig_tfs_auc

    if(true_data_to_compare==1){
      fig_trainft_auc=ggplot(auc_trainft.sum, aes(x=Seed, y=AUC_trainft, color=Class,group=Class)) +
        geom_errorbar(aes(ymin=AUC_trainft-sd, ymax=AUC_trainft+sd),width=0.2,position = pd)+
        geom_line(linetype="dashed", aes(color= Class),size=0.2,position=pd) +
        geom_point(size=1,aes(color = Class), alpha = 1,position=pd)+ labs(x = "Seed",y="",title = "AUC_True")+
        theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(y_min,y_max+sd_max)

      library(ggpubr)
      fig1=ggarrange(fig_tfs_auc,fig_trainft_auc,
                     nrow=1,ncol=2,common.legend = TRUE, legend="bottom")
    }else{
      fig1=fig_tfs_auc
    }


    pdf(file=paste(model[i],"plot1.pdf",sep="_"),width=8,height=6)
    plot(fig1)
    dev.off()


  ### part 1: calculate the median
  options(scipen = 200)

  AUC_tfs = numeric()
  if(true_data_to_compare==1){
    AUC_trainft = numeric()
  }


  for (j in 1:num_of_train_set){
    AUC_tfs = c(AUC_tfs, c(t(auc_tfs[,,j,i][1:num_of_seeds,])))

    if(true_data_to_compare==1){
      AUC_trainft = c(AUC_trainft, c(t(auc_trainft[,,j,i][1:num_of_seeds,])))
    }
  }

  auc_tfs_df<-data.frame(
    AUC_tfs=AUC_tfs,
    Seed=as.factor(rep(seed2,each=100)),
    Class=as.factor(rep(group,each=num_of_seeds*100))
  )

  if(true_data_to_compare==1){
    auc_trainft_df<-data.frame(
      AUC_trainft=AUC_trainft,
      Seed=as.factor(rep(seed2,each=100)),
      Class=as.factor(rep(group,each=num_of_seeds*100))
    )
  }

  auc_tfs.sum<-summarySE(auc_tfs_df,measurevar = "AUC_tfs",groupvars = c("Seed","Class"))
  if(true_data_to_compare==1){
    auc_trainft.sum<-summarySE(auc_trainft_df,measurevar = "AUC_trainft",groupvars = c("Seed","Class"))
  }
  auc_median <- data.frame(Seed=group, sdDvByMean_median_tfe=rep(0,length(group)), ratio_tfe=rep(0,length(group)), sdDvByMean_median_trainft=rep(0,length(group)), ratio_trainft=rep(0,length(group)))

  m=1
  for(n0_train in group){
    auc_median[m,2] = median(auc_tfs.sum[auc_tfs.sum[,2]==n0_train,][,5]/auc_tfs.sum[auc_tfs.sum[,2]==n0_train,][,4])
    if(true_data_to_compare==1){
      auc_median[m,4] = median(auc_trainft.sum[auc_trainft.sum[,2]==n0_train,][,5]/auc_trainft.sum[auc_trainft.sum[,2]==n0_train,][,4])
    }

    auc_median[m,3] = auc_median[m,2]/auc_median[1,2]
    if(true_data_to_compare==1){
      auc_median[m,5] = auc_median[m,4]/auc_median[1,4]
    }
    m=m+1
  }



  n = n_train_total
  nn=seq(1,max(n_train_sets)*2+50,1)
  if(true_data_to_compare==1){
    group2=c("SD-tfe","SD-True")
  }else{
    group2=c("SD-tfe")
  }
  newx = data.frame(n=nn)

  ###LR

  # fit.tfe = lm(1/t(auc_median)[2,]~n)
  # fit.true = lm(1/t(auc_median)[4,]~n)
  #
  # predy.tfe=1/predict(fit.tfe,newx)
  # predy.true=1/predict(fit.true,newx)
  y_tfe = t(auc_median)[2,]
  if(true_data_to_compare==1){
    y_true = t(auc_median)[4,]
    srss = mean((y_true-y_tfe)^2)
  }

  # rss_tfe = 0
  # b_tfe = 0
  # rss_true = 0
  # b_true = 0
  # for (b in seq(from=0.01, to=4, by=0.01)){
  #   print(b)
  #
  #   fit.tfe = nls(y_tfe ~ a*n^(-b)+c, start = list(a=0.5, c=0))
  #   if(b==0.01){
  #     rss_tfe = environment(fit.tfe[["m"]][["getPars"]])[["dev"]]
  #     b_tfe = b
  #     }
  #   else{
  #     if(environment(fit.tfe[["m"]][["getPars"]])[["dev"]]<rss_tfe){
  #       rss_tfe = environment(fit.tfe[["m"]][["getPars"]])[["dev"]]
  #       b_tfe = b
  #     }
  #   }
  #
  #   fit.true = nls(y_true ~ a*n^(-b)+c, start = list(a=1, c=0))
  #   if(b==0.01){
  #     rss_true = environment(fit.true[["m"]][["getPars"]])[["dev"]]
  #     b_true = b
  #     }
  #   else{
  #     if(environment(fit.true[["m"]][["getPars"]])[["dev"]]<rss_true){
  #       rss_true = environment(fit.true[["m"]][["getPars"]])[["dev"]]
  #       b_true = b
  #     }
  #   }
  # }

  # fit.tfe = nls(y_tfe ~ a*n^(-b)+c, start = list(a=1, b=0.1, c=1), control = list(maxiter = 500, minFactor=1/10240000), trace = T)
  # fit.true = nls(y_true ~ a*n^(-b)+c, start = list(a=1, b=0.1, c=1))

  fit.tfe = nls(y_tfe ~ a*n^(-b), start = list(a=0.5, b=0.1))
  if(true_data_to_compare==1){
    fit.true = nls(y_true ~ a*n^(-b), start = list(a=0.5, b=0.1))
  }


  predy.tfe=predict(fit.tfe,newx)
  if(true_data_to_compare==1){
    predy.true=predict(fit.true,newx)
  }

  if(true_data_to_compare==1){
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
  }else{
    SD <-data.frame(
      sd = c(t(auc_median)[2,]),
      size = rep(n,1),
      Class = rep(group2,each=num_of_train_set)
    )
    SD.pred <-data.frame(
      sd = c(predy.tfe),
      nn = rep(nn,1),
      Class = rep(group2,each=length(nn))
    )

  }


  auc_min=1
  auc_max=0
  if(true_data_to_compare==1){
    auc_min = min(c(t(auc_median)[2,], t(auc_median)[4,]))
    auc_max = max(c(t(auc_median)[2,], t(auc_median)[4,]))
  }else{
    auc_min = min(c(t(auc_median)[2,]))
    auc_max = max(c(t(auc_median)[2,]))
  }


  pdf(file=paste(model[i],"plot2.pdf",sep="_"),width=10,height=6)
  if(true_data_to_compare==1){
    plot(ggplot(data = SD, aes(x = size, y = sd, group=Class,color=Class))+
           geom_point()+geom_label(label=paste("RSS =",round(srss,8)),x=max(n_train_sets)*2, y=max(auc_min-0.02,0)+((auc_max+0.05)-max(auc_min-0.02,0))*0.8,label.padding = unit(0.55, "lines"),color = "black")+
           geom_line(data = SD.pred, aes(x=nn, y=sd, group=Class,color=Class), na.rm=TRUE)+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
           theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(max(auc_min-0.02,0),auc_max+0.05))
  }else{
    plot(ggplot(data = SD, aes(x = size, y = sd, group=Class,color=Class))+
           geom_point()+
           geom_line(data = SD.pred, aes(x=nn, y=sd, group=Class,color=Class), na.rm=TRUE)+ labs(x = "Size of training data",y="Median of (std. of AUC / mean of AUC)",title = "Standard deviation of AUC")+
           theme_bw()+theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ylim(max(auc_min-0.02,0),auc_max+0.05))
  }


  dev.off()

  }

  setwd("../")


}
