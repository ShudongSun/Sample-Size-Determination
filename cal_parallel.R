library(SSD)
library(parallel)

fun <- function(x){
  return(calculate_AUC_base(seed=x))
}

# system.time({
#   res <- lapply(1:20, fun);
# });

clnum <- detectCores()-1

cl <- makeCluster(getOption("cl.cores", clnum));

clusterEvalQ(cl, c(library(SSD)))
clusterExport(cl, c("calculate_AUC_base"),
              envir=environment())

seeds = c(123,5,3)

system.time({
  res <- parLapply(cl, seeds,  fun)
});

file = paste0("./auc_res.Rdata")
save(res,file=file)

load("./auc_res.Rdata")

calculate_std_of_AUC_and_draw_plot(res)


