data_mat <- readRDS(system.file("extdata", "mouse_sie_10x.rds", package = "scDesign2"))
nonspikes <- which(!grepl("ercc", rownames(data_mat), ignore.case = TRUE))
data_mat <- data_mat[nonspikes, ,drop = FALSE]
dim(data_mat)
table(colnames(data_mat))
needed_cell_type <- c("Enterocyte.Progenitor.Early","Stem")

set.seed(1)
all_idx <- unlist(sapply(needed_cell_type, function(x){
  cell_type_idx <- which(colnames(data_mat) == x)
  n_cell_total <- length(cell_type_idx)
  sample(cell_type_idx, 800)
}))
allcount <- data_mat[, all_idx]
x_data = t(allcount)
y_data = c(rep(0,800),rep(1,800))
data = list(x_data=x_data, y_data=y_data)

Sys.time()

###
n01_all= c(800,800)
n01_p=c(15,15)
n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150))
n01_test=c(300,300)
seed=1
method="pca2_mvnorm"
model=c("svm","randomforest")
func=NULL
data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5)))
#data_input=NULL
true_data_to_compare=1

###
# Sys.time()
# result = calculate_AUCs(n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300), num_of_seeds=20, random_seeds=FALSE, calculate_std_of_AUC_and_produce_plot=TRUE, method="scDesign2", model=c("svm","randomforest"), data_input=data)
# Sys.time()
