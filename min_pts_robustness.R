## Analyzing results from min_pts_storage_boot
files = list.files(paste0(getwd(),"/Houston_Mobile_Dynamic_PCA/min_pts_storage_boot"))

min_pts_res <- vector(mode = "list",length = length(files))

outer_function <- function(a,b,fun){
  outer(a,b,function(x,y) vapply(seq_along(x), function(i) fun(x[[i]], y[[i]]), numeric(1)))
}

mean_absolute_difference <- function(x,y){
  different_idxs <- which(x!=y)
  if(length(different_idxs)>0){
    x <- unlist(x,use.names = FALSE)
    y <- unlist(y,use.names = FALSE)
    diff_x <- x[different_idxs]
    diff_y <- y[different_idxs]
    abs_differences <- abs(diff_x-diff_y)
    return(mean(abs_differences))
  } else{
    return(0)
  }
}

for (i in 1:length(files)){
  min_pts_res[[i]] <- read.csv(file = 
                      paste0(getwd(),"/Houston_Mobile_Dynamic_PCA/min_pts_storage_boot/",files[i]),
               row.names = 1)
  
  colnames(min_pts_res[[i]]) <- "min_pts"

}




outer_function(min_pts_res,min_pts_res,function(x,y) length(which(x != y)))

outer_function(min_pts_res,min_pts_res, function(x,y) mean_absolute_difference(x,y))
