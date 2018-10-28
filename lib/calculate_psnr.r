##helper function that calculates MSE
mse <- function(x_hat,x) { 
  mean(rowMeans((x_hat-x)^2))
}

#calculate PSNR
psnr <- function(x_hat,x) {
  10 * log10(255/mse(x_hat,x))
  }

#function that takes in list of predicted images (ie. images that have been run through a model) 
#and the true_label (ie. high resolution images) and returns a list of PSNR values 
calculate_psnr <- function(prediction, true_label){
  if(length(prediction) != length(true_label)){
    stop("input lists must be same length")
  }
  l <- vector("list", length(prediction))
  for (i in 1:length(prediction)){
    l[[i]] <- psnr(prediction[[i]], true_label[[i]])
  }
  return(l) 
}


