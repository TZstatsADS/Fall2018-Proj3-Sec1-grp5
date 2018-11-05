#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Sizhu Chen, Hengyang Lin
### Project 3

feature <- function(LR_dir, HR_dir, n_points=1000, method = "Normal", ratio = 0){
  
  library("EBImage")
  ########################### functions ##########################
  feat_each_mat<-function(x,y){
    #featMat
    x1<-cbind(rep(0,nrow(x)),x,rep(0,nrow(x)))
    x1<-rbind(rep(0,ncol(x1)),x1,rep(0,ncol(x1)))# add 0
    samp<-diag(x[samp_ind[,1],samp_ind[,2]])
    ep<-t(apply(samp_ind+1,1,find_point,dt=x1))-samp #row = samp ; col=8
    #labMat
    fp<-t(apply(2*samp_ind-1,1,find_point2,dt=y))-samp #row = samp ; col=4
    return(list(ep,fp))
  }
  
  move_mat<-matrix(c(rep(-1,3),0,rep(1,3),0,-1,0,rep(1,3),0,-1,-1),ncol=8,byrow = T)
  move_mat2<-rbind(c(0,0,1,1),c(0,1,0,1))
  
  find_point<-function(v,dt){
    return(diag(dt[(move_mat+v)[1,],(move_mat+v)[2,]]))
  }
  find_point2<-function(v,dt){
    return(diag(dt[(move_mat2+v)[1,],(move_mat2+v)[2,]]))
  } 
  
  ################################### start ##########################
  n_files <- length(list.files(LR_dir))
 
  featMat <- array(NA, c(n_files*n_points, 8, 3))
  labMat <- array(NA, c(n_files*n_points, 4, 3))
  
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    set.seed(22)
    samp_ind<-cbind(sample(1:dim(imgLR)[1],n_points,replace = T),
                      sample(1:dim(imgLR)[2],n_points,replace = T))
    #dim <- dim(imgLR)[1:2]
    #samp <- sample(1:(dim[1]*dim[2]), size = n_points, replace = FALSE)
    #res <- samp %% dim[1]
    #coord_row <- ifelse(res == 0, samp%/%dim[1], samp%/%dim[1]+1)
    #coord_col <- ifelse(res == 0, dim[1], res)
    #samp_ind <- matrix(c(coord_col, coord_row), ncol = 2)
    
    if(method == "Laplacian"){
      f_high <- matrix(1, nc=3, nr=3)
      f_high[2,2] <- -8
      img_highPass <- filter2(imgLR, f_high)
      for(c in 1:3){
        img_highPass <- ifelse(img_highPass < 0, 0, img_highPass)
      }
      img_sum <- img_highPass[,,1]+img_highPass[,,2]+img_highPass[,,3]
      n <- as.integer(ratio*1000)
      samp <- order(t(img_sum), decreasing = TRUE)[1:n]
      res <- samp %% dim[1]
      coord_row <- ifelse(res == 0, samp%/%dim[1], samp%/%dim[1]+1)
      coord_col <- ifelse(res == 0, dim[1], res)
      coord <- matrix(c(coord_col, coord_row), ncol = 2)
      samp_ind[1:n,] <- coord
    }
    #else if(method == "SIFT"){
      
    #}
    for(j in 1:3){
      lt<-feat_each_mat(imgLR[,,j],imgHR[,,j])
      featMat[(1000*i-999):(1000*i),,j]<-lt[[1]]
      labMat[(1000*i-999):(1000*i),,j]<-lt[[2]]
    }
    print(i)
  }
  return(list(feature = featMat, label = labMat))
}



