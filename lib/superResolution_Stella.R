########################
### Super-resolution ###
########################

### Author: Sizhu Chen
### Project 3

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  library(OpenImageR)
  ########################### functions #######################################
  # x is a matrix (a layer of pic)
  feat<-function(x){
    ind<-cbind(rep(1:nrow(x),ncol(x)),rep(1:ncol(x),each=nrow(x)))
    x1<-cbind(rep(0,nrow(x)),x,rep(0,nrow(x)))
    x1<-rbind(rep(0,ncol(x1)),x1,rep(0,ncol(x1)))# add 0
    samp<-as.vector(x)
    ep<-t(apply(ind+1,1,find_point,dt=x1))-samp #row = nrow(x)*ncol(x) ; col=8
    return(ep)
  }
  move_mat<-matrix(c(rep(-1,3),0,rep(1,3),0,-1,0,rep(1,3),0,-1,-1),ncol=8,byrow = T)
  find_point<-function(v,dt){
    return(diag(dt[(move_mat+v)[1,],(move_mat+v)[2,]]))
  } 
  ##############################################################################
  ### read LR/HR image pairs
  n_files <- length(list.files(LR_dir))
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    for(j in 1:3){
      featMat[,,j]<-feat(imgLR[,,j])
    }
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    
    out_img<-Image(predMat, colormode=Color) 
    writeImage(out_img,file_name = pathHR)
    
  }
}


