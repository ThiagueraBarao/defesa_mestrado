library(lattice)
library(latticeExtra)
library(ggplot2)
library(reshape2)
library(boot)

setwd("C:/Users/thiag/Documents/1 -Mestrado/SprintFinal/Defesa/Results/")

for (err in c("MAE","MSE")) {
  
  boot_list <- list()
  
  
  path <- paste0(err,"/")
  
  filenames <- list.files(path, pattern="*rds")
  
  for (file in filenames) {
    
    title <- sub('\\.rds$', '', file)
    
    readed_file <- readRDS(paste0(path,file), refhook = NULL)
    
    base_split <- strsplit(file, split = "&")[[1]]
    par__nobs <- base:::strsplit(base_split[2], split = "__")[[1]][2]
    par__n_ma <- base:::strsplit(base_split[3], split = "__")[[1]][2]
    par__n_fold_ts <- base:::strsplit(base_split[4], split = "__")[[1]][2]
    par__n_ts <- substr(base:::strsplit(base_split[5], split = "__")[[1]][2],1,1)
    
    
    ci_list <- list()
    
    for (modelo_ci in c("ARIMA","VAR","VEC")) {
      boot_fun <- function(data, indices,col){
        d <- data[indices,c(col)]
        mean(d)
      }
      
      boot_sample <- boot(readed_file, boot_fun,col=modelo_ci, R=1000)
      # get 95% confidence interval
      boot_sample_ci <- boot.ci(boot_sample, type="bca",conf = 0.95)
      
      mean_ci <- boot_sample_ci$t0
      sup_ci <- boot_sample_ci$bca[,4]
      inf_ci <- boot_sample_ci$bca[,5]
      
      
      boot_list[[paste0(file,modelo_ci)]] <- list("file"=file,"n_obs"=par__nobs,"n_ma"=par__n_ma,"n_fold_ts"=par__n_fold_ts,"n_ts"=par__n_ts,"model"=modelo_ci,"mean"=mean_ci[1],"sup"=as.numeric(sup_ci),"inf"=as.numeric(inf_ci))
      
      ci_list[[modelo_ci]] <- list(model=modelo_ci,"mean"=mean_ci[1],"sup"=as.numeric(sup_ci),"inf"=as.numeric(inf_ci))
      
    }
    
    ci_df <- do.call(rbind.data.frame, ci_list)
    
    
    
    
    # Chart
    
    png(file=paste0("consolidado/",err,"/boot_charts/",title,".png"),width=1920, height=1080)
    
    plots__ <- ggplot(ci_df) +
      geom_bar( aes(x=model, y=mean), stat="identity", fill="skyblue", alpha=0.7) +
      geom_errorbar( aes(x=model, ymin=inf, ymax=sup), width=0.4, colour="black", alpha=0.9, size=0.9)+
      ggtitle(paste0("Average ",err, " | With Empirical Confidence Intervals")) +
      ylab(err)
    
    plot(plots__)
    
    dev.off()
    
    
    
  }
  
  boot_consolidado <- do.call(rbind.data.frame, boot_list)
  
  writexl::write_xlsx(boot_consolidado,paste0("consolidado/",err,"/boot.xlsx"))
  
}


