path <- "C:/Users/thiag/Documents/1 -Mestrado/SprintFinal/Results/DM/"

filenames <- list.files(path, pattern="*rds")

DM_list <- list()

for (file in filenames) {
  
  readed_DM <- readRDS(paste0(path,file), refhook = NULL)
  
  base_split <- strsplit(file, split = "&")[[1]]
  par__nobs <- base:::strsplit(base_split[2], split = "__")[[1]][2]
  par__n_ma <- base:::strsplit(base_split[3], split = "__")[[1]][2]
  par__n_fold_ts <- base:::strsplit(base_split[4], split = "__")[[1]][2]
  par__n_ts <- substr(base:::strsplit(base_split[5], split = "__")[[1]][2],1,1)

  
  DM_list[[file]] <- list("file"=file,"n_obs"=par__nobs,"n_ma"=par__n_ma,"n_fold_ts"=par__n_fold_ts,"n_ts"=par__n_ts,"SAME_1_pct"=mean(readed_DM>0.01),"SAME_5_pct"=mean(readed_DM>0.05),"SAME_10_pct"=mean(readed_DM>0.1))
  
}

DM_consolidado <- do.call(rbind.data.frame, DM_list)

writexl::write_xlsx(DM_consolidado,"C:/Users/thiag/Documents/1 -Mestrado/SprintFinal/Results/consolidado/DM.xlsx")