library(lattice)
library(latticeExtra)
library(ggplot2)
library(reshape2)
library(boot)

err <- "MAE"

setwd("C:/Users/thiag/Documents/1 -Mestrado/SprintFinal/Defesa/Results/")

path <- paste0(err,"/")

filenames <- list.files(path, pattern="*rds")

for (file in filenames) {
  
  readed_file <- readRDS(paste0(path,file), refhook = NULL)
  readed_file$id <- rownames(readed_file)
  
  readed_file_melted <- melt(readed_file[,c("id","ARIMA","VAR","VEC")], id="id",value.name= err)
  
  # ECDF PLOT
  title <- sub('\\.rds$', '', file)
  print(title)

  png(file=paste0("consolidado/ECDF_charts/",title,".png"),width=1920, height=1080)
  
  
  p <- ggplot(readed_file_melted, aes(MAE, colour = variable)) + stat_ecdf()
  p <- p + scale_color_manual(values=c("blue","yellow","black"))
  p <- p + labs(y="ECDF",title=title) 
  

  plot(p)
  
  dev.off()

  
  # ks.test(resultados_df$ARIMA,resultados_df$VAR,alternative = c("less"))
  # ks.test(resultados_df$ARIMA,resultados_df$VEC,alternative = c("less"))
  # ks.test(resultados_df$VAR,resultados_df$VEC,alternative = c("less"))
  
  
}
