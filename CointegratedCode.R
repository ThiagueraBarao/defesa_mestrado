# Libraries
library(stats)
require(tsDyn)
library(vars)
library(Metrics)
library(data.table)
library(dplyr)
library(progress)
library(urca)
library(tseries)
library(forecast)
library(DescTools)
require(utils)

# Simulation Params Grid
grid_params <- expand.grid(
  arima_n_obs = c(500,250, 100),
  n_ma  = c(0, 12),
  n_fold_ts = c(0.85,0.9, 0.95),
  n_ts = c(3,2)
)

seed <- 2022
ite_total <- 20

setwd("C:/Users/thiag/Documents/1 -Mestrado/SprintFinal/Defesa/Results/")
dir.create("MAE")
dir.create("MSE")
dir.create("DM")
dir.create("consolidado")
dir.create("consolidado/ECDF_charts")
dir.create("consolidado/MAE")
dir.create("consolidado/MSE")
dir.create("consolidado/MAE/boot_charts")
dir.create("consolidado/MSE/boot_charts")


# Loop
for (grid_i in c(1:nrow(grid_params))) {

  selecionados <- grid_params[grid_i, ]

  arima_n_obs <- selecionados[["arima_n_obs"]]
  plot <- F
  n_ma <- selecionados[["n_ma"]]
  n_fold_ts <- selecionados[["n_fold_ts"]]
  n_ts <- selecionados[["n_ts"]]

  label_to_save <- paste0(
    "n_obs__", arima_n_obs, " & ",
    "n_ma__", n_ma, " & ",
    "n_fold_ts__", n_fold_ts * 100, " & ",
    "n_ts__", n_ts)

  progress_general <- round(grid_i/nrow(grid_params),2)
  pb <- progress_bar$new(
    format = paste0("  ", progress_general, "  simulating [:bar] :percent eta: :eta"),
    total = ite_total, clear = FALSE, width= 60)

  results_list <- list()
  results_list_mse <- list()

  diebold_mariano_list_ARIMA <- list()
  diebold_mariano_list_VAR <- list()
  diebold_mariano_list_VAR_ARIMA <- list()
  df_list <- list()
  
  for (i in c(1:ite_total)) {
    
    pb$tick()
    
    seed <- seed + 1
    set.seed(seed)

    # Generate DataFrame
    sd <- 0.8
    cointegration_vector <- c(
      1,
      rnorm(1, 2, 1),
      rnorm(1, 2, 1))
  
    y3 <- stats::arima.sim(
      list(
        order = c(3, 1, 2),
        ar = c(0.6, -0.4, -0.3),
        ma = c(-0.3, 0.2),
        sd = sd),
      n = arima_n_obs)
  
    y2 <- stats::arima.sim(
      list(
        order = c(3, 1, 2),
        ar = c(0.5, -0.3, -0.4),
        ma = c(-0.2, -0.25),
        sd = sd),
      n = arima_n_obs)
  
    u <- stats::arima.sim(
      list(
        order = c(1, 0, 0),
        sd = sd, 
        ar = 0.2),
        n = (arima_n_obs + 1))
  
    y1 <- u + cointegration_vector[2] * y2 + cointegration_vector[3] * y3
  
    df <- as.data.frame(scale(cbind(y1, y2, y3))) %>% as.data.table()
  
    df___diff <- df %>% mutate_each(funs(. - lag(.))) %>% as.data.table()
  
    jotest <- ca.jo(df, type = "eigen")
    resultado_coint <- sum(jotest@cval[, 1] < jotest@teststat)
  
    if (resultado_coint == 0) {
      print("pulou coint")
      next
    }
    if (adf.test(df$y1)$p.value <= 0.05 | adf.test(df$y2)$p.value <= 0.05 | adf.test(df$y3)$p.value <= 0.05) {
      print("pulou adf")
      next
    }
    if (adf.test(diff(df$y1))$p.value > 0.05 | adf.test(diff(df$y2))$p.value > 0.05 | adf.test(diff(df$y3))$p.value > 0.05) {
      print("pulou o adf diff")
      # next
    }
  
    if (n_ma != 0) {
      df_ma <- as.data.table(na.omit(forecast::ma(df,n_ma)))
      colnames(df_ma) <- c("y1","y2","y3")
      df <- df_ma
    }
  
    if (n_ts == 2) {
      df <- df[,1:2]
    }
  
    n <- nrow(df)
    fold_time <- floor(n * n_fold_ts)
  
    train__ <- df[1:fold_time]
    train__diff <- df___diff[2:fold_time]
    test__ <- df[(fold_time + 1):n]
    n_ahead <- nrow(test__)
    last_obs_value <- as.numeric(train__[fold_time, "y1"])
  
    # Models
    modelo___arima <- forecast::auto.arima(
      x = train__$y1, allowdrift = F, method = "ML",d = 1)
  
    # Best Model by BIC
    BIC_var_list <- list()
    BIC_vec_list <- list()
  
    for (order_var in c(1:10)) {
      var_i <- vars::VAR(train__diff, p = order_var)
      vec_i <- tsDyn::VECM(train__,
        lag = order_var, estim = "ML",
        include = c("const"), r = 1)
  
      BIC_var_list[[order_var]] <- BIC(var_i)
      BIC_vec_list[[order_var]] <- BIC(vec_i)
    }
  
    ## Selected order for VAR and VEC
    var_order <- which.min(unlist(BIC_var_list))
    vec_order <- which.min(unlist(BIC_vec_list))
  
    #VAR
    modelo___VAR <- vars::VAR(train__diff, p = var_order)
    #VECM
    modelo___VEC <- tsDyn::VECM(train__, lag = vec_order, estim = "ML",
      include = c("const"), r = 1)
  
    # Prediction
    ## ARIMA
    modelo___arima_predict <- as.numeric(
      predict(modelo___arima, n.ahead = n_ahead)$pred)
    ## VAR
    modelo___VAR_predict_diff <- as.numeric(
      predict(modelo___VAR, n.ahead = n_ahead)$fcst$y1[, "fcst"])
    modelo___VAR_predict <- last_obs_value + cumsum(modelo___VAR_predict_diff)
    ## VEC
    modelo___VEC_predict <- as.numeric(
      predict(modelo___VEC, n.ahead=n_ahead)[, "y1"])
  
    ## Consolidated
    prediction__df <- as.data.table(
      cbind(test__$y1, modelo___arima_predict,
        modelo___VAR_predict, modelo___VEC_predict))
  
    # Obs_Test
    y_obs_test <- test__$y1
  
    residuals_ARIMA <- y_obs_test - modelo___arima_predict
    residuals_VAR <- y_obs_test - modelo___VAR_predict
    residuals_VEC <- y_obs_test - modelo___VEC_predict
  
    diebold_mariano_test_VEC_ARIMA <-  dm.test(residuals_ARIMA, residuals_VEC)
    diebold_mariano_test_VEC_VAR <-  dm.test(residuals_VAR, residuals_VEC)
    diebold_mariano_test_VAR_ARIMA <- dm.test(residuals_VAR, residuals_ARIMA)
  
    diebold_mariano_list_ARIMA <- append(diebold_mariano_list_ARIMA,
      diebold_mariano_test_VEC_ARIMA$p.value)
    diebold_mariano_list_VAR <- append(diebold_mariano_list_VAR,
      diebold_mariano_test_VEC_VAR$p.value)
    diebold_mariano_list_VAR_ARIMA <- append(diebold_mariano_list_VAR_ARIMA,
      diebold_mariano_test_VAR_ARIMA$p.value)
  
    #Metrics
    ## MAE
    modelo___arima_predict___MAE <- mae(y_obs_test, modelo___arima_predict)
    modelo___VAR_predict___MAE <- mae(y_obs_test, modelo___VAR_predict)
    modelo___VEC_predict___MAE <- mae(y_obs_test, modelo___VEC_predict)
  
    resultados <- as.data.frame(
      cbind("ARIMA"=modelo___arima_predict___MAE,"VAR"=modelo___VAR_predict___MAE,
        "VEC"=modelo___VEC_predict___MAE))
  
    resultados$BEST_ALL <- as.matrix(apply(
      resultados[,c("ARIMA","VAR","VEC")], 1, which.min))
  
    results_list[[i]] <- resultados
    ## MSE
    modelo___arima_predict___MSE <- mse(y_obs_test, modelo___arima_predict)
    modelo___VAR_predict___MSE <- mse(y_obs_test, modelo___VAR_predict)
    modelo___VEC_predict___MSE <- mse(y_obs_test, modelo___VEC_predict)
  
    resultados_MSE <- as.data.frame(cbind("ARIMA" = modelo___arima_predict___MSE,
      "VAR" = modelo___VAR_predict___MSE, "VEC" = modelo___VEC_predict___MSE))
    resultados_MSE$BEST_ALL <- as.matrix(apply(
      resultados_MSE[,c("ARIMA", "VAR", "VEC")], 1, which.min))
  
    results_list_mse[[i]] <- resultados_MSE
  
    #Plots
    if (plot) {
      plot.ts(df$y1, pch = 19,col = "black")
      len_train <- train__$y1
      lines(c(rep(NA,length(len_train)),modelo___arima_predict),type = "b", pch = 16,col = "blue")
      lines(c(rep(NA,length(len_train)),modelo___VAR_predict),type = "b", pch = 17,col = "red")
      lines(c(rep(NA,length(len_train)),modelo___VEC_predict),type = "b", pch = 18,col = "green")
      legend("topleft",
             legend=c("actuals","ARIMA","VAR","VECM"),
             col=c("black","blue","red","green"),
             lty = 1:2, cex=0.8)
      round_metrics = 3
      Metrics_Comp <- paste0("ARIMA(",round(modelo___arima_predict___MAE,round_metrics),") | ","VAR(",round(modelo___VAR_predict___MAE,round_metrics),") | ","VEC(",round(modelo___VEC_predict___MAE,3),")")
      
      title(paste("i :",i," |","MAE: ",Metrics_Comp))
    }

  }
  
  ## MAE
  resultados_df <- do.call(rbind.data.frame, results_list)
  table(resultados_df$BEST_ALL)
  ## MSE
  resultados_df_MSE <- do.call(rbind.data.frame, results_list_mse)
  table(resultados_df_MSE$BEST_ALL)
  ## Diebold Mariano
  resultados_df_diebold_mariano_list_ARIMA <- do.call(rbind.data.frame, diebold_mariano_list_ARIMA)
  resultados_df_diebold_mariano_list_VAR <- do.call(rbind.data.frame, diebold_mariano_list_VAR)
  resultados_df_diebold_mariano_list_VAR_ARIMA <- do.call(rbind.data.frame, diebold_mariano_list_VAR_ARIMA)

  colnames(resultados_df_diebold_mariano_list_ARIMA) <- "p-value"
  colnames(resultados_df_diebold_mariano_list_VAR) <- "p-value"
  colnames(resultados_df_diebold_mariano_list_VAR_ARIMA) <- "p-value"

  if (plot) {
    ## MAE
    ggplot(melt(resultados_df[,1:3]), aes(value, fill = variable)) + geom_density(alpha = 0.2) + ggtitle("Distribution : MAE") + scale_x_continuous(limits = c(0, 2))
    ## MSE
    ggplot(melt(resultados_df_MSE[,1:3]), aes(value, fill = variable)) + geom_density(alpha = 0.2) + ggtitle("Distribution : MSE") + scale_x_continuous(limits = c(0, 2))
  }



  saveRDS(resultados_df, file = paste0("MAE/MAE & ",label_to_save,".rds"))

  saveRDS(resultados_df_MSE, file = paste0("MSE/MSE & ",label_to_save,".rds"))

  saveRDS(resultados_df_diebold_mariano_list_ARIMA, file = paste0("DM/DM ARIMA & ",label_to_save,".rds"))

  saveRDS(resultados_df_diebold_mariano_list_VAR, file = paste0("DM/DM VAR & ",label_to_save,".rds"))

  saveRDS(resultados_df_diebold_mariano_list_VAR_ARIMA, file = paste0("DM/DM VAR ARIMA & ",label_to_save,".rds"))

}