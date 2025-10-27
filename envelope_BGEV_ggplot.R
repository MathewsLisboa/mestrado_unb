
envelope_BGEV_ggplot <- function(y, X, theta, main.title = '', faixa.fixed = NULL, labels.fixed = NULL) {
  source("BGEV_MLE.r")
  B <- 100  # number of replicates
  kk1 <- ncol(X)
  n <- nrow(X)
  
  #***parameters for parametric bootstrap***#
  beta_p <- theta[1:kk1]
  sigma_p <- theta[kk1 + 1.0]
  delta_p <- theta[kk1 + 2.0]
  etahat_p <- X %*% beta_p
  m_p <- etahat_p
  #************************************************************#
  
  # Funções BGEV (mantidas do código original)
  pbgev <- function(y, m, sigma, delta) {
    mu <- m - (-sigma * log(-log(0.5)))^(1 / (delta + 1))
    T_val <- (y - mu) * (abs(y - mu)^delta)
    cdf <- pgev(T_val, loc = 0, scale = sigma, shape = 0)
    return(cdf)
  }
  
  qbgev <- function(p, m, sigma, delta) {
    mu <- m - (-sigma * log(-log(0.5)))^(1 / (delta + 1))
    quantile <- sign(qgevd(p, 0, sigma, 0)) * (abs(qgevd(p, 0, sigma, 0)))^(1 / (delta + 1)) + mu
    return(quantile)
  }
  
  rbgev <- function(n, m, sigma, delta) {
    U <- runif(n)
    rnumber <- qbgev(U, m, sigma, delta)
    return(rnumber)
  }
  
  #------------> quantile residuals for the observed sample<--------------#
  
  Menvelope_rq <- matrix(numeric(0),nrow=n,ncol=B)
  
  #------------> quantile residuals for the observed sample<--------------#
  
  rq <- qnorm(pbgev(y, m_p, sigma_p, delta_p))
  
  suppressWarnings(set.seed(c(1994,1991), kind="Marsaglia-Multicarry")) #to generate same plot
  
  for(j in 1:B){		
    ygen <- rbgev(n,  m_p, sigma_p, delta_p)
    fit_b <- MLE_BGEV(y=ygen, X=X, method="BFGS")
    rq_b <- qnorm(pbgev(ygen, X%*%as.numeric(fit_b$beta), fit_b$sigma, fit_b$delta))
    Menvelope_rq[,j] = rq_b
  }
  
  Menvelope_rq <- apply(Menvelope_rq,2,sort);  
  
  res_rq <-    rq;    
  res_min_rq  <-    as.numeric(t(apply(t(Menvelope_rq), 2,quantile, probs =0.05)));         
  res_mean_rq <-    as.numeric(t(apply(t(Menvelope_rq), 2,quantile, probs =0.5)));                              
  res_max_rq  <-    as.numeric(t(apply(t(Menvelope_rq), 2,quantile, probs =0.95)));           
  faixa <- range(res_rq,res_min_rq,res_max_rq)
  if(is.vector(faixa.fixed)) faixa <- faixa.fixed
  if(is.vector(labels.fixed)) labels <- labels.fixed
  
  
  df_envelope <- data.frame(
    obs_index = 1:n,
    res_rq = sort(rq),
    res_min_rq = res_min_rq,
    res_mean_rq = res_mean_rq,
    res_max_rq = res_max_rq,
    theoretical = qnorm(ppoints(n))
  )
  
  # Criar o gráfico com ggplot2
  p <- ggplot(df_envelope, aes(x = theoretical)) +
    geom_ribbon(aes(ymin = res_min_rq, ymax = res_max_rq), 
                fill = "lightblue", alpha = 0.5) +
    geom_line(aes(y = res_mean_rq), linetype = "dashed", size = 1.2, color = "darkblue") +
    geom_point(aes(y = res_rq), size = 2.5, shape = 16, color = "black") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dotted", size = 1) +
    labs(
      title = main.title,
      x = "Normal quantiles",
      y = "Residuals"
    ) +
    coord_cartesian(ylim = faixa) +
    theme_classic() +
    theme(
      text = element_text(size = 14),
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  # Adicionar labels se fornecidos
  
  
  return(p)
}

# Exemplo de uso (descomente para testar):
# p <- envelope_BGEV_ggplot(y = seus_dados, X = sua_matrix, theta = seus_parametros, main.title = "Envelope BGEV")
# print(p)