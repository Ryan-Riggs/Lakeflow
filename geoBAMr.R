# Sys.getenv("BINPREF")
# [1] ""
# 
# 
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# 
# Sys.which("make")
# ## "C:\\rtools40\\usr\\bin\\make.exe"
# 
# install.packages("jsonlite", type = "source")
# 
# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) dir.create(dotR)
# M <- file.path(dotR, "Makevars.win")
# if (!file.exists(M)) file.create(M)
# cat("\n CXX14FLAGS += -mtune=native -O3 -mmmx -msse -msse2 -msse3 -mssse3 -msse4.1 -msse4.2",
#     file = M, sep = "\n", append = FALSE)
# 
# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")
# 
# Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
# 
# 
# library(rstan)
# example(stan_model, package = "rstan", run.dontrun = TRUE)
# 
# 
# #First get devtools package
# if (!require("devtools")) {
#   install.packages("devtools")
#   library("devtools")
# }
# 
# #Then install from github
# devtools:: install_github("craigbrinkerhoff/geoBAMr", force=TRUE)





























data = readxl::read_xlsx("E:\\research\\RivLake\\mohave_pseudo_5yr_20210602.xlsx")
data$datetime = as.Date(data$datetime)
data = data[data$datetime>as.Date("2010-09-30"),]


##NEed location as rows, days as columns. 
test = bam_data(w = data$`w_in (m)`, s = data$`s_in (L/L)`, dA = data$`dA_in (m2)`, Qhat = data$`Q_in (m3/s)`)


#test = bam_data(w = data$`w_in (m)`, Qhat = data$`Q_in (m3/s)`)

##Repeated location due to bug when only using 1 dimension. 
win = rbind(data$w_ou,data$w_ou)
dA = rbind(data$dA_ou, data$dA_ou)
sin = rbind(data$s_ou, data$s_ou)
test = bam_data(w = win,s = sin, dA = dA, Qhat = data$Q_out)
bam_plot(test)
bam_plot(test) + scale_y_log10()
test_priors = bam_priors(bamdata = test)
sec_man = bam_estimate(bamdata = test, bampriors = bam_priors(test), variant = "manning", iter = 1000, cores = 5)
bam_hydrograph(fit = sec_man)

val = bam_validate(sec_man, data$Q_out)
bam_plot(val)
##in

qpred <- bam_qpred(fit = sec_man, chain = "all") %>% 
  dplyr::transmute(.data$time, qpred = mean)


win_in = rbind(data$w_in,data$w_in)
dA_in = rbind(data$dA_in, data$dA_in)
sin_in = rbind(data$s_in, data$s_in)
test_in = bam_data(w = win_in,s = sin_in, dA = dA_in, Qhat = data$Q_in)
sec_man_in = bam_estimate(bamdata = test_in, bampriors = bam_priors(test_in), variant = "manning", iter = 1000, cores = 5)
bam_hydrograph(fit = sec_man_in)
val_in = bam_validate(sec_man_in, data$Q_in)
bam_plot(val_in)

qpred_in <- bam_qpred(fit = sec_man_in, chain = "all") %>% 
  dplyr::transmute(.data$time, qpred = mean)

diff = qpred_in$qpred - qpred$qpred
diff_wE = diff + data$ET

plot(data$dQ, diff, xlim = c(-300, 400), ylim = c(-300, 400), xlab = "Gauge dQ", ylab = "BAM dQ",col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), pch = 19)
abline(0,1, lty = 2)
box(lwd = 2)
r2 = signif(summary(lm(diff~data$dQ))$r.squared, 3)
legend("topleft", legend = (paste0("R2 = ", r2)), bty = "n")
points(data$dQ, diff_wE, col = "red")
summary(lm(diff~data$dQ))
############################################################################################################################
##With Swot uncertainty. 
############################################################################################################################
library(geoBAMr)
data(data)
attach(data)

data = readxl::read_xlsx("E:\\research\\RivLake\\mohave_uncer_5yr_20210602.xlsx")
data$datetime = as.Date(data$datetime)
data = data[data$datetime>as.Date("2010-09-30"),]

data[data<0] = NA
data = data[complete.cases(data),]

##Repeated location due to bug when only using 1 dimension. 
win = rbind(data$w_ou_e,data$w_ou_e)
dA = rbind(data$dA_ou_e, data$dA_ou_e)
sin = rbind(data$s_ou_e, data$s_ou_e)
test = bam_data(w = win,s = sin, dA = dA, Qhat = data$Q_out)
bam_plot(test)
bam_plot(test) + scale_y_log10()
test_priors = bam_priors(bamdata = test)
sec_man_out_e = bam_estimate(bamdata = test, bampriors = bam_priors(test), variant = "manning",chains = 3, iter = 4000, cores = 5)
# bam_hydrograph(fit = sec_man)
# 
# val = bam_validate(sec_man, data$Q_out)
# bam_plot(val)
##in

qpred <- bam_qpred(fit = sec_man_out_e, chain = "all") %>% 
  dplyr::transmute(.data$time, qpred = mean)


win_in = rbind(data$w_in_e,data$w_in_e)
dA_in = rbind(data$dA_in_e, data$dA_in_e)
sin_in = rbind(data$s_in_e, data$s_in_e)
test_in = bam_data(w = win_in,s = sin_in, dA = dA_in, Qhat = data$Q_in)
sec_man_in = bam_estimate(bamdata = test_in, bampriors = bam_priors(test_in), variant = "manning", iter = 4000, cores = 5)
bam_hydrograph(fit = sec_man_in)
val_in = bam_validate(sec_man_in, data$Q_in)
bam_plot(val_in)

qpred_in <- bam_qpred(fit = sec_man_in, chain = "all") %>% 
  dplyr::transmute(.data$time, qpred = mean)

diff = qpred_in$qpred - qpred$qpred
diff_wE = diff + data$ET

plot(data$dQ, diff, xlim = c(-300, 400), ylim = c(-300, 400))
abline(0,1)
points(data$dQ, diff_wE, col = "red")




#####################################################################################################################
##Add in random noise.  
#####################################################################################################################
corruption_function = function(f){
  corrupt = rbinom(length(f), 1, .05)
  corrupt = as.logical(corrupt)
  noise = rnorm(sum(corrupt), mean(f, na.rm = TRUE),sd(f, na.rm= TRUE))
  f[corrupt] = f[corrupt]+noise
  return(f)
}


data = readxl::read_xlsx("E:\\research\\RivLake\\mohave_pseudo_5yr_20210602.xlsx")
data$datetime = as.Date(data$datetime)
data = data[data$datetime>as.Date("2010-09-30"),]

data$w_ou = corruption_function(data$w_ou)
data$s_ou = corruption_function(data$s_ou)
data$dA_ou = corruption_function(data$dA_ou)


##Repeated location due to bug when only using 1 dimension. 
win = rbind(data$w_ou,data$w_ou)
dA = rbind(data$dA_ou, data$dA_ou)
sin = rbind(data$s_ou, data$s_ou)
test = bam_data(w = win,s = sin, dA = dA, Qhat = data$Q_out)
bam_plot(test)
bam_plot(test) + scale_y_log10()
test_priors = bam_priors(bamdata = test)
sec_man = bam_estimate(bamdata = test, bampriors = bam_priors(test), variant = "manning", iter = 1000, cores = 5)
bam_hydrograph(fit = sec_man)

val = bam_validate(sec_man, data$Q_out)
bam_plot(val)
##in

qpred <- bam_qpred(fit = sec_man, chain = "all") %>% 
  dplyr::transmute(.data$time, qpred = mean)

data$w_in = corruption_function(data$w_in)
data$s_in = corruption_function(data$s_in)
data$dA_in = corruption_function(data$dA_in)

win_in = rbind(data$w_in,data$w_in)
dA_in = rbind(data$dA_in, data$dA_in)
sin_in = rbind(data$s_in, data$s_in)
test_in = bam_data(w = win_in,s = sin_in, dA = dA_in, Qhat = data$Q_in)
sec_man_in = bam_estimate(bamdata = test_in, bampriors = bam_priors(test_in), variant = "manning", iter = 1000, cores = 5)
bam_hydrograph(fit = sec_man_in)
val_in = bam_validate(sec_man_in, data$Q_in)
bam_plot(val_in)

qpred_in <- bam_qpred(fit = sec_man_in, chain = "all") %>% 
  dplyr::transmute(.data$time, qpred = mean)

diff = qpred_in$qpred - qpred$qpred
diff_wE = diff + data$ET

plot(data$dQ, diff, xlim = c(-300, 400), ylim = c(-300, 400), xlab = "Gauge dQ", ylab = "BAM dQ")
abline(0,1)
r2 = signif(summary(lm(diff~data$dQ))$r.squared, 3)
legend("topleft", legend = (paste0("R2 = ", r2)), bty = "n")
points(data$dQ, diff_wE, col = "red")
summary(lm(diff~data$dQ))































