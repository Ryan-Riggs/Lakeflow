library(dplyr)
library(data.table)
################################################################
##Read in data. 
################################################################

########################################################################################################
##Errors vs synthetics. 
########################################################################################################
synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")



# plot(synthetic$w_in, data$w_in_e, xlim = c(80,150), ylim = c(80,150))
# abline(0,1)
# plot(synthetic$w_ou, data$w_ou_e, xlim = c(90,200), ylim = c(90,200))
# abline(0,1)
# plot(synthetic$dA_in, data$dA_in_e, xlim = c(300,700), ylim = c(300,700))
# abline(0,1)
# plot(synthetic$dA_ou, data$dA_ou_e, xlim = c(200,600), ylim = c(200,600))
# abline(0,1)
# plot(synthetic$s_in, data$s_in_e, xlim = c(4e-5,7e-5),ylim = c(4e-5,7e-5))
# abline(0,1)
# plot(synthetic$s_ou, data$s_ou_e, xlim = c(4e-5,7e-5), ylim = c(4e-5,7e-5))
# abline(0,1)
# summary(lm(data$s_in_e~synthetic$s_in))
# summary(lm(data$w_in_e~synthetic$w_in))
# summary(lm(data$dA_in_e~synthetic$dA_in))
# 
# plot(data$datetime, data$s_in_e, type = "l")
# lines(synthetic$datetime, synthetic$s_in, col = "red")
# 
# plot(data$datetime, data$w_in_e, type = "l")
# lines(synthetic$datetime, synthetic$w_in, col = "red")
# 
# plot(data$datetime, data$dA_in_e, type = "l")
# lines(synthetic$datetime, synthetic$dA_in, col = "red")
# 
# plot(data$w_ou_e, data$s_ou_e)
# plot(synthetic$w_ou, synthetic$s_ou, col = "red")
############################################################################################################
##Read in data, create own slope errors and filter out any negatives. 
############################################################################################################
#synthetic = readxl::read_xlsx("E:\\research\\RivLake\\mohave_pseudo_5yr_20210602.xlsx")
#data = readxl::read_xlsx("E:\\research\\RivLake\\mohave_uncer_5yr_20210602.xlsx")

synthetic$slopes_in_e1 = rnorm(nrow(synthetic), 0, 1.7e-5)#rnorm(nrow(synthetic), mean(synthetic$s_in)/10, sd(synthetic$s_in)/10)
smp = sample(1:nrow(synthetic), nrow(synthetic)*.5)
synthetic$slopes_in_e = synthetic$s_in+synthetic$slopes_in_e1
synthetic$slopes_in_e[smp] = synthetic$slopes_in_e[smp]-(synthetic$slopes_in_e1[smp]*2)
synthetic$slopes_ou_e1 = rnorm(nrow(synthetic), 0, 1.7e-5)#rnorm(nrow(synthetic), mean(synthetic$s_ou)/10, sd(synthetic$s_ou)/10)
smp1 = sample(1:nrow(synthetic), nrow(synthetic)*.5)
synthetic$slopes_ou_e = synthetic$s_ou+synthetic$slopes_ou_e1
synthetic$slopes_ou_e[smp1] = synthetic$slopes_ou_e[smp1]-(synthetic$slopes_ou_e1[smp1]*2)

plot(synthetic$datetime, synthetic$slopes_in_e, type = "l")
lines(synthetic$datetime, synthetic$s_in, col = "red")
plot(synthetic$s_in, synthetic$slopes_in_e)
abline(0,1)

data$s_in_e = synthetic$slopes_in_e
data$s_ou_e = synthetic$slopes_ou_e


##Add in own dA in and out errors. 
synthetic$dA_in_e1 = rnorm(nrow(synthetic), 0, synthetic$dA_in*.15)#rnorm(nrow(synthetic), mean(synthetic$dA_in)/10, sd(synthetic$dA_in)/10)
synthetic$dA_in_e = synthetic$dA_in+synthetic$dA_in_e1
synthetic$dA_in_e[smp] = synthetic$dA_in_e[smp]-(synthetic$dA_in_e1[smp]*2)

synthetic$dA_ou_e1 = rnorm(nrow(synthetic), 0, synthetic$dA_ou*.15)#rnorm(nrow(synthetic), mean(synthetic$dA_ou)/10, sd(synthetic$dA_ou)/10)
synthetic$dA_ou_e = synthetic$dA_ou+synthetic$dA_ou_e1
synthetic$dA_ou_e[smp1] = synthetic$dA_ou_e[smp1]-(synthetic$dA_ou_e1[smp1]*2)


data$dA_in_e = synthetic$dA_in_e
data$dA_ou_e = synthetic$dA_ou_e

##Add in own w in and out errors. 
synthetic$w_in_e1 = rnorm(nrow(synthetic), 0, synthetic$w_in*.15)#rnorm(nrow(synthetic), mean(synthetic$w_in)/10, sd(synthetic$w_in)/10)
synthetic$w_in_e = synthetic$w_in+synthetic$w_in_e1
synthetic$w_in_e[smp] = synthetic$w_in_e[smp]-(synthetic$w_in_e1[smp]*2)

synthetic$w_ou_e1 = rnorm(nrow(synthetic), 0, synthetic$w_ou*.15)#rnorm(nrow(synthetic), mean(synthetic$w_ou)/10, sd(synthetic$w_ou)/10)
synthetic$w_ou_e = synthetic$w_ou+synthetic$w_ou_e1
synthetic$w_ou_e[smp1] = synthetic$w_ou_e[smp1]-(synthetic$w_ou_e1[smp1]*2)


data$w_in_e = synthetic$w_in_e
data$w_ou_e = synthetic$w_ou_e



data = data[data$s_in_e>0&data$s_ou_e>0,]
#data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
###############################################################################
###Join in lateral flows from previous dataset. 
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
###############################################################################
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
data = group(data, 180, method = "n_rand")


ryan = list()
nseIn = list()
nseOu = list()
library(doParallel)
stopImplicitCluster()
registerDoParallel(6)
r = foreach(j = 1:72, .combine = rbind)%dopar%{
library(data.table)
library(dplyr)
library(R.utils)
library(hydroGOF)
library(groupdata2)
'%!in%' <- function(x,y)!('%in%'(x,y))
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
# synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
# colnames(synthetic) = colnames(data)
# data = synthetic
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
data = group(data, 10, method = "greedy")
data$year = year(data$datetime)
data$month = month(data$datetime)
inflowAvg = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
outflowAvg = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
inflowMonth = aggregate(data$Q_in, by = list(data$month), FUN = mean)$x
outflowMonth = aggregate(data$Q_out, by = list(data$month), FUN = mean)$x
valid = data

#set.seed(j)
#data = sample_n(data, 10)
data = data[data$.groups==j,]
valid = data#valid[valid$datetime%!in%data$datetime,]
#valid = valid[valid$.groups==j+1,]
sout = sqrt(data$s_ou_e)
sin = sqrt(data$s_in_e)
wout = data$w_ou_e^(-2/3)
win = data$w_in_e^(-2/3)
d_x_area_ou = data$dA_ou_e
d_x_area_in = data$dA_in_e
delta_s_q = data$dV#/86400#data$dQ
mean_dv = mean(abs(delta_s_q))


# mean_dv = mean(abs(data$dQ), na.rm = TRUE)
# start = Sys.time()
# ##Preprocess parts of the eqn to improve efficiency. 
# sin = sqrt(data$s_in)
# sout = sqrt(data$s_ou)
# win = data$w_in^(-2/3)
# wout = data$w_ou^(-2/3)

t = 1000
total = t
rows = 0
outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 8))
colnames(outputs)= c("vals","ni", "ai","no","ao", "inflow", "outflow", "comb")
outputs$ni = c(.02,.05)#c(.02, .05)
outputs$no = c(.02,.05)#c(.02, .05)
outputs$ai = c(100,400)#c(100,300)
outputs$ao = c(100,500)#c(200,500)#c(200,2000)
################################################################################################################################
##Manning's equation. 
################################################################################################################################
eqn1 = function(n, a, da, w, s){
  flow = (n^-1)*((a+da)^(5/3))*w*s
  return(flow)
}
################################################################################################################################
##Lakeflow equation. 
#Randomly estimate non SWOT observable parameters (ni, no, ai, ao) and keep a set of numbers (rw),
#that produce a mean d_s_q within a percentage (x) of the mean observed d_s_q.
################################################################################################################################
lakeflow = function(x, rw){
  t = rw
  outputs = get("outputs", environment())
  outputs = outputs#[1:t,]
  total = t
  rows = 0
  while(rows<total){
    ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
    no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
    ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
    ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
    inflow = eqn1(ni, ai, d_x_area_in, win, sin)
    outflow = eqn1(no, ao, d_x_area_ou, wout, sout)
    diff = inflow - outflow - data$ET# + data$sum
    #vals = delta_s_q - diff
    vals = diff-delta_s_q
    vals = unlist(vals)
    #if(mean(abs(vals[delta_s_q!=0]/abs(delta_s_q[delta_s_q!=0])))<x){
    #if(mean(abs(vals))/mean_dv<x){
    d = vals/abs(data$dV)
    rmse = sqrt(median(vals^2))
    if(rmse<x){
      #print(rows)
      outputs$vals[rows+1] <- rmse#mean(abs(vals))/mean_dv
      outputs$ni[rows+1] = ni
      outputs$ai[rows+1] = ai
      outputs$no[rows+1] = no
      outputs$ao[rows+1] = ao
      rows = rows +1
    } else{next}
  }
  return(assign("outputs", as.data.frame(outputs), envir = .GlobalEnv))
}






lakeflow = function(x, rw,y){
  t = rw
  outputs = get("outputs", environment())
  outputs = outputs#[1:t,]
  total = t
  rows = 0
  while(rows<total){
    ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
    no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
    ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
    ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
    inflow = eqn1(ni, ai, d_x_area_in, win, sin)
    outflow = eqn1(no, ao, d_x_area_ou, wout, sout)
    diff = inflow - outflow - data$ET + data$sum
    #vals = delta_s_q - diff
    vals = diff-delta_s_q
    vals = unlist(vals)
    #if(mean(abs(vals[delta_s_q!=0]/abs(delta_s_q[delta_s_q!=0])))<x){
    #if(mean(abs(vals))/mean_dv<x){
    d = vals/abs(data$dV)
    rmse = sqrt(mean(vals^2))
    months = data$month
    inf = mean(abs(inflow-inflowMonth[months])/inflowMonth[months])
    ouf = mean(abs(outflow-outflowMonth[months])/outflowMonth[months])
    
    rdiff = mean(abs(vals/delta_s_q))
    inf = mean(abs(inflow-inflowAvg))/inflowAvg
    ouf = mean(abs(outflow-outflowAvg))/outflowAvg
    r = cor(diff, delta_s_q, method = "pearson")
    if(rmse*inf*ouf<x){#rmse<x&mean(abs(inflow-inflowAvg))/inflowAvg<y&mean(abs(outflow-outflowAvg))/outflowAvg<y){
      #print(rows)
      outputs$vals[rows+1] <-rdiff#rmse#mean(abs(vals))/mean_dv
      outputs$ni[rows+1] = ni
      outputs$ai[rows+1] = ai
      outputs$no[rows+1] = no
      outputs$ao[rows+1] = ao
      outputs$inflow[rows+1] = mean(abs(inflow-inflowAvg))/inflowAvg
      outputs$outflow[rows+1] = mean(abs(outflow-outflowAvg))/outflowAvg
      outputs$comb[rows+1] = rmse*inf*ouf#outputs$inflow[rows+1]*outputs$outflow[rows+1]*outputs$vals[rows+1]
      rows = rows +1
    } else{next}
  }
  return(assign("outputs", as.data.frame(outputs), envir = .GlobalEnv))
}

################################################################################################################################
##Run lakeflow on a set of values where:
#values:x parameter. 
#rws: rw parameter. 
#As lakeflow runs, the random values of ni, no, ai, ao are created from the min/max of the previous iteration.  
#Processing time: ~0.5-1.5 minutes. 
################################################################################################################################
values = rep(100, 100)#rev(seq(10, 1000, 5))
rws = rep(t, length(values))#c(rep(t, length(values)-1), 50)
yvalues = rev(seq(.1, 10, length.out = length(values)))

values = rev(seq(.01, 12, .1))
rws = rep(t, length(values))
start = Sys.time()
all = list()
for(i in 1:length(values)){
  print(round(i/length(values)*100))
  outputs = try(withTimeout(lakeflow(values[i], rws[i]), timeout = 20))
  if(is.error(outputs)){
    break
  }
  outputs = as.data.frame(outputs)
  assign("outputs", as.data.frame(outputs), envir = .GlobalEnv)
  all[[i]] = outputs
}
end = Sys.time()
end-start
allVals = rbindlist(all)

# m = apply(allVals, 2, median)
# (m[2:5] - c(0.035,164,0.035, 299))/c(0.035, 164,0.035, 299)*100


v = allVals[order(allVals$comb)]
outputs = v[1:1000,]
allVals = outputs


# 
# val = quantile(outputs$vals, .5)
# yval = min(c(quantile(outputs$inflow, .5), quantile(outputs$outflow, .5)))
# 
# v = rep(val, 10)
# rw = rep(1000, 10)
# y = rep(yval, 10)
# 
# l = lapply(v, lakeflow, rw = rw, y = y)
# 
# 
# 
# 
# allVals = rbindlist(l)
mn = allVals[allVals$comb==min(allVals$comb),]
m = unlist(mn[1,])
  

#m = apply(allVals, 2, median)
sdVals = apply(allVals, 2, sd)
uncIn = sqrt((((sdVals[2]/m[2])*1)^2)+(((sdVals[3]/m[3])*(5/3))^2))#+(((1.7e-5/sin)*.5)^2)+(((d_x_area_in*.15/d_x_area_in)*(5/3))^2)+(((win*.15/win)*(2/3))^2))
uncOu = sqrt((((sdVals[4]/m[4])*1)^2)+(((sdVals[5]/m[5])*(5/3))^2))#+(((1.7e-5/sout)*.5)^2)+(((d_x_area_ou*.15/d_x_area_ou)*(5/3))^2)+(((wout*.15/wout)*(2/3))^2))

######Validate the inflow. 
# sout = sqrt(valid$s_ou_e)
# sin = sqrt(valid$s_in_e)
# wout = valid$w_ou_e^(-2/3)
# win = valid$w_in_e^(-2/3)
# d_x_area_ou = valid$dA_ou_e
# d_x_area_in = valid$dA_in_e

inflow = eqn1(m[2], m[3], d_x_area_in, win, sin)
outflow = eqn1(m[4], m[5], d_x_area_ou, wout, sout)

uncIn = uncIn * inflow
uncOu = uncOu * outflow

# uncIn = sqrt((((1.7e-5/sin)*.5)^2)+(((d_x_area_in*.15/d_x_area_in)*(5/3))^2)+(((win*.15/win)*(2/3))^2))
# uncIn = uncIn * inflow



plot(valid$datetime,valid$Q_out, type = "l")
valid$model = outflow
#sm = sample(nrow(valid), 25)
arrows(valid$datetime, valid$model-uncOu,valid$datetime, valid$model+uncOu,col = "red", angle = 0)
points(valid$datetime,valid$model, col = "blue", pch = 16)
#errorIn = valid$Q_in - inflow
#plot(uncIn,abs(errorIn), log = "xy", xlim = c(5, 200), ylim = c(5, 200))

#plot(valid$datetime,valid$Q_in, type = "l")
#valid$model = inflow
#sm = sample(nrow(valid), 25)
#arrows(valid$datetime, valid$model-uncIn,valid$datetime, valid$model+uncIn,col = "red", angle = 0)
#points(valid$datetime,valid$model, col = "blue", pch = 16)


#nseIn[[j]] = NSE(inflow, valid$Q_in)
#nseOu[[j]] = NSE(outflow, valid$Q_out)
#ryan[[j]] = c(m, NSE(inflow, valid$Q_in), NSE(outflow, valid$Q_out))
ryan[[j]] = cbind(inflow, valid$Q_in, outflow, valid$Q_out)
}
stopImplicitCluster()
df = as.data.frame(r)
NSE(df$inflow, df$V2)
NSE(df$outflow, df$V4)

plot(df$V2, type = "l")
points(df$inflow, col = "red")
plot(df$V4, type = "l")
points(df$outflow, col = "red")
plot(df$outflow, df$V4, xlim = c(100,700), ylim = c(100,700))
abline(0,1)





apply(df, 2, median)

lm(abs(errorIn)~uncIn+0)




processing = function(v, r){
  outputs = try(withTimeout(lakeflow(v, r), timeout = 30))
  outputs = as.data.frame(outputs)
  assign("outputs", as.data.frame(outputs), envir = .GlobalEnv)
  return(outputs)
}



all = rbindlist(ryan)
apply(all[all$vals<1.5], 2, hist)
apply(all, 2, median)



##New
ni = 0.035
no = 0.035
ai = 164.85
ao = 299.64


##old
ni = .035
no = .011
ao = 999
ai = 462


inflow = eqn1(ni, ai, d_x_area_in, win, sin)
outflow = eqn1(no, ao, d_x_area_ou, wout, sout)
diff = inflow - outflow# - data$ET + data$sum
vals = delta_s_q - diff
vals = unlist(vals)

#mean(abs(vals))/mean_dv
mean(abs(vals))/mean_dv


library(data.table)
allVals = rbindlist(all)
allFilt = allVals[allVals$vals<1,]
# plot(allFilt$vals, allFilt$no)
# plot(allFilt$vals, allFilt$ao)
# plot(allFilt$vals, allFilt$ni)
# plot(allFilt$vals, allFilt$ai)

mn = allFilt[allFilt$vals==min(allFilt$vals),]
((mn[1,2:5]-c(.035,462,.011,999))/c(.035,462,.011,999))*100

lm(allFilt$vals~allFilt$ni)
lm(allFilt$vals~allFilt$no)
lm(allFilt$vals~allFilt$ai)


plot(delta_s_q, inflow-outflow-data$ET+data$sum)




library(geoBAMr)
data(data)
attach(data)
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data$year = year(data$datetime)
input = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]

data[data<0] = NA
data = data[complete.cases(data),]

##Repeated location due to bug when only using 1 dimension. 
win = rbind(data$w_ou_e,data$w_ou_e)
dA = rbind(data$dA_ou_e, data$dA_ou_e)
sin = rbind(data$s_ou_e, data$s_ou_e)
test = bam_data(w = win,s = sin, dA = dA, Qhat = input)
bam_plot(test)
bam_plot(test) + scale_y_log10()
test_priors = bam_priors(bamdata = test)
sec_man_out_e = bam_estimate(bamdata = test, bampriors = bam_priors(test), variant = "manning",chains = 3, iter = 4000, cores = 5)


qpred <- bam_qpred(fit = sec_man_out_e, chain = "all") %>% 
  dplyr::transmute(.data$time, qpred = mean)


NSE(qpred$qpred, data$Q_out)
plot(data$Q_out, type = "l")
points(qpred$qpred, col = "red")


library(rBayesianOptimization)
library(groupdata2)
library(Rcpp)












data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
# synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
# colnames(synthetic) = colnames(data)
# data = synthetic
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
data = group(data, 10, method = "greedy")
data$year = year(data$datetime)
data$month = month(data$datetime)
inflowAvg = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
outflowAvg = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
inflowMonth = aggregate(data$Q_in, by = list(data$month), FUN = mean)$x
outflowMonth = aggregate(data$Q_out, by = list(data$month), FUN = mean)$x
valid = data

#set.seed(j)
#data = sample_n(data, 10)
data = data[data$.groups==j,]
valid = data#valid[valid$datetime%!in%data$datetime,]
#valid = valid[valid$.groups==j+1,]
sout = sqrt(data$s_ou_e)
sin = sqrt(data$s_in_e)
wout = data$w_ou_e^(-2/3)
win = data$w_in_e^(-2/3)
d_x_area_ou = data$dA_ou_e
d_x_area_in = data$dA_in_e
delta_s_q = data$dV#/86400#data$dQ
mean_dv = mean(abs(delta_s_q))
fn = function(ni, no, ai, ao){
  list(Score = delta_s_q[1] - (eqn1(ni, ai, d_x_area_in[1], win[1], sin[1]) - eqn1(no[1], ao[1], d_x_area_ou[1], wout[1], sout[1]) - data$ET[1] + data$sum[1]), 
       Pred = c(.035,.035,200,300))
}

opt = BayesianOptimization(
fn, 
bounds = list(ni = c(.02,.05), no = c(.02,.05), ai = c(100,500), ao = c(100,500)) ,
init_points = 4, n_iter = 1,acq = "poi", eps = 0.5,
verbose = TRUE
)

library(hydroGOF)
NSE(eqn1(opt$Best_Par[1], opt$Best_Par[3], d_x_area_in, win, sin), data$Q_in)
NSE(eqn1(opt$Best_Par[2], opt$Best_Par[4], d_x_area_ou, wout, sout), data$Q_out)

ryan = list()
library(doParallel)
stopImplicitCluster()
registerDoParallel(6)
r = foreach(i = 1:10, .combine = rbind)%dopar%{
  library(rBayesianOptimization)
  library(data.table)
  library(dplyr)
  library(lubridate)
  data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
  synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
  colnames(synthetic) = colnames(data)
  data = synthetic
  lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
  data$Date = as.Date(data$datetime)
  lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
  comb = inner_join(data, lateral)
  data = comb
  data = na.omit(data)
  data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
  sout = sqrt(data$s_ou_e)
  sin = sqrt(data$s_in_e)
  wout = data$w_ou_e^(-2/3)
  win = data$w_in_e^(-2/3)
  d_x_area_ou = data$dA_ou_e
  d_x_area_in = data$dA_in_e
  delta_s_q = data$dV#/86400#data$dQ
  data$year = year(data$datetime)
  inflowAvg = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
  outflowAvg = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
  print(i)
  fn = function(ni, no, ai, ao){
    inflow = abs((inflowAvg - eqn1(ni, ai, d_x_area_in[i], win[i], sin[i])))/inflowAvg
    outflow = abs((outflowAvg - eqn1(no, ao, d_x_area_ou[i], wout[i], sout[i])))/outflowAvg
    
    #y = delta_s_q[i] - inflow - outflow - data$ET[i]+data$sum[i]
    
    y = (delta_s_q[i] - (eqn1(ni, ai, d_x_area_in[i], win[i], sin[i]) - eqn1(no, ao, d_x_area_ou[i], wout[i], sout[i]) - data$ET[i] + data$sum[i]))
    a = inflow * outflow * y
    
    list(Score = a, 
         Pred = c(.035,.035,200,300))
  }
  
  opt = BayesianOptimization(
    fn, 
    bounds = list(ni = c(.02,.05), no = c(.02,.05), ai = c(100,500), ao = c(100,600)) ,
    init_points = 10, n_iter = 1,acq = "ucb", kappa = 2.5, eps = 0.0,
    verbose = TRUE
  )
  df = opt$History
  df$min = abs(df$Value)
  m = df[df$min == min(df$min),]
  
ryan[[i]]  = opt$Best_Par
}
stopImplicitCluster()
df = as.data.frame(r)
m = apply(df, 2, min)


NSE(eqn1(m[2], m[4], d_x_area_in, win, sin), data$Q_in)
NSE(eqn1(m[3], m[5], d_x_area_ou, wout, sout), data$Q_out)


#############################################################################
##Parbayesian optimization. 
###############################################################################
library(ParBayesianOptimization)
library(rBayesianOptimization)
library(data.table)
library(dplyr)
library(lubridate)
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
# synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
# colnames(synthetic) = colnames(data)
# data = synthetic
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
sout = sqrt(data$s_ou_e)
sin = sqrt(data$s_in_e)
wout = data$w_ou_e^(-2/3)
win = data$w_in_e^(-2/3)
d_x_area_ou = data$dA_ou_e
d_x_area_in = data$dA_in_e
delta_s_q = data$dV#/86400#data$dQ
data$year = year(data$datetime)
inflowAvg = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
outflowAvg = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
print(i)



fn = function(ni, no, ai, ao){
  inflow1 = abs((inflowAvg - eqn1(ni, ai, d_x_area_in, win, sin)))/inflowAvg
  outflow1 = abs((outflowAvg - eqn1(no, ao, d_x_area_ou, wout, sout)))/outflowAvg
  
  #y = delta_s_q[i] - inflow - outflow - data$ET[i]+data$sum[i]
  
  #y = (delta_s_q[i] - (eqn1(ni, ai, d_x_area_in[i], win[i], sin[i]) - eqn1(no, ao, d_x_area_ou[i], wout[i], sout[i]) - data$ET[i] + data$sum[i]))
  inflow = (ni^-1)*((ai+d_x_area_in)^(5/3))*win*sin
  outflow = (no^-1)*((ao+d_x_area_ou)^(5/3))*wout*sout
  
  
  a = (delta_s_q - (inflow-outflow-data$ET+data$sum))
  b = mean(abs(a))
  c = b * mean(inflow1)*mean(outflow1)
  
  list(Score = -1*abs(c))
}
bounds = list(ni = c(.02,.05), no = c(.02,.05), ai = c(100,500), ao = c(100,600))
set.seed(6)
initGrid <- data.frame(ni=c(.02,.025, .035,.04, .05), no= c(.02,.025, .035,.04, .05), ai = c(100,200, 300,400, 500), ao = c(100,200, 300,400, 500))


ptObjSimp <- bayesOpt(
  FUN = fn
  , bounds = bounds
  #, initGrid = initGrid
  , iters.n = 5
  , initPoints = 10000
)

best = getBestPars(ptObjSimp)
plot(ptObjSimp)

delta_s_q[1] - (((.035^-1)*((164+d_x_area_in[1])^(5/3))*win[1]*sin[1]) - ((.035^-1)*((299+d_x_area_ou[1])^(5/3))*wout[1]*sout[1]))

NSE(eqn1(best$ni, best$ai, d_x_area_in, win, sin), data$Q_in)
NSE(eqn1(best$no, best$ao, d_x_area_ou, wout, sout), data$Q_out)
NSE(eqn1(best$ni, best$ai, d_x_area_in, win, sin)-eqn1(best$no, best$ao, d_x_area_ou, wout, sout)-data$ET+data$sum,data$dV)

plot(data$Q_out, type = "l")
points(eqn1(best$no, best$ao, d_x_area_ou, wout, sout), pch = 19, col = "blue")














Folds <- list(
  Fold1 = as.integer(seq(1,nrow(data),by = 3))
  , Fold2 = as.integer(seq(2,nrow(data),by = 3))
  , Fold3 = as.integer(seq(3,nrow(data),by = 3))
)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
clusterExport(cl)
clusterEvalQ(cl, c('Folds', 'inflowAvg', 'outflowAvg'),expr= {
  library(ParBayesianOptimization)
  library(rBayesianOptimization)
  library(data.table)
  library(dplyr)
  library(lubridate)
  data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
  # synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
  # colnames(synthetic) = colnames(data)
  # data = synthetic
  lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
  data$Date = as.Date(data$datetime)
  lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
  comb = inner_join(data, lateral)
  data = comb
  data = na.omit(data)
  data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
  sout = sqrt(data$s_ou_e)
  sin = sqrt(data$s_in_e)
  wout = data$w_ou_e^(-2/3)
  win = data$w_in_e^(-2/3)
  d_x_area_ou = data$dA_ou_e
  d_x_area_in = data$dA_in_e
  delta_s_q = data$dV#/86400#data$dQ
  data$year = year(data$datetime)
  inflowAvg = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
  outflowAvg = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
  eqn1 = function(n, a, da, w, s){
    flow = (n^-1)*((a+da)^(5/3))*w*s
    return(flow)
  }
  fn = function(ni, no, ai, ao){
    inflow1 = abs((inflowAvg - eqn1(ni, ai, as.numeric(d_x_area_in), as.numeric(win), as.numeric(sin))))/as.numeric(inflowAvg)
    outflow1 = abs((outflowAvg - eqn1(no, ao, as.numeric(d_x_area_ou), as.numeric(wout), as.numeric(sout))))/as.numeric(outflowAvg)
    
    #y = delta_s_q[i] - inflow - outflow - data$ET[i]+data$sum[i]
    
    #y = (delta_s_q[i] - (eqn1(ni, ai, d_x_area_in[i], win[i], sin[i]) - eqn1(no, ao, d_x_area_ou[i], wout[i], sout[i]) - data$ET[i] + data$sum[i]))
    inflow = (ni^-1)*((ai+as.numeric(d_x_area_in))^(5/3))*as.numeric(win)*as.numeric(sin)
    outflow = (no^-1)*((ao+as.numeric(d_x_area_ou))^(5/3))*as.numeric(wout)*as.numeric(sout)
    
    
    a = (as.numeric(delta_s_q) - (as.numeric(inflow)-as.numeric(outflow)-as.numeric(data$ET)+as.numeric(data$sum)))
    b = mean(abs(as.numeric(a)))
    c = as.numeric(b) * mean(as.numeric(inflow1))*mean(as.numeric(outflow1))
    
    list(Score = -1*abs(as.numeric(c)))
  }
  bounds = list(ni = c(.02,.05), no = c(.02,.05), ai = c(100,500), ao = c(100,600))
 
})

tWithPar <- system.time(
  optObj <- bayesOpt(
    FUN = fn
    , bounds = bounds
    #, initGrid = initGrid
    , iters.n = 4
    , initPoints = 5
    , iters.k = 4
    , parallel = TRUE
  )
)
stopCluster(cl)
registerDoSEQ()

###########################################################################################################################
##Genetic algorithm. 
###########################################################################################################################

source("E:/research/2019_08_30_rivObs/git/src/Error_stats_functions.R")
library(dplyr)
library(data.table)
library(GA)
library(lubridate)
library(groupdata2)
library(doParallel)
ryan = list()
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
# synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
# colnames(synthetic) = colnames(data)
# data = synthetic
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5&data$dV!=0,]
data$year = year(data$datetime)
toi_df = data
inflowAvg = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
outflowAvg = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
period = seq.Date(min(data$Date), max(data$Date), 7)
data = data[data$Date%in%period,]
data = group(data,610, method = "greedy")
df = data
#for(i in 1:length(unique(df$.groups))) {
data = df[df$.groups == i,]
sout = sqrt(data$s_ou_e)
sin = sqrt(data$s_in_e)
wout = data$w_ou_e^(-2/3)
win = data$w_in_e^(-2/3)
d_x_area_ou = data$dA_ou_e
d_x_area_in = data$dA_in_e
delta_s_q = data$dV#/86400#data$dQ
fun = function(ni, no, ai, ao){
  # inflow1 = mean(data$Q_in) - mean(eqn1(signif(ni,2), round(ai), d_x_area_in, win, sin))
  # outflow1 = mean(data$Q_out) - mean(eqn1(signif(no,2), round(ao), d_x_area_ou, wout, sout))
  
  inflow = (signif(ni,2)^-1)*((round(ai)+d_x_area_in)^(5/3))*win*sin
  outflow = (signif(no,2)^-1)*((round(ao)+d_x_area_ou)^(5/3))*wout*sout
  
  inflow = inflow +data$sum
  outflow = outflow + data$ET
  
  sv = data$V[1]
  
  a = (delta_s_q-(inflow-outflow))
  a = mean(abs(a)^2)
  # c = a*inflow1*outflow1
  # c = mean(abs(delta_s_q)) - inflow1-outflow1-mean(data$ET)+mean(data$sum)
  Score = -1*a#*inflow1*outflow1#-1*mean(abs(c))
}
postfit <- function(object, ...)
{
  pop <- object@population
  # update info
  if(!exists(".pop", envir = globalenv()))
    assign(".pop", NULL, envir = globalenv())
  .pop <- get(".pop", envir = globalenv())
  assign(".pop", append(.pop, list(pop)), envir = globalenv()) 
  # output the input ga object (this is needed!!)
  object 
}
t = ga(type = "real-valued",
       fitness =function(x) fun(x[1], x[2], x[3], x[4]),
       lower = c(.02,.02,100,200),
       upper = c(.06,.06,300,500),
       popSize= 10000,
       maxiter =50,
       run = 10,
       parallel = TRUE,
       postFitness = postfit,
       elitism = round(nrow(data)*.20))
summary(t)
print(i)
ryan1 = as.data.frame(t@population)
ryan1 = lapply(.pop, as.data.frame)
ryan1 = rbindlist(ryan1)
df1 = apply(ryan1, 2, mean)
ryan[[i]] = cbind(df1[1], df1[3], df1[2], df1[4])
#ryan[[i]] = cbind(eqn1(df1[1], df1[3], d_x_area_in, win, sin), data$Q_in,eqn1(df1[2], df1[4], d_x_area_ou, wout, sout), data$Q_out)
}
ryan = as.data.frame(t@population)
ryan = lapply(.pop, as.data.frame)
ryan = rbindlist(ryan)

outputs = apply(ryan, 2, mean)
NSE(eqn1(outputs[1], outputs[3], d_x_area_in, win, sin), data$Q_in)
NSE(eqn1(outputs[2], outputs[4], d_x_area_ou, wout, sout), data$Q_out)
RRMSE(eqn1(outputs[1], outputs[3], d_x_area_in, win, sin), data$Q_in)
RRMSE(eqn1(outputs[2], outputs[4], d_x_area_ou, wout, sout), data$Q_out)
rBias(eqn1(outputs[1], outputs[3], d_x_area_in, win, sin), data$Q_in)
rBias(eqn1(outputs[2], outputs[4], d_x_area_ou, wout, sout), data$Q_out)


qin_est = eqn1(outputs[1], outputs[3], d_x_area_in, win, sin)
qou_est = eqn1(outputs[2], outputs[4], d_x_area_ou, wout, sout)
df = cbind(data$Q_in, qin_est,data$Q_out, qou_est, data$date)
df = as.data.frame(df)
colnames(df) = c("Q_in", "QM_in", "Q_out", "QM_out", "Date")
df$color = "black"
df$Date = as.Date(df$Date, format = "%m/%d/%Y")
df$Q_in = as.numeric(df$Q_in)
df$Q_out = as.numeric(df$Q_out)
modelDf = left_join(toi_df, df, by = "Date")
df = modelDf
df$Q_in = df$Q_in.x
df$Q_out = df$Q_out.x
###############################################################################################
##Figures

par(mfrow = c(1,2))
mn = min(cbind(df$Q_in, df$Q_out))
mx = max(cbind(df$Q_in, df$Q_out))
##Inflow. 
plot(df$Date, df$Q_in, type = "l",main="Inflow", col = "darkgray", xaxt="n", yaxt="n", bty="n", xlab="", ylab="", ylim=c(mn, mx))
box(lwd = 2)
mtext("Discharge (cms)", side = 2, line = 3, cex = 1.25)
mtext("Date", side = 1, line = 3, cex = 1.25)
spacing = c(100,300,500,700)
times = df$Date
ticks <- seq(times[1], times[length(times)], by = "months")
ticks = ticks[month(ticks)==1]
axis.Date(side = 1, labels = TRUE,at=ticks, cex.axis = 1, format = "%Y")#, at = c(0, 300, 600, 900))
axis(side = 2, las = 2, at = spacing, cex.axis = 1)
points(df$Date, df$QM_in, col = alpha("darkgreen", .7), pch = 19)
df$QM_in = as.numeric(df$QM_in)
df$Q_in = as.numeric(df$Q_in)
rb = paste0("rBias=",signif(rBias(df$QM_in, df$Q_in), 3), "%")
rr = paste0("RRMSE=",signif(RRMSE(df$QM_in, df$Q_in), 3), "%")
nse = paste0("NSE=",signif(NSE(df$QM_in, df$Q_in), 2))
legend("topleft", c(nse, rb, rr), bty ="n")

##Outflow. 
plot(df$Date, df$Q_out,main="Outflow", type = "l", col = "darkgray", xaxt="n", yaxt="n", bty="n", xlab="", ylab="",ylim=c(mn, mx))
box(lwd = 2)
mtext("Discharge (cms)", side = 2, line = 3, cex = 1.25)
mtext("Date", side = 1, line = 3, cex = 1.25)
spacing = c(100,300,500,700)
times = df$Date
ticks <- seq(times[1], times[length(times)], by = "months")
ticks = ticks[month(ticks)==1]
axis.Date(side = 1, labels = TRUE,at=ticks, cex.axis = 1, format = "%Y")#, at = c(0, 300, 600, 900))
axis(side = 2, las = 2, at = spacing, cex.axis = 1)
points(df$Date, df$QM_out, col = alpha("darkgreen", .75), pch = 19)
df$QM_out= as.numeric(df$QM_out)
df$Q_out =as.numeric(df$Q_out)
rb = paste0("rBias=",signif(rBias(df$QM_out, df$Q_out), 3), "%")
rr = paste0("RRMSE=",signif(RRMSE(df$QM_out, df$Q_out), 3), "%")
nse = paste0("NSE=",signif(NSE(df$QM_out, df$Q_out), 2))
legend("topleft", c(nse, rb, rr), bty ="n")

###############################################################################################
###KGE's of 0.5 for inflow and outflow. 
unc = lapply(.pop, as.data.frame)
unc = rbindlist(unc)

unc = as.data.frame(t@population)
sdVals = apply(unc, 2, sd)
uncIn = sqrt((((sdVals[1]/df[1])*1)^2)+(((sdVals[3]/df[3])*(5/3))^2))#+(((1.7e-5/sin)*.5)^2)+(((d_x_area_in*.15/d_x_area_in)*(5/3))^2)+(((win*.15/win)*(2/3))^2))
uncOu = sqrt((((sdVals[2]/df[2])*1)^2)+(((sdVals[4]/df[4])*(5/3))^2))#+(((1.7e-5/sout)*.5)^2)+(((d_x_area_ou*.15/d_x_area_ou)*(5/3))^2)+(((wout*.15/wout)*(2/3))^2))
uncOu = uncOu *eqn1(df[2], df[4], d_x_area_ou, wout, sout)


plot(data$datetime,data$Q_out, type = "l")
points(data$datetime,eqn1(df[2], df[4], d_x_area_ou, wout, sout), pch = 19, col = "blue")
arrows(data$datetime, eqn1(df[2], df[4], d_x_area_ou, wout, sout)-uncOu,data$datetime, eqn1(df[2], df[4], d_x_area_ou, wout, sout)+uncOu,col = "red", angle = 0)

error = eqn1(df[2], df[4], d_x_area_ou, wout, sout)-data$Q_out
plot(abs(uncOu), abs(error),log="xy", xlim = c(0.1,300),  ylim = c(0.1,300))






plot(data$V, data$Q_in)
points(data$V, eqn1(summary(t)$solution[1], summary(t)$solution[3], d_x_area_in, win, sin), pch = 19, col = "blue")


volume = data$V
swot = (data$Q_in-100)-(data$Q_out-100)
plot(volume, type = "l")
points(volume+swot, col = "red")




















dt = lapply(ryan, as.data.frame)
dt = rbindlist(dt)
plot(dt$V2, type = "l",xlab = "Time", ylab = "Inflow Q (cms)")
points(dt$V1, col = "blue", pch = 19)

plot(dt$V4, type = "l",xlab = "Time", ylab = "Outflow Q (cms)")
points(dt$V3, col = "blue", pch = 19)

NSE(dt$V1, dt$V2)
NSE(dt$V3, dt$V4)












c1 = function(ni,no, ai,ao){
  inf = (ni^-1)*((ai+d_x_area_in)^(5/3))*win*sin
  out = (no^-1)*((ao+d_x_area_ou)^(5/3))*wout*sout
  min(c(inf, out))
  }

fitness = function(x){
  x = fun(x[1], x[2], x[3], x[4])
  pen = sqrt(.Machine$double.xmax)
  penalty1 = max(c1(x[1], x[2], x[3], x[4]), 0)*pen
  x-penalty1
}
t = ga(type = "real-valued", 
       fitness =function(x) fitness(x[1], x[2], x[3], x[4]),
       lower = c(.02,.02,100,100), 
       upper = c(.05,.05,800,600),
       popSize= 10000,
       maxiter =20,
       run = 10,
       parallel = TRUE)
summary(t)








###################################################################
##Does performance improve from using the lake technique? 
###################################################################
##Inflow
########################################################################
ryan = list()
nseIn = list()
nseOu = list()
library(doParallel)
stopImplicitCluster()
registerDoParallel(6)
r = foreach(j = 1:70, .combine = rbind)%dopar%{
  library(data.table)
  library(dplyr)
  library(R.utils)
  library(hydroGOF)
  library(groupdata2)
  '%!in%' <- function(x,y)!('%in%'(x,y))
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data$year = year(data$datetime)
input = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
valid = data
data = group(data, 10, method = "greedy")
data = data[data$.groups==j,]
#valid = valid[valid$datetime%!in%data$datetime,]
valid = data

q = data$Q_in
w = data$w_in_e
e = data$w_in_e
s = data$s_in_e
a = data$s_in_e

mean_q = input
sin = sqrt(s)
win = w^(-2/3)
d_x_area_in = a

t = 1000
total = t
rows = 0
outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
colnames(outputs)= c("vals","ni", "ai","no","ao")
outputs$ni = c(.02,.08)#c(.02, .05)
outputs$ai = c(100,500)#c(100,300)
eqn1 = function(n, a, da, w, s){
  flow = (n^-1)*((a+da)^(5/3))*w*s
  return(flow)
}
lakeflow = function(x, rw){
  t = rw
  outputs = get("outputs", environment())
  outputs = outputs[1:t,]
  total = t
  rows = 0
  while(rows<total){
    ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
    ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
    inflow = eqn1(ni, ai, d_x_area_in, win, sin)
    vals = unlist(inflow)
    #if(mean(abs(vals[delta_s_q!=0]/abs(delta_s_q[delta_s_q!=0])))<x){
    if(mean(abs(vals))/mean_q<x){
      #print(rows)
      outputs$vals[rows+1] <- mean(abs(vals))/mean_dv
      outputs$ni[rows+1] = ni
      outputs$ai[rows+1] = ai
      rows = rows +1
    } else{next}
  }
  return(assign("outputs", as.data.frame(outputs), envir = .GlobalEnv))
}

values = rev(seq(.1, 2, .01))
rws = rep(t, length(values))#c(rep(t, length(values)-1), 50)

start = Sys.time()
all = list()
for(i in 1:length(values)){
  print(round(i/length(values)*100))
  outputs = try(withTimeout(lakeflow(values[i], rws[i]), timeout = 30))
  if(is.error(outputs)){
    break
  }
  #outputs = lakeflow(values[i], rws[i])
  outputs = as.data.frame(outputs)
  assign("outputs", as.data.frame(outputs), envir = .GlobalEnv)
  #all[[i]] = outputs[outputs$vals/mean_dv<1,]
  all[[i]] = outputs
}
allVals = rbindlist(all)
m = apply(allVals, 2, median)

ni = m[2]
ai = m[3]

nInflow = ni
aInflow = ai

sin = sqrt(valid$s_in_e)
win = valid$w_in_e^(-2/3)
d_x_area_in = valid$dA_in_e
nseIn = NSE(eqn1(ni, ai, d_x_area_in, win, sin), valid$Q_in)
inVals = eqn1(ni, ai, d_x_area_in, win, sin)
##########################################################################
##Outflow. 
##########################################################################
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data$year = year(data$datetime)
input = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
valid = data
data = group(data, 10, method = "greedy")
data = data[data$.groups==j,]
#valid = valid[valid$datetime%!in%data$datetime,]
valid = data

q = data$Q_ou
w = data$w_ou_e
e = data$w_ou_e
s = data$s_ou_e
a = data$s_ou_e

mean_q = input
sin = sqrt(s)
win = w^(-2/3)
d_x_area_in = a

t = 1000
total = t
rows = 0
outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
colnames(outputs)= c("vals","ni", "ai","no","ao")
outputs$ni = c(.02,.08)#c(.02, .05)
outputs$ai = c(100,1000)#c(100,300)
eqn1 = function(n, a, da, w, s){
  flow = (n^-1)*((a+da)^(5/3))*w*s
  return(flow)
}
lakeflow = function(x, rw){
  t = rw
  outputs = get("outputs", environment())
  outputs = outputs[1:t,]
  total = t
  rows = 0
  while(rows<total){
    ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
    ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
    inflow = eqn1(ni, ai, d_x_area_in, win, sin)
    vals = unlist(inflow)
    #if(mean(abs(vals[delta_s_q!=0]/abs(delta_s_q[delta_s_q!=0])))<x){
    if(mean(abs(vals))/mean_q<x){
      #print(rows)
      outputs$vals[rows+1] <- mean(abs(vals))/mean_dv
      outputs$ni[rows+1] = ni
      outputs$ai[rows+1] = ai
      rows = rows +1
    } else{next}
  }
  return(assign("outputs", as.data.frame(outputs), envir = .GlobalEnv))
}

values = rev(seq(.1, 2, .01))
rws = rep(t, length(values))#c(rep(t, length(values)-1), 50)

start = Sys.time()
all = list()
for(i in 1:length(values)){
  print(round(i/length(values)*100))
  outputs = try(withTimeout(lakeflow(values[i], rws[i]), timeout = 30))
  if(is.error(outputs)){
    break
  }
  #outputs = lakeflow(values[i], rws[i])
  outputs = as.data.frame(outputs)
  assign("outputs", as.data.frame(outputs), envir = .GlobalEnv)
  #all[[i]] = outputs[outputs$vals/mean_dv<1,]
  all[[i]] = outputs
}
allVals = rbindlist(all)
m = apply(allVals, 2, median)

ni = m[2]
ai = m[3]

nOutflow = ni
aOutflow = ai

sin = sqrt(valid$s_ou_e)
win = valid$w_ou_e^(-2/3)
d_x_area_in = valid$dA_ou_e
nseOu = NSE(eqn1(ni, ai, d_x_area_in, win, sin), valid$Q_in)
outVals = eqn1(ni, ai, d_x_area_in, win, sin)
###################################################################################
##LakeFlow.
###################################################################################
data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
lateral = fread("E:\\research\\RivLake\\lake_river_mass_balance_04152021\\data\\lateral_flows_GRADES\\Lake_Mohave.csv")
data$Date = as.Date(data$datetime)
lateral$Date = as.Date(lateral$date, format = "%m/%d/%Y")
comb = inner_join(data, lateral)
data = comb
data = na.omit(data)
data$year = year(data$datetime)
input = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
data = group(data, 10, method = "greedy")
data = data[data$.groups==j,]
#valid = valid[valid$datetime%!in%data$datetime,]
valid = data
sout = sqrt(data$s_ou_e)
sin = sqrt(data$s_in_e)
wout = data$w_ou_e^(-2/3)
win = data$w_in_e^(-2/3)
d_x_area_ou = data$dA_ou_e
d_x_area_in = data$dA_in_e
delta_s_q = data$dV#/86400#data$dQ
mean_dv = mean(abs(delta_s_q))
inflowAvg = mean(aggregate(data$Q_in, by = list(data$year), FUN = mean)$x)
outflowAvg = mean(aggregate(data$Q_out, by = list(data$year), FUN = mean)$x)

# mean_dv = mean(abs(data$dQ), na.rm = TRUE)
# start = Sys.time()
# ##Preprocess parts of the eqn to improve efficiency. 
# sin = sqrt(data$s_in)
# sout = sqrt(data$s_ou)
# win = data$w_in^(-2/3)
# wout = data$w_ou^(-2/3)

t = 1000
total = t
rows = 0
outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
colnames(outputs)= c("vals","ni", "ai","no","ao")
outputs$ni = c(.02,.08)#c(.02, .05)
outputs$no = c(.08,.08)#c(.02, .05)
outputs$ai = c(100,500)#c(100,300)
outputs$ao = c(100,1000)#c(200,500)#c(200,2000)
################################################################################################################################
##Manning's equation. 
################################################################################################################################
eqn1 = function(n, a, da, w, s){
  flow = (n^-1)*((a+da)^(5/3))*w*s
  return(flow)
}
################################################################################################################################
##Lakeflow equation. 
#Randomly estimate non SWOT observable parameters (ni, no, ai, ao) and keep a set of numbers (rw),
#that produce a mean d_s_q within a percentage (x) of the mean observed d_s_q.
################################################################################################################################
lakeflow = function(x, rw,y){
  t = rw
  outputs = get("outputs", environment())
  outputs = outputs#[1:t,]
  total = t
  rows = 0
  while(rows<total){
    ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
    no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
    ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
    ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
    inflow = eqn1(ni, ai, d_x_area_in, win, sin)
    outflow = eqn1(no, ao, d_x_area_ou, wout, sout)
    diff = inflow - outflow - data$ET + data$sum
    #vals = delta_s_q - diff
    vals = diff-delta_s_q
    vals = unlist(vals)
    #if(mean(abs(vals[delta_s_q!=0]/abs(delta_s_q[delta_s_q!=0])))<x){
    #if(mean(abs(vals))/mean_dv<x){
    d = vals/abs(data$dV)
    rmse = sqrt(mean(vals^2))
    months = data$month
    inf = mean(abs(inflow-inflowMonth[months])/inflowMonth[months])
    ouf = mean(abs(outflow-outflowMonth[months])/outflowMonth[months])
    
    rdiff = mean(abs(vals/delta_s_q))
    inf = mean(abs(inflow-inflowAvg))/inflowAvg
    ouf = mean(abs(outflow-outflowAvg))/outflowAvg
    if(rmse*inf*ouf<x){#rmse<x&mean(abs(inflow-inflowAvg))/inflowAvg<y&mean(abs(outflow-outflowAvg))/outflowAvg<y){
      #print(rows)
      outputs$vals[rows+1] <-rdiff#rmse#mean(abs(vals))/mean_dv
      outputs$ni[rows+1] = ni
      outputs$ai[rows+1] = ai
      outputs$no[rows+1] = no
      outputs$ao[rows+1] = ao
      outputs$inflow[rows+1] = mean(abs(inflow-inflowAvg))/inflowAvg
      outputs$outflow[rows+1] = mean(abs(outflow-outflowAvg))/outflowAvg
      outputs$comb[rows+1] = rmse*inf*ouf#outputs$inflow[rows+1]*outputs$outflow[rows+1]*outputs$vals[rows+1]
      rows = rows +1
    } else{next}
  }
  return(assign("outputs", as.data.frame(outputs), envir = .GlobalEnv))
}

################################################################################################################################
##Run lakeflow on a set of values where:
#values:x parameter. 
#rws: rw parameter. 
#As lakeflow runs, the random values of ni, no, ai, ao are created from the min/max of the previous iteration.  
#Processing time: ~0.5-1.5 minutes. 
################################################################################################################################
values = rep(100, 100)#rev(seq(10, 1000, 5))
rws = rep(t, length(values))#c(rep(t, length(values)-1), 50)
yvalues = rev(seq(.1, 10, length.out = length(values)))

values = rev(seq(.01, 10, .1))
rws = rep(t, length(values))
start = Sys.time()
all = list()
for(i in 1:length(values)){
  print(round(i/length(values)*100))
  outputs = try(withTimeout(lakeflow(values[i], rws[i]), timeout = 20))
  if(is.error(outputs)){
    break
  }
  outputs = as.data.frame(outputs)
  assign("outputs", as.data.frame(outputs), envir = .GlobalEnv)
  all[[i]] = outputs
}
end = Sys.time()
end-start
allVals = rbindlist(all)

# m = apply(allVals, 2, median)
# (m[2:5] - c(0.035,164,0.035, 299))/c(0.035, 164,0.035, 299)*100


v = allVals[order(allVals$comb)]
outputs = v[1:1000,]
allVals = outputs


# 
# val = quantile(outputs$vals, .5)
# yval = min(c(quantile(outputs$inflow, .5), quantile(outputs$outflow, .5)))
# 
# v = rep(val, 10)
# rw = rep(1000, 10)
# y = rep(yval, 10)
# 
# l = lapply(v, lakeflow, rw = rw, y = y)
# 
# 
# 
# 
# allVals = rbindlist(l)
mn = allVals[allVals$comb==min(allVals$comb),]
m = unlist(mn[1,])

inflow = eqn1(m[2], m[3], d_x_area_in, win, sin)
outflow = eqn1(m[4], m[5], d_x_area_ou, wout, sout)

nseInLk = NSE(inflow, valid$Q_in)
nseOuLk = NSE(outflow, valid$Q_out)

inDiff = nseInLk - nseIn
ouDiff = nseOuLk - nseOu
#ryan[[j]] = c(j, inDiff, ouDiff, nInflow, aInflow, nOutflow, aOutflow, m[2], m[3], m[4],m[5])
ryan[[j]] = cbind(valid$Q_in, valid$Q_out, inflow, outflow, inVals, outVals)
}
stopImplicitCluster()
df = as.data.frame(r)
apply(df, 2, median)
#Tends to improve inflow NSE by ~0.03 and outflow by ~0.3

plot(df$V1, type = "l")
points(df$inflow, col = "green", pch = 19)
points(df$inVals, col = "red", pch = 19)
plot(df$V1, df$inflow, xlim = c(100, 400), ylim = c(100,400))
points(df$V1, df$inVals, col= "red")

plot(df$V2, type = "l")
points(df$outflow, col = "green", pch = 19)
points(df$outVals, col = "red", pch = 19)

NSE(df$inflow, df$V1)
NSE(df$outflow, df$V2)

NSE(df$inVals, df$V1)
NSE(df$outVals, df$V2)



#####################################################################################################
##Add in errors 1 by 1 to see which causes the biggest decrease in performance. 
#####################################################################################################
ryan = list()
nseIn = list()
nseOu = list()
library(doParallel)
stopImplicitCluster()
registerDoParallel(6)
r = foreach(j = 7:13, .combine = rbind)%dopar%{
  library(data.table)
  library(dplyr)
  library(R.utils)
  library(hydroGOF)
  library(groupdata2)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  data = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_uncer_5yr_20210728.xlsx")
  synthetic = readxl::read_xlsx("E:\\research\\RivLake\\updated\\mohave_pseudo_5yr_20210723.xlsx")
  # synthetic = synthetic[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
  # data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
  
  names = colnames(data)
  colnames(synthetic) = names
  synthetic[,j] = data[,j]
  data = synthetic
  
  
  data = na.omit(data)
  data = group(data, 10, method = "greedy")
  #data = data[data$s_in_e>1.7e-5&data$s_ou_e>1.7e-5,]
  valid = data
  
  #set.seed(j)
  #data = sample_n(data, 10)
  data = data[data$.groups==1,]
  valid = valid[valid$datetime%!in%data$datetime,]
  #valid = valid[valid$.groups==j+1,]
  sout = sqrt(data$s_ou_e)
  sin = sqrt(data$s_in_e)
  wout = data$w_ou_e^(-2/3)
  win = data$w_in_e^(-2/3)
  d_x_area_ou = data$dA_ou_e
  d_x_area_in = data$dA_in_e
  delta_s_q = data$dQ#data$dV#/86400#data$dQ
  mean_dv = mean(abs(delta_s_q))

  t = 1000
  total = t
  rows = 0
  outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
  colnames(outputs)= c("vals","ni", "ai","no","ao")
  outputs$ni = c(.01,.07)#c(.02, .05)
  outputs$no = c(.01,.07)#c(.02, .05)
  outputs$ai = c(100,500)#c(100,300)
  outputs$ao = c(100,900)#c(200,500)#c(200,2000)
  ################################################################################################################################
  ##Manning's equation. 
  ################################################################################################################################
  eqn1 = function(n, a, da, w, s){
    flow = (n^-1)*((a+da)^(5/3))*w*s
    return(flow)
  }
  ################################################################################################################################
  ##Lakeflow equation. 
  #Randomly estimate non SWOT observable parameters (ni, no, ai, ao) and keep a set of numbers (rw),
  #that produce a mean d_s_q within a percentage (x) of the mean observed d_s_q.
  ################################################################################################################################
  lakeflow = function(x, rw){
    t = rw
    outputs = get("outputs", environment())
    outputs = outputs[1:t,]
    total = t
    rows = 0
    while(rows<total){
      ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
      no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
      ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
      ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
      inflow = eqn1(ni, ai, d_x_area_in, win, sin)
      outflow = eqn1(no, ao, d_x_area_ou, wout, sout)
      diff = inflow - outflow - data$ET# + data$sum
      vals = delta_s_q - diff
      vals = unlist(vals)
      #if(mean(abs(vals[delta_s_q!=0]/abs(delta_s_q[delta_s_q!=0])))<x){
      if(mean(abs(vals))/mean_dv<x){
        #print(rows)
        outputs$vals[rows+1] <- mean(abs(vals))/mean_dv
        outputs$ni[rows+1] = ni
        outputs$ai[rows+1] = ai
        outputs$no[rows+1] = no
        outputs$ao[rows+1] = ao
        rows = rows +1
      } else{next}
    }
    return(assign("outputs", as.data.frame(outputs), envir = .GlobalEnv))
  }
  
  ################################################################################################################################
  ##Run lakeflow on a set of values where:
  #values:x parameter. 
  #rws: rw parameter. 
  #As lakeflow runs, the random values of ni, no, ai, ao are created from the min/max of the previous iteration.  
  #Processing time: ~0.5-1.5 minutes. 
  ################################################################################################################################
  values = rev(seq(1, 2, .01))
  rws = rep(t, length(values))#c(rep(t, length(values)-1), 50)
  
  start = Sys.time()
  all = list()
  for(i in 1:length(values)){
    print(round(i/length(values)*100))
    outputs = try(withTimeout(lakeflow(values[i], rws[i]), timeout = 60))
    if(is.error(outputs)){
      break
    }
    #outputs = lakeflow(values[i], rws[i])
    outputs = as.data.frame(outputs)
    assign("outputs", as.data.frame(outputs), envir = .GlobalEnv)
    #all[[i]] = outputs[outputs$vals/mean_dv<1,]
    all[[i]] = outputs
  }
  end = Sys.time()
  end - start
  allVals = rbindlist(all)
  #apply(allVals[allVals$vals<1], 2, hist)
  apply(allVals, 2, median)
  
  m = apply(allVals, 2, median)
  (m[2:5] - c(0.035,164,0.035, 299))/c(0.035, 164,0.035, 299)*100
  
  ######Validate the inflow. 
  sout = sqrt(valid$s_ou_e)
  sin = sqrt(valid$s_in_e)
  wout = valid$w_ou_e^(-2/3)
  win = valid$w_in_e^(-2/3)
  d_x_area_ou = valid$dA_ou_e
  d_x_area_in = valid$dA_in_e
  
  inflow = eqn1(m[2], m[3], d_x_area_in, win, sin)
  outflow = eqn1(m[4], m[5], d_x_area_ou, wout, sout)
  
  # plot(valid$datetime, valid$Q_in, type = "l")
  # points(valid$datetime, inflow, col = "red")
  # legend("topleft",legend = NSE(inflow, valid$Q_in))
  
  nseIn[[j]] = NSE(inflow, valid$Q_in)
  nseOu[[j]] = NSE(outflow, valid$Q_out)
  ryan[[j]] = c(m, NSE(inflow, valid$Q_in), NSE(outflow, valid$Q_out))
}
stopImplicitCluster()
df = as.data.frame(r)
df$error = names[7:13]
df
df$inError = df$V6[nrow(df)] - df$V6
df$ouError = df$V7[nrow(df)] - df$V7


apply(df, 2, median)






























































################################################################################################################################
##Run this next chunk of code together all at once. 
################################################################################################################################
##How many observations. 
t = 200
total = t
rows = 0
outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
colnames(outputs)= c("vals","ni", "ai","no","ao")
while(rows<total){
  ni = signif(runif(1, 0.01, .1),2)
  no = signif(runif(1,0.01, .1),2)
  ai = round(runif(1, 200, 500))
  ao = round(runif(1, 200, 2000))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.35){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.25){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.15){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.1){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.075){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.05){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
t = 100
outputs = outputs[1:t,]
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.025){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.01){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.005){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
t = 10
outputs = outputs[1:t,]
total = t
rows = 0
while(rows<total){
  ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  if(mean(abs(vals))/mean_dv<.0025){
    print(rows)
    outputs$vals[rows+1] = mean(abs(vals))
    outputs$ni[rows+1] = ni
    outputs$ai[rows+1] = ai
    outputs$no[rows+1] = no
    outputs$ao[rows+1] = ao
    rows = rows +1
  } else{next}
}
#}, tm)
  end = Sys.time()
  start - end
  #times[i] = start-end
  #}
########################################################################################################################################
##Check out the outputs df, should be nearly all correct values. 
########################################################################################################################################
plot(outputs$ni)
plot(outputs$no)
plot(outputs$ai)
plot(outputs$ao)
#########################################################################################################################################
##Functions to do the above. Currently still developing, not refining properly. Something is wrong with the reassigning min/max ranges.
#########################################################################################################################################
##Preprocess parts of the eqn to improve efficiency. 
sin = sqrt(data$s_in)
sout = sqrt(data$s_ou)
win = data$w_in^(-2/3)
wout = data$w_ou^(-2/3)
start = Sys.time()
total = 100
rows = 0
min = 2
vals = 1
outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
colnames(outputs)= c("vals","ni", "ai","no","ao")
outputs$ni = c(0.01, .1)
outputs$no = c(0.01, .1)
outputs$ai = c(200, 2000)
outputs$ao = c(500,2000)
outputs$vals = 5
mean_dv = mean(abs(data$dQ), na.rm = TRUE)
rng = quantile(outputs$vals, probs = seq(0,1, .1))
mn = outputs[outputs$vals<=rng[2],]
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
mcFun = function(x){
  # ni = signif(runif(1, min(outputs$ni), max(outputs$ni)),2)
  # no = signif(runif(1, min(outputs$no), max(outputs$no)),2)
  # ai = round(runif(1, min(outputs$ai), max(outputs$ai)))
  # ao = round(runif(1, min(outputs$ao), max(outputs$ao)))
  ni = signif(runif(1, x[1], x[2]),2)
  no = signif(runif(1, x[3], x[4]),2)
  ai = round(runif(1, x[5], x[6]))
  ao = round(runif(1, x[7], x[8]))
  # ni = signif(runif(1, min(mn$ni), max(mn$ni)),2)
  # no = signif(runif(1, min(mn$no), max(mn$no)),2)
  # ai = round(runif(1, min(mn$ai), max(mn$ai)))
  # ao = round(runif(1, min(mn$ao), max(mn$ao)))
  inflow = eqn1(ni, ai, data$dA_in, win, sin)
  outflow = eqn1(no, ao, data$dA_ou, wout, sout)
  diff = inflow - outflow
  vals = data$dQ - diff
  return(list(mean(abs(vals)), ni, no, ai, ao))
}

refine = function(x){
  outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
  colnames(outputs)= c("vals","ni", "ai","no","ao")
  while(rows<total){
    out = mcFun(rn)
    if(out[[1]][1]/mean_dv<x){
      print(rows)
      outputs$vals[rows+1] = out[[1]][1]
      outputs$ni[rows+1] = out[[2]][1]
      outputs$ai[rows+1] = out[[4]][1]
      outputs$no[rows+1] = out[[3]][1]
      outputs$ao[rows+1] = out[[5]][1]
      rows = rows +1
    } else{next}
  }
  return(outputs)
}
outputs = refine(20)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = refine(15)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = refine(10)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = refine(7.5)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = refine(5)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = refine(2.5)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = refine(1)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = refine(.5)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))

end = Sys.time()
start - end

#################################################################
##A second undeveloped function. Doesn't seem to be refining properly, gets stuck around 2.5%. 
#################################################################
##Preprocess parts of the eqn to improve efficiency. 
sin = sqrt(data$s_in)
sout = sqrt(data$s_ou)
win = data$w_in^(-2/3)
wout = data$w_ou^(-2/3)
start = Sys.time()
total = 100
rows = 0
min = 2
vals = 1
outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
colnames(outputs)= c("vals","ni", "ai","no","ao")
outputs$ni = c(0.01, .1)
outputs$no = c(0.01, .1)
outputs$ai = c(200, 2000)
outputs$ao = c(500,2000)
outputs$vals = 5
mean_dv = mean(abs(data$dQ), na.rm = TRUE)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
mcFun = function(x, d){
  outputs = as.data.frame(matrix(numeric(), nrow =total, ncol = 5))
  colnames(outputs)= c("vals","ni", "ai","no","ao")
  repeat{
    ni = signif(runif(1, x[1], x[2]),2)
    no = signif(runif(1, x[3], x[4]),2)
    ai = round(runif(1, x[5], x[6]))
    ao = round(runif(1, x[7], x[8]))
    inflow = eqn1(ni, ai, data$dA_in, win, sin)
    outflow = eqn1(no, ao, data$dA_ou, wout, sout)
    diff = inflow - outflow
    vals = data$dQ - diff
    if(mean(abs(vals))/mean_dv<d){
      print(rows)
      outputs$vals[rows+1] = mean(abs(vals))
      outputs$ni[rows+1] = ni
      outputs$ai[rows+1] = ai
      outputs$no[rows+1] = no
      outputs$ao[rows+1] = ao
      rows = rows +1
    } else{next}
    if(rows = 100){break}
    } 
  return(outputs)
}


t = c(.35,.2, .15, .1, .05, .025, .01, .005, .0025)
sapply(t, mcFun, d = rn)


outputs = mcFun(rn, .35)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = mcFun(rn, .2)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = mcFun(rn, .15)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = mcFun(rn, .1)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = mcFun(rn, .05)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = mcFun(rn, .025)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = mcFun(rn, .01)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))
outputs = mcFun(rn, .005)
rn = as.vector(c(min(outputs$ni), max(outputs$ni),min(outputs$no), max(outputs$no),min(outputs$ai), max(outputs$ai),min(outputs$ao), max(outputs$ao)))





library(randomsearch)
fun = function(x){
  inflow = eqn1(x[1], x[3], data$dA_in, win, sin)
  outflow = eqn1(x[2], x[4], data$dA_ou, wout, sout)
  data$dQ = inflow -outflow
}


randomsearch(fun, max.evals = 10, lower = 0, upper = 2000, )


obj.fun = makeSingleObjectiveFunction(
  fn = function(x) eqn1(x[1], x[3], data$dA_in, win, sin) - eqn1(x[2], x[4], data$dA_ou, wout, sout)-data$dQ,
  par.set = makeNumericParamSet(len = 4, lower = c(0.01, 0.01, 200, 200), upper = c(.1,.1,500,2000)),
  minimize = TRUE)
res = randomsearch(obj.fun, max.evals = 1000)
output = as.data.frame(res)
optimize(f = function(x) eqn1(x[1], x[3], data$dA_in, win, sin) - eqn1(x[2], x[4], data$dA_ou, wout, sout)-data$dQ,
         interval = c(0,1000))
f = function(x) eqn1(x, y, dan, wn, sn)-dq
optim(.01:.1,fn = f,500:4000, dan = data$dA_in, wn = win, sn= sin,dq = data$Q_in ,control = list(maxit = 10000))
library(randomForest)
rf = randomForest(dQ~dA_in+w_in+s_in+dA_ou+w_ou+s_ou, data = data)
uniroot(f, interval = c(0,1))
optim()





################################################################
##Read in data. 
################################################################
# times = as.vector(1)
# tm = 45
# ##Function to cause a restart of the algorithm if it takes too long. Needed to account for instances where true observations are filtered out causing an issue in the final stages of refining. 
# try_with_time_limit <- function(expr, cpu = Inf, elapsed = Inf)
# {
#   y <- try({setTimeLimit(cpu, elapsed); expr}, silent = TRUE) 
#   if(inherits(y, "try-error")) repeat(expr) else y 
# }
# try_with_time_limit <- function(expr, cpu = Inf, elapsed = Inf)
# {
#   y <- try({setTimeLimit(cpu, elapsed);expr}, silent = TRUE) 
#   if(inherits(y, "try-error")) repeat(try_with_time_limit(expr, tm)) else y 
# }
# 
# for(i in 1:length(times)){
# try_with_time_limit(
#   for(j in 1:1){
data = readxl::read_xlsx("E:\\research\\RivLake\\mohave_pseudo_5yr_20210602.xlsx")
data$datetime = as.Date(data$datetime)
data = data[data$datetime>as.Date("2010-09-30"),]
data = sample_n(data, 10)

eqn1 = function(n, a, da, w, s){
  flow = (n^-1)*((a+da)^(5/3))*w*s
  return(flow)
}

mean_dv = mean(abs(data$dQ), na.rm = TRUE)
start = Sys.time()
##Preprocess parts of the eqn to improve efficiency. 
sin = sqrt(data$s_in)
sout = sqrt(data$s_ou)
win = data$w_in^(-2/3)
wout = data$w_ou^(-2/3)

outputs = as.data.frame(matrix(numeric(), nrow =200, ncol = 5))
colnames(outputs)= c("vals","ni", "ai","no","ao")
outputs$ni = c(0.01, .1)
outputs$no = c(0.01, .1)
outputs$ai = c(200, 2000)
outputs$ao = c(500,2000)
outputs$vals = 5
##########################################################################
fn = function(total, rows, val, nii, noo, aii, aoo){
  while(rows<total){
    ni = signif(runif(1, min(nii), max(nii)),2)
    no = signif(runif(1, min(noo), max(noo)),2)
    ai = round(runif(1, min(aii), max(aii)))
    ao = round(runif(1, min(aoo), max(aoo)))
    inflow = eqn1(ni, ai, data$dA_in, win, sin)
    outflow = eqn1(no, ao, data$dA_ou, wout, sout)
    diff = inflow - outflow
    vals = data$dQ - diff
    if(mean(abs(vals))/mean_dv<val){
      print(rows)
      #print(environment())
      ev = environment()
      outputs$vals[rows+1] = mean(abs(vals))
      outputs$ni[rows+1] = ni
      outputs$ai[rows+1] = ai
      outputs$no[rows+1] = no
      outputs$ao[rows+1] = ao
      assign("outputs", outputs, envir = ev)
      rows = rows +1
    } else{next}
  }
  return(assign("outputs", outputs, envir = globalenv()))
}


totalVals = c(200,200,200, 200, 200,200,200,100,100,10)
rowsVals = rep(0, length(totalVals))
thresholdVals =c(.35, .25, .15,.1,.075,.05,.025,.01,.005,.0025)
inputs = 1:length(totalVals)

ni = c(.01, .1)
no = c(.01, .1)
ai = c(200,2000)
ao = c(500,2000)

outputs = fn(totalVals[1], rowsVals[1], thresholdVals[1], ni, no, ai, ao)
outputs = fn(totalVals[2], rowsVals[2], thresholdVals[2], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = fn(totalVals[3], rowsVals[3], thresholdVals[3], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = fn(totalVals[4], rowsVals[4], thresholdVals[4], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = fn(totalVals[5], rowsVals[5], thresholdVals[5], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = fn(totalVals[6], rowsVals[6], thresholdVals[6], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = fn(totalVals[7], rowsVals[7], thresholdVals[7], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = fn(totalVals[8], rowsVals[8], thresholdVals[8], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = outputs[1:100,]
outputs = fn(totalVals[9], rowsVals[9], thresholdVals[9], outputs$ni, outputs$no, outputs$ai, outputs$ao)
outputs = fn(totalVals[10], rowsVals[10], thresholdVals[10], outputs$ni, outputs$no, outputs$ai, outputs$ao)








