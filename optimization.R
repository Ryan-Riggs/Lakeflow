library(dplyr)
################################################################
##Read in data. 
################################################################
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
################################################################################################################################
##Run this next chunk of code together all at once. 
################################################################################################################################
##How many observations. 
total = 100
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
total = 100
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
total = 100
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
total = 100
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
total = 100
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
total = 100
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

total = 100
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
total = 100
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
total = 100
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
total = 100
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
  end = Sys.time()
  start - end
  times[i] = start-end
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





















