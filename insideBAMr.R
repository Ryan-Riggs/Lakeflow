#n = 0.032
print(sec_man_in, pars = c("logn_man","A0"))
n = 0.03438964
nQ = n*qpred_in$qpred
ws = data$w_in^(-2/3)*data$s_in^(.5)
comb = (nQ/ws)^3
crt = nthroot(comb, 5)
a = crt -data$dA_in

maybe_in = eqn(n, mean(a), data$dA_in, data$w_in, data$s_in)


#maybe_in = eqn(n, mean(c(745.41, 752.74))exp(), data$dA_in, data$w_in, data$s_in)
plot(maybe_in, data$Q_in, xlim = c(0,800), ylim = c(0,800))

KGE(maybe_in, data$Q_in)





print(sec_man, pars = c("logn_man", "logA_man"))
#n = 0.039
n = 0.02351775
nQ = n*qpred$qpred
ws = data$w_ou^(-2/3)*data$s_ou^(.5)
comb = (nQ/ws)^3
crt = nthroot(comb, 5)
a = crt -data$dA_ou
maybe = eqn(n, mean(a), data$dA_ou, data$w_ou, data$s_ou)
plot(maybe, data$Q_out, xlim = c(0,800), ylim = c(0,800))
KGE(maybe, data$Q_out)


new_diff = maybe_in-maybe

plot(data$dQ, new_diff, xlim = c(-300, 400), ylim = c(-300, 400), xlab = "Gauge dQ", ylab = "BAM dQ",col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), pch = 19)
abline(0,1, lty = 2)
box(lwd = 2)
r2 = signif(summary(lm(diff~data$dQ))$r.squared, 3)
legend("topleft", legend = (paste0("R2 = ", r2)), bty = "n")
#points(data$dQ, diff_wE, col = "red")
summary(lm(diff~data$dQ))


plot(eqn(.011, 999, data$dA_ou, data$w_ou, data$s_ou), data$Q_out)
points(eqn(.024, 1887, data$dA_ou, data$w_ou, data$s_ou), data$Q_out, col = "red")






