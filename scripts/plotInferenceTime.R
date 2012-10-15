d = read.csv('../scala/experiments/compareInfer/infer.out', sep="\t", header=TRUE)
dBig = d[d$nVars > 5,]
plot(dBig$nVars, jitter(dBig$time20rs), log="y", col='green', ylim=c(0.00001,10000))
points(dBig$nVars, jitter(dBig$timeExact), col='blue')
points(dBig$nVars, jitter(dBig$time1kBeam), col='red')
