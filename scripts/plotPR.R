files = list.files('../scala/experiments/dnmar_random20')
par(mfcol=c(4,6))
for(f in files) {
  if(f != 'log') {
    print(f)
    p1 = read.csv(paste('../scala/experiments/dnmar_random20/',f,sep=""), sep="\t", header=FALSE)
    p2 = read.csv(paste('../scala/experiments/multir/',f,sep=""), sep="\t", header=FALSE)
    plot(p1[,2], p1[,1], xlim=c(0,0.9), ylim=c(0,1), main=f, xlab='recall', ylab='precision')
    points(p2[,2], p2[,1], col='red')
  }
}
