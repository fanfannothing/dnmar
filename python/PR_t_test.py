import sys
import random
import math

pred1 = []
pred2 = []

def AUC(prec, rec):
    lastP = 1.0
    lastR = 0.0

    aucSum = 0.0
    
    for i in range(len(prec)):
        p = prec[i]
        r = rec[i]

        #if lastR - r > 0:
        if r - lastR > 0:
            aucSum += (p + lastP) * (r - lastR) / 2.0
        lastP = p
        lastR = r

    return aucSum

rank = 0
for line in open(sys.argv[1]):
    fields = line.strip().split("\t")
    #pred1.append((float(fields[0]), float(fields[1]), fields[3]))
    pred1.append((rank, fields[3], fields[4]))
    rank += 1

rank = 0
for line in open(sys.argv[2]):
    fields = line.strip().split("\t")
    #pred1.append((float(fields[0]), float(fields[1]), fields[3]))
    pred2.append((rank, fields[3], fields[4]))
    rank += 1

random.shuffle(pred1)
random.shuffle(pred2)

def Rank2PR(ranked):
    tp = 0.0
    fp = 0.0
    NP = len([x for x in ranked if x[1] == "true"])

    Plist = []
    Rlist = []
    
    for (rank, correct, sentence) in sorted(ranked, key=(lambda x: x[0])):
        if correct == "true":
            tp += 1.0
        else:
            fp += 1.0
        fn = NP - tp
        P = 0.0
        R = 0.0
        if tp > 0:
            P = tp / (tp + fp)
            R = tp / (tp + fn)

        Plist.append(P)
        Rlist.append(R)

    return (Plist, Rlist)

(p, r) = Rank2PR(pred1)
print AUC(p,r)



def AUC_paired_t_test(folds):
    p = []

    for fold in range(folds):
        start = int(len(pred1) / folds) * fold
        end   = int(len(pred1) / folds) * (fold+1) - 1

        test1 = pred1[start:end]
        test2 = []
        for pr in pred2:
            if pr[2] in [x[2] for x in test1]:
                test2.append(pr)

        (p1,r1) = Rank2PR(test1) #self.PlotPR(eval1, [float(x) * 0.05 for x in range(2000)], data=self.testSet[start:end])
        auc1 = AUC(p1,r1)
        (p2,r2) = Rank2PR(test2) #self.PlotPR(eval2, [float(x) * 0.05 for x in range(2000)], data=self.testSet[start:end])
        auc2 = AUC(p2,r2)

        print "%s\t%s" % (start, end)
        print "%s\t%s" % (auc1,auc2)

        p.append(auc1 - auc2)

    pBar = sum(p) / len(p)
    n    = len(p)
    var  = 0.0
    for pi in p:
        var += math.pow(pi - pBar, 2)

    return (pBar * math.sqrt(n) / math.sqrt(var / (n - 1)), p)

print AUC_paired_t_test(30)
