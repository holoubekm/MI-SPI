# n = 20;
# alpha = 0.01;
# x = rnorm(n, mean=10.5, sd=1.3);
# 
# # 1.1
# hypothesisTest = t.test(x, mu=10, conf.level = 1-alpha);
# print(hypothesisTest);
# 
# #1.1.a
# avg = mean(x);
# stDev = sd(x);
# 
# critVal = qt(1-alpha/2, n-1, lower.tail = TRUE);
# lowBound = avg - (stDev * critVal)/sqrt(n);
# upBound = avg + (stDev * critVal)/sqrt(n);
# print((stDev * critVal)/sqrt(n))
# 
# #!.1.b
# #TODO if
# print(lowBound);
# print(upBound);
# 
# #1.1.c TODO if
# tVal = sqrt(n)*(avg-10)/stDev;
# critVal = qt(1-alpha/2, n-1, lower.tail = TRUE);
# 
# lowBound = pt(tVal, n-1, lower.tail = FALSE);
# upBound = pt(tVal, n-1, lower.tail = TRUE);
# pVal = 2*min(lowBound, upBound);
# print(2*lowBound);
# print(2*upBound);
# 
# #1.2
# #vybereme jednostranny t-test a cele to zopakujeme
# #Expiricky vime, ze avg = 10.5 a u = 10, proto je vyhodnejsi alternativa greater, protoze s danou pravdepodobnosti jsme schopni rict, ze hodnota je vetsi nez deset. Diky vlastnostem generovanych dat takto casteji zavrhneme H0 a pouzijeme Halph
# 
# n = 20;
# alpha = 0.01
# x = rnorm(n, mean=10, sd=1)
# error = rnorm(n, mean=0.5, sd=0.8306624)
# y = x + error
# 
# t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha)
# 

#2.1
# n = 20;
# alpha = 0.01
# x = rnorm(n, mean=10, sd=1)
# error = rnorm(n, mean=0.5, sd=0.8306624)
# y = x + error
# 
# t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha)
 

# n = 20;
# alpha = 0.01
# x = rnorm(n, mean=10, sd=1)
# error = rnorm(n, mean=0.5, sd=0.8306624)
# y = x + error
# 
# res = t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha)
# print(res)
# if(abs(res$statistic) > qt(1-alpha, n-1, lower.tail = TRUE)) {
#   print('Reject null hypothesis!')
# }


# 2.1.b
# res = t.test(x-y, alternative = "less", conf.level = 1-alpha)
# print(res)

#2.2
# n1 = 20;
# n2 = 25;
# alpha = 0.01
# x=rnorm(n1, mean=10, sd=1.3)
# y=rnorm(n2, mean=11.25, sd=1.3)
# res = t.test(x, y=y, paired = FALSE, var.equal = TRUE, alternative = "less", conf.level = 1-alpha)
# print(res)
# if(abs(res$statistic) > qt(1-alpha, n1+n2-2, lower.tail = TRUE)) {
#   print('Reject null hypothesis!')
# }


n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.28, sd=1.2)
res = t.test(x, y=y, paired = FALSE, var.equal = FALSE, alternative = "less", conf.level = 1-alpha)
print(res)
if(abs(res$statistic) > qt(1-alpha, n1+n2-2, lower.tail = TRUE)) {
  print('Reject null hypothesis!')
}


# 
# #2.2
# n1 = 20;
# n2 = 25;
# alpha = 0.01
# x=rnorm(n1, mean=10, sd=1.3)
# y=rnorm(n2, mean=11.25, sd=1.3)
# 
# res = t.test(x, y=y, paired = FALSE, var.equal = TRUE, alternative = "less", conf.level = 1-alpha)
# print(res);

df = n1 + n2 - 2;
sdiff = sqrt(var(x)/n1+var(y)/n2);
tVal = (mean(x)-mean(y))/(sdiff);
print(tVal)
if(abs(tVal) > qt(1-alpha, df, lower.tail = TRUE)) {
  print('Reject null hypothesis!')
}

# critVal = qt(1-alpha, df, lower.tail = TRUE);
#print(pt(tVal, df, lower.tail = TRUE))
#print(qt(res$p.value, df));

#2.3
# n1 = 20;
# n2 = 25;
# alpha = 0.01
# x=rnorm(n1, mean=10, sd=1.3)
# y=rnorm(n2, mean=11.28, sd=1.2)

# t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)
#zodpadkovat vsechny kroky
#bude nutne prepsat vypocet bodu 2.2 podle vzorce pro rozdilne rozptyly
#... kdyz se muze clovek rozptylovat, muze se i ptylovat?

#3.1.a,b,c
# sequenceLength = 4000000;
# x = runif(sequenceLength, 0, 100)
# print(system.time(sort(x)))

#pouzijeme hypotezu X1 < X2 ..tj, je algoritmus X1 v prumeru rychlejsi?
#
