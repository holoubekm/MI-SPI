K = 5;
L = 6;

# 1. I.
n = K*20;
u = runif(n = n, min = 0, max = 1);
x = qexp(u, rate = L);

# 1. II.
hist(x = u, freq = FALSE);
hist(x = x, freq = FALSE);

xWidth = max(x) - min(x);
xGrid = seq(min(x) - 0.2 * xWidth, max(x) + 0.2 * xWidth, length = 30);
lines(xGrid, dexp(xGrid, rate = L), col='red', lw = 2, lty = 2);


# !. III.
plot(ecdf(x), verticals=TRUE, do.points = FALSE, main = "Empiricka distribucni funkce");
lines(xGrid, pexp(xGrid, rate = L), col='red', lw = 2, lty = 2);

# 1. IV.
y = rexp(1000, rate = L)
qqplot(x, y); abline(0,1, col='red', lwd=2);

# 1. V.


# 1. VI.
# H_0 data maji distribuci ~ exp(rate = L)
# H_a data nemaji tuto distribuci
# significance level = 0.05


prob.exp <- dexp(c(1:n), rate = L)
aaa=chisq.test(x, p=prob.exp, rescale.p = TRUE)

ret = ks.test(x, 'pexp', rate = L)$p.value
if(ret <= 0.05) {
  print('H_0 Rejected -> x doesn\'t have the exp distribution.');
} else {
  print("H_0 Can't be rejected, data may be exponential.");
}

return

# 2. I.
lambda = function(t) { 100 + 50/exp((t - 420)^2/(3600*L)) + 100/exp((L*(t - 480 - 30*L)^2)/360000)};

# 2. II.
t = 0;
i = 0;
totalArrivals = K*10;
arrivals = numeric(totalArrivals);
for(i in 1:totalArrivals) {
  t = t + rexp(1, rate = lambda(t));
  arrivals[i] = t;
}
yGrid = numeric(totalArrivals);
plot(arrivals, yGrid, type='p');


# 2. III.
totalTime = 24*60;
arrivals = numeric(totalTime);
while(t < totalTime) {
  t = t + rexp(1, rate = lambda(t));
  minute = t %/% 1;
  arrivals[minute] = arrivals[minute] + 1;
}
arrivals[totalTime] = arrivals[totalTime - 1];

xGrid = c(1:totalTime);
plot(xGrid, arrivals, type='l', col="grey");
curve(lambda, 1, totalTime, col='red', add=TRUE);

# 2. IV. 
# Diskuze kvality dat

# 3. I.
ratio_courier = K/(K+L);
ratio_state = 1-ratio_courier;

# 3. II.
arrivals_courier = numeric(totalTime);
arrivals_state = numeric(totalTime);
for (i in (1:totalTime)) {
  arrivals_courier[i] = ratio_courier * arrivals[i];
  arrivals_state[i] = ratio_state * arrivals[i];
}


# 3. III.
lambda_courier = function(t) { lambda(t) * ratio_courier };
lambda_state = function(t) { lambda(t) * ratio_state };

plot(xGrid, arrivals_courier, type='l', col="grey");
curve(lambda_courier, 1, totalTime, col='red', add=TRUE);

plot(xGrid, arrivals_state, type='l', col="grey");
curve(lambda_state, 1, totalTime, col='red', add=TRUE);

