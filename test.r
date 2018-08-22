
set.seed(5)
x1 <- rnorm(100)
x2 <- rnorm(100)
y1 <- rnorm(100, x1 * 0.8 -2, 0.8)

fit1 <- lm(y1 ~ x1+x2)
fit2 <- lm(y1 ~ x1)

logLik(fit1)
logLik(fit2)

n <- 10
k <- 4
logL <- -20

AIC1 <- function(logL, n, k) {
  -2*logL + 2*k*n/(n-k-1)
}

AIC2 <- function(logL, n, k) {
  (-2*logL + 2*k*n/(n-k-1))/n
}

a1 <- AIC1(logLik(fit1) %>% as.numeric, n = 100, k = 3)
a2 <- AIC1(logLik(fit2) %>% as.numeric, n = 100, k = 2)

a3 <- AIC2(logLik(fit1) %>% as.numeric, n = 100, k = 3)
a4 <- AIC2(logLik(fit2) %>% as.numeric, n = 100, k = 2)

exp(a1-a2)
exp(a3-a4)
